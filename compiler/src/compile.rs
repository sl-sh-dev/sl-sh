use slvm::error::*;
use slvm::opcodes::*;
use slvm::value::*;
use slvm::Handle;

use crate::backquote::*;
use crate::compile::compile_call::{
    compile_call, compile_call_myself, compile_call_reg, compile_callg,
};
use crate::compile::compile_cond::{compile_and, compile_if, compile_or, compile_while};
use crate::compile::compile_fn::compile_fn;
use crate::compile::compile_let::compile_let;
use crate::compile::compile_math::compile_math;
use crate::compile::compile_seq::{compile_cons, compile_vec};
use crate::compile::compile_store::{compile_def, compile_set};
use crate::pass1::pass1;
use compile_state::state::*;

mod compile_call;
mod compile_cond;
pub mod compile_fn;
mod compile_let;
mod compile_math;
mod compile_seq;
mod compile_store;
mod destructure;
mod util;

fn is_macro(env: &SloshVm, val: Value) -> bool {
    match val {
        Value::Lambda(_) => matches!(env.get_heap_property(val, ":macro"), Some(Value::True)),
        Value::Closure(_) => matches!(env.get_heap_property(val, ":macro"), Some(Value::True)),
        _ => false,
    }
}

fn compile_list(
    env: &mut SloshVm,
    state: &mut CompileState,
    car: Value,
    cdr: &[Value],
    result: usize,
) -> VMResult<()> {
    if !(compile_math(env, state, car, cdr, result)?
        || compile_cons(env, state, car, cdr, result)?
        || compile_vec(env, state, car, cdr, result)?)
    {
        match car {
            Value::Symbol(i) if i == env.specials().doc_string => {
                if cdr.len() == 1 {
                    state.doc_string = Some(cdr[0]);
                    return Ok(());
                } else {
                    return Err(VMError::new_compile("Malformed doc-string form."));
                }
            }
            Value::Symbol(i) if i == env.specials().fn_ => {
                if cdr.len() > 1 {
                    compile_fn(env, state, cdr[0], &cdr[1..], result, false)?
                } else {
                    return Err(VMError::new_compile("Malformed fn form."));
                }
            }
            Value::Symbol(i) if i == env.specials().mac_ => {
                if cdr.len() > 1 {
                    compile_fn(env, state, cdr[0], &cdr[1..], result, true)?
                } else {
                    return Err(VMError::new_compile("Malformed macro form."));
                }
            }
            Value::Symbol(i) if i == env.specials().if_ => {
                compile_if(env, state, cdr, result)?;
            }
            Value::Symbol(i) if i == env.specials().while_ => {
                let tail = state.tail;
                state.tail = false;
                compile_while(env, state, cdr, result)?;
                state.tail = tail;
            }
            Value::Symbol(i) if i == env.specials().do_ => {
                let last_thing = cdr.len() - 1;
                let old_tail = state.tail;
                state.tail = false;
                for (i, r) in cdr.iter().enumerate() {
                    if i == last_thing {
                        state.tail = old_tail;
                    }
                    compile(env, state, *r, result)?;
                }
            }
            Value::Symbol(i) if i == env.specials().def => {
                state.tail = false;
                compile_def(env, state, cdr, result)?;
            }
            Value::Symbol(i) if i == env.specials().set => {
                state.tail = false;
                compile_set(env, state, cdr, result)?;
            }
            Value::Symbol(i) if i == env.specials().quote => {
                state.tail = false;
                if cdr.len() != 1 {
                    return Err(VMError::new_compile(format!(
                        "quote takes one argument, got {}, line {}",
                        cdr.len(),
                        env.line_num()
                    )));
                }
                mkconst(env, state, cdr[0], result)?;
            }
            Value::Symbol(i) if i == env.specials().backquote => {
                state.tail = false;
                if cdr.len() != 1 {
                    return Err(VMError::new_compile(format!(
                        "backquote takes one argument, got {}, line {}",
                        cdr.len(),
                        env.line_num()
                    )));
                }
                backquote(env, state, cdr[0], result)?;
            }
            Value::Symbol(i) if i == env.specials().recur => {
                compile_call_myself(env, state, cdr, result, true)?
            }
            Value::Symbol(i) if i == env.specials().this_fn => {
                compile_call_myself(env, state, cdr, result, false)?
            }
            Value::Symbol(i) if i == env.specials().eq => {
                if cdr.len() <= 1 {
                    return Err(VMError::new_compile("Requires at least two arguments."));
                } else {
                    let mut max = 0;
                    for (i, v) in cdr.iter().enumerate() {
                        compile(env, state, *v, result + i + 1)?;
                        max = result + i + 1;
                    }
                    state.chunk.encode3(
                        EQ,
                        result as u16,
                        (result + 1) as u16,
                        max as u16,
                        env.own_line(),
                    )?;
                }
            }
            Value::Symbol(i) if i == env.specials().equal => {
                if cdr.len() <= 1 {
                    return Err(VMError::new_compile("Requires at least two arguments. 2"));
                } else {
                    let mut max = 0;
                    for (i, v) in cdr.iter().enumerate() {
                        compile(env, state, *v, result + i + 1)?;
                        max = result + i + 1;
                    }
                    state.chunk.encode3(
                        EQUAL,
                        result as u16,
                        (result + 1) as u16,
                        max as u16,
                        env.own_line(),
                    )?;
                }
            }
            Value::Symbol(i) if i == env.specials().type_ => {
                if cdr.len() != 1 {
                    return Err(VMError::new_compile("Requires one argument."));
                } else {
                    compile(env, state, cdr[0], result + 1)?;
                    state.chunk.encode2(
                        TYPE,
                        result as u16,
                        (result + 1) as u16,
                        env.own_line(),
                    )?;
                }
            }
            Value::Symbol(i) if i == env.specials().not => {
                if cdr.len() != 1 {
                    return Err(VMError::new_compile("Requires one argument."));
                } else {
                    compile(env, state, cdr[0], result + 1)?;
                    state
                        .chunk
                        .encode2(NOT, result as u16, (result + 1) as u16, env.own_line())?;
                }
            }
            Value::Symbol(i) if i == env.specials().err => {
                let len = cdr.len();
                if len != 1 && len != 2 {
                    return Err(VMError::new_compile("Requires one or two arguments."));
                } else {
                    if len == 2 {
                        compile(env, state, cdr[0], result)?;
                        compile(env, state, cdr[1], result + 1)?;
                    } else {
                        let error = env.intern("error");
                        compile(env, state, Value::Keyword(error), result)?;
                        compile(env, state, cdr[0], result + 1)?;
                    }
                    state
                        .chunk
                        .encode2(ERR, result as u16, (result + 1) as u16, env.own_line())?;
                }
            }
            Value::Symbol(i) if i == env.specials().and => {
                compile_and(env, state, cdr, result)?;
            }
            Value::Symbol(i) if i == env.specials().or => {
                compile_or(env, state, cdr, result)?;
            }
            Value::Symbol(i) if i == env.specials().str_ => {
                let mut max = 0;
                for (i, v) in cdr.iter().enumerate() {
                    compile(env, state, *v, result + i + 1)?;
                    max = result + i + 1;
                }
                state.chunk.encode3(
                    STR,
                    result as u16,
                    (result + 1) as u16,
                    max as u16,
                    env.own_line(),
                )?;
            }
            Value::Symbol(i) if i == env.specials().let_ => {
                compile_let(env, state, cdr, result)?;
            }
            Value::Symbol(i) if i == env.specials().call_cc => {
                if cdr.len() != 1 {
                    return Err(VMError::new_compile("Requires one argument."));
                }
                compile(env, state, cdr[0], result)?;
                state
                    .chunk
                    .encode2(CCC, result as u16, result as u16, env.own_line())?;
            }
            Value::Symbol(i) if i == env.specials().defer => {
                if !cdr.is_empty() {
                    compile_fn(env, state, Value::Nil, &cdr[0..], result, false)?;
                    state.chunk.encode1(DFR, result as u16, env.own_line())?;
                    state.defers += 1;
                } else {
                    return Err(VMError::new_compile(
                        "Malformed defer form, need at least one form.",
                    ));
                }
            }
            Value::Symbol(i) if i == env.specials().on_error => {
                if cdr.len() != 1 {
                    return Err(VMError::new_compile("Requires one argument."));
                }
                compile(env, state, cdr[0], result)?;
                state.chunk.encode1(ONERR, result as u16, env.own_line())?;
            }
            Value::Symbol(i) => {
                if let Some(idx) = state.get_symbol(i) {
                    compile_call_reg(env, state, idx as u16, cdr, result)?
                } else if let Some(slot) = env.global_intern_slot(i) {
                    // Have to at least pre-declare a global.
                    let global = env.get_global(slot);
                    if let Value::Undefined = global {
                        eprintln!("Warning: {} not defined.", env.get_interned(i));
                    }
                    if is_macro(env, global) {
                        let (mac, caps) = match global {
                            Value::Lambda(h) => (env.get_lambda(h), None),
                            Value::Closure(h) => {
                                let (mac, caps) = env.get_closure(h);
                                // Closures are read only so lets just break the lifetime away vs
                                // allocate the same thing again...
                                let caps = unsafe { (caps as *const [Handle]).as_ref().unwrap() };
                                (mac, Some(caps))
                            }
                            _ => panic!("Invalid macro!"),
                        };
                        env.pause_gc();
                        let exp = env.do_call(mac, cdr, caps)?;
                        env.unpause_gc();
                        pass1(env, state, exp)?;
                        compile(env, state, exp, result)?
                    } else {
                        compile_callg(env, state, slot, cdr, result)?
                    }
                } else {
                    let sym = env.get_interned(i);
                    return Err(VMError::new_compile(format!("Symbol {sym} not defined (maybe you need to use 'def {sym}' to pre-declare it).")));
                }
            }
            Value::Builtin(builtin) => {
                compile_call(env, state, Value::Builtin(builtin), cdr, result)?
            }
            Value::Lambda(h) => compile_call(env, state, Value::Lambda(h), cdr, result)?,
            Value::Pair(_) | Value::List(_, _) => {
                let (ncar, ncdr) = car.get_pair(env).expect("Pair/List not a Pair or List?");
                env.set_line_val(state, car);
                if let Value::List(h, idx) = ncdr {
                    // This unsafe should be fine (it breaks the lifetime away from env) since the
                    // vector that backs a list is read only.
                    // Do this to avoid a useless allocation in the common case (see the code below).
                    let ncdr = unsafe {
                        (&env.get_vector(h)[idx as usize..] as *const [Value])
                            .as_ref()
                            .unwrap()
                    };
                    compile_list(env, state, ncar, ncdr, result)?;
                } else {
                    let ncdr: Vec<Value> = ncdr.iter(env).collect();
                    compile_list(env, state, ncar, &ncdr[..], result)?;
                }
                compile_call_reg(env, state, result as u16, cdr, result)?
            }
            _ => {
                println!("Boo, {}", car.display_value(env));
            }
        }
    }
    state.doc_string = None;
    Ok(())
}

pub fn mkconst(
    env: &mut SloshVm,
    state: &mut CompileState,
    exp: Value,
    result: usize,
) -> VMResult<()> {
    match exp {
        Value::True => state.chunk.encode1(REGT, result as u16, env.own_line())?,
        Value::False => state.chunk.encode1(REGF, result as u16, env.own_line())?,
        Value::Nil => state.chunk.encode1(REGN, result as u16, env.own_line())?,
        Value::Undefined => state.chunk.encode1(REGC, result as u16, env.own_line())?,
        Value::Byte(i) => state
            .chunk
            .encode2(REGB, result as u16, i as u16, env.own_line())?,
        Value::Int32(i) if i >= 0 && i <= u16::MAX as i32 => {
            state
                .chunk
                .encode2(REGI, result as u16, i as u16, env.own_line())?;
        }
        Value::UInt32(i) if i <= u16::MAX as u32 => {
            state
                .chunk
                .encode2(REGU, result as u16, i as u16, env.own_line())?;
        }
        _ => {
            let const_i = state.add_constant(exp);
            state
                .chunk
                .encode2(CONST, result as u16, const_i as u16, env.own_line())?;
        }
    }
    Ok(())
}

pub fn compile(
    env: &mut SloshVm,
    state: &mut CompileState,
    exp: Value,
    result: usize,
) -> VMResult<()> {
    if state.max_regs < result {
        state.max_regs = result;
    }
    match exp {
        Value::Pair(_) | Value::List(_, _) => {
            let (car, cdr) = exp.get_pair(env).expect("Pair/List not a Pair or List?");
            env.set_line_val(state, exp);
            if let Value::List(h, idx) = cdr {
                // This unsafe should be fine (it breaks the lifetime away from env) since the
                // vector that backs a list is read only.
                // Do this to avoid a useless allocation in the common case (see the code below).
                let cdr = unsafe {
                    (&env.get_vector(h)[idx as usize..] as *const [Value])
                        .as_ref()
                        .unwrap()
                };
                compile_list(env, state, car, cdr, result)?;
            } else {
                let cdr: Vec<Value> = cdr.iter(env).collect();
                compile_list(env, state, car, &cdr[..], result)?;
            }
        }
        Value::Symbol(i) => {
            if let Some(idx) = state.get_symbol(i) {
                if result != idx {
                    state
                        .chunk
                        .encode2(MOV, result as u16, idx as u16, env.own_line())?;
                }
            } else if let Some(slot) = env.global_intern_slot(i) {
                state
                    .chunk
                    .encode_refi(result as u16, slot, env.own_line())?;
            } else {
                let sym = env.get_interned(i);
                return Err(VMError::new_compile(format!("Symbol {sym} not defined (maybe you need to use 'def {sym}' to pre-declare it).")));
            }
        }
        Value::True => state.chunk.encode1(REGT, result as u16, env.own_line())?,
        Value::False => state.chunk.encode1(REGF, result as u16, env.own_line())?,
        Value::Nil => state.chunk.encode1(REGN, result as u16, env.own_line())?,
        Value::Undefined => state.chunk.encode1(REGC, result as u16, env.own_line())?,
        Value::Byte(i) => state
            .chunk
            .encode2(REGB, result as u16, i as u16, env.own_line())?,
        Value::Int32(i) if i >= 0 && i <= u16::MAX as i32 => {
            state
                .chunk
                .encode2(REGI, result as u16, i as u16, env.own_line())?
        }
        Value::UInt32(i) if i <= u16::MAX as u32 => {
            state
                .chunk
                .encode2(REGU, result as u16, i as u16, env.own_line())?
        }
        _ => {
            let const_i = state.add_constant(exp);
            state
                .chunk
                .encode2(CONST, result as u16, const_i as u16, env.own_line())?;
        }
    }
    state.chunk.input_regs = 0;
    state.chunk.extra_regs = state.max_regs;
    Ok(())
}

#[cfg(test)]
mod compile_tests;
