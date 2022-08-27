use crate::compile::destructure::{DestructState, DestructType};
use crate::compile::util::get_args_iter;
use crate::pass1::pass1;
use crate::{compile, CompileEnvironment, CompileState};
use slvm::{VMError, VMResult, Value, CLOSE, CONST, JMPNU, MOV, SRET};
use std::sync::Arc;

fn mk_state(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    args: Value,
) -> VMResult<(CompileState, Vec<Value>, Vec<DestructType>)> {
    let line = env.own_line().unwrap_or(1);
    let mut new_state =
        CompileState::new_state(state.chunk.file_name, line, Some(state.symbols.clone()));
    env.set_line_val(&mut new_state, args);
    let args_iter: Vec<Value> = get_args_iter(env, args, "fn")?.collect();
    let mut opt = false;
    let mut rest = false;
    let mut opt_comps = Vec::new();
    let mut destructures = Vec::new();
    let mut next_is_opt = false;
    new_state.chunk.dbg_args = Some(Vec::new());
    let mut total_args = 0_usize;
    for a in args_iter {
        if next_is_opt {
            opt_comps.pop();
            opt_comps.push(a);
            next_is_opt = false;
            continue;
        }
        match a {
            Value::Symbol(i) => {
                if i == env.specials().rest {
                    rest = true;
                } else if i == env.specials().optional {
                    opt = true;
                } else {
                    new_state.symbols.borrow_mut().insert(i);
                    if let Some(dbg_args) = new_state.chunk.dbg_args.as_mut() {
                        dbg_args.push(i);
                    }
                    if opt {
                        new_state.chunk.opt_args += 1;
                        opt_comps.push(Value::Nil);
                    } else {
                        new_state.chunk.args += 1;
                    }
                    total_args += 1;
                }
            }
            Value::Keyword(i) if i == env.specials().numeq => {
                if !opt {
                    return Err(VMError::new_compile(
                        "invalid args, := must come after % (optional)",
                    ));
                }
                if next_is_opt {
                    return Err(VMError::new_compile("invalid args, := := invalid"));
                }
                next_is_opt = true;
            }
            Value::Vector(handle) => {
                new_state.symbols.borrow_mut().reserve_reg();
                if let Some(dbg_args) = new_state.chunk.dbg_args.as_mut() {
                    dbg_args.push(env.specials().scratch);
                }
                if opt {
                    new_state.chunk.opt_args += 1;
                    opt_comps.push(Value::Nil);
                } else {
                    new_state.chunk.args += 1;
                }
                total_args += 1;
                destructures.push(DestructType::Vector(handle, total_args));
            }
            Value::Map(handle) => {
                new_state.symbols.borrow_mut().reserve_reg();
                if let Some(dbg_args) = new_state.chunk.dbg_args.as_mut() {
                    dbg_args.push(env.specials().scratch);
                }
                if opt {
                    new_state.chunk.opt_args += 1;
                    opt_comps.push(Value::Nil);
                } else {
                    new_state.chunk.args += 1;
                }
                total_args += 1;
                destructures.push(DestructType::Map(handle, total_args));
            }
            _ => return Err(VMError::new_compile("invalid args, must be symbols")),
        }
    }
    new_state.chunk.rest = rest;
    Ok((new_state, opt_comps, destructures))
}

pub(crate) fn compile_fn(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    args: Value,
    cdr: &[Value],
    result: usize,
    is_macro: bool,
) -> VMResult<()> {
    let (mut new_state, opt_comps, destructure_patterns) = mk_state(env, state, args)?;
    for r in cdr.iter() {
        pass1(env, &mut new_state, *r).unwrap();
    }
    let reserved = new_state.reserved_regs();
    for (i, r) in opt_comps.into_iter().enumerate() {
        let target_reg = new_state.chunk.args as usize + i + 1;
        new_state
            .chunk
            .encode1(JMPNU, target_reg as u16, env.own_line())?;
        let encode_offset = new_state.chunk.code.len();
        new_state.chunk.encode_jump_offset(0)?;
        let start_offset = new_state.chunk.code.len();
        compile(env, &mut new_state, r, reserved)?;
        new_state
            .chunk
            .encode2(MOV, target_reg as u16, reserved as u16, env.own_line())?;
        new_state.chunk.reencode_jump_offset(
            encode_offset,
            (new_state.chunk.code.len() - start_offset) as i32,
        )?;
    }
    let mut destruct_state = DestructState::new();
    for destruct_type in destructure_patterns {
        destruct_state.do_destructure(env, &mut new_state, destruct_type)?;
        let mut free_reg = new_state.reserved_regs();
        destruct_state.compile(env, &mut new_state, &mut free_reg)?;
    }
    let reserved = new_state.reserved_regs();
    let last_thing = cdr.len() - 1;
    for (i, r) in cdr.iter().enumerate() {
        if i == last_thing {
            new_state.tail = true;
        }
        compile(env, &mut new_state, *r, reserved)?;
    }
    new_state
        .chunk
        .encode1(SRET, reserved as u16, env.own_line())
        .unwrap();
    let mut closure = false;
    if !new_state.symbols.borrow().captures.borrow().is_empty() {
        let mut caps = Vec::new();
        for (_, _, c) in new_state.symbols.borrow().captures.borrow().iter() {
            caps.push(*c as u32);
        }
        new_state.chunk.captures = Some(caps);
        closure = true;
    }
    new_state.chunk.input_regs = reserved;
    new_state.chunk.extra_regs = new_state.max_regs - reserved;
    env.vm_mut().pause_gc();
    let lambda = env.vm_mut().alloc_lambda(Arc::new(new_state.chunk));
    env.vm_mut().unpause_gc();
    if is_macro {
        // Unwrap safe since we just allocated lambda on the heap.
        env.vm_mut()
            .set_heap_property(lambda.get_handle().unwrap(), ":macro", Value::True);
    }
    let const_i = state.add_constant(lambda);
    state
        .chunk
        .encode2(CONST, result as u16, const_i as u16, env.own_line())?;
    if closure {
        state
            .chunk
            .encode2(CLOSE, result as u16, result as u16, env.own_line())?;
    }
    Ok(())
}
