use crate::compile::destructure::{DestructState, DestructType, resolve_destruct_containers};
use crate::compile::util::get_args_iter;
use crate::pass1::pass1;
use crate::{CompileState, SloshVm, compile};
use compile_state::state::SloshVmTrait;
use slvm::{CLOSE, CONST, JMPNU, MOV, SRET, VMError, VMResult, Value};
use std::sync::Arc;

pub fn mk_state(
    env: &mut SloshVm,
    state: &mut CompileState,
    args: Value,
) -> VMResult<(CompileState, Vec<Value>, Vec<DestructType>)> {
    let line = env.own_line().unwrap_or(1);
    let mut new_state =
        CompileState::new_state(state.chunk.file_name, line, Some(state.symbols.clone()));
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
        let a = resolve_destruct_containers(env, a);
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
            Value::Keyword(i) if i == env.specials().equal => {
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
            _ => {
                return Err(VMError::new_compile(format!(
                    "invalid args, must be symbols got {}/{}",
                    a.display_type(env),
                    a.display_value(env)
                )));
            }
        }
    }
    new_state.chunk.rest = rest;
    Ok((new_state, opt_comps, destructures))
}

pub(crate) fn compile_fn(
    env: &mut SloshVm,
    state: &mut CompileState,
    args: Value,
    cdr: &[Value],
    result: usize,
    is_macro: bool,
) -> VMResult<()> {
    let (mut new_state, opt_comps, destructure_patterns) = mk_state(env, state, args)?;
    for r in cdr.iter() {
        pass1(env, &mut new_state, *r)?;
    }
    let reserved = new_state.reserved_regs();
    for (i, r) in opt_comps.into_iter().enumerate() {
        let target_reg = new_state.chunk.args as usize + i + 1;
        let jmp_idx = new_state.chunk.add_jump(0);
        new_state
            .chunk
            .encode2(JMPNU, target_reg as u16, jmp_idx as u16, env.own_line())?;
        compile(env, &mut new_state, r, reserved)?;
        new_state
            .chunk
            .encode2(MOV, target_reg as u16, reserved as u16, env.own_line())?;
        new_state
            .chunk
            .update_jump(jmp_idx, new_state.chunk.code.len() as u32);
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
        .encode1(SRET, reserved as u16, env.own_line())?;
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
    env.pause_gc();
    let lambda = env.alloc_lambda(Arc::new(new_state.chunk));
    env.unpause_gc();
    if is_macro {
        // Unwrap safe since we just allocated lambda on the heap.
        env.set_heap_property(lambda, ":macro", Value::True);
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
