use crate::{compile, CompileState, SloshVm};
use compile_state::state::SloshVmTrait;
use slvm::*;

fn make_math_comp(
    env: &mut SloshVm,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
    op: u8,
) -> VMResult<()> {
    if cdr.len() <= 1 {
        return Err(VMError::new_compile("Requires at least two arguments."));
    } else {
        let mut max = 0;
        for (i, v) in cdr.iter().enumerate() {
            max = result + i + 1;
            compile(env, state, *v, max)?;
        }
        state.chunk.encode3(
            op,
            result as u16,
            (result + 1) as u16,
            max as u16,
            env.own_line(),
        )?;
    }
    Ok(())
}

fn compile_inc_dec(
    env: &mut SloshVm,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
    opcode: OpCode,
) -> VMResult<()> {
    let mut global_slot = None;
    let dest = if let Value::Symbol(si) = cdr[0] {
        if let Some(idx) = state.get_symbol(si) {
            idx
        } else if let Some(slot) = env.global_intern_slot(si) {
            global_slot = Some(slot);
            state
                .chunk
                .encode_refi(result as u16, slot, env.own_line())?;
            result
        } else {
            let sym = env.get_interned(si);
            return Err(VMError::new_compile(format!(
                "Special {sym} not defined (maybe you need to use 'def {sym}' to pre-declare it)."
            )));
        }
    } else {
        return Err(VMError::new_compile("expected symbol"));
    };
    if cdr.len() == 1 {
        state
            .chunk
            .encode2(opcode, dest as u16, 1, env.own_line())?;
    } else if cdr.len() == 2 {
        let amount = match cdr[1] {
            Value::Byte(i) => i as u16,
            Value::Int(i) => {
                let i = from_i56(&i);
                if i >= 0 && i <= u16::MAX as i64 {
                    i as u16
                } else {
                    return Err(VMError::new_compile("second arg to large"));
                }
            }
            _ => return Err(VMError::new_compile("second arg must be integer")),
        };
        state
            .chunk
            .encode2(opcode, dest as u16, amount, env.own_line())?;
    } else {
        return Err(VMError::new_compile("malformed"));
    }
    if dest != result {
        // Make sure we produce something...
        state
            .chunk
            .encode2(MOV, result as u16, dest as u16, env.own_line())?;
    }
    if let Some(global_slot) = global_slot {
        state
            .chunk
            .encode_def(dest as u16, global_slot, env.own_line(), false)?;
    }
    Ok(())
}

pub(crate) fn compile_math(
    env: &mut SloshVm,
    state: &mut CompileState,
    car: Value,
    cdr: &[Value],
    result: usize,
) -> VMResult<bool> {
    match car {
        Value::Special(i) if i == env.specials().inc => {
            compile_inc_dec(env, state, cdr, result, INC)?;
        }
        Value::Special(i) if i == env.specials().dec => {
            compile_inc_dec(env, state, cdr, result, DEC)?;
        }
        Value::Special(i) if i == env.specials().add => {
            if cdr.is_empty() {
                compile(env, state, 0.into(), result)?;
            } else if cdr.len() == 1 {
                compile(env, state, cdr[0], result)?;
            } else {
                for (i, v) in cdr.iter().enumerate() {
                    if i > 0 {
                        compile(env, state, *v, result + 1)?;
                        state.chunk.encode2(
                            ADD,
                            result as u16,
                            (result + 1) as u16,
                            env.own_line(),
                        )?;
                    } else {
                        compile(env, state, *v, result)?;
                    }
                }
            }
        }
        Value::Special(i) if i == env.specials().sub => {
            if cdr.is_empty() {
                return Err(VMError::new_compile(
                    "Malformed -, requires at least one argument.",
                ));
            } else if cdr.len() == 1 {
                if let Ok(i) = cdr[0].get_int(env) {
                    // XXX TODO- handle int 64
                    compile(env, state, (-i).into(), result)?;
                } else if let Ok(f) = cdr[0].get_float(env) {
                    let var = (-f).into();
                    compile(env, state, var, result)?;
                }
            } else {
                for (i, v) in cdr.iter().enumerate() {
                    if i > 0 {
                        compile(env, state, *v, result + 1)?;
                        state.chunk.encode2(
                            SUB,
                            result as u16,
                            (result + 1) as u16,
                            env.own_line(),
                        )?;
                    } else {
                        compile(env, state, *v, result)?;
                    }
                }
            }
        }
        Value::Special(i) if i == env.specials().mul => {
            if cdr.is_empty() {
                compile(env, state, 1.into(), result)?;
            } else if cdr.len() == 1 {
                compile(env, state, cdr[0], result)?;
            } else {
                for (i, v) in cdr.iter().enumerate() {
                    if i > 0 {
                        compile(env, state, *v, result + 1)?;
                        state.chunk.encode2(
                            MUL,
                            result as u16,
                            (result + 1) as u16,
                            env.own_line(),
                        )?;
                    } else {
                        compile(env, state, *v, result)?;
                    }
                }
            }
        }
        Value::Special(i) if i == env.specials().div => {
            if cdr.len() <= 1 {
                return Err(VMError::new_compile(
                    "Malformed /, requires at least two arguments.",
                ));
            } else {
                for (i, v) in cdr.iter().enumerate() {
                    if i > 0 {
                        compile(env, state, *v, result + 1)?;
                        state.chunk.encode2(
                            DIV,
                            result as u16,
                            (result + 1) as u16,
                            env.own_line(),
                        )?;
                    } else {
                        compile(env, state, *v, result)?;
                    }
                }
            }
        }
        Value::Special(i) if i == env.specials().numeq => {
            make_math_comp(env, state, cdr, result, NUMEQ)?;
        }
        Value::Special(i) if i == env.specials().numlt => {
            make_math_comp(env, state, cdr, result, NUMLT)?;
        }
        Value::Special(i) if i == env.specials().numlte => {
            make_math_comp(env, state, cdr, result, NUMLTE)?;
        }
        Value::Special(i) if i == env.specials().numgt => {
            make_math_comp(env, state, cdr, result, NUMGT)?;
        }
        Value::Special(i) if i == env.specials().numgte => {
            make_math_comp(env, state, cdr, result, NUMGTE)?;
        }
        _ => return Ok(false),
    }
    Ok(true)
}
