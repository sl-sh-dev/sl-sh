use crate::{compile, CompileState, SloshVm};
use compile_state::state::SloshVmTrait;
use slvm::*;

pub(crate) fn compile_cons(
    env: &mut SloshVm,
    state: &mut CompileState,
    car: Value,
    cdr: &[Value],
    result: usize,
) -> VMResult<bool> {
    match car {
        Value::Special(i) if i == env.specials().list => {
            state.tail = false;
            let mut max = 0;
            for r in cdr {
                compile(env, state, *r, result + max + 1)?;
                max += 1;
            }
            state.chunk.encode3(
                LIST,
                result as u16,
                (result + 1) as u16,
                (result + max) as u16,
                env.own_line(),
            )?;
        }
        Value::Special(i) if i == env.specials().list_append => {
            state.tail = false;
            let mut max = 0;
            for r in cdr {
                compile(env, state, *r, result + max + 1)?;
                max += 1;
            }
            state.chunk.encode3(
                APND,
                result as u16,
                (result + 1) as u16,
                (result + max) as u16,
                env.own_line(),
            )?;
        }
        Value::Special(i) if i == env.specials().cons => {
            state.tail = false;
            if cdr.len() != 2 {
                return Err(VMError::new_compile(format!(
                    "takes two arguments, got {}, line {}",
                    cdr.len(),
                    env.line_num()
                )));
            }
            compile(env, state, cdr[0], result)?;
            compile(env, state, cdr[1], result + 1)?;
            state.chunk.encode3(
                CONS,
                result as u16,
                result as u16,
                (result + 1) as u16,
                env.own_line(),
            )?;
        }
        Value::Special(i) if i == env.specials().car => {
            state.tail = false;
            if cdr.len() != 1 {
                return Err(VMError::new_compile(format!(
                    "takes one argument, got {}, line {}",
                    cdr.len(),
                    env.line_num()
                )));
            }
            compile(env, state, cdr[0], result + 1)?;
            state
                .chunk
                .encode2(CAR, result as u16, (result + 1) as u16, env.own_line())?;
        }
        Value::Special(i) if i == env.specials().cdr => {
            state.tail = false;
            if cdr.len() != 1 {
                return Err(VMError::new_compile(format!(
                    "takes one argument, got {}, line {}",
                    cdr.len(),
                    env.line_num()
                )));
            }
            compile(env, state, cdr[0], result + 1)?;
            state
                .chunk
                .encode2(CDR, result as u16, (result + 1) as u16, env.own_line())?;
        }
        Value::Special(i) if i == env.specials().xar => {
            state.tail = false;
            if cdr.len() != 2 {
                return Err(VMError::new_compile(format!(
                    "takes two arguments, got {}, line {}",
                    cdr.len(),
                    env.line_num()
                )));
            }
            compile(env, state, cdr[0], result)?;
            compile(env, state, cdr[1], result + 1)?;
            state
                .chunk
                .encode2(XAR, result as u16, (result + 1) as u16, env.own_line())?;
        }
        Value::Special(i) if i == env.specials().xdr => {
            state.tail = false;
            if cdr.len() != 2 {
                return Err(VMError::new_compile(format!(
                    "takes two arguments, got {}, line {}",
                    cdr.len(),
                    env.line_num()
                )));
            }
            compile(env, state, cdr[0], result)?;
            compile(env, state, cdr[1], result + 1)?;
            state
                .chunk
                .encode2(XDR, result as u16, (result + 1) as u16, env.own_line())?;
        }
        _ => return Ok(false),
    }
    Ok(true)
}

pub(crate) fn compile_vec(
    env: &mut SloshVm,
    state: &mut CompileState,
    car: Value,
    cdr: &[Value],
    result: usize,
) -> VMResult<bool> {
    match car {
        Value::Special(i) if i == env.specials().make_hash => {
            state.tail = false;
            let mut max = 0;
            for r in cdr {
                compile(env, state, *r, result + max + 1)?;
                max += 1;
            }
            state.chunk.encode3(
                MAPMK,
                result as u16,
                (result + 1) as u16,
                (result + max + 1) as u16,
                env.own_line(),
            )?;
        }
        Value::Special(i) if i == env.specials().vec => {
            state.tail = false;
            let mut max = 0;
            for r in cdr {
                compile(env, state, *r, result + max + 1)?;
                max += 1;
            }
            state.chunk.encode3(
                VEC,
                result as u16,
                (result + 1) as u16,
                (result + max + 1) as u16,
                env.own_line(),
            )?;
        }
        Value::Special(i) if i == env.specials().make_vec => {
            state.tail = false;
            if cdr.is_empty() {
                state.chunk.encode3(
                    VEC,
                    result as u16,
                    result as u16,
                    result as u16,
                    env.own_line(),
                )?;
            } else if cdr.len() == 1 {
                compile(env, state, cdr[0], result + 1)?;
                state
                    .chunk
                    .encode2(VECMK, result as u16, (result + 1) as u16, env.own_line())?;
            } else if cdr.len() == 2 {
                compile(env, state, cdr[0], result + 1)?;
                compile(env, state, cdr[1], result + 2)?;
                state.chunk.encode3(
                    VECMKD,
                    result as u16,
                    (result + 1) as u16,
                    (result + 2) as u16,
                    env.own_line(),
                )?;
            } else {
                return Err(VMError::new_compile(format!(
                    "takes up to two arguments, got {}, line {}",
                    cdr.len(),
                    env.line_num()
                )));
            }
        }
        Value::Special(i) if i == env.specials().vec_push => {
            state.tail = false;
            if cdr.len() != 2 {
                return Err(VMError::new_compile(format!(
                    "takes two arguments, got {}, line {}",
                    cdr.len(),
                    env.line_num()
                )));
            }
            compile(env, state, cdr[0], result)?;
            compile(env, state, cdr[1], result + 1)?;
            state
                .chunk
                .encode2(VECPSH, result as u16, (result + 1) as u16, env.own_line())?;
        }
        Value::Special(i) if i == env.specials().vec_pop => {
            state.tail = false;
            if cdr.len() != 1 {
                return Err(VMError::new_compile(format!(
                    "takes one argument, got {}, line {}",
                    cdr.len(),
                    env.line_num()
                )));
            }
            compile(env, state, cdr[0], result + 1)?;
            state
                .chunk
                .encode2(VECPOP, (result + 1) as u16, result as u16, env.own_line())?;
        }
        Value::Special(i) if i == env.specials().len => {
            state.tail = false;
            if cdr.len() != 1 {
                return Err(VMError::new_compile(format!(
                    "takes one argument, got {}, line {}",
                    cdr.len(),
                    env.line_num()
                )));
            }
            compile(env, state, cdr[0], result + 1)?;
            state
                .chunk
                .encode2(LEN, result as u16, (result + 1) as u16, env.own_line())?;
        }
        Value::Special(i) if i == env.specials().clear => {
            state.tail = false;
            if cdr.len() != 1 {
                return Err(VMError::new_compile(format!(
                    "takes one argument, got {}, line {}",
                    cdr.len(),
                    env.line_num()
                )));
            }
            compile(env, state, cdr[0], result)?;
            state.chunk.encode1(CLR, result as u16, env.own_line())?;
        }
        _ => return Ok(false),
    }
    Ok(true)
}
