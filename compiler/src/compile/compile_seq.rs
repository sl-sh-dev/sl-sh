use crate::{compile, CompileEnvironment, CompileState};
use slvm::*;

pub(crate) fn compile_cons(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    car: Value,
    cdr: &[Value],
    result: usize,
) -> VMResult<bool> {
    match car {
        Value::Symbol(i) if i == env.specials().list => {
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
        Value::Symbol(i) if i == env.specials().list_append => {
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
        Value::Symbol(i) if i == env.specials().cons => {
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
        Value::Symbol(i) if i == env.specials().car => {
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
        Value::Symbol(i) if i == env.specials().cdr => {
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
        Value::Symbol(i) if i == env.specials().xar => {
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
        Value::Symbol(i) if i == env.specials().xdr => {
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
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    car: Value,
    cdr: &[Value],
    result: usize,
) -> VMResult<bool> {
    match car {
        Value::Symbol(i) if i == env.specials().vec => {
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
        Value::Symbol(i) if i == env.specials().make_vec => {
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
        Value::Symbol(i) if i == env.specials().vec_push => {
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
        Value::Symbol(i) if i == env.specials().vec_pop => {
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
        Value::Symbol(i) if i == env.specials().vec_nth => {
            state.tail = false;
            if cdr.len() != 2 {
                return Err(VMError::new_compile(format!(
                    "takes two arguments, got {}, line {}",
                    cdr.len(),
                    env.line_num()
                )));
            }
            compile(env, state, cdr[0], result + 1)?;
            compile(env, state, cdr[1], result + 2)?;
            state.chunk.encode3(
                VECNTH,
                (result + 1) as u16,
                result as u16,
                (result + 2) as u16,
                env.own_line(),
            )?;
        }
        Value::Symbol(i) if i == env.specials().vec_set => {
            state.tail = false;
            if cdr.len() != 3 {
                return Err(VMError::new_compile(format!(
                    "takes three arguments, got {}, line {}",
                    cdr.len(),
                    env.line_num()
                )));
            }
            compile(env, state, cdr[0], result)?;
            compile(env, state, cdr[1], result + 1)?;
            compile(env, state, cdr[2], result + 2)?;
            state.chunk.encode3(
                VECSTH,
                result as u16,
                (result + 2) as u16,
                (result + 1) as u16,
                env.own_line(),
            )?;
        }
        Value::Symbol(i) if i == env.specials().vec_len => {
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
                .encode2(VECLEN, result as u16, (result + 1) as u16, env.own_line())?;
        }
        Value::Symbol(i) if i == env.specials().vec_clr => {
            state.tail = false;
            if cdr.len() != 1 {
                return Err(VMError::new_compile(format!(
                    "takes one argument, got {}, line {}",
                    cdr.len(),
                    env.line_num()
                )));
            }
            compile(env, state, cdr[0], result)?;
            state.chunk.encode1(VECCLR, result as u16, env.own_line())?;
        }
        _ => return Ok(false),
    }
    Ok(true)
}
