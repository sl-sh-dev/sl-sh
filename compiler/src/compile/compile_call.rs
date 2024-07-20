use crate::{compile, CompileState, SloshVm};
use compile_state::state::SloshVmTrait;
use slvm::{VMResult, Value, BMOV, CALL, CALLM, CONST, MOV, TCALL, TCALLM};

fn compile_params(
    env: &mut SloshVm,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
    tail: bool,
) -> VMResult<()> {
    if !cdr.is_empty() {
        for (i, r) in cdr.iter().enumerate() {
            compile(env, state, *r, result + i)?;
        }
        let line = env.own_line();
        if tail {
            state
                .chunk
                .encode3(BMOV, 1, result as u16, cdr.len() as u16, line)?;
        }
    }
    Ok(())
}

pub(crate) fn compile_call(
    env: &mut SloshVm,
    state: &mut CompileState,
    callable: Value,
    cdr: &[Value],
    result: usize,
) -> VMResult<()> {
    let b_reg = result + cdr.len() + 1;
    if b_reg > state.max_regs {
        state.max_regs = b_reg;
    }
    let const_i = state.add_constant(callable);
    let tail = state.tail && state.defers == 0;
    state.tail = false;
    compile_params(env, state, cdr, result + 1, tail)?;
    let line = env.own_line();
    state
        .chunk
        .encode2(CONST, b_reg as u16, const_i as u16, line)?;
    if tail {
        state
            .chunk
            .encode2(TCALL, b_reg as u16, cdr.len() as u16, line)?;
    } else {
        state
            .chunk
            .encode3(CALL, b_reg as u16, cdr.len() as u16, result as u16, line)?;
    }
    Ok(())
}

pub(crate) fn compile_callg(
    env: &mut SloshVm,
    state: &mut CompileState,
    global: u32,
    cdr: &[Value],
    result: usize,
) -> VMResult<()> {
    let tail = state.tail && state.defers == 0;
    state.tail = false;
    compile_params(env, state, cdr, result + 1, tail)?;
    let line = env.own_line();
    if tail {
        state.chunk.encode_tcallg(global, cdr.len() as u16, line)?;
    } else {
        state
            .chunk
            .encode_callg(global, cdr.len() as u16, result as u16, line)?;
    }
    Ok(())
}

pub(crate) fn compile_call_reg(
    env: &mut SloshVm,
    state: &mut CompileState,
    reg: u16,
    cdr: &[Value],
    result: usize,
) -> VMResult<()> {
    let tail = state.tail && state.defers == 0;
    state.tail = false;
    if tail {
        if result as u16 + 1 != reg {
            state
                .chunk
                .encode2(MOV, result as u16 + 1, reg, env.own_line())?;
        }
        // Lie abpout this being a tail call because we can not emit the BMOV yet.
        compile_params(env, state, cdr, result + 2, false)?;
        let b_reg = result + cdr.len() + 3;
        if b_reg > state.max_regs {
            state.max_regs = b_reg;
        }
        if result + 1 != b_reg {
            state
                .chunk
                .encode2(MOV, b_reg as u16, result as u16 + 1, env.own_line())?;
        }
        state
            .chunk
            .encode3(BMOV, 1, result as u16 + 2, cdr.len() as u16, env.own_line())?;
        state
            .chunk
            .encode2(TCALL, b_reg as u16, cdr.len() as u16, env.own_line())?;
    } else {
        compile_params(env, state, cdr, result + 1, tail)?;
        state
            .chunk
            .encode3(CALL, reg, cdr.len() as u16, result as u16, env.own_line())?;
    }
    Ok(())
}

pub(crate) fn compile_call_myself(
    env: &mut SloshVm,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
    force_tail: bool,
) -> VMResult<()> {
    let tail = force_tail || (state.tail && state.defers == 0);
    state.tail = false;
    compile_params(env, state, cdr, result + 1, tail)?;
    let line = env.own_line();
    if tail {
        state.chunk.encode1(TCALLM, cdr.len() as u16, line)?;
    } else {
        state
            .chunk
            .encode2(CALLM, cdr.len() as u16, result as u16, line)?;
    }
    Ok(())
}
