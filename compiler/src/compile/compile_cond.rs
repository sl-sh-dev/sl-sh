use crate::{compile, CompileState, SloshVm};
use compile_state::state::SloshVmTrait;
use slvm::*;

pub(crate) fn compile_if(
    env: &mut SloshVm,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
) -> VMResult<()> {
    if cdr.is_empty() {
        return Err(VMError::new_compile(format!(
            "requires at least one argument, got 0, line {}",
            env.line_num()
        )));
    }
    let tail = state.tail;
    state.tail = false;
    let mut cdr_i = cdr.iter().peekable();
    let jmp_idx = state.chunk.add_jump(0);
    while let Some(r) = cdr_i.next() {
        let next = cdr_i.next();
        if next.is_none() {
            state.tail = tail;
        }
        compile(env, state, *r, result)?;
        if let Some(r) = next {
            state.tail = tail;
            let jmp2_idx = state.chunk.add_jump(0);
            state
                .chunk
                .encode2(JMPF, result as u16, jmp2_idx as u16, env.own_line())?;
            compile(env, state, *r, result)?;
            if cdr_i.peek().is_some() {
                state.chunk.encode1(JMP, jmp_idx as u16, env.own_line())?;
            }
            state
                .chunk
                .update_jump(jmp2_idx, state.chunk.code.len() as u32);
        }
        state.tail = false;
    }
    state
        .chunk
        .update_jump(jmp_idx, state.chunk.code.len() as u32);
    Ok(())
}

pub(crate) fn compile_while(
    env: &mut SloshVm,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
) -> VMResult<()> {
    let mut cdr_i = cdr.iter();
    if let Some(conditional) = cdr_i.next() {
        let jmp_cond = state.chunk.add_jump(0);
        state.chunk.encode1(JMP, jmp_cond as u16, env.own_line())?;
        let jmp_loop_start = state.chunk.add_jump(state.chunk.code.len() as u32);
        for r in cdr_i {
            compile(env, state, *r, result)?;
        }

        state
            .chunk
            .update_jump(jmp_cond, state.chunk.code.len() as u32);
        compile(env, state, *conditional, result)?;
        state
            .chunk
            .encode2(JMPT, result as u16, jmp_loop_start as u16, env.own_line())?;
        Ok(())
    } else {
        Err(VMError::new_compile(format!(
            "requires at least one argument, got 0, line {}",
            env.line_num()
        )))
    }
}

pub(crate) fn compile_and(
    env: &mut SloshVm,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
) -> VMResult<()> {
    if cdr.is_empty() {
        return Err(VMError::new_compile(format!(
            "requires at least one argument, got 0, line {}",
            env.line_num()
        )));
    }
    let tail = state.tail;
    state.tail = false;
    let mut cdr_i = cdr.iter().peekable();
    let mut next = cdr_i.next();
    let jmp_idx = state.chunk.add_jump(0);
    while let Some(r) = next {
        next = cdr_i.next();
        if next.is_none() {
            state.tail = tail;
        }
        compile(env, state, *r, result)?;
        if next.is_some() {
            state
                .chunk
                .encode2(JMPF, result as u16, jmp_idx as u16, env.own_line())?;
        }
        state.tail = false;
    }
    let end_ip = state.chunk.code.len();
    state.chunk.update_jump(jmp_idx, end_ip as u32);
    Ok(())
}

pub(crate) fn compile_or(
    env: &mut SloshVm,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
) -> VMResult<()> {
    if cdr.is_empty() {
        return Err(VMError::new_compile(format!(
            "requires at least one argument, got 0, line {}",
            env.line_num()
        )));
    }
    let tail = state.tail;
    state.tail = false;
    let mut cdr_i = cdr.iter().peekable();
    let mut next = cdr_i.next();
    let jmp_idx = state.chunk.add_jump(0);
    while let Some(r) = next {
        next = cdr_i.next();
        if next.is_none() {
            state.tail = tail;
        }
        compile(env, state, *r, result)?;
        if next.is_some() {
            state
                .chunk
                .encode2(JMPT, result as u16, jmp_idx as u16, env.own_line())?;
        }
        state.tail = false;
    }
    let end_ip = state.chunk.code.len();
    state.chunk.update_jump(jmp_idx, end_ip as u32);
    Ok(())
}
