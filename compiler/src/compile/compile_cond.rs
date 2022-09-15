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
    let mut end_patches = Vec::new();
    let mut cdr_i = cdr.iter().peekable();
    while let Some(r) = cdr_i.next() {
        let next = cdr_i.next();
        if next.is_none() {
            state.tail = tail;
        }
        compile(env, state, *r, result)?;
        if let Some(r) = next {
            state.tail = tail;
            state.chunk.encode1(JMPF, result as u16, env.own_line())?;
            let encode_offset = state.chunk.code.len();
            state.chunk.encode_jump_offset(0)?;
            let tmp_start_ip = state.chunk.code.len();
            compile(env, state, *r, result)?;
            if cdr_i.peek().is_some() {
                state.chunk.encode0(JMP, env.own_line())?;
                state.chunk.encode_jump_offset(0)?;
                end_patches.push(state.chunk.code.len());
            }
            state.chunk.reencode_jump_offset(
                encode_offset,
                (state.chunk.code.len() - tmp_start_ip) as i32,
            )?;
        }
        state.tail = false;
    }
    let end_ip = state.chunk.code.len();
    for i in end_patches {
        let jmp_forward = (end_ip - i) as i32;
        state.chunk.reencode_jump_offset(i - 3, jmp_forward)?;
    }
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
        let loop_start = state.chunk.code.len();
        compile(env, state, *conditional, result)?;
        state.chunk.encode1(JMPF, result as u16, env.own_line())?;
        let encode_offset = state.chunk.code.len();
        state.chunk.encode_jump_offset(0)?;
        let jmp_ip = state.chunk.code.len();
        for r in cdr_i {
            compile(env, state, *r, result)?;
        }
        state.chunk.encode0(JMP, env.own_line())?;
        state
            .chunk
            .encode_jump_offset(-(((state.chunk.code.len() + 3) - loop_start) as i32))?;
        state
            .chunk
            .reencode_jump_offset(encode_offset, (state.chunk.code.len() - jmp_ip) as i32)?;
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
    let mut end_patches = Vec::new();
    let mut cdr_i = cdr.iter().peekable();
    let mut next = cdr_i.next();
    while let Some(r) = next {
        next = cdr_i.next();
        if next.is_none() {
            state.tail = tail;
        }
        compile(env, state, *r, result)?;
        if cdr_i.peek().is_some() {
            state.chunk.encode1(JMPF, result as u16, env.own_line())?;
            state.chunk.encode_jump_offset(0)?;
            end_patches.push(state.chunk.code.len());
        }
        state.tail = false;
    }
    let end_ip = state.chunk.code.len();
    for i in end_patches {
        let jmp_forward = (end_ip - i) as i32;
        state.chunk.reencode_jump_offset(i - 3, jmp_forward)?;
    }
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
    let mut end_patches = Vec::new();
    let mut cdr_i = cdr.iter().peekable();
    let mut next = cdr_i.next();
    while let Some(r) = next {
        next = cdr_i.next();
        if next.is_none() {
            state.tail = tail;
        }
        compile(env, state, *r, result)?;
        if cdr_i.peek().is_some() {
            state.chunk.encode1(JMPT, result as u16, env.own_line())?;
            state.chunk.encode_jump_offset(0)?;
            end_patches.push(state.chunk.code.len());
        }
        state.tail = false;
    }
    let end_ip = state.chunk.code.len();
    for i in end_patches {
        let jmp_forward = (end_ip - i) as i32;
        state.chunk.reencode_jump_offset(i - 3, jmp_forward)?;
    }
    Ok(())
}
