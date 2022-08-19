use slvm::error::*;
use slvm::opcodes::*;
use slvm::value::*;
use slvm::{Handle, Interned};

use crate::state::*;
use crate::{compile, CompileEnvironment};

pub fn setup_dbg(state: &mut CompileState, reg: usize, name: Interned) {
    if let Some(dbg_args) = state.chunk.dbg_args.as_mut() {
        if dbg_args.len() < reg - 1 {
            dbg_args.resize(reg - 1, state.specials.scratch);
        }
        if reg < dbg_args.len() {
            // This register will have multiple names, maybe concat them or something.
            dbg_args[reg] = name;
        } else {
            dbg_args.push(name);
        }
    }
}

pub fn setup_destructures(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    free_reg: &mut usize,
    destructures: &Vec<(u16, u16, u16, bool)>,
) -> VMResult<()> {
    for (start_reg, len, reg, rest) in destructures {
        if *rest {
            state.chunk.encode3(
                LDSCR,
                *start_reg as u16,
                *len as u16,
                *reg as u16,
                env.own_line(),
            )?;
        } else {
            state.chunk.encode3(
                LDSC,
                *start_reg as u16,
                *len as u16,
                *reg as u16,
                env.own_line(),
            )?;
        }
        let temp_max = (*start_reg + *len) as usize;
        if temp_max > *free_reg {
            *free_reg = temp_max;
        }
    }
    Ok(())
}

pub fn setup_optionals(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    free_reg: usize,
    all_optionals: &Vec<Vec<(usize, Value)>>,
) -> VMResult<()> {
    for opt_comps in all_optionals {
        for (target_reg, default) in opt_comps {
            state
                .chunk
                .encode1(JMPNU, *target_reg as u16, env.own_line())?;
            let encode_offset = state.chunk.code.len();
            state.chunk.encode_jump_offset(0)?;
            let start_offset = state.chunk.code.len();
            compile(env, state, *default, free_reg)?;
            state
                .chunk
                .encode2(MOV, *target_reg as u16, free_reg as u16, env.own_line())?;
            state.chunk.reencode_jump_offset(
                encode_offset,
                (state.chunk.code.len() - start_offset) as i32,
            )?;
        }
    }
    Ok(())
}

pub fn do_destructure(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    vector_handle: Handle,
    reg: usize,
    all_optionals: &mut Vec<Vec<(usize, Value)>>,
    destructures: &mut Vec<(u16, u16, u16, bool)>,
) -> VMResult<()> {
    let vector = env.vm().get_vector(vector_handle);
    let mut stack = vec![(vector, reg)];
    let mut start_reg = state.reserved_regs();//reg + 1;
    while let Some((vector, reg)) = stack.pop() {
        let mut len = vector.len();
        let mut rest = false;
        let mut opt = false;
        let mut opt_set_next = false;
        let mut rest_cnt = 0;
        let mut opt_comps = Vec::new();
        for name in vector {
            if opt_set_next {
                len -= 1;
                opt_set_next = false;
                if let Some((reg, _)) = opt_comps.pop() {
                    opt_comps.push((reg, *name));
                }
            } else {
                match name {
                    Value::Symbol(i) if *i == state.specials.rest => {
                        len -= 1;
                        rest = true;
                    }
                    Value::Symbol(i) if *i == state.specials.optional => {
                        len -= 1;
                        opt = true;
                    }
                    Value::Keyword(i) if *i == state.specials.numeq => {
                        if opt {
                            len -= 1;
                            opt_set_next = true;
                        } else {
                            return Err(VMError::new_compile(
                                ":= only valid for optionals (after %)",
                            ));
                        }
                    }
                    Value::Symbol(i) => {
                        let reg = state.symbols.borrow_mut().insert(*i) + 1;
                        setup_dbg(state, reg, *i);
                        if rest {
                            rest_cnt += 1;
                        } else if opt {
                            opt_comps.push((reg, Value::Nil));
                        }
                    }
                    Value::Vector(h) => {
                        let vector = env.vm().get_vector(*h);
                        let reg = state.symbols.borrow_mut().reserve_reg() + 1;
                        setup_dbg(state, reg, state.specials.scratch);
                        stack.push((vector, reg));
                    }
                    _ => return Err(VMError::new_compile("not a valid destructure")),
                }
            }
            if rest_cnt > 1 {
                return Err(VMError::new_compile("not a valid destructure (invalid &)"));
            }
        }
        if rest && rest_cnt == 0 {
            // Allow extras but don't need them.
            let reg = state.symbols.borrow_mut().reserve_reg() + 1;
            setup_dbg(state, reg, state.specials.scratch);
        }
        if opt_set_next {
            return Err(VMError::new_compile("not a valid destructure (invalid :=)"));
        }
        destructures.push((start_reg as u16, len as u16, reg as u16, rest));
        if !opt_comps.is_empty() {
            all_optionals.push(opt_comps);
        }
        start_reg += len;
    }
    Ok(())
}
