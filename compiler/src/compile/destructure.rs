use slvm::error::*;
use slvm::opcodes::*;
use slvm::value::*;
use slvm::{Handle, Interned};

use crate::state::*;
use crate::{compile, CompileEnvironment};

pub struct Destructure {
    start_reg: u16,
    len: u16,
    reg: u16,
    map_keys: Option<Vec<Value>>,
    rest: bool,
    allow_extra: bool,
}

pub enum DestructType {
    Vector(Handle, usize),
    Map(Handle, usize),
}

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
    destructures: &Vec<Destructure>,
) -> VMResult<()> {
    for destructure in destructures {
        if let Some(keys) = &destructure.map_keys {
            for (i, key) in keys.iter().enumerate() {
                compile(env, state, *key, destructure.start_reg as usize + i)?;
            }
            state.chunk.encode3(
                MDSC,
                destructure.start_reg,
                destructure.len,
                destructure.reg,
                env.own_line(),
            )?;
        } else if destructure.rest && !destructure.allow_extra {
            state.chunk.encode3(
                LDSCR,
                destructure.start_reg,
                destructure.len,
                destructure.reg,
                env.own_line(),
            )?;
        } else {
            state.chunk.encode3(
                LDSC,
                destructure.start_reg,
                destructure.len,
                destructure.reg,
                env.own_line(),
            )?;
        }
        let temp_max = (destructure.start_reg + destructure.len) as usize;
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

fn do_vector_destructure(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    vector_handle: Handle,
    reg: usize,
    all_optionals: &mut Vec<Vec<(usize, Value)>>,
    destructures: &mut Vec<Destructure>,
    stack: &mut Vec<DestructType>,
    start_reg: &mut usize,
) -> VMResult<()> {
    let vector = env.vm().get_vector(vector_handle);
    let mut len = vector.len();
    let mut rest = false;
    let mut opt = false;
    let mut opt_set_next = false;
    let mut rest_cnt = 0;
    let mut opt_comps = Vec::new();
    let mut allow_extra = false;
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
                    let reg = state.symbols.borrow_mut().reserve_reg() + 1;
                    setup_dbg(state, reg, state.specials.scratch);
                    stack.push(DestructType::Vector(*h, reg));
                }
                Value::Map(h) => {
                    let reg = state.symbols.borrow_mut().reserve_reg() + 1;
                    setup_dbg(state, reg, state.specials.scratch);
                    stack.push(DestructType::Map(*h, reg));
                }
                _ => return Err(VMError::new_compile("not a valid destructure")),
            }
        }
        if rest_cnt > 1 {
            return Err(VMError::new_compile("not a valid destructure (invalid &)"));
        }
    }
    if rest && rest_cnt == 0 {
        allow_extra = true;
    }
    if opt_set_next {
        return Err(VMError::new_compile("not a valid destructure (invalid :=)"));
    }
    destructures.push(Destructure {
        start_reg: *start_reg as u16,
        len: len as u16,
        reg: reg as u16,
        map_keys: None,
        rest,
        allow_extra,
    });
    if !opt_comps.is_empty() {
        all_optionals.push(opt_comps);
    }
    *start_reg += len;
    Ok(())
}

fn do_map_destructure(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    map_handle: Handle,
    reg: usize,
    destructures: &mut Vec<Destructure>,
    stack: &mut Vec<DestructType>,
    start_reg: &mut usize,
) -> VMResult<()> {
    let map = env.vm().get_map(map_handle);
    let mut keys = Vec::new();
    for (key, val) in map {
        match key {
            Value::Symbol(i) => {
                let reg = state.symbols.borrow_mut().insert(*i) + 1;
                setup_dbg(state, reg, *i);
                keys.push(*val);
            }
            Value::Vector(h) => {
                let reg = state.symbols.borrow_mut().reserve_reg() + 1;
                setup_dbg(state, reg, state.specials.scratch);
                stack.push(DestructType::Vector(*h, reg));
                keys.push(*val);
            }
            Value::Map(h) => {
                let reg = state.symbols.borrow_mut().reserve_reg() + 1;
                setup_dbg(state, reg, state.specials.scratch);
                stack.push(DestructType::Map(*h, reg));
                keys.push(*val);
            }
            _ => return Err(VMError::new_compile("not a valid destructure")),
        }
    }
    let len = map.len();
    destructures.push(Destructure {
        start_reg: *start_reg as u16,
        len: len as u16,
        reg: reg as u16,
        map_keys: Some(keys),
        rest: false,
        allow_extra: false,
    });
    *start_reg += len;
    Ok(())
}

pub fn do_destructure(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    destruct_type: DestructType,
    all_optionals: &mut Vec<Vec<(usize, Value)>>,
    destructures: &mut Vec<Destructure>,
) -> VMResult<()> {
    let mut stack = vec![destruct_type]; //DestructType::Vector(vector_handle, reg)];
    let mut start_reg = state.reserved_regs(); //reg + 1;
    while let Some(destruct_type) = stack.pop() {
        match destruct_type {
            DestructType::Vector(vector, reg) => do_vector_destructure(
                env,
                state,
                vector,
                reg,
                all_optionals,
                destructures,
                &mut stack,
                &mut start_reg,
            )?,
            DestructType::Map(map, reg) => do_map_destructure(
                env,
                state,
                map,
                reg,
                destructures,
                &mut stack,
                &mut start_reg,
            )?,
        }
    }
    Ok(())
}
