use slvm::error::*;
use slvm::opcodes::*;
use slvm::value::*;
use slvm::{Handle, Interned};

use crate::state::*;
use crate::{compile, mkconst, CompileEnvironment};

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

pub struct DestructState {
    all_optionals: Vec<Vec<(usize, Value)>>,
    destructures: Vec<Destructure>,
}

pub fn setup_dbg(env: &CompileEnvironment, state: &mut CompileState, reg: usize, name: Interned) {
    if let Some(dbg_args) = state.chunk.dbg_args.as_mut() {
        if dbg_args.len() < reg - 1 {
            dbg_args.resize(reg - 1, env.specials().scratch);
        }
        if reg < dbg_args.len() {
            // This register will have multiple names, maybe concat them or something.
            dbg_args[reg] = name;
        } else {
            dbg_args.push(name);
        }
    }
}

impl DestructState {
    pub fn new() -> Self {
        Self {
            all_optionals: Vec::new(),
            destructures: Vec::new(),
        }
    }

    fn setup_destructures(
        &self,
        env: &mut CompileEnvironment,
        state: &mut CompileState,
        free_reg: &mut usize,
    ) -> VMResult<()> {
        for destructure in &self.destructures {
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

    fn setup_optionals(
        &self,
        env: &mut CompileEnvironment,
        state: &mut CompileState,
        free_reg: usize,
    ) -> VMResult<()> {
        for opt_comps in &self.all_optionals {
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
        &mut self,
        env: &mut CompileEnvironment,
        state: &mut CompileState,
        vector_handle: Handle,
        reg: usize,
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
                    Value::Symbol(i) if *i == env.specials().rest => {
                        len -= 1;
                        rest = true;
                    }
                    Value::Symbol(i) if *i == env.specials().optional => {
                        len -= 1;
                        opt = true;
                    }
                    Value::Keyword(i) if *i == env.specials().numeq => {
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
                        setup_dbg(env, state, reg, *i);
                        if rest {
                            rest_cnt += 1;
                        } else if opt {
                            opt_comps.push((reg, Value::Nil));
                        }
                    }
                    Value::Vector(h) => {
                        let reg = state.symbols.borrow_mut().reserve_reg() + 1;
                        setup_dbg(env, state, reg, env.specials().scratch);
                        stack.push(DestructType::Vector(*h, reg));
                    }
                    Value::Map(h) => {
                        let reg = state.symbols.borrow_mut().reserve_reg() + 1;
                        setup_dbg(env, state, reg, env.specials().scratch);
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
        self.destructures.push(Destructure {
            start_reg: *start_reg as u16,
            len: len as u16,
            reg: reg as u16,
            map_keys: None,
            rest,
            allow_extra,
        });
        if !opt_comps.is_empty() {
            self.all_optionals.push(opt_comps);
        }
        *start_reg += len;
        Ok(())
    }

    fn do_map_destructure(
        &mut self,
        env: &mut CompileEnvironment,
        state: &mut CompileState,
        map_handle: Handle,
        reg: usize,
        stack: &mut Vec<DestructType>,
        start_reg: &mut usize,
    ) -> VMResult<()> {
        let map = env.vm().get_map(map_handle);
        let mut keys = Vec::new();
        for (key, val) in map {
            match key {
                Value::Symbol(i) => {
                    let reg = state.symbols.borrow_mut().insert(*i) + 1;
                    setup_dbg(env, state, reg, *i);
                    keys.push(*val);
                }
                Value::Vector(h) => {
                    let reg = state.symbols.borrow_mut().reserve_reg() + 1;
                    setup_dbg(env, state, reg, env.specials().scratch);
                    stack.push(DestructType::Vector(*h, reg));
                    keys.push(*val);
                }
                Value::Map(h) => {
                    let reg = state.symbols.borrow_mut().reserve_reg() + 1;
                    setup_dbg(env, state, reg, env.specials().scratch);
                    stack.push(DestructType::Map(*h, reg));
                    keys.push(*val);
                }
                _ => return Err(VMError::new_compile("not a valid destructure")),
            }
        }
        let len = map.len();
        self.destructures.push(Destructure {
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
        &mut self,
        env: &mut CompileEnvironment,
        state: &mut CompileState,
        destruct_type: DestructType,
    ) -> VMResult<()> {
        let mut stack = vec![destruct_type];
        let mut start_reg = state.reserved_regs();
        while let Some(destruct_type) = stack.pop() {
            match destruct_type {
                DestructType::Vector(vector, reg) => {
                    self.do_vector_destructure(env, state, vector, reg, &mut stack, &mut start_reg)?
                }
                DestructType::Map(map, reg) => {
                    self.do_map_destructure(env, state, map, reg, &mut stack, &mut start_reg)?
                }
            }
        }
        Ok(())
    }

    pub fn compile(
        &self,
        env: &mut CompileEnvironment,
        state: &mut CompileState,
        free_reg: &mut usize,
    ) -> VMResult<()> {
        self.setup_destructures(env, state, free_reg)?;
        self.setup_optionals(env, state, *free_reg)?;
        let kw = Value::Keyword(env.vm_mut().intern("destructure"));
        let err_str = Value::Keyword(env.vm_mut().intern("missing structure"));
        // For each destructure raise an error if something was missing.
        for destructure in &self.destructures {
            state.chunk.encode2(
                JMPRNU,
                destructure.start_reg,
                destructure.len,
                env.own_line(),
            )?;
            let encode_offset = state.chunk.code.len();
            state.chunk.encode_jump_offset(0)?;
            let start_offset = state.chunk.code.len();
            mkconst(env, state, kw, *free_reg)?;
            mkconst(env, state, err_str, *free_reg + 1)?;
            state.chunk.encode2(
                ERR,
                *free_reg as u16,
                (*free_reg + 1) as u16,
                env.own_line(),
            )?;
            state.chunk.reencode_jump_offset(
                encode_offset,
                (state.chunk.code.len() - start_offset) as i32,
            )?;
        }
        Ok(())
    }
}
