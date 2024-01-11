use compile_state::state::CompileState;
use slvm::opcodes::*;
use slvm::{Handle, Interned, VMError, VMResult, Value};
use std::collections::HashMap;

use crate::{compile, mkconst, SloshVm, SloshVmTrait};

pub enum Register {
    Named(Interned, u16),
    Reserved(u16),
}

pub struct Destructure {
    start_reg: u16,
    len: u16,
    reg: u16,
    map_keys: Option<Vec<Value>>,
    rest: bool,
    allow_extra: bool,
    register_labels: Vec<Register>,
}

pub enum DestructType {
    Vector(Handle, usize),
    Map(Handle, usize),
}

pub struct DestructState {
    all_optionals: Vec<Vec<(usize, Value)>>,
    destructures: Vec<Destructure>,
}

/// When doing destructuring we need to turn 'vec' and 'make-hash' calls into the literal vectors and
/// maps outside of execution.
/// Takes a Value and either returns it or the literal vec or hash-map if it is a call to create one.
/// Only works with Value::List for detection currently, this is what will come from the reader in
/// these cases, may need to expand this to handle Value::Pair as well for macros (? TODO).
pub fn resolve_destruct_containers(env: &mut SloshVm, arg: Value) -> Value {
    if let Value::List(h, s) = arg {
        let v = env.get_vector(h);
        let s = s as usize;
        let i_vec = env.specials().vec;
        let i_hash = env.specials().make_hash;
        match &v[s] {
            Value::Symbol(i) if *i == i_vec => {
                let v = env.alloc_vector(v[s + 1..].to_vec());
                env.heap_sticky(v);
                v
            }
            Value::Symbol(i) if *i == i_hash => {
                let mut iter = v[s + 1..].iter();
                let mut map = HashMap::new();
                while let Some(key) = iter.next() {
                    if let Some(val) = iter.next() {
                        map.insert(*key, *val);
                    }
                }
                let v = env.alloc_map(map);
                env.heap_sticky(v);
                v
            }
            _ => arg,
        }
    } else {
        arg
    }
}

pub fn setup_dbg(env: &SloshVm, state: &mut CompileState, reg: usize, name: Interned) {
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
        env: &mut SloshVm,
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

            // Apply labels to registers so shadowing will work correctly.
            for r in &destructure.register_labels {
                match r {
                    Register::Named(i, sreg) => {
                        let reg = state.symbols.borrow_mut().insert(*i);
                        if reg != *sreg as usize {
                            panic!("Failed to line up regs {} vs {}", reg, *sreg);
                        }
                        setup_dbg(env, state, reg, *i);
                    }
                    Register::Reserved(sreg) => {
                        let reg = state.symbols.borrow_mut().reserve_reg();
                        if reg != *sreg as usize {
                            panic!("Failed to line up regs 2");
                        }
                        setup_dbg(env, state, reg, env.specials().scratch);
                    }
                }
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
        env: &mut SloshVm,
        state: &mut CompileState,
        free_reg: usize,
    ) -> VMResult<()> {
        for opt_comps in &self.all_optionals {
            for (target_reg, default) in opt_comps {
                let jmp_idx = state.chunk.add_jump(0);
                state
                    .chunk
                    .encode2(JMPNU, *target_reg as u16, jmp_idx as u16, env.own_line())?;
                compile(env, state, *default, free_reg)?;
                state
                    .chunk
                    .encode2(MOV, *target_reg as u16, free_reg as u16, env.own_line())?;
                state
                    .chunk
                    .update_jump(jmp_idx, state.chunk.code.len() as u32);
            }
        }
        Ok(())
    }

    fn do_vector_destructure(
        &mut self,
        env: &mut SloshVm,
        vector_handle: Handle,
        reg: usize,
        stack: &mut Vec<DestructType>,
        next_reg: &mut usize,
    ) -> VMResult<()> {
        // Unnecessary allocation(s) to appease the borrow checker because of the call to resolve_destruct_containers...
        let vector: Vec<Value> = env.get_vector(vector_handle).to_vec();
        let mut len = vector.len();
        let mut rest = false;
        let mut opt = false;
        let mut opt_set_next = false;
        let mut rest_cnt = 0;
        let mut opt_comps = Vec::new();
        let mut allow_extra = false;
        let mut register_labels = Vec::new();
        let start_reg = *next_reg;
        for name in &vector {
            if opt_set_next {
                len -= 1;
                opt_set_next = false;
                if let Some((reg, _)) = opt_comps.pop() {
                    opt_comps.push((reg, *name));
                }
            } else {
                let name = resolve_destruct_containers(env, *name);
                match &name {
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
                        register_labels.push(Register::Named(*i, *next_reg as u16));
                        if rest {
                            rest_cnt += 1;
                        } else if opt {
                            opt_comps.push((*next_reg, Value::Nil));
                        }
                        *next_reg += 1;
                    }
                    Value::Vector(h) => {
                        register_labels.push(Register::Reserved(*next_reg as u16));
                        stack.push(DestructType::Vector(*h, *next_reg));
                        *next_reg += 1;
                    }
                    Value::Map(h) => {
                        register_labels.push(Register::Reserved(*next_reg as u16));
                        stack.push(DestructType::Map(*h, *next_reg));
                        *next_reg += 1;
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
            start_reg: start_reg as u16,
            len: len as u16,
            reg: reg as u16,
            map_keys: None,
            rest,
            allow_extra,
            register_labels,
        });
        if !opt_comps.is_empty() {
            self.all_optionals.push(opt_comps);
        }
        Ok(())
    }

    fn do_map_destructure(
        &mut self,
        env: &mut SloshVm,
        map_handle: Handle,
        current_reg: usize,
        stack: &mut Vec<DestructType>,
        next_reg: &mut usize,
    ) -> VMResult<()> {
        let or_i = env.intern("or");
        // Unnecessary allocation(s) to appease the borrow checker because of the call to resolve_destruct_containers...
        let map = env.get_map(map_handle).clone();
        let mut keys = Vec::new();
        let mut opt_comps = Vec::new();
        let mut len = map.len();
        let mut register_labels = Vec::new();
        let optionals = if let Some(opts) = map.get(&Value::Keyword(or_i)) {
            let opts = resolve_destruct_containers(env, *opts);
            if let Value::Map(handle) = opts {
                env.get_map(handle).clone()
            } else {
                return Err(VMError::new_compile(":or must be followed by a map"));
            }
        } else {
            HashMap::new()
        };
        let start_reg = *next_reg;
        for (key, val) in &map {
            let key = resolve_destruct_containers(env, *key);
            match &key {
                Value::Keyword(i) if *i == or_i => {
                    len -= 1;
                    continue; // Skip checking for optionals.
                }
                Value::Symbol(i) => {
                    register_labels.push(Register::Named(*i, *next_reg as u16));
                    keys.push(*val);
                    *next_reg += 1;
                }
                Value::Vector(h) => {
                    register_labels.push(Register::Reserved(*next_reg as u16));
                    stack.push(DestructType::Vector(*h, *next_reg));
                    keys.push(*val);
                    *next_reg += 1;
                }
                Value::Map(h) => {
                    register_labels.push(Register::Reserved(*next_reg as u16));
                    stack.push(DestructType::Map(*h, *next_reg));
                    keys.push(*val);
                    *next_reg += 1;
                }
                _ => return Err(VMError::new_compile("not a valid destructure")),
            }
            if let Some(opt_val) = optionals.get(val) {
                opt_comps.push((*next_reg - 1, *opt_val));
            }
        }
        self.destructures.push(Destructure {
            start_reg: start_reg as u16,
            len: len as u16,
            reg: current_reg as u16,
            map_keys: Some(keys),
            rest: false,
            allow_extra: false,
            register_labels,
        });
        if !opt_comps.is_empty() {
            self.all_optionals.push(opt_comps);
        }
        Ok(())
    }

    pub fn do_destructure(
        &mut self,
        env: &mut SloshVm,
        state: &mut CompileState,
        destruct_type: DestructType,
    ) -> VMResult<()> {
        let mut stack = vec![destruct_type];
        // Track the next available reg across all the destructuring so can handle shadowing properly.
        // This should stay in sync with the order names are applied otherwise local names will be
        // broken...
        let mut next_reg = state.symbols.borrow().regs_count();
        while let Some(destruct_type) = stack.pop() {
            match destruct_type {
                DestructType::Vector(vector, reg) => {
                    self.do_vector_destructure(env, vector, reg, &mut stack, &mut next_reg)?;
                    env.heap_unsticky(Value::Vector(vector));
                }
                DestructType::Map(map, reg) => {
                    self.do_map_destructure(env, map, reg, &mut stack, &mut next_reg)?;
                    env.heap_unsticky(Value::Map(map));
                }
            }
        }
        Ok(())
    }

    pub fn compile(
        &mut self,
        env: &mut SloshVm,
        state: &mut CompileState,
        free_reg: &mut usize,
    ) -> VMResult<()> {
        self.setup_destructures(env, state, free_reg)?;
        self.setup_optionals(env, state, *free_reg)?;
        let kw = Value::Keyword(env.intern("destructure"));
        let err_str = Value::StringConst(env.intern("missing structure"));
        // For each destructure raise an error if something was missing.
        for destructure in &self.destructures {
            let jmp_idx = state.chunk.add_jump(0);
            state.chunk.encode3(
                JMPRNU,
                destructure.start_reg,
                destructure.len,
                jmp_idx as u16,
                env.own_line(),
            )?;
            mkconst(env, state, kw, *free_reg)?;
            mkconst(env, state, err_str, *free_reg + 1)?;
            state.chunk.encode2(
                ERR,
                *free_reg as u16,
                (*free_reg + 1) as u16,
                env.own_line(),
            )?;
            state
                .chunk
                .update_jump(jmp_idx, state.chunk.code.len() as u32);
        }
        self.destructures.clear();
        self.all_optionals.clear();
        if state.max_regs < *free_reg {
            state.max_regs = *free_reg;
        }
        Ok(())
    }
}
