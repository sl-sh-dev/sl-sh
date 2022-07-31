use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use slvm::error::*;
use slvm::opcodes::*;
use slvm::value::*;
use slvm::vm::*;
use slvm::Handle;

use crate::backquote::*;
use crate::state::*;

pub struct CompileEnvironment<'vm> {
    vm: &'vm mut Vm,
    use_line: bool,
    line: u32,
}

impl<'vm> CompileEnvironment<'vm> {
    pub fn new(vm: &'vm mut Vm) -> Self {
        Self {
            vm,
            use_line: true,
            line: 1,
        }
    }

    pub fn vm(&self) -> &Vm {
        self.vm
    }

    pub fn vm_mut(&mut self) -> &mut Vm {
        self.vm
    }

    pub fn set_line(&mut self, state: &mut CompileState, handle: Handle) {
        if let (Some(Value::UInt(dline)), Some(Value::StringConst(file_intern))) = (
            self.vm.get_heap_property(handle, "dbg-line"),
            self.vm.get_heap_property(handle, "dbg-file"),
        ) {
            let file_name = self.vm.get_interned(file_intern);
            if file_name == state.chunk.file_name && dline as u32 > self.line {
                self.line = dline as u32;
            }
        }
    }

    pub fn set_line_val(&mut self, state: &mut CompileState, val: Value) {
        if let Some(handle) = val.get_handle() {
            if let (Some(Value::UInt(dline)), Some(Value::StringConst(file_intern))) = (
                self.vm.get_heap_property(handle, "dbg-line"),
                self.vm.get_heap_property(handle, "dbg-file"),
            ) {
                let file_name = self.vm.get_interned(file_intern);
                if file_name == state.chunk.file_name && dline as u32 > self.line {
                    self.line = dline as u32;
                }
            }
        }
    }

    pub fn own_line(&self) -> Option<u32> {
        if self.use_line {
            Some(self.line)
        } else {
            None
        }
    }

    pub fn line_num(&self) -> u32 {
        if self.use_line {
            self.line
        } else {
            0
        }
    }
}

fn compile_params(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
    tail: bool,
) -> VMResult<()> {
    for (i, r) in cdr.iter().enumerate() {
        compile(env, state, *r, result + i)?;
    }
    let line = env.own_line();
    if tail {
        state
            .chunk
            .encode3(BMOV, 1, result as u16, cdr.len() as u16, line)?;
    }
    Ok(())
}

fn compile_call(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    callable: Value,
    cdr: &[Value],
    result: usize,
) -> VMResult<()> {
    let b_reg = result + cdr.len() + 1;
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

fn compile_callg(
    env: &mut CompileEnvironment,
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

fn compile_call_reg(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    reg: u16,
    cdr: &[Value],
    result: usize,
) -> VMResult<()> {
    let tail = state.tail && state.defers == 0;
    state.tail = false;
    let b_reg = if tail {
        let b_reg = result + cdr.len() + 2;
        state
            .chunk
            .encode2(MOV, b_reg as u16, reg as u16, env.own_line())?;
        b_reg
    } else {
        0
    };
    compile_params(env, state, cdr, result + 1, tail)?;
    let line = env.own_line();
    if tail {
        state
            .chunk
            .encode2(TCALL, b_reg as u16, cdr.len() as u16, line)?;
    } else {
        state
            .chunk
            .encode3(CALL, reg, cdr.len() as u16, result as u16, line)?;
    }
    Ok(())
}

fn compile_call_myself(
    env: &mut CompileEnvironment,
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

fn get_args_iter<'vm>(
    env: &'vm CompileEnvironment,
    args: Value,
    name: &str,
) -> VMResult<Box<dyn Iterator<Item = Value> + 'vm>> {
    match args {
        Value::Pair(_) | Value::List(_, _) => Ok(args.iter(env.vm)),
        Value::Nil => Ok(args.iter(env.vm)),
        _ => {
            return Err(VMError::new_compile(format!("{}, invalid args", name)));
        }
    }
}

fn mk_state(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    args: Value,
) -> VMResult<(CompileState, Option<Vec<Value>>)> {
    let line = env.own_line().unwrap_or(1);
    let mut new_state = CompileState::new_state(
        env.vm_mut(),
        state.chunk.file_name,
        line,
        Some(state.symbols.clone()),
    );
    env.set_line_val(&mut new_state, args);
    let args_iter: Vec<Value> = get_args_iter(env, args, "fn")?.collect();
    let mut opt = false;
    let mut rest = false;
    let mut opt_comps = Vec::new();
    new_state.chunk.dbg_args = Some(Vec::new());
    for a in args_iter {
        match a {
            Value::Symbol(i) => {
                if i == new_state.specials.rest {
                    rest = true;
                } else {
                    new_state.symbols.borrow_mut().data.borrow_mut().add_sym(i);
                    if let Some(dbg_args) = new_state.chunk.dbg_args.as_mut() {
                        dbg_args.push(i);
                    }
                    if opt {
                        new_state.chunk.opt_args += 1;
                    } else {
                        new_state.chunk.args += 1;
                    }
                }
            }
            Value::Pair(_) | Value::List(_, _) => {
                env.set_line_val(&mut new_state, a);
                let mut args_iter = get_args_iter(env, a, "fn")?;
                opt = true;
                if let Some(Value::Symbol(i)) = args_iter.next() {
                    new_state.symbols.borrow_mut().data.borrow_mut().add_sym(i);
                    if let Some(dbg_args) = new_state.chunk.dbg_args.as_mut() {
                        dbg_args.push(i);
                    }
                    new_state.chunk.opt_args += 1;
                    if let Some(r) = args_iter.next() {
                        opt_comps.push(r);
                    }
                    // XXX Check to make sure only two elements...
                }
            }
            _ => {
                return Err(VMError::new_compile(
                    "Malformed fn, invalid args, must be symbols.",
                ))
            }
        }
    }
    new_state.chunk.rest = rest;
    if !opt_comps.is_empty() {
        Ok((new_state, Some(opt_comps)))
    } else {
        Ok((new_state, None))
    }
}

fn compile_fn(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    args: Value,
    cdr: &[Value],
    result: usize,
    is_macro: bool,
) -> VMResult<()> {
    let (mut new_state, opt_comps) = mk_state(env, state, args)?;
    for r in cdr.iter() {
        pass1(env, &mut new_state, *r).unwrap();
    }
    let reserved = new_state.reserved_regs();
    if let Some(opt_comps) = opt_comps {
        for (i, r) in opt_comps.into_iter().enumerate() {
            let target_reg = new_state.chunk.args as usize + i + 1;
            new_state
                .chunk
                .encode1(JMPNU, target_reg as u16, env.own_line())?;
            let encode_offset = new_state.chunk.code.len();
            new_state.chunk.encode_jump_offset(0)?;
            let start_offset = new_state.chunk.code.len();
            compile(env, &mut new_state, r, reserved)?;
            new_state
                .chunk
                .encode2(MOV, target_reg as u16, reserved as u16, env.own_line())?;
            new_state.chunk.reencode_jump_offset(
                encode_offset,
                (new_state.chunk.code.len() - start_offset) as i32,
            )?;
        }
    }
    let last_thing = cdr.len() - 1;
    for (i, r) in cdr.iter().enumerate() {
        if i == last_thing {
            new_state.tail = true;
        }
        compile(env, &mut new_state, *r, reserved)?;
    }
    new_state
        .chunk
        .encode1(SRET, reserved as u16, env.own_line())
        .unwrap();
    let mut closure = false;
    if !new_state.symbols.borrow().captures.borrow().is_empty() {
        let mut caps = Vec::new();
        for (_, _, c) in new_state.symbols.borrow().captures.borrow().iter() {
            caps.push((*c + 1) as u32);
        }
        new_state.chunk.captures = Some(caps);
        closure = true;
    }
    new_state.chunk.input_regs = reserved;
    new_state.chunk.extra_regs = new_state.max_regs - reserved;
    env.vm.pause_gc();
    let lambda = env.vm.alloc_lambda(Arc::new(new_state.chunk));
    env.vm.unpause_gc();
    if is_macro {
        // Unwrap safe since we just allocated lambda on the heap.
        env.vm
            .set_heap_property(lambda.get_handle().unwrap(), ":macro", Value::True);
    }
    let const_i = state.add_constant(lambda);
    state
        .chunk
        .encode2(CONST, result as u16, const_i as u16, env.own_line())?;
    if closure {
        state
            .chunk
            .encode2(CLOSE, result as u16, result as u16, env.own_line())?;
    }
    Ok(())
}

fn make_math_comp(
    env: &mut CompileEnvironment,
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

fn compile_math(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    car: Value,
    cdr: &[Value],
    result: usize,
) -> VMResult<bool> {
    match car {
        Value::Symbol(i) if i == state.specials.inc => {
            let dest = if let Value::Symbol(si) = cdr[0] {
                if let Some(idx) = state.get_symbol(si) {
                    idx + 1
                } else if let Some(slot) = env.vm.global_intern_slot(i) {
                    state
                        .chunk
                        .encode_refi(result as u16, slot, env.own_line())?;
                    result
                } else {
                    let sym = env.vm.get_interned(i);
                    return Err(VMError::new_compile(format!("Symbol {sym} not defined (maybe you need to use 'def {sym}' to pre-declare it).")));
                }
            } else {
                return Err(VMError::new_compile("inc!: expected symbol"));
            };
            if cdr.len() == 1 {
                state.chunk.encode2(INC, dest as u16, 1, env.own_line())?;
            } else if cdr.len() == 2 {
                let amount = match cdr[1] {
                    Value::Byte(i) => i as u16,
                    Value::Int(i) if i >= 0 && i <= u16::MAX as i64 => i as u16,
                    Value::Int(_) => return Err(VMError::new_compile("inc!: second arg to large")),
                    Value::UInt(i) if i <= u16::MAX as u64 => i as u16,
                    Value::UInt(_) => {
                        return Err(VMError::new_compile("inc!: second arg < 0 or to large"))
                    }
                    _ => return Err(VMError::new_compile("inc!: second arg must be integer")),
                };
                state
                    .chunk
                    .encode2(INC, dest as u16, amount, env.own_line())?;
            } else {
                return Err(VMError::new_compile("inc!: malformed"));
            }
        }
        Value::Symbol(i) if i == state.specials.dec => {
            let dest = if let Value::Symbol(si) = cdr[0] {
                if let Some(idx) = state.get_symbol(si) {
                    idx + 1
                } else if let Some(slot) = env.vm.global_intern_slot(i) {
                    state
                        .chunk
                        .encode_refi(result as u16, slot, env.own_line())?;
                    result
                } else {
                    let sym = env.vm.get_interned(i);
                    return Err(VMError::new_compile(format!("Symbol {sym} not defined (maybe you need to use 'def {sym}' to pre-declare it).")));
                }
            } else {
                return Err(VMError::new_compile("dec!: expected symbol"));
            };
            if cdr.len() == 1 {
                state.chunk.encode2(DEC, dest as u16, 1, env.own_line())?;
            } else if cdr.len() == 2 {
                let amount = match cdr[1] {
                    Value::Byte(i) => i as u16,
                    Value::Int(i) if i >= 0 && i <= u16::MAX as i64 => i as u16,
                    Value::Int(_) => return Err(VMError::new_compile("inc!: second arg to large")),
                    Value::UInt(i) if i <= u16::MAX as u64 => i as u16,
                    Value::UInt(_) => {
                        return Err(VMError::new_compile("inc!: second arg < 0 or to large"))
                    }
                    _ => return Err(VMError::new_compile("inc!: second arg must be integer")),
                };
                state
                    .chunk
                    .encode2(DEC, dest as u16, amount, env.own_line())?;
            } else {
                return Err(VMError::new_compile("dec!: malformed"));
            }
        }
        Value::Symbol(i) if i == state.specials.add => {
            if cdr.is_empty() {
                compile(env, state, Value::Int(0), result)?;
            } else if cdr.len() == 1 {
                compile(env, state, cdr[0], result)?;
            } else {
                for (i, v) in cdr.iter().enumerate() {
                    if i > 0 {
                        compile(env, state, *v, result + 1)?;
                        state.chunk.encode3(
                            ADDM,
                            result as u16,
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
        Value::Symbol(i) if i == state.specials.sub => {
            if cdr.is_empty() {
                return Err(VMError::new_compile(
                    "Malformed -, requires at least one argument.",
                ));
            } else if cdr.len() == 1 {
                if let Ok(i) = cdr[0].get_int() {
                    compile(env, state, Value::Int(-i), result)?;
                } else if let Ok(f) = cdr[0].get_float() {
                    compile(env, state, Value::float(-f), result)?;
                }
            } else {
                for (i, v) in cdr.iter().enumerate() {
                    if i > 0 {
                        compile(env, state, *v, result + 1)?;
                        state.chunk.encode3(
                            SUBM,
                            result as u16,
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
        Value::Symbol(i) if i == state.specials.mul => {
            if cdr.is_empty() {
                compile(env, state, Value::Int(1), result)?;
            } else if cdr.len() == 1 {
                compile(env, state, cdr[0], result)?;
            } else {
                for (i, v) in cdr.iter().enumerate() {
                    if i > 0 {
                        compile(env, state, *v, result + 1)?;
                        state.chunk.encode3(
                            MULM,
                            result as u16,
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
        Value::Symbol(i) if i == state.specials.div => {
            if cdr.len() <= 1 {
                return Err(VMError::new_compile(
                    "Malformed /, requires at least two arguments.",
                ));
            } else {
                for (i, v) in cdr.iter().enumerate() {
                    if i > 0 {
                        compile(env, state, *v, result + 1)?;
                        state.chunk.encode3(
                            DIVM,
                            result as u16,
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
        Value::Symbol(i) if i == state.specials.numeq => {
            make_math_comp(env, state, cdr, result, NUMEQ)?;
        }
        Value::Symbol(i) if i == state.specials.numneq => {
            make_math_comp(env, state, cdr, result, NUMNEQ)?;
        }
        Value::Symbol(i) if i == state.specials.numlt => {
            make_math_comp(env, state, cdr, result, NUMLT)?;
        }
        Value::Symbol(i) if i == state.specials.numlte => {
            make_math_comp(env, state, cdr, result, NUMLTE)?;
        }
        Value::Symbol(i) if i == state.specials.numgt => {
            make_math_comp(env, state, cdr, result, NUMGT)?;
        }
        Value::Symbol(i) if i == state.specials.numgte => {
            make_math_comp(env, state, cdr, result, NUMGTE)?;
        }
        _ => return Ok(false),
    }
    Ok(true)
}

fn compile_cons(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    car: Value,
    cdr: &[Value],
    result: usize,
) -> VMResult<bool> {
    match car {
        Value::Symbol(i) if i == state.specials.list => {
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
        Value::Symbol(i) if i == state.specials.list_append => {
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
        Value::Symbol(i) if i == state.specials.cons => {
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
        Value::Symbol(i) if i == state.specials.car => {
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
        Value::Symbol(i) if i == state.specials.cdr => {
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
        Value::Symbol(i) if i == state.specials.xar => {
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
        Value::Symbol(i) if i == state.specials.xdr => {
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

fn compile_vec(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    car: Value,
    cdr: &[Value],
    result: usize,
) -> VMResult<bool> {
    match car {
        Value::Symbol(i) if i == state.specials.vec => {
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
        Value::Symbol(i) if i == state.specials.make_vec => {
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
        Value::Symbol(i) if i == state.specials.vec_push => {
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
        Value::Symbol(i) if i == state.specials.vec_pop => {
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
        Value::Symbol(i) if i == state.specials.vec_nth => {
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
        Value::Symbol(i) if i == state.specials.vec_set => {
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
        Value::Symbol(i) if i == state.specials.vec_len => {
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
        Value::Symbol(i) if i == state.specials.vec_clr => {
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

fn compile_if(
    env: &mut CompileEnvironment,
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

fn compile_while(
    env: &mut CompileEnvironment,
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

fn compile_and(
    env: &mut CompileEnvironment,
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

fn compile_or(
    env: &mut CompileEnvironment,
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

fn compile_def(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
) -> VMResult<()> {
    match (cdr.len(), cdr.get(0)) {
        (_, None) => return Err(VMError::new_compile("def: expected symbol")),
        (1, Some(Value::Symbol(si))) => {
            // 'def symbol' predeclares a symbol to be used later, no bytecode.
            let _ = env.vm.reserve_index(*si);
        }
        (2, Some(Value::Symbol(si))) => {
            compile(env, state, cdr[1], result + 1)?;
            let si_const = env.vm.reserve_index(*si);
            state
                .chunk
                .encode_refi(result as u16, si_const, env.own_line())?;
            state
                .chunk
                .encode2(DEF, result as u16, (result + 1) as u16, env.own_line())?;
        }
        (3, Some(Value::Symbol(si))) => {
            let si_const = env.vm.reserve_index(*si);
            // Set docstring
            let set_prop = env.vm.intern("set-prop");
            if let Some(set_prop) = env.vm.global_intern_slot(set_prop) {
                let doc_const = state
                    .chunk
                    .add_constant(Value::Keyword(env.vm.intern("doc-string")));
                state
                    .chunk
                    .encode_refi((result + 1) as u16, si_const, env.own_line())?;
                state.chunk.encode2(
                    CONST,
                    (result + 2) as u16,
                    doc_const as u16,
                    env.own_line(),
                )?;
                compile(env, state, cdr[1], result + 3)?;
                state
                    .chunk
                    .encode_callg(set_prop as u32, 3, result as u16, env.own_line())?;
            }

            compile(env, state, cdr[2], result + 1)?;
            state
                .chunk
                .encode_refi(result as u16, si_const, env.own_line())?;
            state
                .chunk
                .encode2(DEF, result as u16, (result + 1) as u16, env.own_line())?;
        }
        _ => return Err(VMError::new_compile("def: malformed")),
    }
    Ok(())
}

fn compile_set(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
) -> VMResult<()> {
    if cdr.len() == 2 {
        if let Value::Symbol(si) = cdr[0] {
            if let Some(idx) = state.get_symbol(si) {
                compile(env, state, cdr[1], result)?;
                state
                    .chunk
                    .encode2(SET, (idx + 1) as u16, result as u16, env.own_line())?;
            } else if let Some(si_const) = env.vm.global_intern_slot(si) {
                compile(env, state, cdr[1], result + 1)?;
                state
                    .chunk
                    .encode_refi(result as u16, si_const, env.own_line())?;
                state
                    .chunk
                    .encode2(DEF, result as u16, (result + 1) as u16, env.own_line())?;
            } else {
                let sym = env.vm.get_interned(si);
                return Err(VMError::new_compile(format!("Symbol {sym} not defined (maybe you need to use 'def {sym}' to pre-declare it).")));
            }
        } else {
            return Err(VMError::new_compile("set!: expected symbol"));
        }
    } else {
        return Err(VMError::new_compile("set!: malformed"));
    }
    Ok(())
}

fn compile_let(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
    star: bool,
) -> VMResult<()> {
    fn inner(
        env: &mut CompileEnvironment,
        state: &mut CompileState,
        cdr: &[Value],
        result: usize,
        star: bool,
        old_tail: bool,
    ) -> VMResult<()> {
        let start_defers = state.defers;
        let symbols = Rc::new(RefCell::new(Symbols::with_let(
            state.symbols.clone(),
            result,
        )));
        if star {
            state.symbols = symbols.clone();
        }
        let mut cdr_iter = cdr.iter();
        let args = cdr_iter.next().unwrap(); // unwrap safe, length is at least 1
        let mut opt_comps: Vec<(usize, Value)> = Vec::new();
        let scratch = env.vm.intern("[SCRATCH]");
        env.set_line_val(state, *args);
        let args_iter: Vec<Value> = get_args_iter(env, *args, "let")?.collect();
        // XXX fixme
        //new_state.chunk.dbg_args = Some(Vec::new());
        for a in args_iter {
            env.set_line_val(state, a);
            let mut args_iter = get_args_iter(env, a, "let")?;
            if let Some(Value::Symbol(i)) = args_iter.next() {
                let reg = symbols.borrow_mut().insert(i) + 1;
                if let Some(dbg_args) = state.chunk.dbg_args.as_mut() {
                    if dbg_args.len() < reg - 1 {
                        dbg_args.resize(reg - 1, scratch);
                    }
                    if reg < dbg_args.len() {
                        // This register will have multiple names, maybe concat them or something.
                        dbg_args[reg] = i;
                    } else {
                        dbg_args.push(i);
                    }
                }
                if let Some(r) = args_iter.next() {
                    opt_comps.push((reg, r));
                } else {
                    opt_comps.push((reg, Value::Nil));
                }
                // XXX Check to make sure only two elements...
            }
        }
        let mut free_reg = result;
        for (reg, val) in opt_comps {
            compile(env, state, val, reg)?;
            free_reg = reg + 1;
        }
        if !star {
            state.symbols = symbols;
        }
        // TODO XXX check for underflow...
        let last_thing = cdr.len() - 2;
        for (i, r) in cdr_iter.enumerate() {
            if i == last_thing {
                state.tail = old_tail;
            }
            compile(env, state, *r, free_reg)?;
        }
        if free_reg != result {
            state
                .chunk
                .encode2(MOV, result as u16, free_reg as u16, env.own_line())?;
        }
        for _ in start_defers..state.defers {
            state.chunk.encode0(DFRPOP, env.own_line())?;
        }
        Ok(())
    }

    if cdr.is_empty() {
        return Err(VMError::new_compile(
            "Too few arguments, need at least 1 got 0.",
        ));
    }
    let old_symbols = state.symbols.clone();
    let old_tail = state.tail;
    state.tail = false;
    let old_defers = state.defers;
    let result = inner(env, state, cdr, result, star, old_tail);
    state.tail = old_tail;
    state.symbols = old_symbols;
    state.defers = old_defers;
    result
}

fn is_macro(env: &CompileEnvironment, val: Value) -> bool {
    match val {
        Value::Lambda(h) => matches!(env.vm.get_heap_property(h, ":macro"), Some(Value::True)),
        Value::Closure(h) => matches!(env.vm.get_heap_property(h, ":macro"), Some(Value::True)),
        _ => false,
    }
}

fn compile_list(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    car: Value,
    cdr: &[Value],
    result: usize,
) -> VMResult<()> {
    if !(compile_math(env, state, car, cdr, result)?
        || compile_cons(env, state, car, cdr, result)?
        || compile_vec(env, state, car, cdr, result)?)
    {
        match car {
            Value::Symbol(i) if i == state.specials.fn_ => {
                if cdr.len() > 1 {
                    compile_fn(env, state, cdr[0], &cdr[1..], result, false)?
                } else {
                    return Err(VMError::new_compile("Malformed fn form."));
                }
            }
            Value::Symbol(i) if i == state.specials.mac_ => {
                if cdr.len() > 1 {
                    compile_fn(env, state, cdr[0], &cdr[1..], result, true)?
                } else {
                    return Err(VMError::new_compile("Malformed macro form."));
                }
            }
            Value::Symbol(i) if i == state.specials.if_ => {
                compile_if(env, state, cdr, result)?;
            }
            Value::Symbol(i) if i == state.specials.while_ => {
                let tail = state.tail;
                state.tail = false;
                compile_while(env, state, cdr, result)?;
                state.tail = tail;
            }
            Value::Symbol(i) if i == state.specials.do_ => {
                let last_thing = cdr.len() - 1;
                let old_tail = state.tail;
                state.tail = false;
                for (i, r) in cdr.iter().enumerate() {
                    if i == last_thing {
                        state.tail = old_tail;
                    }
                    compile(env, state, *r, result)?;
                }
            }
            Value::Symbol(i) if i == state.specials.def => {
                state.tail = false;
                compile_def(env, state, cdr, result)?;
            }
            Value::Symbol(i) if i == state.specials.set => {
                state.tail = false;
                compile_set(env, state, cdr, result)?;
            }
            Value::Symbol(i) if i == state.specials.quote => {
                state.tail = false;
                if cdr.len() != 1 {
                    return Err(VMError::new_compile(format!(
                        "quote takes one argument, got {}, line {}",
                        cdr.len(),
                        env.line_num()
                    )));
                }
                mkconst(env, state, cdr[0], result)?;
            }
            Value::Symbol(i) if i == state.specials.backquote => {
                state.tail = false;
                if cdr.len() != 1 {
                    return Err(VMError::new_compile(format!(
                        "backquote takes one argument, got {}, line {}",
                        cdr.len(),
                        env.line_num()
                    )));
                }
                backquote(env, state, cdr[0], result)?;
            }
            Value::Symbol(i) if i == state.specials.recur => {
                compile_call_myself(env, state, cdr, result, true)?
            }
            Value::Symbol(i) if i == state.specials.this_fn => {
                compile_call_myself(env, state, cdr, result, false)?
            }
            Value::Symbol(i) if i == state.specials.eq => {
                if cdr.len() <= 1 {
                    return Err(VMError::new_compile("Requires at least two arguments."));
                } else {
                    let mut max = 0;
                    for (i, v) in cdr.iter().enumerate() {
                        compile(env, state, *v, result + i + 1)?;
                        max = result + i + 1;
                    }
                    state.chunk.encode3(
                        EQ,
                        result as u16,
                        (result + 1) as u16,
                        max as u16,
                        env.own_line(),
                    )?;
                }
            }
            Value::Symbol(i) if i == state.specials.equal => {
                if cdr.len() <= 1 {
                    return Err(VMError::new_compile("Requires at least two arguments. 2"));
                } else {
                    let mut max = 0;
                    for (i, v) in cdr.iter().enumerate() {
                        compile(env, state, *v, result + i + 1)?;
                        max = result + i + 1;
                    }
                    state.chunk.encode3(
                        EQUAL,
                        result as u16,
                        (result + 1) as u16,
                        max as u16,
                        env.own_line(),
                    )?;
                }
            }
            Value::Symbol(i) if i == state.specials.type_ => {
                if cdr.len() != 1 {
                    return Err(VMError::new_compile("Requires one argument."));
                } else {
                    compile(env, state, cdr[0], result + 1)?;
                    state.chunk.encode2(
                        TYPE,
                        result as u16,
                        (result + 1) as u16,
                        env.own_line(),
                    )?;
                }
            }
            Value::Symbol(i) if i == state.specials.not => {
                if cdr.len() != 1 {
                    return Err(VMError::new_compile("Requires one argument."));
                } else {
                    compile(env, state, cdr[0], result + 1)?;
                    state
                        .chunk
                        .encode2(NOT, result as u16, (result + 1) as u16, env.own_line())?;
                }
            }
            Value::Symbol(i) if i == state.specials.err => {
                let len = cdr.len();
                if len != 1 && len != 2 {
                    return Err(VMError::new_compile("Requires one or two arguments."));
                } else {
                    if len == 2 {
                        compile(env, state, cdr[0], result)?;
                        compile(env, state, cdr[1], result + 1)?;
                    } else {
                        let error = env.vm.intern("error");
                        compile(env, state, Value::Keyword(error), result)?;
                        compile(env, state, cdr[0], result + 1)?;
                    }
                    state
                        .chunk
                        .encode2(ERR, result as u16, (result + 1) as u16, env.own_line())?;
                }
            }
            Value::Symbol(i) if i == state.specials.and => {
                compile_and(env, state, cdr, result)?;
            }
            Value::Symbol(i) if i == state.specials.or => {
                compile_or(env, state, cdr, result)?;
            }
            Value::Symbol(i) if i == state.specials.str_ => {
                let mut max = 0;
                for (i, v) in cdr.iter().enumerate() {
                    compile(env, state, *v, result + i + 1)?;
                    max = result + i + 1;
                }
                state.chunk.encode3(
                    STR,
                    result as u16,
                    (result + 1) as u16,
                    max as u16,
                    env.own_line(),
                )?;
            }
            Value::Symbol(i) if i == state.specials.let_ => {
                compile_let(env, state, cdr, result, false)?;
            }
            Value::Symbol(i) if i == state.specials.letstar => {
                compile_let(env, state, cdr, result, true)?;
            }
            Value::Symbol(i) if i == state.specials.call_cc => {
                if cdr.len() != 1 {
                    return Err(VMError::new_compile("Requires one argument."));
                }
                compile(env, state, cdr[0], result)?;
                state
                    .chunk
                    .encode2(CCC, result as u16, result as u16, env.own_line())?;
            }
            Value::Symbol(i) if i == state.specials.defer => {
                if !cdr.is_empty() {
                    compile_fn(env, state, Value::Nil, &cdr[0..], result, false)?;
                    state.chunk.encode1(DFR, result as u16, env.own_line())?;
                    state.defers += 1;
                } else {
                    return Err(VMError::new_compile(
                        "Malformed defer form, need at least one form.",
                    ));
                }
            }
            Value::Symbol(i) if i == state.specials.on_error => {
                if cdr.len() != 1 {
                    return Err(VMError::new_compile("Requires one argument."));
                }
                compile(env, state, cdr[0], result)?;
                state.chunk.encode1(ONERR, result as u16, env.own_line())?;
            }
            Value::Symbol(i) => {
                if let Some(idx) = state.get_symbol(i) {
                    compile_call_reg(env, state, (idx + 1) as u16, cdr, result)?
                } else if let Some(slot) = env.vm.global_intern_slot(i) {
                    // Have to at least pre-declare a global.
                    let global = env.vm.get_global(slot);
                    if let Value::Undefined = global {
                        eprintln!("Warning: {} not defined.", env.vm.get_interned(i));
                    }
                    if is_macro(env, global) {
                        match global {
                            Value::Lambda(h) => {
                                let mac = env.vm.get_lambda(h);
                                env.vm.pause_gc();
                                let exp = env.vm.do_call(mac, cdr, None)?;
                                env.vm.unpause_gc();
                                pass1(env, state, exp)?;
                                compile(env, state, exp, result)?
                            }
                            Value::Closure(h) => {
                                let (mac, caps) = env.vm.get_closure(h);
                                let caps = caps.to_vec();
                                env.vm.pause_gc();
                                let exp = env.vm.do_call(mac, cdr, Some(&caps))?;
                                env.vm.unpause_gc();
                                pass1(env, state, exp)?;
                                compile(env, state, exp, result)?
                            }
                            _ => panic!("Invalid macro!"),
                        }
                    } else {
                        compile_callg(env, state, slot as u32, cdr, result)?
                    }
                } else {
                    let sym = env.vm.get_interned(i);
                    return Err(VMError::new_compile(format!("Symbol {sym} not defined (maybe you need to use 'def {sym}' to pre-declare it).")));
                }
            }
            Value::Builtin(builtin) => {
                compile_call(env, state, Value::Builtin(builtin), cdr, result)?
            }
            Value::Lambda(h) => compile_call(env, state, Value::Lambda(h), cdr, result)?,
            Value::Pair(h) | Value::List(h, _) => {
                let (ncar, ncdr) = car.get_pair(env.vm).expect("Pair/List not a Pair or List?");
                env.set_line(state, h);
                let ncdr: Vec<Value> = ncdr.iter(env.vm).collect();
                compile_list(env, state, ncar, &ncdr[..], result)?;
                compile_call_reg(env, state, result as u16, cdr, result)?
            }
            _ => {
                println!("Boo, {}", car.display_value(env.vm));
            }
        }
    }
    Ok(())
}

pub fn pass1(env: &mut CompileEnvironment, state: &mut CompileState, exp: Value) -> VMResult<()> {
    let fn_ = env.vm.intern("fn");
    let mac_ = env.vm.intern("macro");
    //let def_ = env.vm.intern("def");
    match exp {
        Value::Pair(_) | Value::List(_, _) => {
            let (car, _) = exp.get_pair(env.vm).expect("Pair/List not a Pair or List?");
            // short circuit on an fn form, will be handled with it's own state.
            if let Value::Symbol(i) = car {
                if i == fn_ || i == mac_ {
                    return Ok(());
                }
            }
            // XXX boo on this collect.
            for r in exp.iter(env.vm).collect::<Vec<Value>>() {
                pass1(env, state, r)?;
            }
        }
        Value::Symbol(i) => {
            if state.get_symbol(i).is_none() && state.symbols.borrow().can_capture(i) {
                state.symbols.borrow_mut().insert_capture(env.vm, i);
                if let Some(dbg_args) = state.chunk.dbg_args.as_mut() {
                    dbg_args.push(i);
                }
            }
        }
        Value::True => {}
        Value::False => {}
        Value::Nil => {}
        Value::Undefined => {}
        Value::Byte(_) => {}
        Value::Int(i) if i >= 0 && i <= u16::MAX as i64 => {}
        Value::UInt(i) if i <= u16::MAX as u64 => {}

        Value::String(_) => {}
        Value::Bytes(_) => {}
        Value::Lambda(_) => {}
        Value::Closure(_) => {}
        Value::Continuation(_) => {}
        Value::CallFrame(_) => {}
        Value::Value(_) => {}

        _ => {
            state.add_constant(exp);
        }
    }
    Ok(())
}

pub fn mkconst(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    exp: Value,
    result: usize,
) -> VMResult<()> {
    match exp {
        Value::True => state.chunk.encode1(REGT, result as u16, env.own_line())?,
        Value::False => state.chunk.encode1(REGF, result as u16, env.own_line())?,
        Value::Nil => state.chunk.encode1(REGN, result as u16, env.own_line())?,
        Value::Undefined => state.chunk.encode1(REGC, result as u16, env.own_line())?,
        Value::Byte(i) => state
            .chunk
            .encode2(REGB, result as u16, i as u16, env.own_line())?,
        Value::Int(i) if i >= 0 && i <= u16::MAX as i64 => {
            state
                .chunk
                .encode2(REGI, result as u16, i as u16, env.own_line())?;
        }
        Value::UInt(i) if i <= u16::MAX as u64 => {
            state
                .chunk
                .encode2(REGU, result as u16, i as u16, env.own_line())?;
        }
        _ => {
            let const_i = state.add_constant(exp);
            state
                .chunk
                .encode2(CONST, result as u16, const_i as u16, env.own_line())?;
        }
    }
    Ok(())
}

pub fn compile(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    exp: Value,
    result: usize,
) -> VMResult<()> {
    if state.max_regs < result {
        state.max_regs = result;
    }
    match exp {
        Value::Pair(handle) | Value::List(handle, _) => {
            let (car, cdr) = exp.get_pair(env.vm).expect("Pair/List not a Pair or List?");
            env.set_line(state, handle);
            let cdr: Vec<Value> = cdr.iter(env.vm).collect();
            compile_list(env, state, car, &cdr[..], result)?;
        }
        Value::Symbol(i) => {
            if let Some(idx) = state.get_symbol(i) {
                if result != idx + 1 {
                    state
                        .chunk
                        .encode2(MOV, result as u16, (idx + 1) as u16, env.own_line())?;
                }
            } else if let Some(slot) = env.vm.global_intern_slot(i) {
                state
                    .chunk
                    .encode_refi(result as u16, slot, env.own_line())?;
            } else {
                let sym = env.vm.get_interned(i);
                return Err(VMError::new_compile(format!("Symbol {sym} not defined (maybe you need to use 'def {sym}' to pre-declare it).")));
            }
        }
        Value::True => state.chunk.encode1(REGT, result as u16, env.own_line())?,
        Value::False => state.chunk.encode1(REGF, result as u16, env.own_line())?,
        Value::Nil => state.chunk.encode1(REGN, result as u16, env.own_line())?,
        Value::Undefined => state.chunk.encode1(REGC, result as u16, env.own_line())?,
        Value::Byte(i) => state
            .chunk
            .encode2(REGB, result as u16, i as u16, env.own_line())?,
        Value::Int(i) if i >= 0 && i <= u16::MAX as i64 => {
            state
                .chunk
                .encode2(REGI, result as u16, i as u16, env.own_line())?
        }
        Value::UInt(i) if i <= u16::MAX as u64 => {
            state
                .chunk
                .encode2(REGU, result as u16, i as u16, env.own_line())?
        }
        _ => {
            // XXX this used to ignore References but now does not, is that good?
            let const_i = state.add_constant(exp);
            state
                .chunk
                .encode2(CONST, result as u16, const_i as u16, env.own_line())?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod compile_tests;
