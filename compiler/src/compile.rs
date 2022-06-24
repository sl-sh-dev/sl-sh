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

fn compile_params(
    vm: &mut Vm,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
    tail: bool,
    line: &mut Option<&mut u32>,
) -> VMResult<()> {
    for (i, r) in cdr.iter().enumerate() {
        compile(vm, state, *r, result + i, line)?;
    }
    let line = own_line(line);
    if tail {
        state
            .chunk
            .encode3(BMOV, 1, result as u16, cdr.len() as u16, line)?;
    }
    Ok(())
}

fn compile_call(
    vm: &mut Vm,
    state: &mut CompileState,
    callable: Value,
    cdr: &[Value],
    result: usize,
    line: &mut Option<&mut u32>,
) -> VMResult<()> {
    let b_reg = result + cdr.len() + 1;
    let const_i = state.add_constant(callable);
    let tail = state.tail && state.defers == 0;
    state.tail = false;
    compile_params(vm, state, cdr, result + 1, tail, line)?;
    let line = own_line(line);
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
    vm: &mut Vm,
    state: &mut CompileState,
    global: u32,
    cdr: &[Value],
    result: usize,
    line: &mut Option<&mut u32>,
) -> VMResult<()> {
    let tail = state.tail && state.defers == 0;
    state.tail = false;
    compile_params(vm, state, cdr, result + 1, tail, line)?;
    let line = own_line(line);
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
    vm: &mut Vm,
    state: &mut CompileState,
    reg: u16,
    cdr: &[Value],
    result: usize,
    line: &mut Option<&mut u32>,
) -> VMResult<()> {
    let tail = state.tail && state.defers == 0;
    state.tail = false;
    let b_reg = if tail {
        let b_reg = result + cdr.len() + 2;
        state
            .chunk
            .encode2(MOV, b_reg as u16, reg as u16, own_line(line))?;
        b_reg
    } else {
        0
    };
    compile_params(vm, state, cdr, result + 1, tail, line)?;
    let line = own_line(line);
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
    vm: &mut Vm,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
    line: &mut Option<&mut u32>,
    force_tail: bool,
) -> VMResult<()> {
    let tail = force_tail || (state.tail && state.defers == 0);
    state.tail = false;
    compile_params(vm, state, cdr, result + 1, tail, line)?;
    let line = own_line(line);
    if tail {
        state.chunk.encode1(TCALLM, cdr.len() as u16, line)?;
    } else {
        state
            .chunk
            .encode2(CALLM, cdr.len() as u16, result as u16, line)?;
    }
    Ok(())
}

fn set_line(vm: &Vm, state: &mut CompileState, handle: Handle, line: &mut Option<&mut u32>) {
    if let (Some(Value::UInt(dline)), Some(Value::StringConst(file_intern)), Some(line)) = (
        vm.get_heap_property(handle, "dbg-line"),
        vm.get_heap_property(handle, "dbg-file"),
        line,
    ) {
        let file_name = vm.get_interned(file_intern);
        if file_name == state.chunk.file_name && dline as u32 > **line {
            **line = dline as u32;
        }
    }
}

fn get_args_iter<'vm>(
    vm: &'vm Vm,
    state: &mut CompileState,
    args: Value,
    name: &str,
    line: &mut Option<&mut u32>,
) -> VMResult<Box<dyn Iterator<Item = Value> + 'vm>> {
    match args {
        Value::Pair(handle) => {
            let (_, _) = vm.get_pair(handle);
            set_line(vm, state, handle, line);
            Ok(args.iter(vm))
        }
        Value::Vector(_v) => Ok(args.iter(vm)),
        Value::Nil => Ok(args.iter(vm)),
        _ => {
            return Err(VMError::new_compile(format!("{}, invalid args", name)));
        }
    }
}

fn mk_state(
    vm: &mut Vm,
    state: &mut CompileState,
    args: Value,
    line: &mut Option<&mut u32>,
) -> VMResult<(CompileState, Option<Vec<Value>>)> {
    let mut new_state = CompileState::new_state(
        vm,
        state.chunk.file_name,
        own_line(line).unwrap_or(1),
        Some(state.symbols.clone()),
    );
    let args_iter = get_args_iter(vm, &mut new_state, args, "fn", line)?;
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
            Value::Pair(_) | Value::Vector(_) => {
                let mut args_iter = get_args_iter(vm, &mut new_state, a, "fn", line)?;
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
    vm: &mut Vm,
    state: &mut CompileState,
    args: Value,
    cdr: &[Value],
    result: usize,
    line: &mut Option<&mut u32>,
    is_macro: bool,
) -> VMResult<()> {
    let mut new_owned_line = own_line(line).unwrap_or(1);
    let mut new_line = if line.is_some() {
        Some(&mut new_owned_line)
    } else {
        None
    };
    let (mut new_state, opt_comps) = mk_state(vm, state, args, &mut new_line)?;
    for r in cdr.iter() {
        pass1(vm, &mut new_state, *r).unwrap();
    }
    let reserved = new_state.reserved_regs();
    if let Some(opt_comps) = opt_comps {
        for (i, r) in opt_comps.into_iter().enumerate() {
            let target_reg = new_state.chunk.args as usize + i + 1;
            new_state
                .chunk
                .encode1(JMPNU, target_reg as u16, own_line(&new_line))?;
            let encode_offset = new_state.chunk.code.len();
            new_state.chunk.encode_jump_offset(0)?;
            let start_offset = new_state.chunk.code.len();
            compile(vm, &mut new_state, r, reserved, &mut new_line)?;
            new_state.chunk.encode2(
                MOV,
                target_reg as u16,
                reserved as u16,
                own_line(&new_line),
            )?;
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
        compile(vm, &mut new_state, *r, reserved, &mut new_line)?;
    }
    new_state
        .chunk
        .encode1(SRET, reserved as u16, own_line(&new_line))
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
    vm.pause_gc();
    let lambda = vm.alloc_lambda(Arc::new(new_state.chunk));
    vm.unpause_gc();
    if is_macro {
        // Unwrap safe since we just allocated lambda on the heap.
        vm.set_heap_property(lambda.get_handle().unwrap(), ":macro", Value::True);
    }
    let const_i = state.add_constant(lambda);
    state
        .chunk
        .encode2(CONST, result as u16, const_i as u16, own_line(line))?;
    if closure {
        state
            .chunk
            .encode2(CLOSE, result as u16, result as u16, own_line(line))?;
    }
    Ok(())
}

fn make_math_comp(
    vm: &mut Vm,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
    line: &mut Option<&mut u32>,
    op: u8,
) -> VMResult<()> {
    if cdr.len() <= 1 {
        return Err(VMError::new_compile("Requires at least two arguments."));
    } else {
        let mut max = 0;
        for (i, v) in cdr.iter().enumerate() {
            max = result + i + 1;
            compile(vm, state, *v, max, line)?;
        }
        state.chunk.encode3(
            op,
            result as u16,
            (result + 1) as u16,
            max as u16,
            own_line(line),
        )?;
    }
    Ok(())
}

fn compile_math(
    vm: &mut Vm,
    state: &mut CompileState,
    car: Value,
    cdr: &[Value],
    result: usize,
    line: &mut Option<&mut u32>,
) -> VMResult<bool> {
    match car {
        Value::Symbol(i) if i == state.specials.inc => {
            let dest = if let Value::Symbol(si) = cdr[0] {
                if let Some(idx) = state.get_symbol(si) {
                    idx + 1
                } else {
                    let const_i = vm.reserve_index(si);
                    state
                        .chunk
                        .encode_refi(result as u16, const_i, own_line(line))?;
                    result
                }
            } else {
                return Err(VMError::new_compile("inc!: expected symbol"));
            };
            if cdr.len() == 1 {
                state.chunk.encode2(INC, dest as u16, 1, own_line(line))?;
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
                    .encode2(INC, dest as u16, amount, own_line(line))?;
            } else {
                return Err(VMError::new_compile("inc!: malformed"));
            }
        }
        Value::Symbol(i) if i == state.specials.dec => {
            let dest = if let Value::Symbol(si) = cdr[0] {
                if let Some(idx) = state.get_symbol(si) {
                    idx + 1
                } else {
                    let const_i = vm.reserve_index(si);
                    state
                        .chunk
                        .encode_refi(result as u16, const_i, own_line(line))?;
                    result
                }
            } else {
                return Err(VMError::new_compile("dec!: expected symbol"));
            };
            if cdr.len() == 1 {
                state.chunk.encode2(DEC, dest as u16, 1, own_line(line))?;
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
                    .encode2(DEC, dest as u16, amount, own_line(line))?;
            } else {
                return Err(VMError::new_compile("dec!: malformed"));
            }
        }
        Value::Symbol(i) if i == state.specials.add => {
            if cdr.is_empty() {
                compile(vm, state, Value::Int(0), result, line)?;
            } else if cdr.len() == 1 {
                compile(vm, state, cdr[0], result, line)?;
            } else {
                for (i, v) in cdr.iter().enumerate() {
                    if i > 0 {
                        compile(vm, state, *v, result + 1, line)?;
                        state.chunk.encode3(
                            ADDM,
                            result as u16,
                            result as u16,
                            (result + 1) as u16,
                            own_line(line),
                        )?;
                    } else {
                        compile(vm, state, *v, result, line)?;
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
                    compile(vm, state, Value::Int(-i), result, line)?;
                } else if let Ok(f) = cdr[0].get_float() {
                    compile(vm, state, Value::float(-f), result, line)?;
                }
            } else {
                for (i, v) in cdr.iter().enumerate() {
                    if i > 0 {
                        compile(vm, state, *v, result + 1, line)?;
                        state.chunk.encode3(
                            SUBM,
                            result as u16,
                            result as u16,
                            (result + 1) as u16,
                            own_line(line),
                        )?;
                    } else {
                        compile(vm, state, *v, result, line)?;
                    }
                }
            }
        }
        Value::Symbol(i) if i == state.specials.mul => {
            if cdr.is_empty() {
                compile(vm, state, Value::Int(1), result, line)?;
            } else if cdr.len() == 1 {
                compile(vm, state, cdr[0], result, line)?;
            } else {
                for (i, v) in cdr.iter().enumerate() {
                    if i > 0 {
                        compile(vm, state, *v, result + 1, line)?;
                        state.chunk.encode3(
                            MULM,
                            result as u16,
                            result as u16,
                            (result + 1) as u16,
                            own_line(line),
                        )?;
                    } else {
                        compile(vm, state, *v, result, line)?;
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
                        compile(vm, state, *v, result + 1, line)?;
                        state.chunk.encode3(
                            DIVM,
                            result as u16,
                            result as u16,
                            (result + 1) as u16,
                            own_line(line),
                        )?;
                    } else {
                        compile(vm, state, *v, result, line)?;
                    }
                }
            }
        }
        Value::Symbol(i) if i == state.specials.numeq => {
            make_math_comp(vm, state, cdr, result, line, NUMEQ)?;
        }
        Value::Symbol(i) if i == state.specials.numneq => {
            make_math_comp(vm, state, cdr, result, line, NUMNEQ)?;
        }
        Value::Symbol(i) if i == state.specials.numlt => {
            make_math_comp(vm, state, cdr, result, line, NUMLT)?;
        }
        Value::Symbol(i) if i == state.specials.numlte => {
            make_math_comp(vm, state, cdr, result, line, NUMLTE)?;
        }
        Value::Symbol(i) if i == state.specials.numgt => {
            make_math_comp(vm, state, cdr, result, line, NUMGT)?;
        }
        Value::Symbol(i) if i == state.specials.numgte => {
            make_math_comp(vm, state, cdr, result, line, NUMGTE)?;
        }
        _ => return Ok(false),
    }
    Ok(true)
}

fn compile_cons(
    vm: &mut Vm,
    state: &mut CompileState,
    car: Value,
    cdr: &[Value],
    result: usize,
    line: &mut Option<&mut u32>,
) -> VMResult<bool> {
    match car {
        Value::Symbol(i) if i == state.specials.list => {
            state.tail = false;
            let mut max = 0;
            for r in cdr {
                compile(vm, state, *r, result + max + 1, line)?;
                max += 1;
            }
            state.chunk.encode3(
                LIST,
                result as u16,
                (result + 1) as u16,
                (result + max) as u16,
                own_line(line),
            )?;
        }
        Value::Symbol(i) if i == state.specials.list_append => {
            state.tail = false;
            let mut max = 0;
            for r in cdr {
                compile(vm, state, *r, result + max + 1, line)?;
                max += 1;
            }
            state.chunk.encode3(
                APND,
                result as u16,
                (result + 1) as u16,
                (result + max) as u16,
                own_line(line),
            )?;
        }
        Value::Symbol(i) if i == state.specials.cons => {
            state.tail = false;
            if cdr.len() != 2 {
                return Err(VMError::new_compile(format!(
                    "takes two arguments, got {}, line {}",
                    cdr.len(),
                    line_num(line)
                )));
            }
            compile(vm, state, cdr[0], result, line)?;
            compile(vm, state, cdr[1], result + 1, line)?;
            state.chunk.encode3(
                CONS,
                result as u16,
                result as u16,
                (result + 1) as u16,
                own_line(line),
            )?;
        }
        Value::Symbol(i) if i == state.specials.car => {
            state.tail = false;
            if cdr.len() != 1 {
                return Err(VMError::new_compile(format!(
                    "takes one argument, got {}, line {}",
                    cdr.len(),
                    line_num(line)
                )));
            }
            compile(vm, state, cdr[0], result + 1, line)?;
            state
                .chunk
                .encode2(CAR, result as u16, (result + 1) as u16, own_line(line))?;
        }
        Value::Symbol(i) if i == state.specials.cdr => {
            state.tail = false;
            if cdr.len() != 1 {
                return Err(VMError::new_compile(format!(
                    "takes one argument, got {}, line {}",
                    cdr.len(),
                    line_num(line)
                )));
            }
            compile(vm, state, cdr[0], result + 1, line)?;
            state
                .chunk
                .encode2(CDR, result as u16, (result + 1) as u16, own_line(line))?;
        }
        Value::Symbol(i) if i == state.specials.xar => {
            state.tail = false;
            if cdr.len() != 2 {
                return Err(VMError::new_compile(format!(
                    "takes two arguments, got {}, line {}",
                    cdr.len(),
                    line_num(line)
                )));
            }
            compile(vm, state, cdr[0], result, line)?;
            compile(vm, state, cdr[1], result + 1, line)?;
            state
                .chunk
                .encode2(XAR, result as u16, (result + 1) as u16, own_line(line))?;
        }
        Value::Symbol(i) if i == state.specials.xdr => {
            state.tail = false;
            if cdr.len() != 2 {
                return Err(VMError::new_compile(format!(
                    "takes two arguments, got {}, line {}",
                    cdr.len(),
                    line_num(line)
                )));
            }
            compile(vm, state, cdr[0], result, line)?;
            compile(vm, state, cdr[1], result + 1, line)?;
            state
                .chunk
                .encode2(XDR, result as u16, (result + 1) as u16, own_line(line))?;
        }
        _ => return Ok(false),
    }
    Ok(true)
}

fn compile_vec(
    vm: &mut Vm,
    state: &mut CompileState,
    car: Value,
    cdr: &[Value],
    result: usize,
    line: &mut Option<&mut u32>,
) -> VMResult<bool> {
    match car {
        Value::Symbol(i) if i == state.specials.vec => {
            state.tail = false;
            let mut max = 0;
            for r in cdr {
                compile(vm, state, *r, result + max + 1, line)?;
                max += 1;
            }
            state.chunk.encode3(
                VEC,
                result as u16,
                (result + 1) as u16,
                (result + max + 1) as u16,
                own_line(line),
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
                    own_line(line),
                )?;
            } else if cdr.len() == 1 {
                compile(vm, state, cdr[0], result + 1, line)?;
                state
                    .chunk
                    .encode2(VECMK, result as u16, (result + 1) as u16, own_line(line))?;
            } else if cdr.len() == 2 {
                compile(vm, state, cdr[0], result + 1, line)?;
                compile(vm, state, cdr[1], result + 2, line)?;
                state.chunk.encode3(
                    VECMKD,
                    result as u16,
                    (result + 1) as u16,
                    (result + 2) as u16,
                    own_line(line),
                )?;
            } else {
                return Err(VMError::new_compile(format!(
                    "takes up to two arguments, got {}, line {}",
                    cdr.len(),
                    line_num(line)
                )));
            }
        }
        Value::Symbol(i) if i == state.specials.vec_push => {
            state.tail = false;
            if cdr.len() != 2 {
                return Err(VMError::new_compile(format!(
                    "takes two arguments, got {}, line {}",
                    cdr.len(),
                    line_num(line)
                )));
            }
            compile(vm, state, cdr[0], result, line)?;
            compile(vm, state, cdr[1], result + 1, line)?;
            state
                .chunk
                .encode2(VECPSH, result as u16, (result + 1) as u16, own_line(line))?;
        }
        Value::Symbol(i) if i == state.specials.vec_pop => {
            state.tail = false;
            if cdr.len() != 1 {
                return Err(VMError::new_compile(format!(
                    "takes one argument, got {}, line {}",
                    cdr.len(),
                    line_num(line)
                )));
            }
            compile(vm, state, cdr[0], result + 1, line)?;
            state
                .chunk
                .encode2(VECPOP, (result + 1) as u16, result as u16, own_line(line))?;
        }
        Value::Symbol(i) if i == state.specials.vec_nth => {
            state.tail = false;
            if cdr.len() != 2 {
                return Err(VMError::new_compile(format!(
                    "takes two arguments, got {}, line {}",
                    cdr.len(),
                    line_num(line)
                )));
            }
            compile(vm, state, cdr[0], result + 1, line)?;
            compile(vm, state, cdr[1], result + 2, line)?;
            state.chunk.encode3(
                VECNTH,
                (result + 1) as u16,
                result as u16,
                (result + 2) as u16,
                own_line(line),
            )?;
        }
        Value::Symbol(i) if i == state.specials.vec_set => {
            state.tail = false;
            if cdr.len() != 3 {
                return Err(VMError::new_compile(format!(
                    "takes three arguments, got {}, line {}",
                    cdr.len(),
                    line_num(line)
                )));
            }
            compile(vm, state, cdr[0], result, line)?;
            compile(vm, state, cdr[1], result + 1, line)?;
            compile(vm, state, cdr[2], result + 2, line)?;
            state.chunk.encode3(
                VECSTH,
                result as u16,
                (result + 2) as u16,
                (result + 1) as u16,
                own_line(line),
            )?;
        }
        Value::Symbol(i) if i == state.specials.vec_len => {
            state.tail = false;
            if cdr.len() != 1 {
                return Err(VMError::new_compile(format!(
                    "takes one argument, got {}, line {}",
                    cdr.len(),
                    line_num(line)
                )));
            }
            compile(vm, state, cdr[0], result + 1, line)?;
            state
                .chunk
                .encode2(VECLEN, result as u16, (result + 1) as u16, own_line(line))?;
        }
        Value::Symbol(i) if i == state.specials.vec_clr => {
            state.tail = false;
            if cdr.len() != 1 {
                return Err(VMError::new_compile(format!(
                    "takes one argument, got {}, line {}",
                    cdr.len(),
                    line_num(line)
                )));
            }
            compile(vm, state, cdr[0], result, line)?;
            state.chunk.encode1(VECCLR, result as u16, own_line(line))?;
        }
        _ => return Ok(false),
    }
    Ok(true)
}

fn compile_if(
    vm: &mut Vm,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
    line: &mut Option<&mut u32>,
) -> VMResult<()> {
    if cdr.is_empty() {
        return Err(VMError::new_compile(format!(
            "requires at least one argument, got 0, line {}",
            line_num(line)
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
        compile(vm, state, *r, result, line)?;
        if let Some(r) = next {
            state.tail = tail;
            state.chunk.encode1(JMPF, result as u16, own_line(line))?;
            let encode_offset = state.chunk.code.len();
            state.chunk.encode_jump_offset(0)?;
            let tmp_start_ip = state.chunk.code.len();
            compile(vm, state, *r, result, line)?;
            if cdr_i.peek().is_some() {
                state.chunk.encode0(JMP, own_line(line))?;
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
    vm: &mut Vm,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
    line: &mut Option<&mut u32>,
) -> VMResult<()> {
    let mut cdr_i = cdr.iter();
    if let Some(conditional) = cdr_i.next() {
        let loop_start = state.chunk.code.len();
        compile(vm, state, *conditional, result, line)?;
        state.chunk.encode1(JMPF, result as u16, own_line(line))?;
        let encode_offset = state.chunk.code.len();
        state.chunk.encode_jump_offset(0)?;
        let jmp_ip = state.chunk.code.len();
        while let Some(r) = cdr_i.next() {
            compile(vm, state, *r, result, line)?;
        }
        state.chunk.encode0(JMP, own_line(line))?;
        state.chunk.encode_jump_offset(-(((state.chunk.code.len() + 3) - loop_start) as i32))?;
        state.chunk.reencode_jump_offset(encode_offset, (state.chunk.code.len() - jmp_ip) as i32)?;
        Ok(())
    } else {
        Err(VMError::new_compile(format!(
            "requires at least one argument, got 0, line {}",
            line_num(line)
        )))
    }
}

fn compile_and(
    vm: &mut Vm,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
    line: &mut Option<&mut u32>,
) -> VMResult<()> {
    if cdr.is_empty() {
        return Err(VMError::new_compile(format!(
            "requires at least one argument, got 0, line {}",
            line_num(line)
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
        compile(vm, state, *r, result, line)?;
        if cdr_i.peek().is_some() {
            state.chunk.encode1(JMPF, result as u16, own_line(line))?;
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
    vm: &mut Vm,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
    line: &mut Option<&mut u32>,
) -> VMResult<()> {
    if cdr.is_empty() {
        return Err(VMError::new_compile(format!(
            "requires at least one argument, got 0, line {}",
            line_num(line)
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
        compile(vm, state, *r, result, line)?;
        if cdr_i.peek().is_some() {
            state.chunk.encode1(JMPT, result as u16, own_line(line))?;
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
    vm: &mut Vm,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
    line: &mut Option<&mut u32>,
) -> VMResult<()> {
    if cdr.len() == 2 {
        if let Value::Symbol(si) = cdr[0] {
            compile(vm, state, cdr[1], result + 1, line)?;
            let si_const = vm.reserve_index(si);
            state
                .chunk
                .encode_refi(result as u16, si_const, own_line(line))?;
            state
                .chunk
                .encode2(DEF, result as u16, (result + 1) as u16, own_line(line))?;
        } else {
            return Err(VMError::new_compile("def: expected symbol"));
        }
    } else if cdr.len() == 3 {
        // XXX implement docstrings
        if let Value::Symbol(si) = cdr[0] {
            let si_const = vm.reserve_index(si);
            // Set docstring
            let set_prop = vm.intern("set-prop");
            if let Some(set_prop) = vm.global_intern_slot(set_prop) {
                let doc_const = state
                    .chunk
                    .add_constant(Value::Keyword(vm.intern("doc-string")));
                state
                    .chunk
                    .encode_refi((result + 1) as u16, si_const, own_line(line))?;
                state.chunk.encode2(
                    CONST,
                    (result + 2) as u16,
                    doc_const as u16,
                    own_line(line),
                )?;
                compile(vm, state, cdr[1], result + 3, line)?;
                state
                    .chunk
                    .encode_callg(set_prop as u32, 3, result as u16, own_line(line))?;
            }

            compile(vm, state, cdr[2], result + 1, line)?;
            state
                .chunk
                .encode_refi(result as u16, si_const, own_line(line))?;
            state
                .chunk
                .encode2(DEF, result as u16, (result + 1) as u16, own_line(line))?;
        } else {
            return Err(VMError::new_compile("def: expected symbol"));
        }
    } else {
        return Err(VMError::new_compile("def: malformed"));
    }
    Ok(())
}

fn compile_set(
    vm: &mut Vm,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
    line: &mut Option<&mut u32>,
) -> VMResult<()> {
    if cdr.len() == 2 {
        if let Value::Symbol(si) = cdr[0] {
            if let Some(idx) = state.get_symbol(si) {
                compile(vm, state, cdr[1], result, line)?;
                state
                    .chunk
                    .encode2(SET, (idx + 1) as u16, result as u16, own_line(line))?;
            } else {
                compile(vm, state, cdr[1], result + 1, line)?;
                let si_const = vm.reserve_index(si);
                state
                    .chunk
                    .encode_refi(result as u16, si_const, own_line(line))?;
                state
                    .chunk
                    .encode2(DEF, result as u16, (result + 1) as u16, own_line(line))?;
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
    vm: &mut Vm,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
    line: &mut Option<&mut u32>,
    star: bool,
) -> VMResult<()> {
    fn inner(
        vm: &mut Vm,
        state: &mut CompileState,
        cdr: &[Value],
        result: usize,
        line: &mut Option<&mut u32>,
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
        let mut used_regs = 0;
        let scratch = vm.intern("[SCRATCH]");
        let args_iter = get_args_iter(vm, state, *args, "let", line)?;
        // XXX fixme
        //new_state.chunk.dbg_args = Some(Vec::new());
        for a in args_iter {
            used_regs += 1;
            let mut args_iter = get_args_iter(vm, state, a, "let", line)?;
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
        for (reg, val) in opt_comps {
            compile(vm, state, val, reg, line)?;
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
            compile(vm, state, *r, result + used_regs, line)?;
        }
        if used_regs > 0 {
            state.chunk.encode2(
                MOV,
                result as u16,
                (result + used_regs) as u16,
                own_line(line),
            )?;
        }
        for _ in start_defers..state.defers {
            state.chunk.encode0(DFRPOP, own_line(line))?;
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
    let result = inner(vm, state, cdr, result, line, star, old_tail);
    state.tail = old_tail;
    state.symbols = old_symbols;
    state.defers = old_defers;
    result
}

fn is_macro(vm: &Vm, val: Value) -> bool {
    match val {
        Value::Lambda(h) => matches!(vm.get_heap_property(h, ":macro"), Some(Value::True)),
        Value::Closure(h) => matches!(vm.get_heap_property(h, ":macro"), Some(Value::True)),
        _ => false,
    }
}

fn compile_list(
    vm: &mut Vm,
    state: &mut CompileState,
    car: Value,
    cdr: &[Value],
    result: usize,
    line: &mut Option<&mut u32>,
) -> VMResult<()> {
    if !(compile_math(vm, state, car, cdr, result, line)?
        || compile_cons(vm, state, car, cdr, result, line)?
        || compile_vec(vm, state, car, cdr, result, line)?)
    {
        match car {
            Value::Symbol(i) if i == state.specials.fn_ => {
                if cdr.len() > 1 {
                    compile_fn(vm, state, cdr[0], &cdr[1..], result, line, false)?
                } else {
                    return Err(VMError::new_compile("Malformed fn form."));
                }
            }
            Value::Symbol(i) if i == state.specials.mac_ => {
                if cdr.len() > 1 {
                    compile_fn(vm, state, cdr[0], &cdr[1..], result, line, true)?
                } else {
                    return Err(VMError::new_compile("Malformed macro form."));
                }
            }
            Value::Symbol(i) if i == state.specials.if_ => {
                compile_if(vm, state, cdr, result, line)?;
            }
            Value::Symbol(i) if i == state.specials.while_ => {
                let tail = state.tail;
                state.tail = false;
                compile_while(vm, state, cdr, result, line)?;
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
                    compile(vm, state, *r, result, line)?;
                }
            }
            Value::Symbol(i) if i == state.specials.def => {
                state.tail = false;
                compile_def(vm, state, cdr, result, line)?;
            }
            Value::Symbol(i) if i == state.specials.set => {
                state.tail = false;
                compile_set(vm, state, cdr, result, line)?;
            }
            Value::Symbol(i) if i == state.specials.quote => {
                state.tail = false;
                if cdr.len() != 1 {
                    return Err(VMError::new_compile(format!(
                        "quote takes one argument, got {}, line {}",
                        cdr.len(),
                        line_num(line)
                    )));
                }
                mkconst(vm, state, cdr[0], result, line)?;
            }
            Value::Symbol(i) if i == state.specials.backquote => {
                state.tail = false;
                if cdr.len() != 1 {
                    return Err(VMError::new_compile(format!(
                        "backquote takes one argument, got {}, line {}",
                        cdr.len(),
                        line_num(line)
                    )));
                }
                backquote(vm, state, cdr[0], result, line)?;
            }
            Value::Symbol(i) if i == state.specials.recur => {
                compile_call_myself(vm, state, cdr, result, line, true)?
            }
            Value::Symbol(i) if i == state.specials.this_fn => {
                compile_call_myself(vm, state, cdr, result, line, false)?
            }
            Value::Symbol(i) if i == state.specials.eq => {
                if cdr.len() <= 1 {
                    return Err(VMError::new_compile("Requires at least two arguments."));
                } else {
                    let mut max = 0;
                    for (i, v) in cdr.iter().enumerate() {
                        compile(vm, state, *v, result + i + 1, line)?;
                        max = result + i + 1;
                    }
                    state.chunk.encode3(
                        EQ,
                        result as u16,
                        (result + 1) as u16,
                        max as u16,
                        own_line(line),
                    )?;
                }
            }
            Value::Symbol(i) if i == state.specials.equal => {
                if cdr.len() <= 1 {
                    return Err(VMError::new_compile("Requires at least two arguments. 2"));
                } else {
                    let mut max = 0;
                    for (i, v) in cdr.iter().enumerate() {
                        compile(vm, state, *v, result + i + 1, line)?;
                        max = result + i + 1;
                    }
                    state.chunk.encode3(
                        EQUAL,
                        result as u16,
                        (result + 1) as u16,
                        max as u16,
                        own_line(line),
                    )?;
                }
            }
            Value::Symbol(i) if i == state.specials.type_ => {
                if cdr.len() != 1 {
                    return Err(VMError::new_compile("Requires one argument."));
                } else {
                    compile(vm, state, cdr[0], result + 1, line)?;
                    state.chunk.encode2(
                        TYPE,
                        result as u16,
                        (result + 1) as u16,
                        own_line(line),
                    )?;
                }
            }
            Value::Symbol(i) if i == state.specials.not => {
                if cdr.len() != 1 {
                    return Err(VMError::new_compile("Requires one argument."));
                } else {
                    compile(vm, state, cdr[0], result + 1, line)?;
                    state
                        .chunk
                        .encode2(NOT, result as u16, (result + 1) as u16, own_line(line))?;
                }
            }
            Value::Symbol(i) if i == state.specials.err => {
                let len = cdr.len();
                if len != 1 && len != 2 {
                    return Err(VMError::new_compile("Requires one or two arguments."));
                } else {
                    if len == 2 {
                        compile(vm, state, cdr[0], result, line)?;
                        compile(vm, state, cdr[1], result + 1, line)?;
                    } else {
                        let error = vm.intern("error");
                        compile(vm, state, Value::Keyword(error), result, line)?;
                        compile(vm, state, cdr[0], result + 1, line)?;
                    }
                    state
                        .chunk
                        .encode2(ERR, result as u16, (result + 1) as u16, own_line(line))?;
                }
            }
            Value::Symbol(i) if i == state.specials.and => {
                compile_and(vm, state, cdr, result, line)?;
            }
            Value::Symbol(i) if i == state.specials.or => {
                compile_or(vm, state, cdr, result, line)?;
            }
            Value::Symbol(i) if i == state.specials.str_ => {
                let mut max = 0;
                for (i, v) in cdr.iter().enumerate() {
                    compile(vm, state, *v, result + i + 1, line)?;
                    max = result + i + 1;
                }
                state.chunk.encode3(
                    STR,
                    result as u16,
                    (result + 1) as u16,
                    max as u16,
                    own_line(line),
                )?;
            }
            Value::Symbol(i) if i == state.specials.let_ => {
                compile_let(vm, state, cdr, result, line, false)?;
            }
            Value::Symbol(i) if i == state.specials.letstar => {
                compile_let(vm, state, cdr, result, line, true)?;
            }
            Value::Symbol(i) if i == state.specials.call_cc => {
                if cdr.len() != 1 {
                    return Err(VMError::new_compile("Requires one argument."));
                }
                compile(vm, state, cdr[0], result, line)?;
                state
                    .chunk
                    .encode2(CCC, result as u16, result as u16, own_line(line))?;
            }
            Value::Symbol(i) if i == state.specials.defer => {
                if !cdr.is_empty() {
                    compile_fn(vm, state, Value::Nil, &cdr[0..], result, line, false)?;
                    state.chunk.encode1(DFR, result as u16, own_line(line))?;
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
                compile(vm, state, cdr[0], result, line)?;
                state.chunk.encode1(ONERR, result as u16, own_line(line))?;
            }
            Value::Symbol(i) => {
                if let Some(idx) = state.get_symbol(i) {
                    compile_call_reg(vm, state, (idx + 1) as u16, cdr, result, line)?
                } else {
                    let slot = vm.reserve_index(i);
                    // Is a global so set up a call and will error at runtime if
                    // not callable (dynamic is fun).
                    let global = vm.get_global(slot);
                    if let Value::Undefined = global {
                        eprintln!("Warning: {} not defined.", vm.get_interned(i));
                    }
                    if is_macro(vm, global) {
                        match global {
                            Value::Lambda(h) => {
                                let mac = vm.get_lambda(h);
                                vm.pause_gc();
                                let exp = vm.do_call(mac, cdr, None)?;
                                vm.unpause_gc();
                                pass1(vm, state, exp)?;
                                compile(vm, state, exp, result, line)?
                            }
                            Value::Closure(h) => {
                                let (mac, caps) = vm.get_closure(h);
                                let caps = caps.to_vec();
                                vm.pause_gc();
                                let exp = vm.do_call(mac, cdr, Some(&caps))?;
                                vm.unpause_gc();
                                pass1(vm, state, exp)?;
                                compile(vm, state, exp, result, line)?
                            }
                            _ => panic!("Invalid macro!"),
                        }
                    } else {
                        compile_callg(vm, state, slot as u32, cdr, result, line)?
                    }
                }
            }
            Value::Builtin(builtin) => {
                compile_call(vm, state, Value::Builtin(builtin), cdr, result, line)?
            }
            Value::Lambda(h) => compile_call(vm, state, Value::Lambda(h), cdr, result, line)?,
            Value::Pair(h) => {
                let (ncar, ncdr) = vm.get_pair(h);
                set_line(vm, state, h, line);
                let ncdr: Vec<Value> = ncdr.iter(vm).collect();
                compile_list(vm, state, ncar, &ncdr[..], result, line)?;
                compile_call_reg(vm, state, result as u16, cdr, result, line)?
            }
            Value::Vector(h) => {
                let v = vm.get_vector(h);
                if let Some(ncar) = v.get(0) {
                    let ncar = *ncar;
                    if v.len() > 1 {
                        let ncdr = make_vec_cdr(&v[1..]);
                        compile_list(vm, state, ncar, ncdr, result, line)?;
                    } else {
                        compile_list(vm, state, ncar, &[], result, line)?;
                    }
                    compile_call_reg(vm, state, result as u16, cdr, result, line)?
                }
            }
            _ => {
                println!("Boo, {}", car.display_value(vm));
            }
        }
    }
    Ok(())
}

pub fn pass1(vm: &mut Vm, state: &mut CompileState, exp: Value) -> VMResult<()> {
    let fn_ = vm.intern("fn");
    let mac_ = vm.intern("macro");
    match exp {
        Value::Pair(handle) => {
            let (car, _) = vm.get_pair(handle);
            // short circuit on an fn form, will be handled with it's own state.
            if let Value::Symbol(i) = car {
                if i == fn_ || i == mac_ {
                    return Ok(());
                }
            }
            // XXX boo on this collect.
            for r in exp.iter(vm).collect::<Vec<Value>>() {
                pass1(vm, state, r)?;
            }
        }
        Value::Vector(handle) => {
            // This is ugly, break the lifetime of v from vm safely.
            // Maybe just use unsafe or not compile vectors...
            let v: Vec<Value> = vm.get_vector(handle).to_vec();
            for r in v {
                pass1(vm, state, r)?;
            }
        }
        Value::Symbol(i) => {
            if state.get_symbol(i).is_none() && state.symbols.borrow().can_capture(i) {
                state.symbols.borrow_mut().insert_capture(vm, i);
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

// Need to break the cdr lifetime away from the vm for a call or we have
// to reallocate stuff for no reason.
// Should be safe because compiling code should not be manipulating values on
// the heap (where the underlying vector lives).
// XXX double check this invariant....
fn make_vec_cdr(cdr: &[Value]) -> &'static [Value] {
    unsafe { &*(cdr as *const [Value]) }
}

pub fn mkconst(
    _vm: &mut Vm,
    state: &mut CompileState,
    exp: Value,
    result: usize,
    line: &mut Option<&mut u32>,
) -> VMResult<()> {
    match exp {
        Value::True => state.chunk.encode1(REGT, result as u16, own_line(line))?,
        Value::False => state.chunk.encode1(REGF, result as u16, own_line(line))?,
        Value::Nil => state.chunk.encode1(REGN, result as u16, own_line(line))?,
        Value::Undefined => state.chunk.encode1(REGC, result as u16, own_line(line))?,
        Value::Byte(i) => state
            .chunk
            .encode2(REGB, result as u16, i as u16, own_line(line))?,
        Value::Int(i) if i >= 0 && i <= u16::MAX as i64 => {
            state
                .chunk
                .encode2(REGI, result as u16, i as u16, own_line(line))?;
        }
        Value::UInt(i) if i <= u16::MAX as u64 => {
            state
                .chunk
                .encode2(REGU, result as u16, i as u16, own_line(line))?;
        }
        _ => {
            let const_i = state.add_constant(exp);
            state
                .chunk
                .encode2(CONST, result as u16, const_i as u16, own_line(line))?;
        }
    }
    Ok(())
}

pub fn compile(
    vm: &mut Vm,
    state: &mut CompileState,
    exp: Value,
    result: usize,
    line: &mut Option<&mut u32>,
) -> VMResult<()> {
    if state.max_regs < result {
        state.max_regs = result;
    }
    match exp {
        Value::Pair(handle) => {
            let (car, cdr) = vm.get_pair(handle);
            set_line(vm, state, handle, line);
            let cdr: Vec<Value> = cdr.iter(vm).collect();
            compile_list(vm, state, car, &cdr[..], result, line)?;
        }
        Value::Vector(handle) => {
            let v = vm.get_vector(handle);
            if let Some(car) = v.get(0) {
                let car = *car;
                if v.len() > 1 {
                    let cdr = make_vec_cdr(&v[1..]);
                    compile_list(vm, state, car, cdr, result, line)?;
                } else {
                    compile_list(vm, state, car, &[], result, line)?;
                }
            }
        }
        Value::Symbol(i) => {
            if let Some(idx) = state.get_symbol(i) {
                if result != idx + 1 {
                    state
                        .chunk
                        .encode2(MOV, result as u16, (idx + 1) as u16, own_line(line))?;
                }
            } else {
                let const_i = vm.reserve_index(i);
                state
                    .chunk
                    .encode_refi(result as u16, const_i, own_line(line))?;
            }
        }
        Value::True => state.chunk.encode1(REGT, result as u16, own_line(line))?,
        Value::False => state.chunk.encode1(REGF, result as u16, own_line(line))?,
        Value::Nil => state.chunk.encode1(REGN, result as u16, own_line(line))?,
        Value::Undefined => state.chunk.encode1(REGC, result as u16, own_line(line))?,
        Value::Byte(i) => state
            .chunk
            .encode2(REGB, result as u16, i as u16, own_line(line))?,
        Value::Int(i) if i >= 0 && i <= u16::MAX as i64 => {
            state
                .chunk
                .encode2(REGI, result as u16, i as u16, own_line(line))?
        }
        Value::UInt(i) if i <= u16::MAX as u64 => {
            state
                .chunk
                .encode2(REGU, result as u16, i as u16, own_line(line))?
        }
        _ => {
            // XXX this used to ignore References but now does not, is that good?
            let const_i = state.add_constant(exp);
            state
                .chunk
                .encode2(CONST, result as u16, const_i as u16, own_line(line))?;
        }
    }
    Ok(())
}

fn own_line(line: &Option<&mut u32>) -> Option<u32> {
    line.as_ref().map(|l| **l)
}

fn line_num(line: &mut Option<&mut u32>) -> u32 {
    match line {
        Some(l) => **l,
        None => 0,
    }
}
