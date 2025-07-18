use crate::SloshVm;
use compile_state::state::CompileState;
use sl_compiler::compile;
use sl_compiler::pass1::pass1;
use slvm::{CallFrame, VMError, VMResult, Value};
use std::collections::VecDeque;

/// Common function to disassemble a value (lambda, closure, or expression)
pub fn disassemble_value(vm: &mut SloshVm, exp: Value) -> VMResult<()> {
    match exp {
        Value::Lambda(handle) => {
            let l = vm.get_lambda(handle);
            l.disassemble_chunk(vm, 0)?;
            Ok(())
        }
        Value::Closure(handle) => {
            let (l, _) = vm.get_closure(handle);
            l.disassemble_chunk(vm, 0)?;
            Ok(())
        }
        Value::List(_handle, _start_idx) => {
            let mut state = CompileState::new_state("", 1, None);
            pass1(vm, &mut state, exp)?;
            compile(vm, &mut state, exp, 0)?;
            state.chunk.disassemble_chunk(vm, 0)?;
            Ok(())
        }
        Value::Pair(_handle) => {
            let mut state = CompileState::new_state("", 1, None);
            pass1(vm, &mut state, exp)?;
            compile(vm, &mut state, exp, 0)?;
            state.chunk.disassemble_chunk(vm, 0)?;
            Ok(())
        }
        _ => Err(VMError::new_vm("DASM: Not a callable.")),
    }
}

/// Common function to dump registers for a call frame
pub fn dump_regs(vm: &SloshVm, frame: &CallFrame) {
    let start = frame.stack_top;
    let end = frame.stack_top + frame.chunk.input_regs + frame.chunk.extra_regs + 1;
    let regs = vm.get_registers(start, end);
    let mut reg_names = frame.chunk.dbg_args.as_ref().map(|iargs| iargs.iter());
    for (i, r) in regs.iter().enumerate() {
        let aname = if i == 0 {
            "params/result"
        } else if let Some(reg_names) = reg_names.as_mut() {
            if let Some(n) = reg_names.next() {
                vm.get_interned(*n)
            } else {
                "[SCRATCH]"
            }
        } else {
            "[SCRATCH]"
        };
        if let Value::Value(_) = r {
            println!(
                "{:#03} ^{:#20}: {:#12} {}",
                i,
                aname,
                r.display_type(vm),
                r.pretty_value(vm)
            );
        } else {
            println!(
                "{:#03}  {:#20}: {:#12} {}",
                i,
                aname,
                r.display_type(vm),
                r.pretty_value(vm)
            );
        }
    }
}

/// Common function to dump the entire stack
pub fn dump_stack(vm: &SloshVm) {
    let mut reg_names = None;
    let mut chunks = VecDeque::new();
    for i in 1..=vm.stack_max() {
        let r = vm.get_stack(i);
        if let Value::CallFrame(handle) = r {
            let frame = vm.get_callframe(handle);
            chunks.push_back(frame.chunk.clone());
        }
    }
    if let Some(err_frame) = vm.err_frame() {
        chunks.push_back(err_frame.chunk.clone());
    }

    let mut chunk_own;
    for i in 0..=vm.stack_max() {
        let r = vm.get_stack(i);
        let reg_name = if let Value::CallFrame(_) = r {
            reg_names = if let Some(chunk) = chunks.pop_front() {
                chunk_own = chunk;
                chunk_own.dbg_args.as_ref().map(|iargs| iargs.iter())
            } else {
                None
            };
            "RESULT"
        } else if let Some(reg_names) = reg_names.as_mut() {
            if let Some(n) = reg_names.next() {
                vm.get_interned(*n)
            } else {
                "[SCRATCH]"
            }
        } else {
            "[SCRATCH]"
        };
        if let Value::Value(handle) = r {
            let han_str = format!("{}({})", reg_name, handle.idx());
            println!(
                "{:#03} ^{:#20}: {:#12} {}",
                i,
                han_str,
                r.display_type(vm),
                r.pretty_value(vm)
            );
        } else {
            println!(
                "{:#03}  {:#20}: {:#12} {}",
                i,
                reg_name,
                r.display_type(vm),
                r.pretty_value(vm)
            );
        }
    }
}

/// Dump the call stack
pub fn dump_call_stack(vm: &SloshVm) {
    if let Some(frame) = vm.err_frame() {
        let line = frame.current_line().unwrap_or(0);
        println!(
            "ERROR Frame: {} line: {} ip: {:#010x}",
            frame.chunk.file_name,
            line,
            frame.current_offset()
        );
    }
    for frame in vm.get_call_stack() {
        let line = frame.current_line().unwrap_or(0);
        println!(
            "ID: {} {} line: {} ip: {:#010x}",
            frame.id,
            frame.chunk.file_name,
            line,
            frame.current_offset()
        );
    }
}
