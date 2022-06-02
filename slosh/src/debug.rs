extern crate sl_liner;

use std::io::ErrorKind;
use std::iter::*;

use slvm::heap::*;
use slvm::value::*;
use slvm::vm::*;

use sl_compiler::reader::*;

use sl_liner::{Context, Prompt};

fn dump_regs(vm: &Vm, frame: &CallFrame) {
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

pub fn debug(vm: &mut Vm) {
    let abort = vm.intern("abort");
    let globals = vm.intern("globals");
    let dasm = vm.intern("dasm");
    let regs = vm.intern("regs");
    let stack = vm.intern("stack");
    let mut con = Context::new();

    if let Err(e) = con.history.set_file_name_and_load_history("history_debug") {
        println!("Error loading history: {}", e);
    }
    loop {
        let res = match con.read_line(Prompt::from("DEBUG> "), None) {
            Ok(input) => input,
            Err(err) => match err.kind() {
                ErrorKind::UnexpectedEof => {
                    println!("Enter :abort to exit debug mode and abort the error.");
                    continue;
                }
                ErrorKind::Interrupted => {
                    continue;
                }
                _ => {
                    eprintln!("Error on input: {}", err);
                    continue;
                }
            },
        };

        if res.is_empty() {
            continue;
        }

        con.history
            .push(&res)
            .expect("Failed to push debug history.");
        let mut reader_state = ReaderState::new();
        let exps = read_all(vm, &mut reader_state, &res);
        match exps {
            Ok(exps) => {
                let mut exps = exps.iter();
                match exps.next() {
                    Some(Value::Keyword(k)) if *k == abort => return,
                    Some(Value::Keyword(k)) if *k == globals => vm.dump_globals(),
                    Some(Value::Keyword(k)) if *k == dasm => {
                        if let Some(parm) = exps.next() {
                            if let Ok(stk_idx) = parm.get_int() {
                                let stk_idx = stk_idx.abs() as usize;
                                for (i, frame) in vm.get_call_stack().enumerate() {
                                    if i + 1 == stk_idx {
                                        if let Err(e) = frame.chunk.disassemble_chunk(vm, 0) {
                                            println!("Error in disassembly: {}", e);
                                        }
                                        break;
                                    }
                                }
                            } else {
                                println!("Param not an int.");
                            }
                        } else if let Some(err_frame) = vm.err_frame() {
                            if let Err(e) = err_frame.chunk.disassemble_chunk(vm, 0) {
                                println!("Error in disassembly: {}", e);
                            }
                        } else {
                            println!("Nothing to disassemble.");
                        }
                    }
                    Some(Value::Keyword(k)) if *k == regs => {
                        if let Some(parm) = exps.next() {
                            if let Ok(stk_idx) = parm.get_int() {
                                let stk_idx = stk_idx.abs() as usize;
                                for (i, frame) in vm.get_call_stack().enumerate() {
                                    if i + 1 == stk_idx {
                                        dump_regs(vm, frame);
                                        break;
                                    }
                                }
                            } else {
                                println!("Param not an int.");
                            }
                        } else if let Some(err_frame) = vm.err_frame() {
                            dump_regs(vm, err_frame);
                        } else {
                            println!("At top level.");
                        }
                    }
                    Some(Value::Keyword(k)) if *k == stack => {
                        if let Some(frame) = vm.err_frame() {
                            let ip = frame.current_ip;
                            let line = frame.chunk.offset_to_line(ip).unwrap_or(0);
                            println!("{} line: {} ip: {:#010x}", frame.chunk.file_name, line, ip);
                        }
                        for frame in vm.get_call_stack() {
                            let ip = frame.current_ip;
                            let line = frame.chunk.offset_to_line(ip).unwrap_or(0);
                            println!("{} line: {} ip: {:#010x}", frame.chunk.file_name, line, ip);
                        }
                    }
                    _ => {}
                }
            }
            Err(err) => println!("Reader error: {}", err),
        }
    }
}
