extern crate sl_liner;

use std::collections::VecDeque;
use std::io::ErrorKind;
use std::iter::*;

use slvm::heap::*;
use slvm::value::*;

use sl_compiler::reader::*;

use compile_state::state::{SloshVm, SloshVmTrait};
use sl_liner::{Context, Prompt};

fn dump_regs(vm: &SloshVm, frame: &CallFrame) {
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

fn dump_stack(vm: &SloshVm) {
    //println!("Stack from 0 to {}", vm.stack_max() - 1);
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
        //for (i, r) in vm.stack().iter().enumerate() {
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

pub fn debug(env: &mut SloshVm) {
    let abort = env.intern("abort");
    let globals = env.intern("globals");
    let dasm = env.intern("dasm");
    let regs = env.intern("regs");
    let regs_raw = env.intern("regs-raw");
    let stack = env.intern("stack");
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
        //let mut reader_state = ReaderState::new();
        // This should be fine, we are abusing the reader to parse debug input so should be no chance
        // any string pointers are saved.  Could intern these as well for a legit 'static but should
        // not need that (although a lot of debug commands will be repetitive so may not be a big deal.
        // TODO- once there is a "secure" mode for the reader use that here.
        //let text: &str = &res;
        //let text = unsafe { &*(text as *const str) };
        //let exps = read_all(vm, &mut reader_state, text);
        let mut exps = Reader::from_string(res, env, "", 1, 0);
        //match exps {
        //    Ok(exps) => {
        //let mut exps = exps.iter();
        match exps.next() {
            Some(Ok(Value::Keyword(k))) if k == abort => return,
            Some(Ok(Value::Keyword(k))) if k == globals => env.dump_globals(),
            Some(Ok(Value::Keyword(k))) if k == dasm => {
                if let Some(Ok(parm)) = exps.next() {
                    if let Ok(stk_idx) = parm.get_int(env) {
                        let stk_idx = stk_idx.unsigned_abs() as usize;
                        for (i, frame) in env.get_call_stack().enumerate() {
                            if i + 1 == stk_idx {
                                if let Err(e) = frame.chunk.disassemble_chunk(env, 0) {
                                    println!("Error in disassembly: {}", e);
                                }
                                break;
                            }
                        }
                    } else {
                        println!("Param not an int.");
                    }
                } else if let Some(err_frame) = env.err_frame() {
                    if let Err(e) = err_frame.chunk.disassemble_chunk(env, 0) {
                        println!("Error in disassembly: {}", e);
                    }
                } else {
                    println!("Nothing to disassemble.");
                }
            }
            Some(Ok(Value::Keyword(k))) if k == regs => {
                if let Some(Ok(parm)) = exps.next() {
                    if let Ok(stk_idx) = parm.get_int(env) {
                        let stk_idx = stk_idx.unsigned_abs() as usize;
                        for (i, frame) in env.get_call_stack().enumerate() {
                            if i + 1 == stk_idx {
                                dump_regs(env, frame);
                                break;
                            }
                        }
                    } else {
                        println!("Param not an int.");
                    }
                } else if let Some(err_frame) = env.err_frame() {
                    dump_regs(env, err_frame);
                } else {
                    println!("At top level.");
                }
            }
            Some(Ok(Value::Keyword(k))) if k == regs_raw => {
                dump_stack(env);
            }
            Some(Ok(Value::Keyword(k))) if k == stack => {
                if let Some(frame) = env.err_frame() {
                    let ip = frame.current_ip;
                    let line = frame.chunk.offset_to_line(ip).unwrap_or(0);
                    println!(
                        "ERROR Frame: {} line: {} ip: {:#010x}",
                        frame.chunk.file_name, line, ip
                    );
                }
                for frame in env.get_call_stack() {
                    let ip = frame.current_ip;
                    let line = frame.chunk.offset_to_line(ip).unwrap_or(0);
                    println!(
                        "ID: {} {} line: {} ip: {:#010x}",
                        frame.id, frame.chunk.file_name, line, ip
                    );
                }
            }
            Some(Err(err)) => println!("Reader error: {}", err),
            _ => {}
        }
        //}
        //  Err(err) => println!("Reader error: {}", err),
        //}
    }
}
