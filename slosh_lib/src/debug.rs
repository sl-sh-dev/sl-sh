extern crate sl_liner;

use std::collections::VecDeque;
use std::env;
use std::io::ErrorKind;
use std::path::PathBuf;
use std::sync::Arc;

use compile_state::state::{SloshVm, SloshVmTrait};
use sl_compiler::Reader;
use sl_liner::{Context, Prompt};
use slvm::{CallFrame, Chunk, VMError, VMResult, Value};
use slvm::vm_hashmap::VMHashMap;

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
        println!("Error loading history: {e}");
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
                    eprintln!("Error on input: {err}");
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
            Some(Ok(Value::Keyword(k))) if k == abort => {
                env.reset();
                return;
            }
            Some(Ok(Value::Keyword(k))) if k == globals => env.dump_globals(),
            Some(Ok(Value::Keyword(k))) if k == dasm => {
                if let Some(Ok(parm)) = exps.next() {
                    if let Ok(stk_idx) = parm.get_int(env) {
                        let stk_idx = stk_idx.unsigned_abs() as usize;
                        for (i, frame) in env.get_call_stack().enumerate() {
                            if i + 1 == stk_idx {
                                if let Err(e) = frame.chunk.disassemble_chunk(env, 0) {
                                    println!("Error in disassembly: {e}");
                                }
                                break;
                            }
                        }
                    } else {
                        println!("Param not an int.");
                    }
                } else if let Some(err_frame) = env.err_frame() {
                    if let Err(e) = err_frame.chunk.disassemble_chunk(env, 0) {
                        println!("Error in disassembly: {e}");
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
                    let line = frame.current_line().unwrap_or(0);
                    println!(
                        "ERROR Frame: {} line: {} ip: {:#010x}",
                        frame.chunk.file_name,
                        line,
                        frame.current_offset()
                    );
                }
                for frame in env.get_call_stack() {
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
            Some(Err(err)) => println!("Reader error: {err}"),
            _ => {}
        }
        //}
        //  Err(err) => println!("Reader error: {}", err),
        //}
    }
}

pub fn builtin_dump_regs(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if !registers.is_empty() {
        return Err(VMError::new_compile("dump-regs: takes no args"));
    }
    if let Some(frame) = vm.call_frame() {
        println!("Previous Call Frames Regs (NOTE: tail calls will be 'missing' Call Frames):");
        dump_regs(vm, frame);
        println!();
    }
    let lambda = if let Some(val) = vm.this_fn() {
        match val {
            Value::Lambda(h) => vm.get_lambda(h),
            Value::Closure(h) => {
                let (l, _) = vm.get_closure(h);
                l
            }
            _ => Arc::new(Chunk::new("", 0)),
        }
    } else {
        Arc::new(Chunk::new("", 0))
    };
    let mut reg_names = lambda.dbg_args.as_ref().map(|iargs| iargs.iter());
    let regs = vm.get_current_registers();
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
    Ok(Value::Nil)
}

pub fn get_temp_file_path(filename: &str) -> PathBuf {
    let temp_dir = env::temp_dir();
    //let temp_dir = PathBuf::from("/tmp/");
    temp_dir.join(filename)
}

/// A macro similar to println! that writes output to a file in /tmp. Used to experiment but should
/// not end up in committed code, clippy lint expect(unused) helps enforce this.
///
/// # Examples
///
/// ```ignore
/// file_println!("debug.log", "Hello, world!"); // Writes to a platform specific tmp dir.
/// file_println!("debug.log", "The value is: {}", 42); // Supports format args.
/// ```
///
/// Note on usage:
/// The expect(unused) fails if macro_export attribute is uncommented because that attribute
/// makes a macro part of the public API of the crate signifying usage, potentially by an external
/// crate.
//#[macro_export] // when needed  for debugging, uncomment.
#[expect(unused)] // do not remove, prevents CI from passing if used.
macro_rules! file_println {
    ($filename:expr, $msg:expr) => {
        {
            use std::io::Write;
            let path = $crate::debug::get_temp_file_path($filename);
            let mut file = match std::fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(&path) {
                    Ok(f) => f,
                    Err(e) => {
                        eprintln!("Error opening file {:?}: {}", path, e);
                        std::fs::File::create(&path).expect("Failed to create file")
                    }
                };

            if let Err(e) = writeln!(file, "{}", $msg) {
                eprintln!("Error writing to file {:?}: {}", path, e);
            }
        }
    };
    ($filename:expr, $fmt:expr, $($arg:tt)*) => {
        {
            use std::io::Write;
            let path = $crate::debug::get_temp_file_path($filename);
            let mut file = match std::fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(&path) {
                    Ok(f) => f,
                    Err(e) => {
                        eprintln!("Error opening file {}: {}", path.to_string_lossy(), e);
                        std::fs::File::create(&path).expect("Failed to create file")
                    }
                };

            if let Err(e) = writeln!(file, $fmt, $($arg)*) {
                eprintln!("Error writing to file {:?}: {}", path.to_string_lossy(), e);
            }
        }
    };
}

#[doc = " Usage: (hash-haskey hashmap key)"]
#[doc = ""]
#[doc = " Checks if a key is in a hashmap."]
#[doc = ""]
#[doc = " Section: hashmap"]
#[doc = ""]
#[doc = " Example:"]
#[doc = " (def tst-hash {:key1  \"val one\" \'key2 \"val two\" \"key3\" \"val three\" \\S \"val S\"})"]
#[doc = " (test::assert-equal 4 (len (hash-keys tst-hash)))"]
#[doc = " (test::assert-true (hash-haskey tst-hash :key1))"]
#[doc = " (test::assert-true (hash-haskey tst-hash \'key2))"]
#[doc = " (test::assert-true (hash-haskey tst-hash \"key3\"))"]
#[doc = " (test::assert-true (hash-haskey tst-hash \\S))"]
#[doc = " (test::assert-false (hash-haskey tst-hash \'key1))"]
#[doc = " (test::assert-false (hash-haskey tst-hash :key2))"]
#[doc = " (test::assert-false (hash-haskey tst-hash \"keynone\"))"]
#[doc = " (hash-remove! tst-hash :key1)"]
#[doc = " (test::assert-false (hash-haskey tst-hash :key1))"]
#[doc = " (set! tst-hash :key1 \"val one b\")"]
#[doc = " (test::assert-true (hash-haskey tst-hash :key1))"]

fn parse_hash_hashkey(environment: &mut compile_state::state::SloshVm, args: &[slvm::Value] ) -> slvm::VMResult<slvm::Value> {
    let fn_name = "hash-haskey?";   const PARAMS_LEN: usize = 2usize;
    let arg_types: [bridge_types::Param; PARAMS_LEN] = [bridge_types::Param { handle: bridge_types::TypeHandle::Direct, passing_style: bridge_types::PassingStyle::Reference }, bridge_types::Param { handle: bridge_types::TypeHandle::Direct, passing_style: bridge_types::PassingStyle::Value }];
    static_assertions::assert_impl_all!(Value : bridge_adapters :: lisp_adapters :: SlInto < slvm :: Value > );
    let param = arg_types[1usize];
    match param.handle {
        bridge_types::TypeHandle::Direct => match args.get(1usize) {
            None => { return Err(slvm::VMError::new_vm(format!("{} not given enough arguments, expected at least {} arguments, got {}.", fn_name, 2usize, args.len()))); }
            Some(arg_1) => {
                let arg_1 = *arg_1;
                {
                    use bridge_adapters::lisp_adapters::SlIntoRef;
                    let arg_1: Value = arg_1.sl_into_ref(environment)?;
                    let param = arg_types[0usize];
                    match param.handle {
                        bridge_types::TypeHandle::Direct => match args.get(0usize) {
                            None => { return Err(slvm::VMError::new_vm(format!("{} not given enough arguments, expected at least {} arguments, got {}.", fn_name, 2usize, args.len()))); }
                            Some(arg_0) => {
                                {
                                    use bridge_adapters::lisp_adapters::SlAsRef;
                                    let arg_0: &slvm::vm_hashmap::VMHashMap = arg_0.sl_as_ref(environment)?;
                                    match args.get(PARAMS_LEN) {
                                        Some(_) if PARAMS_LEN == 0 || arg_types[PARAMS_LEN - 1].handle != bridge_types::TypeHandle::VarArgs => { return Err(slvm::VMError::new_vm(format!("{} given too many arguments, expected at least {} arguments, got {}.", fn_name, 2usize, args.len()))); }
                                        _ => {
                                            use bridge_adapters::lisp_adapters::SlInto;
                                            if arg_0.contains_key(environment, arg_1) {
                                                Ok(Value::True)
                                            } else {
                                                Ok(Value::False)
                                            }
                                        }
                                    }
                                }
                            }
                        },
                        _ => { return Err(slvm::VMError::new_vm(format!("{} failed to parse its arguments, internal error.", fn_name, ))); }
                    }
                }
            }
        },
        _ => { return Err(slvm::VMError::new_vm(format!("{} failed to parse its arguments, internal error.", fn_name, ))); }
    }
}
fn intern_hash_hashkey(env: &mut compile_state::state::SloshVm) {
    let fn_name = "hash-haskey?";
    bridge_adapters::add_builtin(env, fn_name, parse_hash_hashkey, "Usage: (hash-haskey hashmap key)\n\nChecks if a key is in a hashmap.\n\nSection: hashmap\n\nExample:\n(def tst-hash {:key1  \"val one\" 'key2 \"val two\" \"key3\" \"val three\" \\S \"val S\"})\n(test::assert-equal 4 (len (hash-keys tst-hash)))\n(test::assert-true (hash-haskey tst-hash :key1))\n(test::assert-true (hash-haskey tst-hash 'key2))\n(test::assert-true (hash-haskey tst-hash \"key3\"))\n(test::assert-true (hash-haskey tst-hash \\S))\n(test::assert-false (hash-haskey tst-hash 'key1))\n(test::assert-false (hash-haskey tst-hash :key2))\n(test::assert-false (hash-haskey tst-hash \"keynone\"))\n(hash-remove! tst-hash :key1)\n(test::assert-false (hash-haskey tst-hash :key1))\n(set! tst-hash :key1 \"val one b\")\n(test::assert-true (hash-haskey tst-hash :key1))\n");
}
#[allow(dead_code)]
#[inline(always)]
#[doc = " Usage: (hash-haskey hashmap key)"]
#[doc = ""]
#[doc = " Checks if a key is in a hashmap."]
#[doc = ""]
#[doc = " Section: hashmap"]
#[doc = ""]
#[doc = " Example:"]
#[doc = " (def tst-hash {:key1  \"val one\" \'key2 \"val two\" \"key3\" \"val three\" \\S \"val S\"})"]
#[doc = " (test::assert-equal 4 (len (hash-keys tst-hash)))"]
#[doc = " (test::assert-true (hash-haskey tst-hash :key1))"]
#[doc = " (test::assert-true (hash-haskey tst-hash \'key2))"]
#[doc = " (test::assert-true (hash-haskey tst-hash \"key3\"))"]
#[doc = " (test::assert-true (hash-haskey tst-hash \\S))"]
#[doc = " (test::assert-false (hash-haskey tst-hash \'key1))"]
#[doc = " (test::assert-false (hash-haskey tst-hash :key2))"]
#[doc = " (test::assert-false (hash-haskey tst-hash \"keynone\"))"]
#[doc = " (hash-remove! tst-hash :key1)"]
#[doc = " (test::assert-false (hash-haskey tst-hash :key1))"]
#[doc = " (set! tst-hash :key1 \"val one b\")"]
#[doc = " (test::assert-true (hash-haskey tst-hash :key1))"]

pub fn hash_hashkey(environment: &mut SloshVm, map: &slvm::vm_hashmap::VMHashMap, key: Value) -> VMResult<Value> {
    if map.contains_key(environment, key) {
        Ok(Value::True)
    } else {
        Ok(Value::False)
    }
}
