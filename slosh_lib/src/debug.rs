extern crate sl_liner;

use std::env;
use std::io::ErrorKind;
use std::path::PathBuf;

use builtins::vm_inspect::{disassemble_value, dump_call_stack, dump_regs, dump_stack};
use compile_state::state::{SloshVm, SloshVmTrait};
use sl_compiler::Reader;
use sl_liner::{Context, Prompt};
use slvm::Value;

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
                    } else if let Err(e) = disassemble_value(env, parm) {
                        println!("Error in disassembly: {e}");
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
                dump_call_stack(env);
            }
            Some(Err(err)) => println!("Reader error: {err}"),
            _ => {}
        }
        //}
        //  Err(err) => println!("Reader error: {}", err),
        //}
    }
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
