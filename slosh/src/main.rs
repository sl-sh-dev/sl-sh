extern crate sl_liner;

use std::cell::RefCell;
use std::ffi::OsString;
use std::fmt::Debug;
use std::fs::{create_dir_all, File};
use std::io::{BufRead, ErrorKind, Write};
use std::ops::DerefMut;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::sync::Arc;
use std::{env, fs};

use slvm::opcodes::*;

use compile_state::state::*;
use sl_compiler::compile::*;
use sl_compiler::reader::*;

use builtins::collections::setup_collection_builtins;
use builtins::conversions::add_conv_builtins;
use builtins::io::add_io_builtins;
use builtins::print::{add_print_builtins, display_value};
use builtins::string::add_str_builtins;
use builtins::{add_global_value, add_misc_builtins};
use sl_liner::vi::AlphanumericAndVariableKeywordRule;
use sl_liner::{keymap, ColorClosure, Context, Prompt};

mod completions;
mod config;
pub mod debug;
#[cfg(any(test, feature = "regex"))]
pub mod docs;
mod liner_rules;
mod load_eval;
mod shell_builtins;

use crate::completions::ShellCompleter;
use crate::liner_rules::make_editor_rules;
use crate::load_eval::{add_load_builtins, load_internal, SLSHRC};
use crate::shell_builtins::add_shell_builtins;
use config::*;
use debug::*;
use shell::platform::{Platform, Sys, STDIN_FILENO};
use sl_compiler::pass1::pass1;
use slvm::{Value, INT_BITS, INT_MAX, INT_MIN};

thread_local! {
    /// Env (job control status, etc) for the shell.
    pub static SHELL_ENV: RefCell<shell::jobs::Jobs> = RefCell::new(shell::jobs::Jobs::new(true));
}

thread_local! {
    /// Env (job control status, etc) for the shell.
    pub static ENV: RefCell<SloshVm> = RefCell::new(new_slosh_vm());
}

const PROMPT_FN: &str = "prompt";

fn get_prompt(env: &mut SloshVm) -> String {
    let i_val = env.intern("__prompt");
    if let Some(idx) = env.global_intern_slot(i_val) {
        match env.get_global(idx) {
            Value::Lambda(h) => {
                let l = env.get_lambda(h);
                match env.do_call(l, &[], None) {
                    Ok(v) => match v {
                        Value::StringConst(i) => env.get_interned(i).to_string(),
                        Value::String(h) => env.get_string(h).to_string(),
                        _ => v.display_value(env),
                    },
                    Err(e) => {
                        eprintln!("Error getting prompt: {e}");
                        "slosh> ".to_string()
                    }
                }
            }
            Value::Closure(h) => {
                let (l, tcaps) = env.get_closure(h);
                let caps = Vec::from(tcaps);
                match env.do_call(l, &[], Some(&caps[..])) {
                    Ok(v) => match v {
                        Value::StringConst(i) => env.get_interned(i).to_string(),
                        Value::String(h) => env.get_string(h).to_string(),
                        _ => v.display_value(env),
                    },
                    Err(e) => {
                        eprintln!("Error getting prompt: {e}");
                        "slosh> ".to_string()
                    }
                }
            }
            _ => env.get_global(idx).display_value(env),
        }
    } else {
        "slosh> ".to_string()
    }
}

/// Given a [`SloshVm`] and a String, usually the rc file for slosh, set
/// the *load-path* global variable to facilitate proper loading of scripts.
///
/// It is assumed users will put slosh scripts at the location(s) dictated by
/// *load-path*.
fn set_initial_load_path(env: &mut SloshVm, load_paths: Vec<&str>) {
    let mut v = vec![];
    for path in load_paths {
        let i_path = env.intern(path);
        v.push(Value::StringConst(i_path));
    }
    let path = env.alloc_vector(v);
    add_global_value(
        env,
        "*load-path*",
        path,
        "Usage: (set '*load-path* '(\"/path/one\" \"/path/two\"))

Set the a list of paths to search for loading scripts with the load form.
Paths are a vector and are searched in index order for the file name of
the path to be loaded.

Section: scripting

Example:
;(set '*load-path '(\"/path\"))
;(load \"script-in-path\")
t
",
    );
}

fn make_path_dir_if_possible(path: impl AsRef<Path> + Debug) -> Option<PathBuf> {
    if let Ok(f_data) = fs::metadata(path.as_ref()) {
        if f_data.is_dir() {
            Some(path.as_ref().into())
        } else {
            None
        }
    } else {
        // This means provided path doesn't exist (PathBuf::exists(...) just verifies
        // if fs::metadata::<&Path>(...).is_ok()). If there is no HOME dir try to create it
        // to guarantee that there is a $HOME and that it is a directory.
        match create_dir_all(path.as_ref()) {
            Ok(_) => Some(path.as_ref().into()),
            Err(e) => {
                eprintln!(
                    "Path [{:?}] did not point to valid directory nor could that directory be created.: {e}",
                    path
                );
                None
            }
        }
    }
}

fn get_home_dir() -> Option<PathBuf> {
    if let Ok(home_dir) = env::var("HOME") {
        make_path_dir_if_possible(home_dir)
    } else {
        None
    }
}

/// Expected that the user's init.slosh will be in the user's home directory
/// at `$HOME/.config/slosh/` otherwise the directory structure will be created.
fn load_sloshrc() {
    if let Some(home_dir) = get_home_dir() {
        let slosh_path = home_dir.join(".config").join("slosh");
        if let Some(slosh_dir) = make_path_dir_if_possible(slosh_path.as_path()) {
            ENV.with(|renv| {
                let mut env = renv.borrow_mut();
                set_initial_load_path(
                    env.deref_mut(),
                    vec![slosh_dir.as_os_str().to_string_lossy().as_ref()],
                );
                let init = slosh_dir.join("init.slosh");
                if fs::metadata::<&Path>(init.as_ref()).is_err() {
                    match File::create::<&Path>(slosh_dir.as_ref()) {
                        Ok(mut f) => match f.write_all(SLSHRC.as_bytes()) {
                            Ok(_) => {}
                            Err(e) => {
                                eprintln!("error writing default config {:?}: {e}", init.as_path())
                            }
                        },
                        Err(e) => {
                            eprintln!("error creating default config {:?}: {e}", init.as_path())
                        }
                    }
                }
                let init = init.as_os_str().to_string_lossy();
                let script = env.intern(init.as_ref());
                let script = env.get_interned(script);
                match load_internal(&mut env, script) {
                    Ok(_) => {}
                    Err(err) => println!("ERROR: {err}"),
                }
            });
        }
    }
}

fn history_file() -> String {
    let mut share_dir = if let Ok(mut home) = env::var("HOME") {
        if home.ends_with('/') {
            home.push_str(".local/share/slosh");
        } else {
            home.push_str("/.local/share/slosh");
        }
        home
    } else {
        "./.local/share/slosh".to_string()
    };
    if let Err(err) = create_dir_all(&share_dir) {
        eprintln!(
            "WARNING: Unable to create share directory: {}- {}",
            share_dir, err
        );
    }
    share_dir.push_str("/history");
    share_dir
}

fn get_color_closure() -> Option<ColorClosure> {
    ENV.with(move |renv| -> Option<ColorClosure> {
        let mut env = renv.borrow_mut();
        let handler_interned = env.intern("__line_handler");
        if let Some(idx) = env.global_intern_slot(handler_interned) {
            match env.get_global(idx) {
                Value::Lambda(h) => Some(Box::new(move |input: &str| -> String {
                    ENV.with(|renv| {
                        let mut env = renv.borrow_mut();
                        let line_handler = env.get_lambda(h);
                        let param = env.alloc_string(input.to_string());
                        env.heap_sticky(param);
                        let res = match env.do_call(line_handler, &[param], None) {
                            Ok(v) => match v {
                                Value::StringConst(i) => env.get_interned(i).to_string(),
                                Value::String(h) => env.get_string(h).to_string(),
                                _ => v.display_value(&env),
                            },
                            Err(e) => {
                                format!("ERROR {e}")
                            }
                        };
                        env.heap_unsticky(param);
                        res
                    })
                })),
                Value::Closure(h) => Some(Box::new(move |input: &str| -> String {
                    ENV.with(|renv| {
                        let mut env = renv.borrow_mut();
                        let (line_handler, tcaps) = env.get_closure(h);
                        let caps = Vec::from(tcaps);
                        let param = env.alloc_string(input.to_string());
                        env.heap_sticky(param);
                        let res = match env.do_call(line_handler, &[param], Some(&caps[..])) {
                            Ok(v) => match v {
                                Value::StringConst(i) => env.get_interned(i).to_string(),
                                Value::String(h) => env.get_string(h).to_string(),
                                _ => v.display_value(&env),
                            },
                            Err(e) => {
                                format!("ERROR {e}")
                            }
                        };
                        env.heap_unsticky(param);
                        res
                    })
                })),
                _ => None,
            }
        } else {
            None
        }
    })
}

fn set_builtins(env: &mut SloshVm) {
    add_shell_builtins(env);
    setup_collection_builtins(env);
    add_print_builtins(env);
    add_load_builtins(env);
    add_str_builtins(env);
    add_misc_builtins(env);
    add_io_builtins(env);
    add_conv_builtins(env);
    env.set_global_builtin("dump-regs", builtin_dump_regs);
    let uid = Sys::current_uid();
    let euid = Sys::effective_uid();
    env::set_var("UID", format!("{uid}"));
    env::set_var("EUID", format!("{euid}"));
    env.set_named_global("*uid*", uid.into());
    env.set_named_global("*euid*", euid.into());
    env.set_named_global("*last-status*", 0.into());
    env.set_named_global("*int-bits*", (INT_BITS as i64).into());
    env.set_named_global("*int-max*", INT_MAX.into());
    env.set_named_global("*int-min*", INT_MIN.into());
    // Initialize the HOST variable
    let host: OsString = Sys::gethostname().unwrap_or_else(|| "Operating system hostname is not a string capable of being parsed by native platform???".into());
    env::set_var("HOST", host);
    if let Ok(dir) = env::current_dir() {
        env::set_var("PWD", dir);
    }
    #[cfg(any(test, feature = "regex"))]
    {
        docs::add_builtins(env);
    }
}

fn main() {
    let exit_code = run_slosh();
    std::process::exit(exit_code);
}

fn run_slosh() -> i32 {
    if let Some(config) = get_config() {
        ENV.with(|renv| {
            let mut env = renv.borrow_mut();
            set_builtins(&mut env);
        });
        if config.command.is_none() && config.script.is_none() {
            load_sloshrc();
            if Sys::is_tty(STDIN_FILENO) {
                run_shell_tty()
            } else {
                run_shell_with_stdin()
            }
        } else if let Some(mut command) = config.command {
            for a in &config.args {
                command.push(' ');
                command.push_str(a);
            }
            if Sys::is_tty(STDIN_FILENO) {
                shell::run::setup_shell_tty(STDIN_FILENO);
            }
            let status = SHELL_ENV.with(|jobs| {
                shell::run::run_one_command(&command, &mut jobs.borrow_mut()).unwrap_or_else(
                    |err| {
                        eprintln!("ERROR executing {command}: {err}");
                        1
                    },
                )
            });
            SHELL_ENV.with(|jobs| {
                jobs.borrow_mut().reap_procs();
            });
            status
        } else if let Some(script) = config.script {
            load_sloshrc();
            let status = ENV.with(|renv| {
                let mut env = renv.borrow_mut();
                let script = env.intern(&script);
                let script = env.get_interned(script);
                match load_internal(&mut env, script) {
                    Ok(_) => 0,
                    Err(err) => {
                        eprintln!("ERROR: {err}");
                        1
                    }
                }
            });
            status
        } else {
            0
        }
    } else {
        0
    }
}

fn run_shell_tty() -> i32 {
    let mut con = Context::new();
    //con.set_completer(Box::new(FilenameCompleter::new(Some("."))));
    con.set_completer(Box::new(ShellCompleter::new()));
    con.set_editor_rules(make_editor_rules());
    let mut vi = keymap::Vi::new();
    let vi_keywords = vec!["_", "-"];
    vi.set_keyword_rule(Box::new(AlphanumericAndVariableKeywordRule::new(
        vi_keywords,
    )));
    /*if let Some((ch1, ch2, timeout)) = repl_settings.vi_esc_sequence {
        vi.set_esc_sequence(ch1, ch2, timeout);
    }
    vi.set_normal_prompt_prefix(repl_settings.vi_normal_prompt_prefix.clone());
    vi.set_normal_prompt_suffix(repl_settings.vi_normal_prompt_suffix.clone());
    vi.set_insert_prompt_prefix(repl_settings.vi_insert_prompt_prefix.clone());
    vi.set_insert_prompt_suffix(repl_settings.vi_insert_prompt_suffix.clone());*/
    //Box::new(keymap::Emacs::new())
    con.set_keymap(Box::new(vi));

    if let Err(e) = con.history.set_file_name_and_load_history(&history_file()) {
        println!("Error loading history: {e}");
    }
    shell::run::setup_shell_tty(STDIN_FILENO);
    SHELL_ENV.with(|jobs| {
        jobs.borrow_mut().cap_term();
    });
    loop {
        SHELL_ENV.with(|jobs| {
            jobs.borrow_mut().reap_procs();
        });
        let prompt = ENV.with(|env| get_prompt(&mut env.borrow_mut()));
        let res = match con.read_line(Prompt::from(prompt), get_color_closure()) {
            Ok(input) => input,
            Err(err) => match err.kind() {
                ErrorKind::UnexpectedEof => {
                    break;
                }
                ErrorKind::Interrupted => {
                    continue;
                }
                _ => {
                    // Usually can just restore the tty and be back in action.
                    SHELL_ENV.with(|jobs| {
                        jobs.borrow_mut().restore_terminal();
                    });
                    eprintln!("Error on input: {err}");
                    continue;
                }
            },
        };

        if res.is_empty() {
            continue;
        }

        let res = if res.contains("\\\n") {
            res.replace("\\\n", "")
        } else {
            res
        };
        con.history.push(&res).expect("Failed to push history.");
        if res.starts_with('(') {
            ENV.with(|env| {
                exec_expression(res, &mut env.borrow_mut());
            });
        } else {
            let status_ = run_command(&res);
        }
    }
    0
}

fn run_command(res: &String) -> i32 {
    let status = SHELL_ENV.with(|jobs| {
        shell::run::run_one_command(res, &mut jobs.borrow_mut()).unwrap_or_else(|err| {
            eprintln!("ERROR executing {res}: {err}");
            1
        })
    });
    ENV.with(|env| {
        env.borrow_mut()
            .set_named_global("*last-status*", status.into());
    });
    status
}

fn run_shell_with_stdin() -> i32 {
    // No tty so just grab lines from stdin and try to use them....
    let mut res = String::new();
    let stdin = std::io::stdin();
    let mut status = 0;
    while let Ok(bytes) = stdin.lock().read_line(&mut res) {
        SHELL_ENV.with(|jobs| {
            jobs.borrow_mut().reap_procs();
        });
        if bytes == 0 {
            break;
        }
        if res.is_empty() {
            continue;
        }
        if res.starts_with('(') {
            ENV.with(|env| {
                exec_expression(res.clone(), &mut env.borrow_mut());
            });
        } else {
            status = run_command(&res);
        }
        res.clear();
    }
    SHELL_ENV.with(|jobs| {
        jobs.borrow_mut().reap_procs();
    });
    status
}

fn read_expression_to_list(res: String, env: &mut SloshVm) -> Result<Vec<Value>, ReadError> {
    let reader = Reader::from_string(res, env, "", 1, 0);
    reader.collect()
}

fn exec_expression(res: String, env: &mut SloshVm) {
    let exps = read_expression_to_list(res, env);
    match exps {
        Ok(exps) => {
            for exp in exps {
                let line_num = env.line_num();
                let mut state = CompileState::new_state(PROMPT_FN, line_num, None);
                if let Err(e) = pass1(env, &mut state, exp) {
                    eprintln!("Compile error (pass1), line {}: {}", env.line_num(), e);
                    return;
                }
                if let Err(e) = compile(env, &mut state, exp, 0) {
                    eprintln!("Compile error, line {}: {}", env.line_num(), e);
                    return;
                }
                if let Err(e) = state.chunk.encode0(RET, env.own_line()) {
                    eprintln!(
                        "Compile error (failed to add return...), line {}: {}",
                        env.line_num(),
                        e
                    );
                    return;
                }
                let chunk = Arc::new(state.chunk.clone());
                match env.execute(chunk.clone()) {
                    Ok(res) => {
                        if !res.is_nil() {
                            println!("{}", display_value(env, res));
                        }
                    }
                    Err(err) => {
                        eprintln!("ERROR: {}", err.display(env));
                        if let Some(err_frame) = env.err_frame() {
                            let line = err_frame.current_line().unwrap_or(0);
                            eprintln!(
                                "{} line: {} ip: {:#010x}",
                                err_frame.chunk.file_name,
                                line,
                                err_frame.current_offset()
                            );
                        }
                        debug(env);
                    }
                }
            }
        }
        Err(err) => println!("Reader error: {err}"),
    }
}

#[cfg(test)]
mod tests {
    extern crate tempdir;

    use super::*;

    use crate::{set_initial_load_path, ENV};
    use std::fs::{create_dir_all, File};
    use std::io::Write;
    use std::ops::DerefMut;
    use temp_env;
    use tempdir::TempDir;

    use crate::tests::utils::exec;
    use compile_state::state::{CompileState, SloshVm, SloshVmTrait};
    use sl_compiler::pass1::pass1;
    use sl_compiler::{compile, ReadError, Reader};
    use slvm::{from_i56, Value, RET};

    // I didn't really know this was possible but for test utilities just use
    // the path attribute!
    #[path = "../../../compiler/src/test_utils/utils.rs"]
    mod utils;

    #[test]
    fn test_load_path_no_home() {
        // create home dir
        let tmp_dir = TempDir::new("test_load_path").unwrap();
        let home_dir = tmp_dir.path().to_str();
        let home_path = home_dir.unwrap().to_string();

        let tmp_0 = tmp_dir.path().join("tmp_0");
        let tmp_1 = tmp_dir.path().join("tmp_1");
        {
            // create a dir with an add fcn that adds 1 in  add.slosh
            create_dir_all(tmp_0.clone()).unwrap();
            let file_0 = tmp_0.as_path().join("add.slosh");
            let mut file_0 = File::create(file_0).unwrap();
            writeln!(file_0, "(def add (fn (x) (+ 1 x)))").unwrap();
            File::flush(&mut file_0).unwrap();

            // create a dir with an add fcn that adds 2 in add.slosh
            create_dir_all(tmp_1.clone()).unwrap();
            let file_1 = tmp_1.as_path().join("add.slosh");
            let mut file_1 = File::create(file_1).unwrap();
            writeln!(file_1, "(def add (fn (x) (+ 2 x)))").unwrap();
            File::flush(&mut file_1).unwrap();
        }

        let v = temp_env::with_var("HOME", home_dir, || {
            ENV.with(|env| {
                let mut vm = env.borrow_mut();
                set_builtins(vm.deref_mut());
                set_initial_load_path(
                    vm.deref_mut(),
                    vec![
                        &home_path,
                        tmp_0.to_str().unwrap().as_ref(),
                        tmp_1.to_str().unwrap().as_ref(),
                    ],
                );
                _ = exec(vm.deref_mut(), "(load \"add.slosh\")");
                let v = exec(vm.deref_mut(), "(add 1)");
                match v {
                    Value::Int(i) => from_i56(&i),
                    _ => {
                        panic!("Value should be an integer");
                    }
                }
            })
        });
        assert_eq!(v, 2i64);
    }
}
