extern crate sl_liner;

use bridge_macros::sl_sh_fn;
use std::cell::RefCell;
use std::ffi::OsString;
use std::fmt::Debug;
use std::fs::{File, create_dir_all};
use std::io::{BufRead, ErrorKind, Write};
use std::ops::DerefMut;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::{env, fs};

pub use sl_compiler::{Reader, compile};

use slvm::opcodes::*;

use compile_state::state::*;
use sl_compiler::reader::*;

use bridge_adapters::add_builtin;
use bridge_types::LooseString;
use builtins::collections::setup_collection_builtins;
use builtins::conversions::add_conv_builtins;
use builtins::fs_meta::add_fs_meta_builtins;
use builtins::fs_temp::add_fs_temp_builtins;
use builtins::io::add_io_builtins;
use builtins::math::add_math_builtins;
use builtins::print::{add_print_builtins, display_value};
use builtins::rand::add_rand_builtins;
use builtins::string::add_str_builtins;
use builtins::{add_global_value, add_misc_builtins};
use sl_liner::vi::AlphanumericAndVariableKeywordRule;
use sl_liner::{ColorClosure, Context, Prompt, keymap};

mod completions;
pub mod debug;
mod liner_rules;

pub use sl_compiler::load_eval::load_one_expression;
pub use sl_compiler::load_eval::run_reader;
mod shell_builtins;

use crate::completions::ShellCompleter;
use crate::liner_rules::make_editor_rules;
use crate::shell_builtins::add_shell_builtins;
use debug::*;
use shell::builtins::expand_tilde;
use shell::config::get_config;
use shell::platform::{Platform, STDIN_FILENO, Sys};
use sl_compiler::load_eval::{
    BUILTINS, COLORS_LISP_NAME, CORE_LISP_NAME, SLSHRC, SLSHRC_NAME, add_load_builtins,
    load_internal,
};
use sl_compiler::pass1::pass1;
use slvm::float::F56;
use slvm::{INT_BITS, INT_MAX, INT_MIN, VMError, VMResult, Value};

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
pub fn set_initial_load_path(env: &mut SloshVm, load_paths: Vec<&str>) {
    let mut v = vec![];
    for path in load_paths {
        let p = expand_tilde(PathBuf::from(path));
        let p = p.to_string_lossy();
        let i_path = env.intern(p.as_ref());
        v.push(Value::StringConst(i_path));
    }
    let path = env.alloc_vector(v);
    add_global_value(
        env,
        "*load-path*",
        path,
        "Usage: (set! '*load-path* '(\"/path/one\" \"/path/two\"))

Set the a list of paths to search for loading scripts with the load form.
Paths are a vector and are searched in index order for the file name of
the path to be loaded.

Section: scripting
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

pub fn load_test(env: &mut SloshVm) {
    match load_internal(env, "test.slosh") {
        Ok(_) => {}
        Err(err) => eprintln!("ERROR: {err}"),
    }
}

pub fn load_builtins_lisp(env: &mut SloshVm) -> VMResult<()> {
    for (name, _) in BUILTINS.iter() {
        load_internal(env, name)?;
    }
    Ok(())
}

pub fn load_builtins_lisp_less_sloshrc(env: &mut SloshVm) -> VMResult<()> {
    for (name, _) in BUILTINS.iter() {
        if SLSHRC_NAME != *name {
            load_internal(env, name)?;
        }
    }
    Ok(())
}

pub fn load_core(env: &mut SloshVm) {
    match load_internal(env, CORE_LISP_NAME) {
        Ok(_) => {}
        Err(err) => eprintln!("ERROR: {err}"),
    }
}

pub fn load_color(env: &mut SloshVm) {
    match load_internal(env, COLORS_LISP_NAME) {
        Ok(_) => {}
        Err(err) => eprintln!("ERROR: {err}"),
    }
}

fn load_core_slosh() {
    ENV.with(|renv| {
        let mut env = renv.borrow_mut();
        load_core(&mut env)
    });
}

/// Expected that the user's init.slosh will be in the user's home directory
/// at `$HOME/.config/slosh/` otherwise the directory structure will be created.
fn load_sloshrc_inner() {
    ENV.with(|renv| {
        let mut env = renv.borrow_mut();
        load_sloshrc(env.deref_mut(), None)
    });
}

/// Usage: (load-rc) | (load-rc "init.slosh)
///
/// Read and eval user's rc file, by default "init.slosh" or a user provided file path
/// found in '$HOME/.config/slosh/'.
///
/// Section: core
#[sl_sh_fn(fn_name = "load-rc", takes_env = true)]
pub fn load_sloshrc(environment: &mut SloshVm, path: Option<LooseString>) {
    if let Some(home_dir) = get_home_dir() {
        let slosh_path = home_dir.join(".config").join("slosh");
        if let Some(slosh_dir) = make_path_dir_if_possible(slosh_path.as_path()) {
            let path = path
                .clone()
                .map(|x| x.to_string())
                .unwrap_or_else(|| SLSHRC_NAME.to_string());
            set_initial_load_path(
                environment,
                vec![slosh_dir.as_os_str().to_string_lossy().as_ref()],
            );
            let init = slosh_dir.join(path);
            if fs::metadata::<&Path>(init.as_ref()).is_err() {
                match File::create::<&Path>(init.as_ref()) {
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
            let script = environment.intern(init.as_ref());
            let script = environment.get_interned(script);
            match load_internal(environment, script) {
                Ok(_) => {}
                Err(e) => eprintln!("Failed to load script ({script}): {e}"),
            }
        } else {
            // home doesn't have slosh config dir
            load_internal(environment, SLSHRC_NAME).expect("Fallback init file should be baked in to binary if user lacks `~/.config/slosh directory.");
        }
    } else {
        // no home
        load_internal(environment, SLSHRC_NAME)
            .expect("Fallback init file should be baked in to binary.");
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

fn get_usage(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() > 1 {
        Err(VMError::new_compile(
            "usage: too many args, requires one symbol as an argument",
        ))
    } else {
        match registers.iter().next() {
            None => Err(VMError::new_compile(
                "usage: no args provides, requires one symbol as an argument",
            )),
            Some(sym) => match sym {
                Value::Symbol(i) => match vm.global_intern_slot(*i) {
                    None => Err(VMError::new_compile(
                        "usage: symbol provided is not defined.",
                    )),
                    Some(slot) => {
                        let mut usage = usage(vm, slot, sym);
                        if usage.trim().is_empty() {
                            let docstring_key = vm.intern_static("doc-string");
                            let raw_doc_string = vm
                                .get_global_property(slot, docstring_key)
                                .and_then(|x| match x {
                                    Value::String(h) => Some(vm.get_string(h).to_string()),
                                    Value::StringConst(i) => Some(vm.get_interned(i).to_string()),
                                    _ => None,
                                })
                                // return default empty string and have parse_doc_string handle error if no doc provided.
                                .unwrap_or_default();
                            if let Some(test) = raw_doc_string.trim().lines().next() {
                                if test.starts_with("Usage:") {
                                    usage = test.to_string();
                                }
                            }
                        } else {
                            usage = format!("Usage: {}", usage);
                        }
                        Ok(vm.alloc_string(usage))
                    }
                },
                _ => Err(VMError::new_compile(
                    "usage: requires one symbol as an argument",
                )),
            },
        }
    }
}

pub fn usage(vm: &mut SloshVm, slot: u32, sym: &Value) -> String {
    let name = sym.display_value(vm);
    let mut doc_str = String::new();
    let sym = vm.get_global(slot);
    let args = match sym {
        Value::Lambda(h) => {
            let l = vm.get_lambda(h);
            l.dbg_args.clone()
        }
        Value::Closure(h) => {
            let (l, _h) = vm.get_closure(h);
            l.dbg_args.clone()
        }
        _ => {
            return doc_str;
        }
    };
    if let Some(args) = args {
        doc_str.push('(');
        doc_str.push_str(&name);
        for a in args {
            let arg = vm.get_interned(a);
            doc_str.push(' ');
            doc_str.push_str(arg);
        }
        doc_str.push(')');
    }
    doc_str
}

/// All rust based slosh builtins. These functions only prime the vm w/ new functions
/// and do not have any side-effects.
pub fn set_builtins(env: &mut SloshVm) {
    setup_collection_builtins(env);
    add_print_builtins(env);
    add_load_builtins(env);
    add_str_builtins(env);
    add_misc_builtins(env);
    add_io_builtins(env);
    add_conv_builtins(env);
    add_fs_meta_builtins(env);
    add_fs_temp_builtins(env);
    add_rand_builtins(env);
    add_math_builtins(env);
    add_doc_builtins(env);
}

/// Loads the user's sloshrc file, has side-effects, and sets some important
/// constants in the environment.
pub fn set_environment(env: &mut SloshVm) {
    intern_load_sloshrc(env);

    env.set_named_global("*int-bits*", (INT_BITS as i64).into());
    env.set_named_global("*int-max*", INT_MAX.into());
    env.set_named_global("*int-min*", INT_MIN.into());
    let i = env.intern("ROOT");
    env.set_named_global("*ns*", Value::Symbol(i));
}

pub fn new_slosh_vm_with_builtins() -> SloshVm {
    let mut env = new_slosh_vm();
    set_builtins(&mut env);
    set_environment(&mut env);
    env
}

fn add_doc_builtins(env: &mut SloshVm) {
    add_builtin(
        env,
        "usage",
        crate::get_usage,
        r#"Usage: (usage 'symbol)

Provides usage information derived from the bytecode. Documentation can also have it's
own usage string provided in the doc string but this function returns what the actual
function's compiled code provides.

Section: core"#,
    );
}

fn export_args(env: &mut SloshVm) {
    let mut v = Vec::new();
    for a in std::env::args() {
        let s = env.alloc_string(a);
        v.push(s);
    }
    // We should always have at least one arg, the shell executable, so this should be fine (won't panic).
    let first = if v.is_empty() {
        Value::Nil
    } else {
        v.remove(0)
    };
    let si = env.set_named_global("*shell-exe*", first);
    let key = env.intern("doc-string");
    let s = env.alloc_string(
        r#"Usage: *shell-exe*

A string that contains the executable that is running the script.

Section: shell"#
            .to_string(),
    );
    env.set_global_property(si, key, s);

    let v = env.alloc_vector(v);
    let si = env.set_named_global("*args*", v);
    let s = env.alloc_string(
        r#"Usage: *args*

A vector of the argumants passed to the script.
The first argument will be the name of the script.

Section: shell"#
            .to_string(),
    );
    env.set_global_property(si, key, s);
}

pub fn set_builtins_and_shell_builtins(env: &mut SloshVm) {
    set_builtins(env);
    set_shell_builtins(env);
}

pub fn set_shell_builtins(env: &mut SloshVm) {
    set_environment(env);
    add_shell_builtins(env);
    env.set_global_builtin("dump-regs", builtin_dump_regs);

    let uid = Sys::current_uid();
    let euid = Sys::effective_uid();
    unsafe {
        env::set_var("UID", format!("{uid}"));
    }
    unsafe {
        env::set_var("EUID", format!("{euid}"));
    }
    bridge_adapters::add_named_global_doc(
        env,
        "*uid*",
        uid.into(),
        r#"Usage: (prn *uid*)

Return system uid as a String.

Section: core

Example:
#t"#,
    );
    bridge_adapters::add_named_global_doc(
        env,
        "*euid*",
        euid.into(),
        r#"Usage: (prn *euid*)

Return effective system uid as a String.

Section: core

Example:
#t"#,
    );
    bridge_adapters::add_named_global_doc(
        env,
        "*last-status*",
        0.into(),
        r#"Usage: (prn *last-status*)

Return last exit code as an Int.

Section: core

Example:
#t"#,
    );
    let val = env.alloc_string("".to_string());
    bridge_adapters::add_named_global_doc(
        env,
        "*last-command*",
        val,
        r#"Usage: (prn *last-command*)

Return last run command as a String.

Section: core

Example:
#t"#,
    );
    bridge_adapters::add_named_global_doc(
        env,
        "*euler*",
        Value::Float(F56::from(std::f64::consts::E)),
        r#"Usage: (prn *euler*)

Float representing euler’s number.

Section: math

Example:
(test::assert-equal 2.718281828459045 *euler*)"#,
    );
    bridge_adapters::add_named_global_doc(
        env,
        "*pi*",
        Value::Float(F56::from(std::f64::consts::PI)),
        r#"Usage: (prn *pi*)

Float representing pi.

Section: math

Example:
(test::assert-equal 3.141592653589793 *pi*)"#,
    );

    // Initialize the HOST variable
    let host: OsString = Sys::gethostname().unwrap_or_else(|| "Operating system hostname is not a string capable of being parsed by native platform???".into());
    unsafe {
        env::set_var("HOST", host);
    }
    if let Ok(dir) = env::current_dir() {
        unsafe {
            env::set_var("PWD", dir);
        }
    }
    export_args(env);
}

pub fn run(modify_vm: impl FnOnce(&mut SloshVm)) -> i32 {
    run_slosh(modify_vm)
}

pub fn run_slosh(modify_vm: impl FnOnce(&mut SloshVm)) -> i32 {
    let mut status = 0;
    if let Some(config) = get_config() {
        ENV.with(|renv| {
            let mut env = renv.borrow_mut();
            env.pause_gc();
            set_builtins(&mut env);
            modify_vm(&mut env);
            set_shell_builtins(&mut env);
            env.unpause_gc();
        });
        if config.command.is_none() && config.script.is_none() {
            load_core_slosh();
            load_sloshrc_inner();
            if Sys::is_tty(STDIN_FILENO) {
                status = run_shell_tty();
            } else {
                status = run_shell_with_stdin();
            }
        } else if let Some(mut command) = config.command {
            for a in &config.args {
                command.push(' ');
                command.push_str(a);
            }
            if Sys::is_tty(STDIN_FILENO) {
                shell::run::setup_shell_tty(STDIN_FILENO);
            }
            let tcommand = command.trim_start();
            status = if tcommand.starts_with('(') || tcommand.starts_with("$(") {
                ENV.with(|env| {
                    exec_expression(command, &mut env.borrow_mut());
                });
                0
            } else {
                SHELL_ENV.with(|jobs| {
                    shell::run::run_one_command(&command, &mut jobs.borrow_mut()).unwrap_or_else(
                        |err| {
                            eprintln!("ERROR executing {command}: {err}");
                            1
                        },
                    )
                })
            };
            SHELL_ENV.with(|jobs| {
                jobs.borrow_mut().reap_procs();
            });
        } else if let Some(script) = config.script {
            load_core_slosh();
            load_sloshrc_inner();
            if Sys::is_tty(STDIN_FILENO) {
                shell::run::setup_shell_tty(STDIN_FILENO);
            }
            status = ENV.with(|renv| {
                let mut env = renv.borrow_mut();
                let script = env.intern(&script);
                add_global_value(&mut env, "*run-script*", Value::StringConst(script),
                                 r#"Usage: (prn *run-script*)

                                 If a script is being run, this variable will contain it's name as provided on the CLI.

                                 Section: shell

                                 Example:
                                 (prn *run-script*)
                                 "#
                );
                let script = env.get_interned(script);
                match load_internal(&mut env, script) {
                    Ok(_) => 0,
                    Err(err) => {
                        eprintln!("ERROR: {err}");
                        1
                    }
                }
            });
        }
    }
    status
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

    if let Err(e) = con.history.set_file_name_and_load_history(history_file()) {
        eprintln!("Error loading history: {e}");
    }
    shell::run::setup_shell_tty(STDIN_FILENO);
    SHELL_ENV.with(|jobs| {
        jobs.borrow_mut().cap_term();
    });
    let mut status = 0;
    loop {
        SHELL_ENV.with(|jobs| {
            jobs.borrow_mut().reap_procs();
        });
        let prompt = ENV.with(|env| get_prompt(&mut env.borrow_mut()));
        let res = match con.read_line(Prompt::from(prompt), get_color_closure()) {
            Ok(input) => input,
            Err(err) => match err.kind() {
                ErrorKind::UnexpectedEof => {
                    status = 1;
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
        status = exec_expr_or_run_command(&res, status);
    }
    status
}

fn exec_expr_or_run_command(res: &String, mut status: i32) -> i32 {
    if res.starts_with('(') || res.starts_with("$(") {
        ENV.with(|env| {
            exec_expression(res.clone(), &mut env.borrow_mut());
        });
    } else {
        status = run_command(res);
    }
    status
}

fn run_command(res: &String) -> i32 {
    let status = SHELL_ENV.with(|jobs| {
        shell::run::run_one_command(res, &mut jobs.borrow_mut()).unwrap_or_else(|err| {
            eprintln!("ERROR executing {res}: {err}");
            1
        })
    });
    ENV.with(|env| {
        let mut env = env.borrow_mut();
        env.set_named_global("*last-status*", status.into());
        let res = env.alloc_string(res.to_string());
        env.set_named_global("*last-command*", res);
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
        status = exec_expr_or_run_command(&res, status);
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
                    if e.key == "compile" || e.key == "read" {
                        eprintln!("Compile error, line {}: {}", env.line_num(), e);
                        return;
                    } else {
                        eprintln!("Comp Time ERROR: {}", e.display(env));
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
    use super::*;

    use crate::{ENV, set_initial_load_path};
    use compiler_test_utils::exec;
    use slvm::{Value, from_i56};
    use std::fs::{File, create_dir_all};
    use std::io::Write;
    use std::ops::DerefMut;
    use temp_env;
    use tempfile::TempDir;

    #[test]
    fn test_load_path_no_home() {
        // create home dir
        let tmp_dir = TempDir::with_prefix("test_load_path").unwrap();
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
                set_builtins_and_shell_builtins(vm.deref_mut());
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
