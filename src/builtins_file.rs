use std::collections::HashMap;
use std::env;
use std::hash::BuildHasher;
use std::io::{self, Write};
use std::path::Path;

use glob::glob;

use crate::builtins_util::*;
use crate::environment::*;
use crate::eval::*;
use crate::interner::*;
use crate::process::*;
use crate::types::*;

fn cd_expand_all_dots(cd: String) -> String {
    let mut all_dots = false;
    if cd.len() > 2 {
        all_dots = true;
        for ch in cd.chars() {
            if ch != '.' {
                all_dots = false;
                break;
            }
        }
    }
    if all_dots {
        let mut new_cd = String::new();
        let paths_up = cd.len() - 2;
        new_cd.push_str("../");
        for _i in 0..paths_up {
            new_cd.push_str("../");
        }
        new_cd
    } else {
        cd
    }
}

fn builtin_cd(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let home = match env::var("HOME") {
        Ok(val) => val,
        Err(_) => "/".to_string(),
    };
    let old_dir = match env::var("OLDPWD") {
        Ok(val) => val,
        Err(_) => home.to_string(),
    };
    let new_dir = if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg_d = arg.get();
            let new_arg = match &arg_d.data {
                ExpEnum::Symbol(s) => match get_expression(environment, s) {
                    Some(exp) => match &exp.exp.get().data {
                        ExpEnum::Function(_) => eval(
                            environment,
                            Expression::alloc_data(ExpEnum::String((*s).into(), None)),
                        )?,
                        ExpEnum::Lambda(_) => eval(
                            environment,
                            Expression::alloc_data(ExpEnum::String((*s).into(), None)),
                        )?,
                        ExpEnum::Macro(_) => eval(
                            environment,
                            Expression::alloc_data(ExpEnum::String((*s).into(), None)),
                        )?,
                        _ => {
                            drop(arg_d);
                            eval(environment, &arg)?
                        }
                    },
                    _ => {
                        drop(arg_d);
                        eval(environment, &arg)?
                    }
                },
                _ => {
                    drop(arg_d);
                    eval(environment, &arg)?
                }
            }
            .as_string(environment)?;
            if let Some(h) = expand_tilde(&new_arg) {
                h
            } else {
                new_arg
            }
        } else {
            return Err(LispError::new("cd can not have more then one form"));
        }
    } else {
        home
    };
    let new_dir = if new_dir == "-" { &old_dir } else { &new_dir };
    let new_dir = cd_expand_all_dots(new_dir.to_string());
    let root = Path::new(&new_dir);
    env::set_var("OLDPWD", env::current_dir()?);
    if let Err(e) = env::set_current_dir(&root) {
        eprintln!("Error changing to {}, {}", root.display(), e);
        Ok(Expression::make_nil())
    } else {
        env::set_var("PWD", env::current_dir()?);
        Ok(Expression::make_true())
    }
}

fn file_test(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
    test: fn(path: &Path) -> bool,
    fn_name: &str,
) -> Result<Expression, LispError> {
    if let Some(p) = args.next() {
        if args.next().is_none() {
            let p = match &eval(environment, p)?.get().data {
                ExpEnum::String(p, _) => {
                    match expand_tilde(&p) {
                        Some(p) => p,
                        None => p.to_string(), // XXX not great.
                    }
                }
                _ => {
                    let msg = format!("{} path must be a string", fn_name);
                    return Err(LispError::new(msg));
                }
            };
            let path = Path::new(&p);
            if test(path) {
                return Ok(Expression::make_true());
            } else {
                return Ok(Expression::make_nil());
            }
        }
    }
    let msg = format!("{} takes a string (a path)", fn_name);
    Err(LispError::new(msg))
}

fn builtin_path_exists(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    file_test(environment, args, |path| path.exists(), "fs-exists?")
}

fn builtin_is_file(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    file_test(environment, args, |path| path.is_file(), "fs-file?")
}

fn builtin_is_dir(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    file_test(environment, args, |path| path.is_dir(), "fs-dir?")
}

fn pipe_write_file(environment: &mut Environment, writer: &mut dyn Write) -> Result<(), LispError> {
    let mut do_write = false;
    if let Some(data_in) = environment.data_in.clone() {
        match &data_in.get().data {
            ExpEnum::True => do_write = true,
            ExpEnum::String(_, _) => do_write = true,
            ExpEnum::Symbol(_) => do_write = true,
            ExpEnum::Float(_) => do_write = true,
            ExpEnum::Int(_) => do_write = true,
            ExpEnum::Char(_) => do_write = true,
            ExpEnum::CodePoint(_) => do_write = true,
            ExpEnum::Process(ProcessState::Running(_)) => {
                do_write = true;
            }
            ExpEnum::File(file) => match &*file.borrow() {
                FileState::Stdin => {
                    do_write = true;
                }
                FileState::Read(_file, _) => {
                    do_write = true;
                }
                FileState::ReadBinary(_file) => {
                    do_write = true;
                }
                _ => {}
            },
            ExpEnum::Nil => {}
            _ => {
                return Err(LispError::new("Invalid expression state before file."));
            }
        }
    }
    if do_write {
        let data_in = environment.data_in.as_ref().unwrap().clone();
        data_in.writef(environment, writer)?;
    }
    Ok(())
}

fn builtin_pipe(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if environment.in_pipe {
        return Err(LispError::new("pipe within pipe, not valid"));
    }
    let old_out_status = environment.state.stdout_status.clone();
    environment.in_pipe = true;
    let mut out = Expression::make_nil();
    environment.state.stdout_status = Some(IOState::Pipe);
    let mut error: Option<Result<Expression, LispError>> = None;
    let mut i = 1; // Meant 1 here.
    let mut pipe = args.next();
    while let Some(p) = pipe {
        let next_pipe = args.next();
        if next_pipe.is_none() {
            environment.state.stdout_status = old_out_status.clone();
            environment.in_pipe = false; // End of the pipe and want to wait.
        }
        environment.data_in = Some(out.clone());
        let res = eval(environment, p);
        match &res {
            Ok(res) => match &res.get().data {
                ExpEnum::Process(ProcessState::Running(pid)) => {
                    if environment.state.pipe_pgid.is_none() {
                        environment.state.pipe_pgid = Some(*pid);
                    }
                }
                ExpEnum::Process(ProcessState::Over(pid, _exit_status)) => {
                    if environment.state.pipe_pgid.is_none() {
                        environment.state.pipe_pgid = Some(*pid);
                    }
                }
                ExpEnum::File(file) => match &mut *file.borrow_mut() {
                    FileState::Stdout => {
                        let stdout = io::stdout();
                        let mut handle = stdout.lock();
                        if let Err(err) = pipe_write_file(environment, &mut handle) {
                            error = Some(Err(err));
                            break;
                        }
                    }
                    FileState::Stderr => {
                        let stderr = io::stderr();
                        let mut handle = stderr.lock();
                        if let Err(err) = pipe_write_file(environment, &mut handle) {
                            error = Some(Err(err));
                            break;
                        }
                    }
                    FileState::Write(f) => {
                        if let Err(err) = pipe_write_file(environment, f) {
                            error = Some(Err(err));
                            break;
                        }
                    }
                    FileState::Read(_, _) => {
                        if i > 1 {
                            error = Some(Err(LispError::new(
                                "Not a valid place for a read file (must be at start of pipe).",
                            )));
                            break;
                        }
                    }
                    FileState::ReadBinary(_) => {
                        if i > 1 {
                            error = Some(Err(LispError::new(
                                "Not a valid place for a read file (must be at start of pipe).",
                            )));
                            break;
                        }
                    }
                    FileState::Stdin => {
                        if i > 1 {
                            error = Some(Err(LispError::new("Not a valid place for stdin.")));
                            break;
                        }
                    }
                    FileState::Closed => {
                        error = Some(Err(LispError::new("Closed file not valid in pipe.")));
                        break;
                    }
                },
                _ => {}
            },
            Err(err) => {
                error = Some(Err(LispError::new(err.to_string())));
                break;
            }
        }
        out = if let Ok(out) = res {
            out.clone()
        } else {
            out.clone()
        };
        i += 1;
        pipe = next_pipe;
    }
    environment.data_in = None;
    environment.in_pipe = false;
    environment.state.pipe_pgid = None;
    environment.state.stdout_status = old_out_status;
    if let Some(error) = error {
        error
    } else {
        Ok(out)
    }
}

fn builtin_wait(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg0) = args.next() {
        if args.next().is_none() {
            let arg0 = eval(environment, arg0)?;
            return match &arg0.get().data {
                ExpEnum::Process(ProcessState::Running(pid)) => {
                    match wait_pid(environment, *pid, None) {
                        Some(exit_status) => {
                            Ok(Expression::alloc_data(ExpEnum::Int(i64::from(exit_status))))
                        }
                        None => Ok(Expression::make_nil()),
                    }
                }
                ExpEnum::Process(ProcessState::Over(_pid, exit_status)) => Ok(
                    Expression::alloc_data(ExpEnum::Int(i64::from(*exit_status))),
                ),
                ExpEnum::Int(pid) => match wait_pid(environment, *pid as u32, None) {
                    Some(exit_status) => {
                        Ok(Expression::alloc_data(ExpEnum::Int(i64::from(exit_status))))
                    }
                    None => Ok(Expression::make_nil()),
                },
                _ => Err(LispError::new("wait error: not a pid")),
            };
        }
    }
    Err(LispError::new("wait takes one form (a pid to wait on)"))
}

fn builtin_pid(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg0) = args.next() {
        if args.next().is_none() {
            let arg0 = eval(environment, arg0)?;
            return match arg0.get().data {
                ExpEnum::Process(ProcessState::Running(pid)) => {
                    Ok(Expression::alloc_data(ExpEnum::Int(i64::from(pid))))
                }
                ExpEnum::Process(ProcessState::Over(pid, _exit_status)) => {
                    Ok(Expression::alloc_data(ExpEnum::Int(i64::from(pid))))
                }
                _ => Err(LispError::new("pid error: not a process")),
            };
        }
    }
    Err(LispError::new("pid takes one form (a process)"))
}

fn builtin_glob(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let mut files = Vec::new();
    for pat in args {
        let pat = match &eval(environment, pat)?.get().data {
            ExpEnum::String(s, _) => s.to_string(),
            _ => return Err(LispError::new("globs need to be strings")),
        };
        let pat = match expand_tilde(&pat) {
            Some(p) => p,
            None => pat,
        };
        match glob(&pat) {
            Ok(paths) => {
                for p in paths {
                    match p {
                        Ok(p) => {
                            if let Some(p) = p.to_str() {
                                files.push(Expression::alloc_data_h(ExpEnum::String(
                                    p.to_string().into(),
                                    None,
                                )));
                            }
                        }
                        Err(err) => {
                            let msg = format!("glob error on while iterating {}, {}", pat, err);
                            return Err(LispError::new(msg));
                        }
                    }
                }
            }
            Err(err) => {
                let msg = format!("glob error on {}, {}", pat, err);
                return Err(LispError::new(msg));
            }
        }
    }
    Ok(Expression::with_list(files))
}

pub fn add_file_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, Reference, S>,
) {
    let root = interner.intern("root");
    data.insert(
        interner.intern("cd"),
        Expression::make_function(
            builtin_cd,
            "Usage: (cd dir-to-change-to)

Change directory.

Section: shell
",
            root,
        ),
    );
    data.insert(
        interner.intern("fs-exists?"),
        Expression::make_function(
            builtin_path_exists,
            "Usage: (fs-exists? path-to-test)

Does the given path exist?

Section: shell
",
            root,
        ),
    );
    data.insert(
        interner.intern("fs-file?"),
        Expression::make_function(
            builtin_is_file,
            "Usage: (fs-file? path-to-test)

Is the given path a file?

Section: shell
",
            root,
        ),
    );
    data.insert(
        interner.intern("fs-dir?"),
        Expression::make_function(
            builtin_is_dir,
            "Usage: (fs-dir? path-to-test)

Is the given path a directory?

Section: shell
",
            root,
        ),
    );
    data.insert(
        interner.intern("pipe"),
        Expression::make_function(
            builtin_pipe,
            "Usage: (pipe (proc-whose-stdout) (is-inpup-here))

Setup a pipe between processes.

Section: shell
",
            root,
        ),
    );
    data.insert(
        interner.intern("wait"),
        Expression::make_function(
            builtin_wait,
            "Usage: (wait proc-to-wait-for)

Wait for a process to end and return it's exit status.

Section: shell
",
            root,
        ),
    );
    data.insert(
        interner.intern("pid"),
        Expression::make_function(
            builtin_pid,
            "Usage: (pid proc)

Return the pid of a process.

Section: shell
",
            root,
        ),
    );
    data.insert(
        interner.intern("glob"),
        Expression::make_function(
            builtin_glob,
            "Usage: (glob /path/with/*)

Takes a list/varargs of globs and return the list of them expanded.

Section: shell
",
            root,
        ),
    );
}
