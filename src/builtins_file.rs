use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::hash::BuildHasher;
use std::io;
use std::path::Path;
use std::rc::Rc;

use crate::builtins::*;
use crate::builtins_util::*;
use crate::environment::*;
use crate::eval::*;
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

fn builtin_cd(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() > 1 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "cd can not have more then one form",
        ))
    } else {
        let mut home = match env::var("HOME") {
            Ok(val) => val,
            Err(_) => "/".to_string(),
        };
        let old_dir = match env::var("OLDPWD") {
            Ok(val) => val,
            Err(_) => home.to_string(),
        };
        let args = to_args_str(environment, args)?;
        let args = args.iter();
        let new_dir = args.peekable().peek().map_or(&home[..], |x| *x);
        let expand_dir = expand_tilde(new_dir);
        let new_dir = if expand_dir.is_some() {
            home = expand_dir.unwrap();
            &home
        } else {
            new_dir
        };
        let new_dir = if new_dir == "-" { &old_dir } else { new_dir };
        let new_dir = cd_expand_all_dots(new_dir.to_string());
        let root = Path::new(&new_dir);
        env::set_var("OLDPWD", env::current_dir()?);
        if let Err(e) = env::set_current_dir(&root) {
            eprintln!("Error changing to {}, {}", root.display(), e);
            Ok(Expression::Atom(Atom::Nil))
        } else {
            env::set_var("PWD", env::current_dir()?);
            Ok(Expression::Atom(Atom::True))
        }
    }
}

fn builtin_use_stdout(
    environment: &mut Environment,
    parts: &[Expression],
) -> io::Result<Expression> {
    let mut last_eval = Ok(Expression::Atom(Atom::Nil));
    let old_out = environment.state.stdout_status.clone();
    let old_err = environment.state.stderr_status.clone();
    if !environment.in_pipe {
        // Don't break a pipe if in one.
        environment.state.stdout_status = Some(IOState::Inherit);
        environment.state.stderr_status = Some(IOState::Inherit);
    }
    for a in parts {
        last_eval = eval(environment, a);
        if last_eval.is_err() {
            break;
        }
    }
    environment.state.stdout_status = old_out;
    environment.state.stderr_status = old_err;
    last_eval
}

fn builtin_err_null(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    environment.state.stderr_status = Some(IOState::Null);
    let res = builtin_progn(environment, args);
    environment.state.stderr_status = None;
    res
}

fn builtin_out_null(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    environment.state.stdout_status = Some(IOState::Null);
    let res = builtin_progn(environment, args);
    environment.state.stdout_status = None;
    res
}

fn builtin_file_trunc(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    if args.len() != 1 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "file_trunc must have one form (file name to create/truncate)",
        ))
    } else {
        let arg0 = eval(environment, &args[0])?;
        if let Expression::Atom(Atom::String(s)) = &arg0 {
            File::create(s)?;
        } else {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "file_trunc must have one form (file name to create/truncate)",
            ));
        }
        Ok(arg0)
    }
}

fn builtin_file_rdr(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() < 2 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "file_rdr must have at least two forms",
        ))
    } else {
        let arg0 = eval(environment, &args[0])?;
        environment.state.stdout_status = Some(IOState::Pipe);
        let res = builtin_progn(environment, &args[1..]);
        environment.state.stdout_status = None;
        if let Ok(res) = &res {
            if let Expression::Atom(Atom::String(s)) = &arg0 {
                let mut writer = std::fs::OpenOptions::new()
                    .read(false)
                    .write(true)
                    .append(true)
                    .create(true)
                    .open(s)?;
                res.writef(environment, &mut writer)?;
            }
        }
        res
    }
}

fn internal_output_to(
    environment: &mut Environment,
    args: &[Expression],
    name: &str,
    is_stdout: bool,
) -> io::Result<Expression> {
    if args.len() < 2 {
        let msg = format!("{} must have at least two forms", name);
        Err(io::Error::new(io::ErrorKind::Other, msg))
    } else {
        let arg0 = eval(environment, &args[0])?;
        if let Expression::Atom(Atom::String(s)) = &arg0 {
            if is_stdout {
                environment.state.stdout_status = Some(IOState::FileAppend(s.clone()));
            } else {
                environment.state.stderr_status = Some(IOState::FileAppend(s.clone()));
            }
        } else {
            let msg = format!("{} must have a file", name);
            return Err(io::Error::new(io::ErrorKind::Other, msg));
        }
        let res = builtin_progn(environment, &args[1..]);
        if is_stdout {
            environment.state.stdout_status = None;
        } else {
            environment.state.stderr_status = None;
        }
        res
    }
}

fn builtin_stdout_to(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    internal_output_to(environment, args, "stdout-to", true)
}

fn builtin_stderr_to(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    internal_output_to(environment, args, "stderr-to", false)
}

fn builtin_path_exists(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    if args.len() != 1 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "path-exists takes one form (a path)",
        ))
    } else {
        let arg0 = eval(environment, &args[0])?;
        if let Expression::Atom(Atom::String(p)) = arg0 {
            let path = Path::new(&p);
            if path.exists() {
                Ok(Expression::Atom(Atom::True))
            } else {
                Ok(Expression::Atom(Atom::Nil))
            }
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "path-exists path must be a string",
            ))
        }
    }
}

fn builtin_is_file(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 1 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "is-file takes one form (a path)",
        ))
    } else {
        let arg0 = eval(environment, &args[0])?;
        if let Expression::Atom(Atom::String(p)) = arg0 {
            let path = Path::new(&p);
            if path.is_file() {
                Ok(Expression::Atom(Atom::True))
            } else {
                Ok(Expression::Atom(Atom::Nil))
            }
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "is-file path must be a string",
            ))
        }
    }
}

fn builtin_is_dir(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 1 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "is-dir takes one form (a path)",
        ))
    } else {
        let arg0 = eval(environment, &args[0])?;
        if let Expression::Atom(Atom::String(p)) = arg0 {
            let path = Path::new(&p);
            if path.is_dir() {
                Ok(Expression::Atom(Atom::True))
            } else {
                Ok(Expression::Atom(Atom::Nil))
            }
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "is-dir path must be a string",
            ))
        }
    }
}

fn builtin_pipe(environment: &mut Environment, parts: &[Expression]) -> io::Result<Expression> {
    if environment.in_pipe {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "pipe within pipe, not valid",
        ));
    }
    let old_out_status = environment.state.stdout_status.clone();
    environment.in_pipe = true;
    let mut out = Expression::Atom(Atom::Nil);
    environment.state.stdout_status = Some(IOState::Pipe);
    let mut error: Option<io::Result<Expression>> = None;
    let mut i = 1; // Meant 1 here.
    for p in parts {
        if i == parts.len() {
            environment.state.stdout_status = old_out_status.clone();
            environment.in_pipe = false; // End of the pipe and want to wait.
        }
        environment.data_in = Some(out.clone());
        let res = eval(environment, p);
        if let Err(err) = res {
            error = Some(Err(err));
            break;
        }
        if let Ok(Expression::Process(ProcessState::Running(pid))) = res {
            if environment.state.pipe_pgid.is_none() {
                environment.state.pipe_pgid = Some(pid);
            }
        }
        if let Ok(Expression::Process(ProcessState::Over(pid, _exit_status))) = res {
            if environment.state.pipe_pgid.is_none() {
                environment.state.pipe_pgid = Some(pid);
            }
        }
        if let Ok(Expression::File(FileState::Write(f))) = &res {
            let mut do_write = false;
            match &environment.data_in {
                Some(Expression::Atom(Atom::Nil)) => {}
                Some(Expression::Atom(_atom)) => {
                    do_write = true;
                }
                Some(Expression::Process(ProcessState::Running(_pid))) => {
                    do_write = true;
                }
                Some(Expression::File(FileState::Read(_file))) => {
                    do_write = true;
                }
                Some(_) => {
                    error = Some(Err(io::Error::new(
                        io::ErrorKind::Other,
                        "Invalid expression state before file.",
                    )));
                    break;
                }
                None => {}
            }
            if do_write {
                if let Err(err) = environment
                    .data_in
                    .as_ref()
                    .unwrap()
                    .writef(environment, &mut *f.borrow_mut())
                {
                    error = Some(Err(err));
                    break;
                }
            }
        }
        if let Ok(Expression::File(FileState::Read(_))) = &res {
            if i > 1 {
                error = Some(Err(io::Error::new(
                    io::ErrorKind::Other,
                    "Not a valid place for a read file (must be at start of pipe).",
                )));
                break;
            }
        }
        if res.is_ok() {
            out = res.unwrap();
        }
        i += 1;
    }
    environment.data_in = None;
    environment.in_pipe = false;
    environment.state.pipe_pgid = None;
    environment.state.stdout_status = old_out_status;
    if error.is_some() {
        error.unwrap()
    } else {
        Ok(out)
    }
}

fn builtin_wait(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 1 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "wait takes one form (a pid to wait on)",
        ))
    } else {
        match eval(environment, &args[0]) {
            Ok(arg) => match arg {
                Expression::Process(ProcessState::Running(pid)) => {
                    match wait_pid(environment, pid, None) {
                        Some(exit_status) => {
                            Ok(Expression::Atom(Atom::Int(i64::from(exit_status))))
                        }
                        None => Ok(Expression::Atom(Atom::Nil)),
                    }
                }
                Expression::Process(ProcessState::Over(_pid, exit_status)) => {
                    Ok(Expression::Atom(Atom::Int(i64::from(exit_status))))
                }
                Expression::Atom(Atom::Int(pid)) => match wait_pid(environment, pid as u32, None) {
                    Some(exit_status) => Ok(Expression::Atom(Atom::Int(i64::from(exit_status)))),
                    None => Ok(Expression::Atom(Atom::Nil)),
                },
                _ => Err(io::Error::new(
                    io::ErrorKind::Other,
                    "wait error: not a pid",
                )),
            },
            Err(err) => {
                let msg = format!("wait error evaluating form: {}", err);
                Err(io::Error::new(io::ErrorKind::Other, msg))
            }
        }
    }
}

fn builtin_pid(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 1 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "pid takes one form (a process)",
        ))
    } else {
        match eval(environment, &args[0]) {
            Ok(arg) => match arg {
                Expression::Process(ProcessState::Running(pid)) => {
                    Ok(Expression::Atom(Atom::Int(i64::from(pid))))
                }
                Expression::Process(ProcessState::Over(pid, _exit_status)) => {
                    Ok(Expression::Atom(Atom::Int(i64::from(pid))))
                }
                _ => Err(io::Error::new(
                    io::ErrorKind::Other,
                    "pid error: not a process",
                )),
            },
            Err(err) => {
                let msg = format!("pid error evaluating form: {}", err);
                Err(io::Error::new(io::ErrorKind::Other, msg))
            }
        }
    }
}

pub fn add_file_builtins<S: BuildHasher>(data: &mut HashMap<String, Rc<Expression>, S>) {
    data.insert("cd".to_string(), Rc::new(Expression::Func(builtin_cd)));
    data.insert(
        "use-stdout".to_string(),
        Rc::new(Expression::Func(builtin_use_stdout)),
    );
    data.insert(
        "out-null".to_string(),
        Rc::new(Expression::Func(builtin_out_null)),
    );
    data.insert(
        "err-null".to_string(),
        Rc::new(Expression::Func(builtin_err_null)),
    );
    data.insert(
        "file-rdr".to_string(),
        Rc::new(Expression::Func(builtin_file_rdr)),
    );
    data.insert(
        "stdout-to".to_string(),
        Rc::new(Expression::Func(builtin_stdout_to)),
    );
    data.insert(
        "stderr-to".to_string(),
        Rc::new(Expression::Func(builtin_stderr_to)),
    );
    data.insert(
        "file-trunc".to_string(),
        Rc::new(Expression::Func(builtin_file_trunc)),
    );
    data.insert(
        "path-exists".to_string(),
        Rc::new(Expression::Func(builtin_path_exists)),
    );
    data.insert(
        "is-file".to_string(),
        Rc::new(Expression::Func(builtin_is_file)),
    );
    data.insert(
        "is-dir".to_string(),
        Rc::new(Expression::Func(builtin_is_dir)),
    );
    data.insert("pipe".to_string(), Rc::new(Expression::Func(builtin_pipe)));
    data.insert("wait".to_string(), Rc::new(Expression::Func(builtin_wait)));
    data.insert("pid".to_string(), Rc::new(Expression::Func(builtin_pid)));
}
