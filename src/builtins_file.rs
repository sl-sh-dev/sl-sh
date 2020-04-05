use std::collections::HashMap;
use std::env;
use std::hash::BuildHasher;
use std::io::{self, Write};
use std::path::Path;
use std::rc::Rc;

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
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
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
            let new_arg = match arg {
                Expression::Atom(Atom::Symbol(s)) => match get_expression(environment, s) {
                    Some(exp) => match &exp.exp {
                        Expression::Function(_) => {
                            eval(environment, &Expression::Atom(Atom::StringRef(s)))?
                        }
                        Expression::Atom(Atom::Lambda(_)) => {
                            eval(environment, &Expression::Atom(Atom::StringRef(s)))?
                        }
                        Expression::Atom(Atom::Macro(_)) => {
                            eval(environment, &Expression::Atom(Atom::StringRef(s)))?
                        }
                        _ => eval(environment, &arg)?,
                    },
                    _ => eval(environment, &arg)?,
                },
                _ => eval(environment, &arg)?,
            }
            .as_string(environment)?;
            if let Some(h) = expand_tilde(&new_arg) {
                h
            } else {
                new_arg
            }
        } else {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "cd can not have more then one form",
            ));
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
        Ok(Expression::nil())
    } else {
        env::set_var("PWD", env::current_dir()?);
        Ok(Expression::Atom(Atom::True))
    }
}

fn file_test(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
    test: fn(path: &Path) -> bool,
    fn_name: &str,
) -> io::Result<Expression> {
    if let Some(p) = args.next() {
        if args.next().is_none() {
            let p = match eval(environment, p)? {
                Expression::Atom(Atom::String(p)) => {
                    match expand_tilde(&p) {
                        Some(p) => p,
                        None => p.to_string(), // XXX not great.
                    }
                }
                Expression::Atom(Atom::StringBuf(p)) => {
                    let pb = p.borrow();
                    match expand_tilde(&pb) {
                        Some(p) => p,
                        None => pb.to_string(), // XXX not great.
                    }
                }
                _ => {
                    let msg = format!("{} path must be a string", fn_name);
                    return Err(io::Error::new(io::ErrorKind::Other, msg));
                }
            };
            let path = Path::new(&p);
            if test(path) {
                return Ok(Expression::Atom(Atom::True));
            } else {
                return Ok(Expression::nil());
            }
        }
    }
    let msg = format!("{} takes a string (a path)", fn_name);
    Err(io::Error::new(io::ErrorKind::Other, msg))
}

fn builtin_path_exists(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    file_test(environment, args, |path| path.exists(), "fs-exists?")
}

fn builtin_is_file(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    file_test(environment, args, |path| path.is_file(), "fs-file?")
}

fn builtin_is_dir(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    file_test(environment, args, |path| path.is_dir(), "fs-dir?")
}

fn pipe_write_file(environment: &mut Environment, writer: &mut dyn Write) -> io::Result<()> {
    let mut do_write = false;
    match &environment.data_in {
        Some(Expression::Atom(_atom)) => {
            do_write = true;
        }
        Some(Expression::Process(ProcessState::Running(_pid))) => {
            do_write = true;
        }
        Some(Expression::File(file)) => match &*file.borrow() {
            FileState::Stdin => {
                do_write = true;
            }
            FileState::Read(_file) => {
                do_write = true;
            }
            _ => {}
        },
        Some(val) => {
            if !val.is_nil() {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "Invalid expression state before file.",
                ));
            }
        }
        None => {}
    }
    if do_write {
        let data_in = environment.data_in.as_ref().unwrap().clone();
        data_in.writef(environment, writer)?;
    }
    Ok(())
}

fn builtin_pipe(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if environment.in_pipe {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "pipe within pipe, not valid",
        ));
    }
    let old_out_status = environment.state.stdout_status.clone();
    environment.in_pipe = true;
    let mut out = Expression::nil();
    environment.state.stdout_status = Some(IOState::Pipe);
    let mut error: Option<io::Result<Expression>> = None;
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
        if let Ok(Expression::File(file)) = &res {
            match &*file.borrow() {
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
                    if let Err(err) = pipe_write_file(environment, &mut *f.borrow_mut()) {
                        error = Some(Err(err));
                        break;
                    }
                }
                FileState::Read(_) => {
                    if i > 1 {
                        error = Some(Err(io::Error::new(
                            io::ErrorKind::Other,
                            "Not a valid place for a read file (must be at start of pipe).",
                        )));
                        break;
                    }
                }
                FileState::Stdin => {
                    if i > 1 {
                        error = Some(Err(io::Error::new(
                            io::ErrorKind::Other,
                            "Not a valid place for stdin.",
                        )));
                        break;
                    }
                }
                FileState::Closed => {
                    error = Some(Err(io::Error::new(
                        io::ErrorKind::Other,
                        "Closed file not valid in pipe.",
                    )));
                    break;
                }
            }
        }
        out = if let Ok(out) = res { out } else { out };
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
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg0) = args.next() {
        if args.next().is_none() {
            let arg0 = eval(environment, arg0)?;
            return match arg0 {
                Expression::Process(ProcessState::Running(pid)) => {
                    match wait_pid(environment, pid, None) {
                        Some(exit_status) => {
                            Ok(Expression::Atom(Atom::Int(i64::from(exit_status))))
                        }
                        None => Ok(Expression::nil()),
                    }
                }
                Expression::Process(ProcessState::Over(_pid, exit_status)) => {
                    Ok(Expression::Atom(Atom::Int(i64::from(exit_status))))
                }
                Expression::Atom(Atom::Int(pid)) => match wait_pid(environment, pid as u32, None) {
                    Some(exit_status) => Ok(Expression::Atom(Atom::Int(i64::from(exit_status)))),
                    None => Ok(Expression::nil()),
                },
                _ => Err(io::Error::new(
                    io::ErrorKind::Other,
                    "wait error: not a pid",
                )),
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "wait takes one form (a pid to wait on)",
    ))
}

fn builtin_pid(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg0) = args.next() {
        if args.next().is_none() {
            let arg0 = eval(environment, arg0)?;
            return match arg0 {
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
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "pid takes one form (a process)",
    ))
}

fn builtin_glob(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    let mut files = Vec::new();
    for pat in args {
        let pat = match eval(environment, pat)? {
            Expression::Atom(Atom::String(s)) => s,
            Expression::Atom(Atom::StringRef(s)) => s.to_string(),
            Expression::Atom(Atom::StringBuf(s)) => s.borrow().to_string(),
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "globs need to be strings",
                ))
            }
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
                                files.push(Expression::Atom(Atom::String(p.to_string())));
                            }
                        }
                        Err(err) => {
                            let msg = format!("glob error on while iterating {}, {}", pat, err);
                            return Err(io::Error::new(io::ErrorKind::Other, msg));
                        }
                    }
                }
            }
            Err(err) => {
                let msg = format!("glob error on {}, {}", pat, err);
                return Err(io::Error::new(io::ErrorKind::Other, msg));
            }
        }
    }
    Ok(Expression::with_list(files))
}

pub fn add_file_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, Rc<Reference>, S>,
) {
    let root = interner.intern("root");
    data.insert(
        interner.intern("cd"),
        Rc::new(Expression::make_function(
            builtin_cd,
            "Change directory.",
            root,
        )),
    );
    data.insert(
        interner.intern("fs-exists?"),
        Rc::new(Expression::make_function(
            builtin_path_exists,
            "Does the given path exist?",
            root,
        )),
    );
    data.insert(
        interner.intern("fs-file?"),
        Rc::new(Expression::make_function(
            builtin_is_file,
            "Is the given path a file?",
            root,
        )),
    );
    data.insert(
        interner.intern("fs-dir?"),
        Rc::new(Expression::make_function(
            builtin_is_dir,
            "Is the given path a directory?",
            root,
        )),
    );
    data.insert(
        interner.intern("pipe"),
        Rc::new(Expression::make_function(
            builtin_pipe,
            "Setup a pipe between processes.",
            root,
        )),
    );
    data.insert(
        interner.intern("wait"),
        Rc::new(Expression::make_function(
            builtin_wait,
            "Wait for a process to end and return it's exit status.",
            root,
        )),
    );
    data.insert(
        interner.intern("pid"),
        Rc::new(Expression::make_function(
            builtin_pid,
            "Return the pid of a process.",
            root,
        )),
    );
    data.insert(
        interner.intern("glob"),
        Rc::new(Expression::make_function(
            builtin_glob,
            "Takes a list of globs and return the list of them expanded.",
            root,
        )),
    );
}
