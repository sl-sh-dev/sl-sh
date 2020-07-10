use nix::{
    sys::{
        signal::{self, Signal},
        termios,
    },
    unistd::{self, Pid},
};
use std::io;
use std::collections::HashMap;
use std::env;
use std::hash::BuildHasher;

use crate::builtins_util::*;
use crate::environment::*;
use crate::eval::*;
use crate::interner::*;
use crate::process::*;
use crate::types::*;

fn builtin_syscall(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(command) = args.next() {
        let command = eval(environment, command)?;
        let command_d = command.get();
        match &command_d.data {
            ExpEnum::Atom(Atom::String(s, _)) => do_command(environment, s, args),
            _ => {
                let msg = format!(
                    "syscall: first argument {} not a string, type {}",
                    command,
                    command.display_type()
                );
                Err(io::Error::new(io::ErrorKind::Other, msg))
            }
        }
    } else {
        Err(io::Error::new(io::ErrorKind::Other, "syscall: empty call"))
    }
}

fn builtin_export(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(key) = args.next() {
        if let Some(val) = args.next() {
            if args.next().is_none() {
                let key = eval(environment, key)?;
                let val = eval(environment, val)?;
                let key_d = &key.get().data;
                let key = match key_d {
                    ExpEnum::Atom(Atom::Symbol(s)) => s,
                    _ => {
                        return Err(io::Error::new(
                            io::ErrorKind::Other,
                            "export: first form must evaluate to a symbol",
                        ));
                    }
                };
                let val = match &val.get().data {
                    ExpEnum::Atom(Atom::Symbol(s)) => {
                        ExpEnum::Atom(Atom::String((*s).into(), None))
                    }
                    ExpEnum::Atom(Atom::String(s, _)) => {
                        ExpEnum::Atom(Atom::String(s.to_string().into(), None))
                    }
                    ExpEnum::Atom(Atom::Int(i)) => {
                        ExpEnum::Atom(Atom::String(format!("{}", i).into(), None))
                    }
                    ExpEnum::Atom(Atom::Float(f)) => {
                        ExpEnum::Atom(Atom::String(format!("{}", f).into(), None))
                    }
                    ExpEnum::Process(ProcessState::Running(_pid)) => ExpEnum::Atom(Atom::String(
                        val.as_string(environment)
                            .unwrap_or_else(|_| "PROCESS FAILED".to_string())
                            .into(),
                        None,
                    )),
                    ExpEnum::Process(ProcessState::Over(_pid, _exit_status)) => {
                        ExpEnum::Atom(Atom::String(
                            val.as_string(environment)
                                .unwrap_or_else(|_| "PROCESS FAILED".to_string())
                                .into(),
                            None,
                        ))
                    }
                    ExpEnum::File(file) => match &*file.borrow() {
                        FileState::Stdin => ExpEnum::Atom(Atom::String(
                            val.as_string(environment)
                                .unwrap_or_else(|_| "STDIN FAILED".to_string())
                                .into(),
                            None,
                        )),
                        FileState::Read(_) => ExpEnum::Atom(Atom::String(
                            val.as_string(environment)
                                .unwrap_or_else(|_| "FILE READ FAILED".to_string())
                                .into(),
                            None,
                        )),
                        _ => {
                            return Err(io::Error::new(
                                io::ErrorKind::Other,
                                "export: value not valid",
                            ))
                        }
                    },
                    _ => {
                        return Err(io::Error::new(
                            io::ErrorKind::Other,
                            "export: value not valid",
                        ));
                    }
                };
                let val = Expression::alloc_data(val).as_string(environment)?;
                let val = match expand_tilde(&val) {
                    Some(v) => v,
                    None => val,
                };
                if !val.is_empty() {
                    env::set_var(key, val.clone());
                } else {
                    env::remove_var(key);
                }
                return Ok(Expression::alloc_data(ExpEnum::Atom(Atom::String(
                    val.into(),
                    None,
                ))));
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "export: can only have two expressions",
    ))
}

fn builtin_unexport(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(key) = args.next() {
        if args.next().is_none() {
            let key = eval(environment, key)?;
            let key_d = &key.get().data;
            if let ExpEnum::Atom(Atom::Symbol(k)) = key_d {
                env::remove_var(k);
                return Ok(Expression::alloc_data(ExpEnum::Nil));
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "unexport can only have one expression (symbol)",
    ))
}

fn builtin_jobs(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if args.next().is_some() {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "jobs takes no arguments",
        ))
    } else {
        for (i, job) in environment.jobs.borrow().iter().enumerate() {
            println!(
                "[{}]\t{}\t{:?}\t{:?}",
                i,
                job.status.to_string(),
                job.pids,
                job.names
            );
        }
        Ok(Expression::alloc_data(ExpEnum::Nil))
    }
}

fn get_stopped_pid(environment: &mut Environment, arg: Option<Expression>) -> Option<u32> {
    if let Some(arg) = arg {
        if let ExpEnum::Atom(Atom::Int(ji)) = &arg.get().data {
            let ji = *ji as usize;
            let jobs = &*environment.jobs.borrow();
            if ji < jobs.len() {
                let pid = jobs[ji].pids[0];
                let mut stop_idx: Option<u32> = None;
                for (i, sp) in environment.stopped_procs.borrow().iter().enumerate() {
                    if *sp == pid {
                        stop_idx = Some(i as u32);
                        break;
                    }
                }
                if let Some(idx) = stop_idx {
                    environment.stopped_procs.borrow_mut().remove(idx as usize);
                }
                Some(pid)
            } else {
                eprintln!("Error job id out of range.");
                None
            }
        } else {
            eprintln!("Error job id must be integer.");
            None
        }
    } else {
        environment.stopped_procs.borrow_mut().pop()
    }
}

fn builtin_bg(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    let arg = if let Some(arg) = args.next() {
        if args.next().is_some() {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "bg can only have one optional form (job id)",
            ));
        }
        Some(eval(environment, arg)?)
    } else {
        None
    };
    let opid = get_stopped_pid(environment, arg);
    if let Some(pid) = opid {
        let ppid = Pid::from_raw(pid as i32);
        if let Err(err) = signal::kill(ppid, Signal::SIGCONT) {
            eprintln!("Error sending sigcont to wake up process: {}.", err);
        } else {
            mark_job_running(environment, pid);
        }
    }
    Ok(Expression::alloc_data(ExpEnum::Nil))
}

fn builtin_fg(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    let arg = if let Some(arg) = args.next() {
        if args.next().is_some() {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "fg can only have one optional form (job id)",
            ));
        }
        Some(eval(environment, arg)?)
    } else {
        None
    };
    let opid = get_stopped_pid(environment, arg);
    if let Some(pid) = opid {
        let term_settings = termios::tcgetattr(nix::libc::STDIN_FILENO).unwrap();
        let ppid = Pid::from_raw(pid as i32);
        if let Err(err) = signal::kill(ppid, Signal::SIGCONT) {
            eprintln!("Error sending sigcont to wake up process: {}.", err);
        } else {
            if let Err(err) = unistd::tcsetpgrp(nix::libc::STDIN_FILENO, ppid) {
                let msg = format!("Error making {} foreground in parent: {}", pid, err);
                eprintln!("{}", msg);
            }
            mark_job_running(environment, pid);
            wait_pid(environment, pid, Some(&term_settings));
        }
    }
    Ok(Expression::alloc_data(ExpEnum::Nil))
}

fn builtin_run_bg(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    environment.run_background = true;
    let mut last_eval = Ok(Expression::alloc_data(ExpEnum::Nil));
    for a in args {
        last_eval = eval(environment, a);
        if let Err(err) = last_eval {
            environment.run_background = false;
            return Err(err);
        }
    }
    environment.run_background = false;
    last_eval
}

fn builtin_exit(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(exit_code) = args.next() {
        if args.next().is_none() {
            let exit_code = eval(environment, exit_code)?;
            return if let ExpEnum::Atom(Atom::Int(exit_code)) = &exit_code.get().data {
                environment.exit_code = Some(*exit_code as i32);
                Ok(Expression::alloc_data(ExpEnum::Nil))
            } else {
                Err(io::Error::new(
                    io::ErrorKind::Other,
                    "exit can only take an optional integer (exit code- defaults to 0)",
                ))
            };
        }
    } else {
        environment.exit_code = Some(0);
        return Ok(Expression::alloc_data(ExpEnum::Nil));
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "exit can only take an optional integer (exit code- defaults to 0)",
    ))
}

pub fn add_system_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, Reference, S>,
) {
    let root = interner.intern("root");
    data.insert(
        interner.intern("syscall"),
        Expression::make_function(
            builtin_syscall,
            "Usage: (syscall system-command arg0 ... argN)

Execute the provided system command with the supplied arguments.

Section: root

Example:
(def 'test-syscall-one (str (syscall \"echo\" -n \"syscall-test\")))
(test::assert-equal \"syscall-test\" test-syscall-one)
",
            root,
        ),
    );
    data.insert(
        interner.intern("export"),
        Expression::make_function(
            builtin_export,
            "Usage: (export symbol string) -> string

Export a key and value to the shell environment.  Second arg will be made a string and returned.

Section: shell

Example:
(test::assert-equal \"ONE\" (export 'TEST_EXPORT_ONE \"ONE\"))
(test::assert-equal \"ONE\" $TEST_EXPORT_ONE)
",
            root,
        ),
    );
    data.insert(
        interner.intern("unexport"),
        Expression::make_function(
            builtin_unexport,
            "Usage: (unexport symbol)

Remove a var from the current shell environment.

Section: shell

Example:
(test::assert-equal \"ONE\" (export 'TEST_EXPORT_ONE \"ONE\"))
(test::assert-equal \"ONE\" $TEST_EXPORT_ONE)
(unexport 'TEST_EXPORT_ONE)
(test::assert-false $TEST_EXPORT_ONE)
",
            root,
        ),
    );
    data.insert(
        interner.intern("jobs"),
        Expression::make_function(
            builtin_jobs,
            "Usage: (jobs)

Print list of jobs with ids.

Section: shell

Example:
;(jobs)
t
",
            root,
        ),
    );
    data.insert(
        interner.intern("bg"),
        Expression::make_function(
            builtin_bg,
            "Usage: (bg job-id?)

Put a job in the background.

If no job id is specified use the last job.

Section: shell

Example:
;(bg)
t
",
            root,
        ),
    );
    data.insert(
        interner.intern("fg"),
        Expression::make_function(
            builtin_fg,
            "Usage: (fg job-id?)

Put a job in the foreground.

If no job id is specified use the last job.

Section: shell

Example:
;(fg)
t
",
            root,
        ),
    );
    data.insert(
        interner.intern("run-bg"),
        Expression::make_special(
            builtin_run_bg,
            "Usage: (run-bg exp0 ... expN)

Like progn except any system commands started within form will be in the background.

Section: shell

Example:
;(run-bg gitk)
t
",
            root,
        ),
    );
    data.insert(
        interner.intern("exit"),
        Expression::make_function(
            builtin_exit,
            "Usage: (exit code?)

Exit shell with optional status code.

Section: shell

Example:
;(exit)
;(exit 0)
t
",
            root,
        ),
    );
}
