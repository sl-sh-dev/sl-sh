use nix::{
    sys::{
        signal::{self, Signal},
        termios,
    },
    unistd::{self, Pid},
};
use std::collections::HashMap;
use std::env;
use std::hash::BuildHasher;
use std::{thread, time};

use crate::builtins_util::*;
use crate::environment::*;
use crate::eval::*;
use crate::interner::*;
use crate::process::*;
use crate::types::*;

fn builtin_syscall(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(command) = args.next() {
        //let command = eval(environment, command)?;
        let command_d = command.get();
        match &command_d.data {
            ExpEnum::Symbol(s, _) => do_command(environment, s, args),
            ExpEnum::String(s, _) => do_command(environment, s, args),
            _ => {
                let msg = format!(
                    "syscall: first argument {} not a symbol or string, type {}",
                    command,
                    command.display_type()
                );
                Err(LispError::new(msg))
            }
        }
    } else {
        Err(LispError::new("syscall: empty call"))
    }
}

fn builtin_get_env(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    fn get_var(environment: &mut Environment, key: &str) -> Result<Expression, LispError> {
        if key.contains('=') || key.trim().is_empty() {
            Err(LispError::new(
                "get-env: invalid key, must not be empty or contain an '='",
            ))
        } else {
            match env::var(key) {
                Ok(val) => Ok(Expression::alloc_data(ExpEnum::String(
                    environment.interner.intern(&val).into(),
                    None,
                ))),
                Err(err) => Err(LispError::new(format!(
                    "get-env: error looking up {}: {}",
                    key, err
                ))),
            }
        }
    }
    if let Some(key) = args.next() {
        if args.next().is_none() {
            return match &key.get().data {
                ExpEnum::Symbol(s, _) => get_var(environment, s),
                ExpEnum::String(s, _) => get_var(environment, s),
                _ => Err(LispError::new("get-env: key must be a symbol or string")),
            };
        }
    }
    Err(LispError::new(
        "get-env: takes one parameter, environment variable to lookup",
    ))
}

fn builtin_export(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(key) = args.next() {
        if let Some(val) = args.next() {
            if args.next().is_none() {
                let key = eval(environment, key)?;
                let val = eval(environment, val)?;
                let key_d = &key.get().data;
                let key = match key_d {
                    ExpEnum::Symbol(s, _) => s,
                    _ => {
                        return Err(LispError::new(
                            "export: first form must evaluate to a symbol",
                        ));
                    }
                };
                let val = match &val.get().data {
                    ExpEnum::Symbol(s, _) => ExpEnum::String((*s).into(), None),
                    ExpEnum::String(s, _) => ExpEnum::String(s.to_string().into(), None),
                    ExpEnum::Int(i) => ExpEnum::String(format!("{}", i).into(), None),
                    ExpEnum::Float(f) => ExpEnum::String(format!("{}", f).into(), None),
                    ExpEnum::Process(ProcessState::Running(_pid)) => ExpEnum::String(
                        val.as_string(environment)
                            .unwrap_or_else(|_| "PROCESS FAILED".to_string())
                            .into(),
                        None,
                    ),
                    ExpEnum::Process(ProcessState::Over(_pid, _exit_status)) => ExpEnum::String(
                        val.as_string(environment)
                            .unwrap_or_else(|_| "PROCESS FAILED".to_string())
                            .into(),
                        None,
                    ),
                    ExpEnum::File(file) => match &*file.borrow() {
                        FileState::Stdin => ExpEnum::String(
                            val.as_string(environment)
                                .unwrap_or_else(|_| "STDIN FAILED".to_string())
                                .into(),
                            None,
                        ),
                        FileState::Read(_, _) => ExpEnum::String(
                            val.as_string(environment)
                                .unwrap_or_else(|_| "FILE READ FAILED".to_string())
                                .into(),
                            None,
                        ),
                        FileState::ReadBinary(_) => ExpEnum::String(
                            val.as_string(environment)
                                .unwrap_or_else(|_| "FILE READ FAILED".to_string())
                                .into(),
                            None,
                        ),
                        _ => return Err(LispError::new("export: value not valid")),
                    },
                    _ => {
                        return Err(LispError::new("export: value not valid"));
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
                return Ok(Expression::alloc_data(ExpEnum::String(val.into(), None)));
            }
        }
    }
    Err(LispError::new("export: can only have two expressions"))
}

fn builtin_unexport(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(key) = args.next() {
        if args.next().is_none() {
            let key = eval(environment, key)?;
            let key_d = &key.get().data;
            if let ExpEnum::Symbol(k, _) = key_d {
                env::remove_var(k);
                return Ok(Expression::alloc_data(ExpEnum::Nil));
            }
        }
    }
    Err(LispError::new(
        "unexport can only have one expression (symbol)",
    ))
}

fn builtin_jobs(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if args.next().is_some() {
        Err(LispError::new("jobs takes no arguments"))
    } else {
        // Update the list before printing.
        reap_procs(environment)?;
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
        if let ExpEnum::Int(ji) = &arg.get().data {
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
) -> Result<Expression, LispError> {
    let arg = if let Some(arg) = args.next() {
        if args.next().is_some() {
            return Err(LispError::new(
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
) -> Result<Expression, LispError> {
    let arg = if let Some(arg) = args.next() {
        if args.next().is_some() {
            return Err(LispError::new(
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
) -> Result<Expression, LispError> {
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

fn builtin_sleep(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(millis) = args.next() {
        if args.next().is_none() {
            if let ExpEnum::Int(millis) = eval(environment, millis)?.get().data {
                if millis > 0 {
                    let millis = time::Duration::from_millis(millis as u64);
                    thread::sleep(millis);
                    return Ok(Expression::make_nil());
                }
            }
            //let now = time::Instant::now();

            // assert!(now.elapsed() >= ten_millis)
        }
    }
    Err(LispError::new(
        "sleep: can only have one argument (milliseconds to sleep- positive integer)",
    ))
}

fn builtin_time(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(form) = args.next() {
        if args.next().is_none() {
            let now = time::Instant::now();
            eval(environment, form)?;
            return Ok(Expression::alloc_data(ExpEnum::Float(
                now.elapsed().as_secs_f64(),
            )));
        }
    }
    Err(LispError::new(
        "time: can only have one argument (form to time)",
    ))
}

fn builtin_exit(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(exit_code) = args.next() {
        if args.next().is_none() {
            let exit_code = eval(environment, exit_code)?;
            return if let ExpEnum::Int(exit_code) = &exit_code.get().data {
                environment.exit_code = Some(*exit_code as i32);
                Ok(Expression::alloc_data(ExpEnum::Nil))
            } else {
                Err(LispError::new(
                    "exit can only take an optional integer (exit code- defaults to 0)",
                ))
            };
        }
    } else {
        environment.exit_code = Some(0);
        return Ok(Expression::alloc_data(ExpEnum::Nil));
    }
    Err(LispError::new(
        "exit can only take an optional integer (exit code- defaults to 0)",
    ))
}

fn builtin_reap_jobs(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    params_done(args, "reap-jobs")?;
    reap_procs(environment)?;
    Ok(Expression::make_nil())
}

pub fn add_system_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, (Expression, String), S>,
) {
    data.insert(
        interner.intern("syscall"),
        Expression::make_special(
            builtin_syscall,
            r#"Usage: (syscall system-command arg0 ... argN)

Execute the provided system command with the supplied arguments.
System-command can be a string or symbol (it is not evaluated).
The args (0..n) are evaluated.

Section: core

Example:
(def test-syscall-one (str (syscall "echo" "-n" "syscall-test")))
(test::assert-equal "syscall-test" test-syscall-one)
(def test-syscall-one (str (syscall echo "-n" "syscall-test2")))
(test::assert-equal "syscall-test2" test-syscall-one)
"#,
        ),
    );
    data.insert(
        interner.intern("get-env"),
        Expression::make_function(
            builtin_get_env,
            r#"Usage: (get_env key) -> string

Export a key and value to the shell environment.  Second arg will be made a string and returned.

Section: shell

Example:
(test::assert-equal "ONE" (export 'TEST_EXPORT_ONE "ONE"))
(test::assert-equal "ONE" $TEST_EXPORT_ONE)
(test::assert-equal "ONE" (get-env TEST_EXPORT_ONE)
(test::assert-error  (get-env TEST_EXPORT_ONE_NA)
"#,
        ),
    );
    data.insert(
        interner.intern("export"),
        Expression::make_function(
            builtin_export,
            r#"Usage: (export symbol string) -> string

Export a key and value to the shell environment.  Second arg will be made a string and returned.

Section: shell

Example:
(test::assert-equal "ONE" (export 'TEST_EXPORT_ONE "ONE"))
(test::assert-equal "ONE" $TEST_EXPORT_ONE)
"#,
        ),
    );
    data.insert(
        interner.intern("unexport"),
        Expression::make_function(
            builtin_unexport,
            r#"Usage: (unexport symbol)

Remove a var from the current shell environment.

Section: shell

Example:
(test::assert-equal "ONE" (export 'TEST_EXPORT_ONE "ONE"))
(test::assert-equal "ONE" $TEST_EXPORT_ONE)
(unexport 'TEST_EXPORT_ONE)
(test::assert-error $TEST_EXPORT_ONE)
"#,
        ),
    );
    data.insert(
        interner.intern("jobs"),
        Expression::make_function(
            builtin_jobs,
            r#"Usage: (jobs)

Print list of jobs with ids.

Section: shell

Example:
;(jobs)
t
"#,
        ),
    );
    data.insert(
        interner.intern("bg"),
        Expression::make_function(
            builtin_bg,
            r#"Usage: (bg job-id?)

Put a job in the background.

If no job id is specified use the last job.

Section: shell

Example:
;(bg)
t
"#,
        ),
    );
    data.insert(
        interner.intern("fg"),
        Expression::make_function(
            builtin_fg,
            r#"Usage: (fg job-id?)

Put a job in the foreground.

If no job id is specified use the last job.

Section: shell

Example:
;(fg)
t
"#,
        ),
    );
    data.insert(
        interner.intern("run-bg"),
        Expression::make_special(
            builtin_run_bg,
            r#"Usage: (run-bg exp0 ... expN)

Like do except any system commands started within form will be in the background.

Section: shell

Example:
;(run-bg gitk)
t
"#,
        ),
    );
    data.insert(
        interner.intern("exit"),
        Expression::make_function(
            builtin_exit,
            r#"Usage: (exit code?)

Exit shell with optional status code.

Section: shell

Example:
;(exit)
;(exit 0)
t
"#,
        ),
    );
    data.insert(
        interner.intern("sleep"),
        Expression::make_function(
            builtin_sleep,
            r#"Usage: (sleep milliseconds) -> nil

Sleep for the provided milliseconds (must be a positive integer).

Section: shell

Example:
(def test-sleep-var (time (sleep 1000)))
(assert-true (> test-sleep-var 1.0))
"#,
        ),
    );
    data.insert(
        interner.intern("time"),
        Expression::make_function(
            builtin_time,
            r#"Usage: (time form) -> eval-time

Evalutes the provided form and returns the seconds it ran for (as float with fractional part).

Section: shell

Example:
(def test-sleep-var (time (sleep 1100)))
(assert-true (> test-sleep-var 1.1))
"#,
        ),
    );
    data.insert(
        interner.intern("reap-jobs"),
        Expression::make_function(
            builtin_reap_jobs,
            r#"Usage: (reap-jobs) -> nil

Reaps any completed jobs.  Only intended to be used by code implemeting the REPL
loop or something similiar, this is probably not the form you are searching for.

Section: shell

Example:
;(reap-jobs)
t
"#,
        ),
    );
}
