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
use std::io::{self, BufReader, Read, Write};
use std::{thread, time};

use crate::builtins_util::*;
use crate::environment::*;
use crate::eval::*;
use crate::interner::*;
use crate::process::*;
use crate::types::*;
use crate::unix::*;
use std::time::SystemTime;

fn builtin_syscall(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(command) = args.next() {
        let command = eval(environment, command)?;
        let command_d = command.get();
        match &command_d.data {
            ExpEnum::Symbol(s, _) => do_command(environment, s, args),
            ExpEnum::String(s, _) => do_command(environment, s, args),
            _ => {
                let msg = format!(
                    "syscall: first argument {} does not eval to a symbol or string, type {}",
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
                Err(_err) => Ok(Expression::alloc_data(ExpEnum::String(
                    environment.interner.intern("").into(),
                    None,
                ))),
            }
        }
    }
    if let Some(key) = args.next() {
        if args.next().is_none() {
            return match &key.get().data {
                ExpEnum::Symbol(s, _) => get_var(environment, s),
                ExpEnum::String(s, _) => get_var(environment, s),
                _ => Err(LispError::new(format!(
                    "get-env: key must be a symbol or string, got {}/{}",
                    key.display_type(),
                    key
                ))),
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
    let key = param_eval(environment, args, "export")?;
    let val = param_eval(environment, args, "export")?;
    params_done(args, "export")?;
    let key_d = &key.get().data;
    let key = match key_d {
        ExpEnum::Symbol(s, _) => s,
        ExpEnum::String(s, _) => s.as_ref(),
        _ => {
            return Err(LispError::new(
                "export: first form must evaluate to a symbol or string",
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
    if key.contains('=') {
        Err(LispError::new("export: key can not contain '='"))
    } else {
        if !val.is_empty() {
            env::set_var(key, val.clone());
        } else {
            env::remove_var(key);
        }
        Ok(Expression::alloc_data(ExpEnum::String(val.into(), None)))
    }
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

fn builtin_epoch(
    _environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if args.next().is_some() {
        Err(LispError::new("epoch takes no arguments"))
    } else {
        match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
            Ok(elapsed) => Ok(Expression::alloc_data(ExpEnum::Int(
                elapsed.as_millis() as i64
            ))),
            Err(err) => Err(LispError::new(format!("epoch failed {}", err))),
        }
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

fn builtin_fork(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(exp) = args.next() {
        if args.next().is_none() {
            let pid = fork(environment, exp, None, None, None)?;
            let res_proc = Expression::alloc_data(ExpEnum::Process(ProcessState::Running(pid)));
            add_process(environment, pid, (res_proc.clone(), None));
            return Ok(res_proc);
        }
    }
    Err(LispError::new(
        "fork: requires one form to exectute in the background",
    ))
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

fn pipe_write_file(pipe_in: i32, writer: &mut dyn Write) -> Result<(), LispError> {
    let mut inf = BufReader::new(fd_to_file(pipe_in));
    let mut buf = [0; 10240];
    let mut n = inf.read(&mut buf[..])?;
    while n > 0 {
        writer.write_all(&buf[..n])?;
        n = inf.read(&mut buf[..])?;
    }
    Ok(())
}

fn builtin_pipe(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    pub struct GrabStdIn {
        pub old_stdin: Option<i32>,
    }

    pub fn grab_stdin(new_stdin: Option<i32>) -> Result<GrabStdIn, LispError> {
        let old_stdin = if let Some(new_stdin) = new_stdin {
            Some(replace_stdin(new_stdin)?)
        } else {
            None
        };
        Ok(GrabStdIn { old_stdin })
    }

    impl Drop for GrabStdIn {
        fn drop(&mut self) {
            if let Some(old_stdin) = self.old_stdin {
                if let Err(err) = dup_stdin(old_stdin) {
                    eprintln!("Error restoring stdin after pipe: {}", err);
                }
            }
        }
    }

    let mut pipe = args.next();
    let mut do_error = false;
    if let Some(p) = &pipe {
        if let ExpEnum::Symbol(":err", _) = &p.get().data {
            do_error = true;
        }
    }
    if do_error {
        pipe = args.next();
    }
    let mut last_pid: Option<u32> = None;
    let mut read = None;
    let mut write;
    let mut next_read;
    let mut res = Ok(Expression::make_nil());
    let mut procs = Vec::new();
    let gpo = set_grab_proc_output(environment, false);
    while let Some(p) = pipe {
        let next_pipe = args.next();
        if next_pipe.is_none() {
            // Last thing in the pipe so do not run in background.
            let _old_stdin = grab_stdin(read)?; // RAII guard for stdin
            gpo.environment.grab_proc_output = gpo.old_grab_proc_output;
            res = eval(gpo.environment, p);
            // If pipe ended in a file then dump final output into it.
            match &res {
                Ok(res_in) => {
                    let res_d = res_in.get();
                    if let ExpEnum::File(file) = &res_d.data {
                        let mut file_b = file.borrow_mut();
                        match &mut *file_b {
                            FileState::Stdout => {
                                let stdout = io::stdout();
                                let mut handle = stdout.lock();
                                pipe_write_file(0, &mut handle)?;
                            }
                            FileState::Stderr => {
                                let stderr = io::stderr();
                                let mut handle = stderr.lock();
                                pipe_write_file(0, &mut handle)?;
                            }
                            FileState::Write(f) => {
                                pipe_write_file(0, f)?;
                            }
                            _ => {
                                drop(file_b);
                                drop(res_d);
                                res = Err(LispError::new("File at pipe end must be writable."));
                            }
                        }
                    }
                }
                Err(_err) => {}
            }
        } else {
            let (read_fd, write_fd) = anon_pipe()?;
            write = Some(write_fd);
            next_read = Some(read_fd);
            let error = if do_error { write } else { None };
            let pid = fork(gpo.environment, p, read, write, error)?;
            last_pid = Some(pid);
            let res_proc = Expression::alloc_data(ExpEnum::Process(ProcessState::Running(pid)));
            procs.push(res_proc.clone());
            add_process(gpo.environment, pid, (res_proc, None));
            if gpo.environment.pipe_pgid.is_none() {
                gpo.environment.pipe_pgid = last_pid;
            }
            read = next_read;
        }
        pipe = next_pipe;
    }
    gpo.environment.pipe_pgid = None;
    if let Ok(res) = res {
        procs.insert(0, res);
        Ok(Expression::alloc_data(ExpEnum::Values(procs)))
    } else {
        if let Some(pid) = last_pid {
            // Send a sigint to the feeding job so it does not hang on a full output buffer.
            if let Err(err) = nix::sys::signal::kill(
                nix::unistd::Pid::from_raw(pid as i32),
                nix::sys::signal::Signal::SIGINT,
            ) {
                eprintln!("ERROR, sending SIGINT to pid {}: {}", pid, err);
            }
        }
        res
    }
}

fn builtin_wait(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg0) = args.next() {
        if args.next().is_none() {
            let arg0 = eval(environment, arg0)?;
            let arg0_d = arg0.get();
            return match &arg0_d.data {
                ExpEnum::Process(ProcessState::Running(pid)) => {
                    let pid = *pid;
                    drop(arg0_d);
                    match wait_pid(environment, pid, None) {
                        Some(exit_status) => {
                            Ok(Expression::alloc_data(ExpEnum::Int(i64::from(exit_status))))
                        }
                        None => Ok(Expression::make_nil()),
                    }
                }
                ExpEnum::Process(ProcessState::Over(_pid, exit_status)) => Ok(
                    Expression::alloc_data(ExpEnum::Int(i64::from(*exit_status))),
                ),
                ExpEnum::Int(pid) => {
                    let pid = *pid;
                    drop(arg0_d);
                    match wait_pid(environment, pid as u32, None) {
                        Some(exit_status) => {
                            Ok(Expression::alloc_data(ExpEnum::Int(i64::from(exit_status))))
                        }
                        None => Ok(Expression::make_nil()),
                    }
                }
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

pub fn add_system_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, (Expression, String), S>,
) {
    data.insert(
        interner.intern("syscall"),
        Expression::make_function(
            builtin_syscall,
            r#"Usage: (syscall system-command arg0 ... argN)

Execute the provided system command with the supplied arguments.
System-command can evalute to a string or symbol.
The args (0..n) are evaluated.

Section: system

Example:
(def test-syscall-one (str (syscall "echo" "-n" "syscall-test")))
(test::assert-equal "syscall-test" test-syscall-one)
(def test-syscall-one (str (syscall 'echo "-n" "syscall-test2")))
(test::assert-equal "syscall-test2" test-syscall-one)
(def test-syscall-echo "echo")
(def test-syscall-one (str (syscall test-syscall-echo "-n" "syscall-test3")))
(test::assert-equal "syscall-test3" test-syscall-one)
"#,
        ),
    );
    data.insert(
        interner.intern("get-env"),
        Expression::make_special(
            builtin_get_env,
            r#"Usage: (get_env key) -> string

Lookup key in the system environment (env variable).  Returns an empty sting if key does not exist.
Note: key is not evaluated.

Section: system

Example:
(test::assert-equal "ONE" (export 'TEST_EXPORT_ONE "ONE"))
(test::assert-equal "ONE" $TEST_EXPORT_ONE))
(test::assert-equal "ONE" (get-env TEST_EXPORT_ONE))
(test::assert-equal "" (get-env TEST_EXPORT_ONE_NA))
"#,
        ),
    );
    data.insert(
        interner.intern("export"),
        Expression::make_function(
            builtin_export,
            r#"Usage: (export symbol string) -> string

Export a key and value to the shell environment.  Second arg will be made a string and returned.
Key can not contain the '=' character.

Section: system

Example:
(test::assert-equal "ONE" (export 'TEST_EXPORT_ONE "ONE"))
(test::assert-equal "ONE" $TEST_EXPORT_ONE))
(test::assert-equal "ONE1" (export 'TEST_EXPORT_ONE ONE1))
(test::assert-equal "ONE1" $TEST_EXPORT_ONE))
(test::assert-equal "TWO" (export "TEST_EXPORT_TWO" "TWO"))
(test::assert-equal "TWO" $TEST_EXPORT_TWO))
(test::assert-equal "THREE" $(export TEST_EXPORT_THREE THREE))
(test::assert-equal "THREE" $TEST_EXPORT_THREE))
(test::assert-error (export '=TEST_EXPORT_THREE "THREE"))
(test::assert-error (export 'TEST=EXPORT_THREE "THREE"))
(test::assert-error (export 'TEST_EXPORT_THREE= "THREE"))
(test::assert-error $(export TEST_EXPORT_THREE= THREE))
"#,
        ),
    );
    data.insert(
        interner.intern("unexport"),
        Expression::make_function(
            builtin_unexport,
            r#"Usage: (unexport symbol)

Remove a var from the current shell environment.

Section: system

Example:
(test::assert-equal "ONE" (export 'TEST_EXPORT_ONE "ONE"))
(test::assert-equal "ONE" $TEST_EXPORT_ONE))
(unexport 'TEST_EXPORT_ONE)
(test::assert-equal "" $TEST_EXPORT_ONE))
"#,
        ),
    );
    data.insert(
        interner.intern("epoch"),
        Expression::make_function(
            builtin_epoch,
            r#"Usage: (epoch)

Prints system time in milliseconds.

Section: shell

Example:
;(epoch)
#t
"#,
        ),
    );
    data.insert(
        interner.intern("jobs"),
        Expression::make_function(
            builtin_jobs,
            r#"Usage: (jobs)

Print list of jobs with ids.

Section: system

Example:
;(jobs)
#t
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

Section: system

Example:
;(bg)
#t
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

Section: system

Example:
;(fg)
#t
"#,
        ),
    );
    data.insert(
        interner.intern("fork"),
        Expression::make_special(
            builtin_fork,
            r#"Usage: (fork exp) -> process

Forks the provided expression in the background as a job and returns the process
object.  If the expression that is forked returns an integer (that fits an i32)
then it will become the exit code.  Calling exit explicitly will also set the
exit code.  Otherwise exit code is 0 for success and 1 for an error.

Section: system

Example:
(def fork-test (fork (+ (* 11 5) 2)))
(test::assert-equal 57 (wait fork-test))
(def fork-time (time (wait (fork (sleep 1000)))))
(test::assert-true (> fork-time 1.0))
"#,
        ),
    );
    data.insert(
        interner.intern("exit"),
        Expression::make_function(
            builtin_exit,
            r#"Usage: (exit code?)

Exit shell with optional status code.

Section: system

Example:
; Exit is overridden in the test harness...
;(test::assert-equal 10 (wait (fork (exit 10))))
;(test::assert-equal 11 (wait (fork (exit 11))))
;(test::assert-equal 12 (wait (fork (exit 12))))
#t
"#,
        ),
    );
    data.insert(
        interner.intern("sleep"),
        Expression::make_function(
            builtin_sleep,
            r#"Usage: (sleep milliseconds) -> nil

Sleep for the provided milliseconds (must be a positive integer).

Section: system

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

Section: system

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

Section: system

Example:
;(reap-jobs)
#t
"#,
        ),
    );
    data.insert(
        interner.intern("pipe"),
        Expression::make_function(
            builtin_pipe,
            r#"Usage: (pipe [expression]+)

Setup a pipe between processes or expressions.  Pipe will take one or more
expressions, each one but the last will be forked into a new process with it's
stdin being the output of the last expression.  The first expression uses the
current stdin and the last expression outputs to the current stdout.  Pipe works
with system commands as well as sl-sh forms (lambdas, etc).  Note it connects
the stdin/stdout of processes so if used with a lambda it should read stdin to
get the previous output and write to stdout to pass to the next expression in
the pipe (i.e. pipe will not interact with parameters or anything else).

If pipe starts with :err then stderr will also be piped into the output,
ie (pipe :err (...)(...)...).

Pipes also support using a read file as the first expression (the file contents
become stdin for the next form) and a write file as the last expression
(previous output will be written to the file).  For instance pipe can be used
to copy a file with (pipe (open IN_FILE :read)(open OUT_FILE :create)), note
this example does not close the files.

Pipes can be nested including piping through a lambda that itself uses pipes.

Pipe will return a multiple values, the first/primary is the final form for the
pipe and the process objects for each part of the pipe are next (first element
can be found with (values-nth 1 return-val), etc).

Section: system

Example:
(def pipe-test (str (pipe (print "one
two
three")(syscall 'grep "two"))))
(test::assert-equal "two
" pipe-test)
(def pipe-test (str (pipe (pipe (syscall 'echo "one
two
twotwo
three")(syscall 'grep "two"))(syscall 'grep "twotwo"))))
(test::assert-equal "twotwo
" pipe-test)
(def pipe-test-dir (str (temp-dir)"/tst-pipe-dir"))
$(mkdir $pipe-test-dir)
(def tsync (open "${pipe-test-dir}/test1" :create))
(pipe (print "one
two
two2
three") (syscall 'grep "two") tsync)
(close tsync)
(def topen (open "${pipe-test-dir}/test1" :read))
(test::assert-equal "two
" (read-line topen))
(test::assert-equal "two2
" (read-line topen))
(test::assert-false (read-line topen))
(close topen)
(def topen (open "${pipe-test-dir}/test1" :read))
(def pipe-test (str (pipe topen (syscall 'grep "two2"))))
(close topen)
(test::assert-equal "two2
" pipe-test)
$(rm "${pipe-test-dir}/test1")
(let ((file-name "${pipe-test-dir}/pipe-err.test")
      (fin))
  (err> (open "${pipe-test-dir}/pipe-test.junk" :create) (do
    (pipe (eprintln "error")(do (print *stdin*)(println "normal"))(open file-name :create :truncate))
    (set! fin (open file-name :read))
    (test::assert-equal "normal\n" (read-line fin))
    (test::assert-equal nil (read-line fin))
    (close fin)
    (pipe :err (eprintln "error")(do (print *stdin*)(println "normal"))(open file-name :create :truncate))
    (set! fin (open file-name :read))
    (test::assert-equal "error\n" (read-line fin))
    (test::assert-equal "normal\n" (read-line fin))
    (close fin)))
  $(rm "${pipe-test-dir}/pipe-test.junk")
  $(rm $file-name)
)
$(rmdir $pipe-test-dir)
"#,
        ),
    );
    data.insert(
        interner.intern("wait"),
        Expression::make_function(
            builtin_wait,
            r#"Usage: (wait proc-to-wait-for)

Wait for a process to end and return it's exit status.
Wait can be called multiple times if it is given a process
object (not just a numeric pid).

Section: system

Example:
(def wait-test (wait (err>null (syscall 'ls "/does/not/exist/123"))))
(test::assert-true (> wait-test 0))
(def wait-test2 (fork (* 11 5)))
(test::assert-equal 55 (wait wait-test2))
(test::assert-equal 55 (wait wait-test2))
(test::assert-equal 55 (wait wait-test2))
"#,
        ),
    );
    data.insert(
        interner.intern("pid"),
        Expression::make_function(
            builtin_pid,
            r#"Usage: (pid proc)

Return the pid of a process.

Section: system

Example:
(def pid-test (syscall 'echo "-n"))
(test::assert-true (int? (pid pid-test)))
(test::assert-true (int? (pid (fork ((fn () nil))))))
(test::assert-error (pid 1))
"#,
        ),
    );
}
