use std::env;
use std::io::{self, Write};
use std::os::unix::io::{AsRawFd, FromRawFd};
use std::os::unix::process::CommandExt;
use std::process::{ChildStdin, ChildStdout, Command, Stdio};
use std::sync::atomic::Ordering;

use glob::glob;
use nix::{
    sys::{
        signal::{self, kill, SigHandler, Signal},
        termios,
        wait::{self, WaitPidFlag, WaitStatus},
    },
    unistd::{self, Pid},
};

use crate::builtins_util::*;
use crate::environment::*;
use crate::eval::*;
use crate::types::*;

pub fn try_wait_pid(environment: &Environment, pid: u32) -> (bool, Option<i32>) {
    let mut opts = WaitPidFlag::WUNTRACED;
    opts.insert(WaitPidFlag::WCONTINUED);
    opts.insert(WaitPidFlag::WNOHANG);
    match wait::waitpid(Pid::from_raw(pid as i32), Some(opts)) {
        Err(nix::Error::Sys(nix::errno::Errno::ECHILD)) => {
            // Does not exist.
            environment.procs.borrow_mut().remove(&pid);
            remove_job(environment, pid);
            (true, None)
        }
        Err(err) => {
            eprintln!("Error waiting for pid {}, {}", pid, err);
            environment.procs.borrow_mut().remove(&pid);
            remove_job(environment, pid);
            (true, None)
        }
        Ok(WaitStatus::Exited(_, status)) => {
            environment.procs.borrow_mut().remove(&pid);
            remove_job(environment, pid);
            (true, Some(status))
        }
        Ok(WaitStatus::Stopped(..)) => {
            environment.stopped_procs.borrow_mut().push(pid);
            mark_job_stopped(environment, pid);
            (true, None)
        }
        Ok(WaitStatus::Continued(_)) => (false, None),
        Ok(_) => (false, None),
    }
}

pub fn wait_pid(
    environment: &Environment,
    pid: u32,
    term_settings: Option<&termios::Termios>,
) -> Option<i32> {
    let result: Option<i32>;
    let mut int_cnt = 0;
    loop {
        if environment.sig_int.load(Ordering::Relaxed) {
            if int_cnt == 0 {
                if let Err(err) = kill(Pid::from_raw(pid as i32), Signal::SIGINT) {
                    eprintln!("ERROR sending SIGINT to child process {}, {}", pid, err);
                }
            } else if int_cnt == 1 {
                if let Err(err) = kill(Pid::from_raw(pid as i32), Signal::SIGTERM) {
                    eprintln!("ERROR sending SIGTERM to child process {}, {}", pid, err);
                }
            } else if let Err(err) = kill(Pid::from_raw(pid as i32), Signal::SIGKILL) {
                eprintln!("ERROR sending SIGKILL to child process {}, {}", pid, err);
            }
            int_cnt += 1;
            environment.sig_int.store(false, Ordering::Relaxed);
        }
        let (stop, status) = try_wait_pid(environment, pid);
        if stop {
            result = status;
            if let Some(status) = status {
                if environment.save_exit_status {
                    env::set_var("LAST_STATUS".to_string(), format!("{}", status));
                    environment.root_scope.borrow_mut().insert_exp(
                        "*last-status*".to_string(),
                        Expression::Atom(Atom::Int(i64::from(status))),
                    );
                }
            }
            break;
        }
        std::thread::sleep(std::time::Duration::from_millis(100));
    }
    // If we were given terminal settings restore them.
    if let Some(settings) = term_settings {
        if let Err(err) =
            termios::tcsetattr(nix::libc::STDIN_FILENO, termios::SetArg::TCSANOW, settings)
        {
            eprintln!("Error resetting shell terminal settings: {}", err);
        }
    }
    // Move the shell back into the foreground.
    if environment.is_tty {
        let pid = unistd::getpid();
        if let Err(err) = unistd::tcsetpgrp(nix::libc::STDIN_FILENO, pid) {
            eprintln!("Error making shell {} foreground: {}", pid, err);
        }
    }
    result
}

fn run_command(
    environment: &mut Environment,
    command: &str,
    args: &mut Vec<Expression>,
    stdin: Stdio,
    stdout: Stdio,
    stderr: Stdio,
    data_in: Option<Atom>,
) -> io::Result<Expression> {
    let mut new_args: Vec<String> = Vec::new();
    for a in args {
        new_args.push(a.as_string(environment)?);
    }
    let mut command = command;
    let ncommand;
    if command.starts_with('~') {
        if let Some(c) = expand_tilde(command) {
            ncommand = c;
            command = &ncommand;
        }
    }
    let mut com_obj = Command::new(command);
    let foreground =
        !environment.in_pipe && !environment.run_background && !environment.state.is_spawn;
    let shell_terminal = nix::libc::STDIN_FILENO;
    com_obj
        .args(&new_args)
        .stdin(stdin)
        .stdout(stdout)
        .stderr(stderr);
    let pgid = environment.state.pipe_pgid;
    let do_job_control = environment.do_job_control;

    unsafe {
        com_obj.pre_exec(move || -> io::Result<()> {
            if do_job_control {
                let pid = unistd::getpid();
                let pgid = match pgid {
                    Some(pgid) => Pid::from_raw(pgid as i32),
                    None => unistd::getpid(),
                };
                if let Err(_err) = unistd::setpgid(pid, pgid) {
                    // Ignore, do in parent and child.
                    //let msg = format!("Error setting pgid for {}: {}", pid, err);
                }
                if foreground {
                    if let Err(_err) = unistd::tcsetpgrp(nix::libc::STDIN_FILENO, pgid) {
                        // Ignore, do in parent and child.
                        //let msg = format!("Error making {} foreground: {}", pid, err);
                    }
                }
            }

            // XXX TODO, do better with these unwraps.
            // Set the handling for job control signals back to the default.
            signal::signal(Signal::SIGINT, SigHandler::SigDfl).unwrap();
            signal::signal(Signal::SIGHUP, SigHandler::SigDfl).unwrap();
            signal::signal(Signal::SIGTERM, SigHandler::SigDfl).unwrap();
            signal::signal(Signal::SIGQUIT, SigHandler::SigDfl).unwrap();
            signal::signal(Signal::SIGTSTP, SigHandler::SigDfl).unwrap();
            signal::signal(Signal::SIGTTIN, SigHandler::SigDfl).unwrap();
            signal::signal(Signal::SIGTTOU, SigHandler::SigDfl).unwrap();
            signal::signal(Signal::SIGCHLD, SigHandler::SigDfl).unwrap();

            Ok(())
        });
    }

    let term_settings = if environment.is_tty && environment.do_job_control {
        Some(termios::tcgetattr(shell_terminal).unwrap())
    } else {
        None
    };
    let proc = com_obj.spawn();

    match proc {
        Ok(mut proc) => {
            let pgid_raw = match pgid {
                Some(pgid) => Pid::from_raw(pgid as i32),
                None => Pid::from_raw(proc.id() as i32),
            };
            if environment.do_job_control {
                let pid = Pid::from_raw(proc.id() as i32);
                if pgid.is_none() {
                    let mut job = Job {
                        pids: Vec::new(),
                        names: Vec::new(),
                        status: JobStatus::Running,
                    };
                    job.pids.push(proc.id());
                    job.names.push(command.to_string());
                    environment.jobs.borrow_mut().push(job);
                } else {
                    let job = environment.jobs.borrow_mut().pop();
                    if let Some(mut job) = job {
                        job.pids.push(proc.id());
                        job.names.push(command.to_string());
                        environment.jobs.borrow_mut().push(job);
                    } else {
                        eprintln!("WARNING: Soemthing in pipe is amiss, probably a command not part of pipe or a bug!");
                    }
                }
                if let Err(_err) = unistd::setpgid(pid, pgid_raw) {
                    // Ignore, do in parent and child.
                }
            }
            if let Some(data_in) = data_in {
                if proc.stdin.is_some() {
                    let mut input: Option<ChildStdin> = None;
                    std::mem::swap(&mut proc.stdin, &mut input);
                    let mut input = input.unwrap();
                    input.write_all(data_in.to_string().as_bytes())?;
                }
            }
            let pid = proc.id();
            let result = if foreground && !environment.in_pipe {
                if environment.do_job_control {
                    if let Err(_err) = unistd::tcsetpgrp(shell_terminal, pgid_raw) {
                        // Ignore, do in parent and child.
                    }
                }
                let status = if let Some(term_settings) = term_settings {
                    wait_pid(environment, proc.id(), Some(&term_settings))
                } else {
                    wait_pid(environment, proc.id(), None)
                };
                match status {
                    Some(code) => Expression::Process(ProcessState::Over(pid, code as i32)),
                    None => Expression::nil(),
                }
            } else {
                Expression::Process(ProcessState::Running(pid))
            };
            add_process(environment, proc);
            Ok(result)
        }
        Err(e) => {
            let mut err_msg = String::new();
            err_msg.push_str(&format!("Failed to execute [{}", command));
            for n in new_args {
                err_msg.push_str(&format!(" {}", n));
            }
            err_msg.push_str(&format!("]: {}", e));
            // Recover from the failed spawn...
            // If we were saved terminal settings restore them.
            if let Some(settings) = term_settings {
                if let Err(err) =
                    termios::tcsetattr(nix::libc::STDIN_FILENO, termios::SetArg::TCSANOW, &settings)
                {
                    eprintln!("Error resetting shell terminal settings: {}", err);
                }
            }
            // Move the shell back into the foreground.
            if environment.is_tty {
                let pid = unistd::getpid();
                if let Err(err) = unistd::tcsetpgrp(nix::libc::STDIN_FILENO, pid) {
                    eprintln!("Error making shell {} foreground: {}", pid, err);
                }
            }
            Err(io::Error::new(io::ErrorKind::Other, err_msg))
        }
    }
}

fn get_std_io(environment: &Environment, is_out: bool) -> io::Result<Stdio> {
    let key = if is_out { "*stdout*" } else { "*stderr*" };
    let out = get_expression(environment, key);
    match out {
        Some(out) => {
            if let Expression::File(f) = &out.exp {
                match &*f.borrow() {
                    FileState::Stdout => {
                        if is_out {
                            Ok(Stdio::inherit())
                        } else {
                            // If ever Windows need raw hangle not fd.
                            unsafe { Ok(Stdio::from_raw_fd(io::stdout().as_raw_fd())) }
                        }
                    }
                    FileState::Stderr => {
                        if !is_out {
                            Ok(Stdio::inherit())
                        } else {
                            // If ever Windows need raw hangle not fd.
                            unsafe { Ok(Stdio::from_raw_fd(io::stderr().as_raw_fd())) }
                        }
                    }
                    FileState::Write(f) => Ok(Stdio::from(f.borrow().get_ref().try_clone()?)),
                    _ => Err(io::Error::new(
                        io::ErrorKind::Other,
                        "Can not write to a non-writable file.",
                    )),
                }
            } else {
                Err(io::Error::new(
                    io::ErrorKind::Other,
                    "Can not write to something not a file.",
                ))
            }
        }
        None => Ok(Stdio::inherit()),
    }
}

fn get_output(
    environment: &Environment,
    out_status: &Option<IOState>,
    err_status: &Option<IOState>,
) -> io::Result<(Stdio, Stdio)> {
    let out_res = match out_status {
        Some(IOState::Null) => Stdio::null(),
        Some(IOState::Inherit) => get_std_io(environment, true)?,
        Some(IOState::Pipe) => Stdio::piped(),
        None => get_std_io(environment, true)?,
    };
    let err_res = match err_status {
        Some(IOState::Null) => Stdio::null(),
        Some(IOState::Inherit) => get_std_io(environment, false)?,
        Some(IOState::Pipe) => Stdio::piped(),
        None => get_std_io(environment, false)?,
    };
    Ok((out_res, err_res))
}

pub fn prep_string_arg(s: &str, nargs: &mut Vec<Expression>) -> io::Result<()> {
    let s = match expand_tilde(&s) {
        Some(p) => p,
        None => s.to_string(), // XXX not great.
    };
    if s.contains('*') || s.contains('?') || s.contains('[') || s.contains('{') {
        match glob(&s) {
            Ok(paths) => {
                let mut i = 0;
                for p in paths {
                    match p {
                        Ok(p) => {
                            i += 1;
                            if let Some(p) = p.to_str() {
                                nargs.push(Expression::Atom(Atom::String(p.to_string())));
                            }
                        }
                        Err(err) => {
                            let msg = format!("glob error on while iterating {}, {}", s, err);
                            return Err(io::Error::new(io::ErrorKind::Other, msg));
                        }
                    }
                }
                if i == 0 {
                    nargs.push(Expression::Atom(Atom::String(s)));
                }
            }
            Err(_err) => {
                nargs.push(Expression::Atom(Atom::String(s)));
            }
        }
    } else {
        nargs.push(Expression::Atom(Atom::String(s)));
    }
    Ok(())
}

pub fn do_command<'a>(
    environment: &mut Environment,
    command: &str,
    parts: Box<dyn Iterator<Item = &Expression> + 'a>,
) -> io::Result<Expression> {
    let mut data = None;
    let foreground =
        !environment.in_pipe && !environment.run_background && !environment.state.is_spawn;
    if let Some(Expression::LazyFn(_, _)) = &environment.data_in {
        environment.data_in = Some(environment.data_in.clone().unwrap().resolve(environment)?);
    }
    let stdin = match &environment.data_in {
        Some(Expression::Atom(atom)) => {
            data = Some(atom.clone());
            Stdio::piped()
        }
        Some(Expression::Process(ProcessState::Running(pid))) => {
            let procs = environment.procs.clone();
            let mut procs = procs.borrow_mut();
            if let Some(proc) = procs.get_mut(&pid) {
                if proc.stdout.is_some() {
                    let mut out: Option<ChildStdout> = None;
                    std::mem::swap(&mut proc.stdout, &mut out);
                    Stdio::from(out.unwrap())
                } else if foreground {
                    Stdio::inherit()
                } else {
                    Stdio::null()
                }
            } else if foreground {
                Stdio::inherit()
            } else {
                Stdio::null()
            }
        }
        Some(Expression::Process(ProcessState::Over(_pid, _exit_status))) => {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "Invalid expression state before command (process is already done).",
            ))
        }
        Some(Expression::Function(_)) => {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "Invalid expression state before command (function).",
            ))
        }
        Some(Expression::Vector(_)) => {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "Invalid expression state before command (list).",
            ))
        }
        Some(Expression::Pair(p)) => {
            if let Some((_, _)) = &*p.borrow() {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "Invalid expression state before command (pair).",
                ));
            } else {
                // Nil
                Stdio::inherit()
            }
        }
        Some(Expression::HashMap(_)) => {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "Invalid expression state before command (hashmap).",
            ))
        }
        Some(Expression::File(file)) => match &*file.borrow() {
            FileState::Stdin => Stdio::inherit(),
            FileState::Read(file) => {
                // If there is ever a Windows version then use raw_handle instead of raw_fd.
                unsafe { Stdio::from_raw_fd(file.borrow().get_ref().as_raw_fd()) }
            }
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "Invalid expression state before command (not a readable file).",
                ))
            }
        },
        Some(Expression::LazyFn(_, _)) => {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "Invalid expression state before command (lazyfn- this should be impossible...).",
            ));
        }
        None => {
            if foreground {
                Stdio::inherit()
            } else {
                Stdio::null()
            }
        }
    };
    let (stdout, stderr) = get_output(
        environment,
        &environment.state.stdout_status,
        &environment.state.stderr_status,
    )?;
    let old_loose_syms = environment.loose_symbols;
    environment.loose_symbols = true;
    let mut args = Vec::new();
    for a in parts {
        if let Expression::Atom(Atom::String(_)) = a {
            let new_a = eval(environment, &a)?;
            args.push(new_a);
        } else {
            let glob_expand = if let Expression::Atom(Atom::Symbol(_)) = a {
                true
            } else {
                false
            };
            // Free standing callables in a process call do not make sense so filter them out...
            // Eval the strings below to make sure any expansions happen.
            let new_a = match a {
                Expression::Atom(Atom::Symbol(s)) => match get_expression(environment, s) {
                    Some(exp) => match &exp.exp {
                        Expression::Function(_) => {
                            eval(environment, &Expression::Atom(Atom::String(s.to_string())))?
                        }
                        Expression::Atom(Atom::Lambda(_)) => {
                            eval(environment, &Expression::Atom(Atom::String(s.to_string())))?
                        }
                        Expression::Atom(Atom::Macro(_)) => {
                            eval(environment, &Expression::Atom(Atom::String(s.to_string())))?
                        }
                        _ => eval(environment, &a)?,
                    },
                    _ => eval(environment, &a)?,
                },
                _ => eval(environment, &a)?,
            };
            if let Expression::Atom(Atom::String(s)) = &new_a {
                if glob_expand {
                    prep_string_arg(&s, &mut args)?;
                } else {
                    args.push(new_a.clone());
                }
            } else {
                args.push(new_a.clone());
            }
        }
    }
    environment.loose_symbols = old_loose_syms;
    run_command(environment, command, &mut args, stdin, stdout, stderr, data)
}
