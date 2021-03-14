use std::env;
use std::io;
use std::os::unix::io::{AsRawFd, FromRawFd};
use std::os::unix::process::CommandExt;
use std::process::{Command, Stdio};

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
use crate::signals::test_clear_sigint;
use crate::types::*;
use crate::unix::*;

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
    environment: &mut Environment,
    pid: u32,
    term_settings: Option<&termios::Termios>,
) -> Option<i32> {
    let result: Option<i32>;
    let mut int_cnt = 0;
    loop {
        if test_clear_sigint() {
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
        }
        let (stop, status) = try_wait_pid(environment, pid);
        if stop {
            result = status;
            if let Some(status) = status {
                if environment.save_exit_status {
                    env::set_var("LAST_STATUS".to_string(), format!("{}", status));
                    environment.root_scope.borrow_mut().insert_exp_data(
                        environment.interner.intern("*last-status*"),
                        ExpEnum::Int(i64::from(status)),
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
            termios::tcsetattr(environment.terminal_fd, termios::SetArg::TCSANOW, settings)
        //termios::tcsetattr(nix::libc::STDIN_FILENO, termios::SetArg::TCSANOW, settings)
        {
            eprintln!("Error resetting shell terminal settings: {}", err);
        }
    }
    // Move the shell back into the foreground.
    if environment.is_tty {
        let pid = unistd::getpid();
        if let Err(err) = unistd::tcsetpgrp(environment.terminal_fd, pid) {
            //if let Err(err) = unistd::tcsetpgrp(nix::libc::STDIN_FILENO, pid) {
            eprintln!("Error making shell {} foreground: {}", pid, err);
        }
    }
    result
}

fn run_command(
    environment: &mut Environment,
    command: &str,
    args: Vec<String>,
    stdin: Stdio,
    stdout: Stdio,
    stderr: Stdio,
) -> Result<Expression, LispError> {
    let mut command = command;
    let ncommand;
    if command.starts_with('~') {
        if let Some(c) = expand_tilde(command) {
            ncommand = c;
            command = &ncommand;
        }
    }
    if environment.in_fork && environment.eval_level == 1 {
        // We are the top level of a new fork so no need to fork again, just exec here.
        // On success exec will not return.
        if let Err(err) = reap_procs(environment) {
            // Try to clean up in case any procs started in this fork.
            eprintln!("Error reaping procs before exec: {}", err);
        }
        // If we still have procs running then maybe don't orphin them (at least not yet).
        if environment.procs.borrow().is_empty() {
            return Err(exec(&command, &args));
        }
    }
    let mut com_obj = Command::new(command);
    let foreground = !environment.run_background;
    let shell_terminal = environment.terminal_fd;
    com_obj
        .args(&args)
        .stdin(stdin)
        .stdout(stdout)
        .stderr(stderr);
    let pgid = environment.pipe_pgid;
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
        Ok(proc) => {
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
                        eprintln!("WARNING: Something in pipe is amiss, probably a command not part of pipe or a bug!");
                    }
                }
                if let Err(_err) = unistd::setpgid(pid, pgid_raw) {
                    // Ignore, do in parent and child.
                }
            }
            let pid = proc.id();
            let result = if foreground {
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
                    Some(code) => Expression::alloc_data(ExpEnum::Process(ProcessState::Over(
                        pid,
                        code as i32,
                    ))),
                    None => Expression::alloc_data(ExpEnum::Nil),
                }
            } else {
                Expression::alloc_data(ExpEnum::Process(ProcessState::Running(pid)))
            };
            add_process(environment, proc);
            Ok(result)
        }
        Err(e) => {
            let mut err_msg = String::new();
            err_msg.push_str(&format!("Failed to execute [{}", command));
            for n in args {
                err_msg.push_str(&format!(" {}", n));
            }
            err_msg.push_str(&format!("]: {}", e));
            // Recover from the failed spawn...
            // If we were saved terminal settings restore them.
            if let Some(settings) = term_settings {
                if let Err(err) =
                    termios::tcsetattr(environment.terminal_fd, termios::SetArg::TCSANOW, &settings)
                // termios::tcsetattr(nix::libc::STDIN_FILENO, termios::SetArg::TCSANOW, &settings)
                {
                    eprintln!("Error resetting shell terminal settings: {}", err);
                }
            }
            // Move the shell back into the foreground.
            if environment.is_tty {
                let pid = unistd::getpid();
                if let Err(err) = unistd::tcsetpgrp(environment.terminal_fd, pid) {
                    //if let Err(err) = unistd::tcsetpgrp(nix::libc::STDIN_FILENO, pid) {
                    eprintln!("Error making shell {} foreground: {}", pid, err);
                }
            }
            Err(LispError::new(err_msg))
        }
    }
}

fn get_std_io(environment: &Environment, is_out: bool) -> Result<Stdio, LispError> {
    let key = if is_out { "*stdout*" } else { "*stderr*" };
    let out = lookup_expression(environment, key);
    match out {
        Some(out) => {
            if let ExpEnum::File(f) = &out.get().data {
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
                    FileState::Write(f) => Ok(Stdio::from(f.get_ref().try_clone()?)),
                    _ => Err(LispError::new("Can not write to a non-writable file.")),
                }
            } else {
                Err(LispError::new("Can not write to something not a file."))
            }
        }
        None => Ok(Stdio::inherit()),
    }
}

fn get_output(environment: &Environment) -> Result<(Stdio, Stdio), LispError> {
    let out_res = if environment.grab_proc_output {
        Stdio::piped()
    } else {
        get_std_io(environment, true)?
    };
    let err_res = get_std_io(environment, false)?;
    Ok((out_res, err_res))
}

fn prep_string_arg(
    _environment: &mut Environment,
    s: &str,
    nargs: &mut Vec<String>,
) -> Result<(), LispError> {
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
                                nargs.push(p.to_string());
                            }
                        }
                        Err(err) => {
                            let msg = format!("glob error on while iterating {}, {}", s, err);
                            return Err(LispError::new(msg));
                        }
                    }
                }
                if i == 0 {
                    nargs.push(s);
                }
            }
            Err(_err) => {
                nargs.push(s);
            }
        }
    } else {
        nargs.push(s);
    }
    Ok(())
}

pub fn do_command(
    environment: &mut Environment,
    command: &str,
    parts: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let foreground = !environment.run_background;
    let stdin = if foreground {
        Stdio::inherit()
    } else {
        Stdio::null()
    };
    let (stdout, stderr) = get_output(environment)?;
    let old_loose_syms = environment.loose_symbols;
    environment.loose_symbols = true;
    let mut args = Vec::new();
    for a_exp in parts {
        let a_exp2 = a_exp.clone();
        let a_exp_a = a_exp.get();
        if let ExpEnum::String(_, _) = a_exp_a.data {
            drop(a_exp_a);
            let new_a = eval(environment, a_exp2)?;
            args.push(new_a.as_string(environment)?);
        } else {
            // Free standing callables in a process call do not make sense so filter them out...
            // Eval the strings below to make sure any expansions happen.
            let new_a = match a_exp_a.data {
                ExpEnum::Symbol(s, _) => match get_expression(environment, a_exp.clone()) {
                    Some(exp) => match &exp.get().data {
                        ExpEnum::Function(_) => {
                            drop(a_exp_a);
                            eval_data(environment, ExpEnum::String(s.into(), None))?
                        }
                        ExpEnum::Lambda(_) => {
                            drop(a_exp_a);
                            eval_data(environment, ExpEnum::String(s.into(), None))?
                        }
                        ExpEnum::Macro(_) => {
                            drop(a_exp_a);
                            eval_data(environment, ExpEnum::String(s.into(), None))?
                        }
                        _ => {
                            drop(a_exp_a);
                            eval(environment, a_exp2)?
                        }
                    },
                    _ => {
                        drop(a_exp_a);
                        eval(environment, a_exp2)?
                    }
                },
                _ => {
                    drop(a_exp_a);
                    eval(environment, a_exp2)?
                }
            };
            let new_a_a = new_a.get();
            if let ExpEnum::String(s, _) = &new_a_a.data {
                prep_string_arg(environment, &s, &mut args)?;
            } else {
                args.push(new_a.as_string(environment)?);
            }
        }
    }
    environment.loose_symbols = old_loose_syms;
    run_command(environment, command, args, stdin, stdout, stderr)
}
