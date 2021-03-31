use std::env;
use std::io;
use std::os::unix::io::AsRawFd;

use nix::{
    sys::{
        signal::{kill, Signal},
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
        {
            eprintln!("Error resetting shell terminal settings: {}", err);
        }
    }
    // Move the shell back into the foreground.
    if environment.is_tty {
        let pid = unistd::getpid();
        if let Err(err) = unistd::tcsetpgrp(environment.terminal_fd, pid) {
            eprintln!("Error making shell {} foreground: {}", pid, err);
        }
    }
    result
}

fn run_command(
    environment: &mut Environment,
    command: &str,
    args: Vec<String>,
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
            return Err(exec(&command, &args).into());
        }
    }
    let foreground = !environment.run_background;
    let pgid = environment.pipe_pgid;

    let term_settings = if environment.is_tty && environment.do_job_control {
        Some(termios::tcgetattr(environment.terminal_fd).unwrap())
    } else {
        None
    };
    let (out_fd, err_fd, pipe_read) = if environment.grab_proc_output {
        let (read_fd, write_fd) = anon_pipe()?;
        (
            Some(write_fd),
            get_std_io(environment, false)?,
            Some(read_fd),
        )
    } else {
        (
            get_std_io(environment, true)?,
            get_std_io(environment, false)?,
            None,
        )
    };
    let proc = fork_exec(environment, None, out_fd, err_fd, &command, &args);

    match proc {
        Ok(proc) => {
            let pgid_raw = match pgid {
                Some(pgid) => Pid::from_raw(pgid as i32),
                None => Pid::from_raw(proc as i32),
            };
            if environment.do_job_control {
                let pid = Pid::from_raw(proc as i32);
                if pgid.is_none() {
                    let mut job = Job {
                        pids: Vec::new(),
                        names: Vec::new(),
                        status: JobStatus::Running,
                    };
                    job.pids.push(proc);
                    job.names.push(command.to_string());
                    environment.jobs.borrow_mut().push(job);
                } else {
                    let job = environment.jobs.borrow_mut().pop();
                    if let Some(mut job) = job {
                        job.pids.push(proc);
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
            let result = if foreground {
                if environment.do_job_control {
                    if let Err(_err) = unistd::tcsetpgrp(nix::libc::STDIN_FILENO, pgid_raw) {
                        // Ignore, do in parent and child.
                    }
                }
                let status = if let Some(term_settings) = term_settings {
                    wait_pid(environment, proc, Some(&term_settings))
                } else {
                    wait_pid(environment, proc, None)
                };
                match status {
                    Some(code) => Expression::alloc_data(ExpEnum::Process(ProcessState::Over(
                        proc,
                        code as i32,
                    ))),
                    None => Expression::alloc_data(ExpEnum::Nil),
                }
            } else {
                Expression::alloc_data(ExpEnum::Process(ProcessState::Running(proc)))
            };
            add_process(environment, proc, pipe_read);
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
                {
                    eprintln!("Error resetting shell terminal settings: {}", err);
                }
            }
            // Move the shell back into the foreground.
            if environment.is_tty {
                let pid = unistd::getpid();
                if let Err(err) = unistd::tcsetpgrp(environment.terminal_fd, pid) {
                    eprintln!("Error making shell {} foreground: {}", pid, err);
                }
            }
            Err(LispError::new(err_msg))
        }
    }
}

fn get_std_io(environment: &Environment, is_out: bool) -> Result<Option<i32>, LispError> {
    let key = if is_out { "*stdout*" } else { "*stderr*" };
    let out = lookup_expression(environment, key);
    match out {
        Some(out) => {
            if let ExpEnum::File(f) = &out.get().data {
                match &*f.borrow() {
                    FileState::Stdout => {
                        if is_out {
                            Ok(None)
                        } else {
                            // If ever Windows need raw hangle not fd.
                            Ok(Some(io::stdout().as_raw_fd()))
                        }
                    }
                    FileState::Stderr => {
                        if !is_out {
                            Ok(None)
                        } else {
                            // If ever Windows need raw hangle not fd.
                            Ok(Some(io::stderr().as_raw_fd()))
                        }
                    }
                    FileState::Write(f) => Ok(Some(dup_fd(f.get_ref().as_raw_fd())?)),
                    _ => Err(LispError::new("Can not write to a non-writable file.")),
                }
            } else {
                Err(LispError::new("Can not write to something not a file."))
            }
        }
        None => Ok(None),
    }
}

pub fn do_command(
    environment: &mut Environment,
    command: &str,
    parts: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    fn add_arg_s(args: &mut Vec<String>, exp: Expression) -> Result<(), LispError> {
        match &exp.get().data {
            ExpEnum::String(s, _) => args.push(s.to_string()),
            ExpEnum::Symbol(s, _) => args.push(s.to_string()),
            ExpEnum::Pair(_, _) => {
                for a in exp.iter() {
                    add_arg_s(args, a)?;
                }
            }
            ExpEnum::Vector(_) => {
                for a in exp.iter() {
                    add_arg_s(args, a)?;
                }
            }
            _ => return Err(LispError::new("Sys command arguements need to be string (or symbols or lists that reduce so strings).")),
        }
        Ok(())
    }
    let mut args = Vec::new();
    for a_exp in parts {
        add_arg_s(&mut args, eval(environment, a_exp)?)?;
    }
    run_command(environment, command, args)
}
