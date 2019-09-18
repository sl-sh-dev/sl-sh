use std::fs::File;
use std::io::{self, Write};
use std::os::unix::process::CommandExt;
use std::process::{ChildStdin, ChildStdout, Command, Stdio};

use glob::glob;
//use nix::sys::signal::{self, SigHandler, Signal};
use nix::{
    sys::{
        signal::{self, SigHandler, Signal},
        termios,
        wait::{self, WaitPidFlag, WaitStatus},
    },
    unistd::{self, Pid},
};

use crate::builtins_util::*;
use crate::environment::*;
use crate::shell::*;
use crate::types::*;

pub fn try_wait_pid(environment: &Environment, pid: u32) -> (bool, Option<i32>) {
    let mut opts = WaitPidFlag::WUNTRACED;
    opts.insert(WaitPidFlag::WCONTINUED);
    opts.insert(WaitPidFlag::WNOHANG);
    match wait::waitpid(Pid::from_raw(pid as i32), Some(opts)) {
        Err(nix::Error::Sys(nix::errno::Errno::ECHILD)) => {
            // Does not exist.
            environment.procs.borrow_mut().remove(&pid);
            (true, None)
        }
        Err(err) => {
            eprintln!("Error waiting for pid {}, {}", pid, err);
            //Err(err)
            environment.procs.borrow_mut().remove(&pid);
            (true, None)
        }
        Ok(WaitStatus::Exited(_, status)) => {
            environment.procs.borrow_mut().remove(&pid);
            (true, Some(status))
        }
        Ok(WaitStatus::Stopped(..)) => {
            environment.state.borrow_mut().stopped_procs.push(pid);
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
    loop {
        let (stop, status) = try_wait_pid(environment, pid);
        if stop {
            result = status;
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
    let pid = unistd::getpid();
    if let Err(err) = unistd::tcsetpgrp(nix::libc::STDIN_FILENO, pid) {
        eprintln!("Error making shell {} foreground: {}", pid, err);
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
        new_args.push(a.make_string(environment)?);
    }
    let mut com_obj = Command::new(command);
    //let pgid = unistd::getpid();
    let foreground = !environment.in_pipe && !environment.state.borrow().is_spawn;
    let shell_terminal = nix::libc::STDIN_FILENO;
    com_obj
        .args(new_args)
        .stdin(stdin)
        .stdout(stdout)
        .stderr(stderr);

    unsafe {
        com_obj.pre_exec(move || -> io::Result<()> {
            let pid = unistd::getpid();
            let pgid = unistd::getpid();
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

    let term_settings = termios::tcgetattr(shell_terminal).unwrap();
    let proc = com_obj.spawn();

    let mut result = Expression::Atom(Atom::Nil);
    match proc {
        Ok(mut proc) => {
            let pid = Pid::from_raw(proc.id() as i32);
            let pgid = Pid::from_raw(proc.id() as i32);
            if let Err(_err) = unistd::setpgid(pid, pgid) {
                // Ignore, do in parent and child.
            }
            if let Some(data_in) = data_in {
                if proc.stdin.is_some() {
                    let mut input: Option<ChildStdin> = None;
                    std::mem::swap(&mut proc.stdin, &mut input);
                    let mut input = input.unwrap();
                    input.write_all(data_in.to_string().as_bytes())?;
                }
            }
            if foreground && !environment.in_pipe {
                if let Err(_err) = unistd::tcsetpgrp(shell_terminal, pgid) {
                    // Ignore, do in parent and child.
                }
                wait_pid(environment, proc.id(), Some(&term_settings));
            }
            let pid = add_process(environment, proc);
            result = Expression::Process(pid);
        }
        Err(e) => {
            eprintln!("Failed to execute {}: {}", command, e);
        }
    };
    Ok(result)
}

fn get_output(environment: &Environment, status: &Option<IOState>) -> io::Result<Stdio> {
    let res = match status {
        Some(IOState::FileAppend(f)) => {
            let outputs = File::create(f)?;
            Stdio::from(outputs)
        }
        Some(IOState::FileOverwrite(f)) => {
            let outputs = File::create(f)?;
            Stdio::from(outputs)
        }
        Some(IOState::Null) => Stdio::null(),
        Some(IOState::Inherit) => Stdio::inherit(),
        Some(IOState::Pipe) => Stdio::piped(),
        None => {
            let use_stdout = environment.state.borrow().eval_level < 3 && !environment.in_pipe;
            if use_stdout {
                Stdio::inherit()
            } else {
                Stdio::piped()
            }
        }
    };
    Ok(res)
}

pub fn do_command(
    environment: &mut Environment,
    command: &str,
    parts: &[Expression],
    data_in: Option<Expression>,
) -> io::Result<Expression> {
    let mut data = None;
    let stdin = match data_in {
        Some(Expression::Atom(Atom::Nil)) => Stdio::inherit(),
        Some(Expression::Atom(atom)) => {
            data = Some(atom);
            Stdio::piped()
        }
        Some(Expression::Process(pid)) => {
            let procs = environment.procs.clone();
            let mut procs = procs.borrow_mut();
            if let Some(proc) = procs.get_mut(&pid) {
                if proc.stdout.is_some() {
                    let mut out: Option<ChildStdout> = None;
                    std::mem::swap(&mut proc.stdout, &mut out);
                    Stdio::from(out.unwrap())
                } else {
                    Stdio::inherit()
                }
            } else {
                Stdio::inherit()
            }
        }
        Some(Expression::Func(_)) => {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "Invalid expression state before command (special form).",
            ))
        }
        Some(Expression::List(_)) => {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "Invalid expression state before command (form).",
            ))
        }
        None => Stdio::inherit(),
    };
    let stdout = get_output(environment, &environment.state.borrow().stdout_status)?;
    let stderr = get_output(environment, &environment.state.borrow().stderr_status)?;
    let mut args = to_args(environment, parts)?;
    let mut nargs: Vec<Expression> = Vec::with_capacity(args.len());
    for arg in args.drain(..) {
        if let Expression::Atom(Atom::String(s)) = &arg {
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
                                    let msg =
                                        format!("glob error on while iterating {}, {}", s, err);
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
        } else {
            nargs.push(arg);
        }
    }
    run_command(
        environment,
        command,
        &mut nargs,
        stdin,
        stdout,
        stderr,
        data,
    )
}

pub fn do_pipe(
    environment: &mut Environment,
    parts: &[Expression],
    data_in: Option<Expression>,
) -> io::Result<Expression> {
    let old_pipe_in = environment.in_pipe;
    let old_out_status = environment.state.borrow().stdout_status.clone();
    environment.in_pipe = true;
    let mut out = match data_in {
        Some(exp) => exp,
        None => Expression::Atom(Atom::Nil),
    };
    environment.state.borrow_mut().stdout_status = Some(IOState::Pipe);
    let mut i = 1; // Meant 1 here.
    for p in parts {
        if i == parts.len() && !old_pipe_in {
            if environment.state.borrow().eval_level > 2 {
                environment.state.borrow_mut().stdout_status = None;
            } else {
                environment.state.borrow_mut().stdout_status = old_out_status.clone();
            }
            environment.in_pipe = false; // End of the pipe and want to wait.
        }
        let res = pipe_eval(environment, p, Some(out));
        if let Err(err) = res {
            environment.in_pipe = old_pipe_in;
            environment.state.borrow_mut().stdout_status = old_out_status;
            return Err(err);
        }
        out = res.unwrap();
        i += 1;
    }
    environment.in_pipe = old_pipe_in;
    environment.state.borrow_mut().stdout_status = old_out_status;
    Ok(out)
}
