use std::fs::File;
use std::io::{self, Write};
use std::os::unix::process::CommandExt;
use std::process::{ChildStdin, ChildStdout, Command, Stdio};

use glob::glob;
use nix::sys::signal::{self, SigHandler, Signal};

use crate::builtins_util::*;
use crate::environment::*;
use crate::types::*;

fn run_command(
    environment: &mut Environment,
    command: &str,
    args: &mut Vec<Expression>,
    stdin: Stdio,
    stdout: Stdio,
    stderr: Stdio,
    data_in: Option<Atom>,
    wait: bool,
) -> io::Result<Expression> {
    let mut new_args: Vec<String> = Vec::new();
    for a in args {
        new_args.push(a.make_string(environment)?);
    }
    let mut com_obj = Command::new(command);
    com_obj
        .args(new_args)
        .stdin(stdin)
        .stdout(stdout)
        .stderr(stderr);

    unsafe {
        com_obj.pre_exec(|| -> io::Result<()> {
            // XXX TODO, do better with these unwraps.
            /*let pgid = unistd::getpid();
            if pgid != unistd::getpgrp() {
                unistd::setpgid(pgid, pgid).unwrap();
            }*/
            signal::signal(Signal::SIGINT, SigHandler::SigDfl).unwrap();
            signal::signal(Signal::SIGHUP, SigHandler::SigDfl).unwrap();
            signal::signal(Signal::SIGTERM, SigHandler::SigDfl).unwrap();
            Ok(())
        });
    }

    let proc = com_obj.spawn();

    let mut result = Expression::Atom(Atom::Nil);
    match proc {
        Ok(mut proc) => {
            if wait {
                if let Err(err) = proc.wait() {
                    eprintln!("Failed to wait for {}: {}", command, err);
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
            let pid = add_process(environment, proc);
            result = Expression::Process(pid);
        }
        Err(e) => {
            eprintln!("Failed to execute {}: {}", command, e);
        }
    };
    Ok(result)
}

pub fn do_command(
    environment: &mut Environment,
    command: &str,
    parts: &[Expression],
    data_in: Expression,
    use_stdout: bool,
) -> io::Result<Expression> {
    let mut data = None;
    let stdin = match data_in {
        Expression::Atom(Atom::Nil) => Stdio::inherit(),
        Expression::Atom(atom) => {
            data = Some(atom);
            Stdio::piped()
        }
        Expression::Process(pid) => {
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
        Expression::Func(_) => {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "Invalid expression state before command (special form).",
            ))
        }
        Expression::List(_) => {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "Invalid expression state before command (form).",
            ))
        }
    };
    let stdout = if use_stdout {
        Stdio::inherit()
    } else if let Some(s) = &environment.state.borrow().stdout_file {
        let outputs = File::create(s)?;
        Stdio::from(outputs)
    } else {
        Stdio::piped()
    };
    let stderr = if environment.err_null {
        Stdio::null()
    } else if let Some(s) = &environment.state.borrow().stderr_file {
        let outputs = File::create(s)?;
        Stdio::from(outputs)
    } else {
        Stdio::inherit()
    };
    let mut args = to_args(environment, parts, false)?;
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
        use_stdout || !environment.in_pipe,
    )
}
