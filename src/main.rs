//extern crate jemallocator;

//#[global_allocator]
//static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

use std::io;
use std::mem;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use nix::{
    sys::signal::{self, SigHandler, Signal},
    unistd,
};

use ::sl_sh::config::*;
use ::sl_sh::shell::*;
use ::sl_sh::types::LispError;

fn main() -> Result<(), LispError> {
    if let Some(config) = get_config() {
        if config.command.is_none() && config.script.is_none() {
            /* See if we are running interactively.  */
            let shell_terminal = nix::libc::STDIN_FILENO;
            if let Ok(true) = unistd::isatty(shell_terminal) {
                /* Loop until we are in the foreground.  */
                let mut shell_pgid = unistd::getpgrp();
                while unistd::tcgetpgrp(shell_terminal) != Ok(shell_pgid) {
                    //kill (- shell_pgid, SIGTTIN);
                    if let Err(err) = signal::kill(shell_pgid, Signal::SIGTTIN) {
                        eprintln!("Error sending sigttin: {}.", err);
                    }
                    shell_pgid = unistd::getpgrp();
                }

                /* Ignore interactive and job-control signals.  */
                unsafe {
                    // Do not ignore SIGINT because we want the thread below to handle it (block it instead).
                    //signal::signal(Signal::SIGINT, SigHandler::SigIgn).unwrap();
                    signal::signal(Signal::SIGQUIT, SigHandler::SigIgn).unwrap();
                    signal::signal(Signal::SIGTSTP, SigHandler::SigIgn).unwrap();
                    signal::signal(Signal::SIGTTIN, SigHandler::SigIgn).unwrap();
                    signal::signal(Signal::SIGTTOU, SigHandler::SigIgn).unwrap();
                    // Ignoring sigchild will mess up waitpid and cause Command::spawn to panic under some conditions.
                    //signal::signal(Signal::SIGCHLD, SigHandler::SigIgn).unwrap();
                }

                /* Put ourselves in our own process group.  */
                let pgid = unistd::getpid();
                if let Err(err) = unistd::setpgid(pgid, pgid) {
                    match err {
                        nix::Error::Sys(nix::errno::Errno::EPERM) => { /* ignore */ }
                        _ => {
                            eprintln!("Couldn't put the shell in its own process group: {}\n", err)
                        }
                    }
                }
                /* Grab control of the terminal.  */
                if let Err(err) = unistd::tcsetpgrp(shell_terminal, pgid) {
                    let msg = format!("Couldn't grab control of terminal: {}\n", err);
                    eprintln!("{}", msg);
                    return Err(LispError::new(msg));
                }

                // Block this signal so the thread below will get SIGINT.
                let mut sigset = signal::SigSet::empty();
                sigset.add(signal::Signal::SIGINT);
                signal::sigprocmask(signal::SigmaskHow::SIG_BLOCK, Some(&sigset), None)
                    .expect("Could not block the signals");

                let sig_int = Arc::new(AtomicBool::new(false));
                let sig_int_t = sig_int.clone();
                let sig_int_stop = Arc::new(AtomicBool::new(false));
                let sig_int_stop_t = sig_int_stop.clone();

                // Thread to handle SIGINT (ctrl-c) by setting a flag so script
                // code can stop (error out) or a process being waiting on will
                // be sent signals to stop (INT -> TERM -> KILL)
                let sig_child = std::thread::spawn(move || {
                    fn ok_errno<T>(ok: T, ecode: libc::c_int) -> Result<T, LispError> {
                        if ecode != 0 {
                            Err(io::Error::from_raw_os_error(ecode).into())
                        } else {
                            Ok(ok)
                        }
                    }
                    let mut set: libc::sigset_t = unsafe { mem::zeroed() };
                    unsafe {
                        libc::sigemptyset(&mut set);
                    }
                    let r = unsafe { ok_errno((), libc::sigaddset(&mut set, libc::SIGINT)) };
                    if let Err(err) = r {
                        eprintln!("got error registering a signal {}", err);
                    }
                    loop {
                        let mut sig: libc::c_int = 0;
                        let errno = unsafe { libc::sigwait(&set, &mut sig) };
                        let e = ok_errno(sig, errno);
                        match e {
                            Ok(code) => {
                                if code == 2 {
                                    sig_int_t.store(true, Ordering::Relaxed);
                                } else {
                                    eprintln!(
                                        "ERROR, got unexpected signal {} from sigwait.",
                                        code
                                    );
                                }
                            }
                            Err(err) => eprintln!("ERROR waiting for signal SIGINT: {}", err),
                        }
                        if sig_int_stop_t.load(Ordering::Relaxed) {
                            break;
                        }
                    }
                });

                let code = start_interactive(sig_int);
                sig_int_stop.store(true, Ordering::Relaxed);
                if let Err(err) = signal::kill(unistd::getpid(), Signal::SIGINT) {
                    eprintln!(
                        "ERROR sending SIGINT to myself (to stop the SIGINT thread): {}.",
                        err
                    );
                }
                if let Err(err) = sig_child.join() {
                    eprintln!("ERROR waiting on SIGINT thread to end: {:?}.", err);
                }
                std::process::exit(code);
            } else {
                // No tty, just read stdin and do something with it..
                let code = read_stdin();
                std::process::exit(code);
            }
        } else if config.command.is_some() {
            let command = config.command.unwrap();
            if let Err(err) = run_one_command(&command, &config.args) {
                eprintln!("Error running {}: {}", command, err);
                return Err(err);
            }
        } else if config.script.is_some() {
            let script = config.script.unwrap();
            let code = run_one_script(&script, &config.args);
            std::process::exit(code);
        }
    }
    Ok(())
}
