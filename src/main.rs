use std::io;

use nix::{
    sys::signal::{self, SigHandler, Signal},
    unistd,
};

use ::slsh::*;

/// Blocks the SIGTSTP/SIGTTOU/SIGTTIN/SIGCHLD signals so that the shell never receives
/// them.
pub fn block_signals() {
    let mut sigset = signal::SigSet::empty();
    sigset.add(signal::Signal::SIGTSTP);
    sigset.add(signal::Signal::SIGTTOU);
    sigset.add(signal::Signal::SIGTTIN);
    sigset.add(signal::Signal::SIGCHLD);
    sigset.add(signal::Signal::SIGINT);
    sigset.add(signal::Signal::SIGHUP);
    sigset.add(signal::Signal::SIGTERM);
    signal::sigprocmask(signal::SigmaskHow::SIG_BLOCK, Some(&sigset), None)
        .expect("Could not block the signals");
}

fn set_unique_pgid() -> nix::Result<()> {
    let pgid = unistd::getpid();
    if pgid != unistd::getpgrp() {
        unistd::setpgid(pgid, pgid)?;
    }
    if pgid != unistd::tcgetpgrp(nix::libc::STDIN_FILENO)? {
        unsafe {
            signal::signal(Signal::SIGTTOU, SigHandler::SigIgn).unwrap();
        }
        unistd::tcsetpgrp(nix::libc::STDIN_FILENO, pgid)?;
    }
    Ok(())
}

/*
extern fn handle_sigtstp(signal: libc::c_int) {
        let signal = Signal::from_c_int(signal).unwrap();
        println!("XXXXX got sigtstp {}", signal);
            //SIGNALED.store(signal == Signal::SIGINT, Ordering::Relaxed);
}

extern fn handle_sigchild(signal: libc::c_int) {
        let signal = Signal::from_c_int(signal).unwrap();
        println!("XXXXX got sigchild {}", signal);
            //SIGNALED.store(signal == Signal::SIGINT, Ordering::Relaxed);
}
*/

/*use std::mem;

fn ok_errno<T>(ok: T, ecode: libc::c_int) -> io::Result<T> {
    if ecode != 0 {
        Err(io::Error::from_raw_os_error(ecode))
    } else {
        Ok(ok)
    }
}*/

fn main() -> io::Result<()> {
    let config = get_config();
    if let Ok(config) = config {
        if config.command.is_none() {
            block_signals();
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
                    signal::signal(Signal::SIGINT, SigHandler::SigIgn).unwrap();
                    signal::signal(Signal::SIGQUIT, SigHandler::SigIgn).unwrap();
                    signal::signal(Signal::SIGTSTP, SigHandler::SigIgn).unwrap();
                    signal::signal(Signal::SIGTTIN, SigHandler::SigIgn).unwrap();
                    signal::signal(Signal::SIGTTOU, SigHandler::SigIgn).unwrap();
                    signal::signal(Signal::SIGCHLD, SigHandler::SigIgn).unwrap();
                }

                /* Put ourselves in our own process group.  */
                let pgid = unistd::getpid();
                if let Err(err) = unistd::setpgid(pgid, pgid) {
                    eprintln!("Couldn't put the shell in its own process group: {}", err);
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "Couldn't put the shell in its own process group",
                    ));
                }
                /* Grab control of the terminal.  */
                if let Err(err) = unistd::tcsetpgrp(shell_terminal, pgid) {
                    eprintln!("Couldn't grab control of terminal: {}", err);
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "Couldn't grab control of terminal.",
                    ));
                }
                /* Save default terminal attributes for shell.  */
                //tcgetattr (shell_terminal, &shell_tmodes);
            }

            /*let _child = std::thread::spawn(move || {
                let mut set: libc::sigset_t = unsafe { mem::zeroed() };
                unsafe {
                    libc::sigemptyset(&mut set);
                }
                let r = unsafe { ok_errno((), libc::sigaddset(&mut set, libc::SIGCHLD)) };
                if let Err(err) = r {
                    eprintln!("got error registering a signal {}", err);
                }
                let r = unsafe { ok_errno((), libc::sigaddset(&mut set, libc::SIGTSTP)) };
                if let Err(err) = r {
                    eprintln!("got error registering a signal {}", err);
                }
                let r = unsafe { ok_errno((), libc::sigaddset(&mut set, libc::SIGINT)) };
                if let Err(err) = r {
                    eprintln!("got error registering a signal {}", err);
                }
                let r = unsafe { ok_errno((), libc::sigaddset(&mut set, libc::SIGHUP)) };
                if let Err(err) = r {
                    eprintln!("got error registering a signal {}", err);
                }
                let r = unsafe { ok_errno((), libc::sigaddset(&mut set, libc::SIGTERM)) };
                if let Err(err) = r {
                    eprintln!("got error registering a signal {}", err);
                }
                loop {
                    let mut sig: libc::c_int = 0;
                    let errno = unsafe { libc::sigwait(&mut set, &mut sig) };
                    let e = ok_errno(sig, errno);
                    //println!("XXX got signal: {:?}", e);
                }
            });*/

            //let handler = SigHandler::Handler(handle_sigtstp);
            //let child_handler = SigHandler::Handler(handle_sigchild);
            /*unsafe {
                //signal::signal(Signal::SIGTSTP, handler);
                //signal::signal(Signal::SIGCHLD, child_handler);
                signal::signal(Signal::SIGINT, SigHandler::SigIgn).unwrap();
                signal::signal(Signal::SIGHUP, SigHandler::SigIgn).unwrap();
                signal::signal(Signal::SIGTERM, SigHandler::SigIgn).unwrap();
            }*/

            /*if let Err(err) = set_unique_pgid() {
                eprintln!("Could not bring shell to foreground: {}", err);
            }*/
            start_interactive();
        } else {
            let command = config.command.unwrap();
            if let Err(err) = run_one_command(&command, &config.args) {
                if let Err(err2) = run_one_script(&command, &config.args) {
                    eprintln!("Error running {}: {}, {}", command, err, err2);
                    return Err(err2);
                }
            }
        }
    }
    Ok(())
}
