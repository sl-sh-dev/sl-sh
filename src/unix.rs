use std::cell::RefCell;
use std::ffi::{CString, OsStr};
use std::fs::File;
use std::io::{self, BufWriter, Read, Write};
use std::os::unix::ffi::OsStrExt;
use std::os::unix::io::FromRawFd;
use std::ptr;
use std::rc::Rc;

use nix::sys::signal::{self, SigHandler, Signal};
use nix::unistd::{self, Pid};

use crate::environment::*;
use crate::eval::*;
use crate::types::*;

pub trait IsMinusOne {
    fn is_minus_one(&self) -> bool;
}

macro_rules! impl_is_minus_one {
    ($($t:ident)*) => ($(impl IsMinusOne for $t {
        fn is_minus_one(&self) -> bool {
            *self == -1
        }
    })*)
}

impl_is_minus_one! { i8 i16 i32 i64 isize }

pub fn cvt<T: IsMinusOne>(t: T) -> Result<T, LispError> {
    if t.is_minus_one() {
        Err(io::Error::last_os_error().into())
    } else {
        Ok(t)
    }
}

pub fn anon_pipe() -> Result<(i32, i32), LispError> {
    // Adapted from sys/unix/pipe.rs in std lib.
    let mut fds = [0; 2];

    // The only known way right now to create atomically set the CLOEXEC flag is
    // to use the `pipe2` syscall. This was added to Linux in 2.6.27, glibc 2.9
    // and musl 0.9.3, and some other targets also have it.
    cfg_if::cfg_if! {
        if #[cfg(any(
            target_os = "dragonfly",
            target_os = "freebsd",
            target_os = "linux",
            target_os = "netbsd",
            target_os = "openbsd",
            target_os = "redox"
        ))] {
            cvt(unsafe { libc::pipe2(fds.as_mut_ptr(), libc::O_CLOEXEC) })?;
            Ok((fds[0], fds[1]))
        } else {
            cvt(unsafe { libc::pipe(fds.as_mut_ptr()) })?;
            Ok((fds[0], fds[1]))
        }
    }
}

pub fn fork(
    environment: &mut Environment,
    exp: Expression,
    stdin: Option<i32>,
    stdout: Option<i32>,
) -> Result<u32, LispError> {
    let result = unsafe { cvt(libc::fork())? };

    let pid = unsafe {
        match result {
            0 => {
                if let Some(stdin) = stdin {
                    if let Err(err) = cvt(libc::dup2(stdin, 0)) {
                        eprintln!("Error setting up stdin (dup) in pipe: {}", err);
                        libc::_exit(10);
                    }
                    if let Err(err) = cvt(libc::close(stdin)) {
                        eprintln!("Error setting up stdin (close) in pipe: {}", err);
                        libc::_exit(10);
                    }
                    environment.root_scope.borrow_mut().insert(
                        "*stdin*",
                        ExpEnum::File(Rc::new(RefCell::new(FileState::Stdin))).into(),
                    );
                    if environment.dynamic_scope.contains_key("*stdin*") {
                        environment.dynamic_scope.remove("*stdin*");
                    }
                }
                if let Some(stdout) = stdout {
                    if let Err(err) = cvt(libc::dup2(stdout, 1)) {
                        eprintln!("Error setting up stdout (dup) in pipe: {}", err);
                        libc::_exit(10);
                    }
                    if let Err(err) = cvt(libc::close(stdout)) {
                        eprintln!("Error setting up stdout (close) in pipe: {}", err);
                        libc::_exit(10);
                    }
                    environment.root_scope.borrow_mut().insert(
                        "*stdout*",
                        ExpEnum::File(Rc::new(RefCell::new(FileState::Stdout))).into(),
                    );

                    if environment.dynamic_scope.contains_key("*stdout*") {
                        environment.dynamic_scope.remove("*stdout*");
                    }
                }
                environment.eval_level = 0;
                environment.run_background = false;
                environment.jobs.borrow_mut().clear();
                environment.do_job_control = false;
                environment.stopped_procs.borrow_mut().clear();
                environment.procs.borrow_mut().clear();
                environment.grab_proc_output = false;
                environment.pipe_pgid = None;
                environment.terminal_fd = if let Ok(fd) = cvt(libc::dup(0)) {
                    fd
                } else {
                    0
                };
                environment.is_tty = false;
                environment.in_fork = true;
                let exit_code = match eval(environment, exp) {
                    Ok(exp) if stdin.is_none() => {
                        let mut outf = BufWriter::new(fd_to_file(1));
                        if let ExpEnum::File(file) = &exp.get().data {
                            let mut file_b = file.borrow_mut();
                            match &mut *file_b {
                                FileState::Read(Some(f_iter), _) => {
                                    // XXX, maybe use the second item (fd) instead of the iterator?
                                    for ch in f_iter {
                                        if let Err(err) = outf.write_all(ch.as_bytes()) {
                                            eprintln!("Error writing to next pipe: {}", err);
                                            break;
                                        }
                                    }
                                }
                                FileState::ReadBinary(inf) => {
                                    let mut buf = [0; 10240];
                                    let mut n = match inf.read(&mut buf[..]) {
                                        Ok(n) => n,
                                        Err(err) => {
                                            eprintln!("Error reading initial pipe input: {}", err);
                                            0
                                        }
                                    };
                                    while n > 0 {
                                        if let Err(err) = outf.write_all(&buf[..n]) {
                                            eprintln!("Error writing to next pipe: {}", err);
                                            break;
                                        }
                                        n = match inf.read(&mut buf[..]) {
                                            Ok(n) => n,
                                            Err(err) => {
                                                eprintln!(
                                                    "Error reading initial pipe input: {}",
                                                    err
                                                );
                                                0
                                            }
                                        };
                                    }
                                }
                                _ => {}
                            }
                        }
                        0
                    }
                    Ok(_) => 0,
                    Err(_) => 1,
                };
                if let Err(err) = reap_procs(environment) {
                    eprintln!("Error reaping procs in a pipe process: {}", err);
                }
                if let Some(ec) = environment.exit_code {
                    libc::_exit(ec);
                }
                libc::_exit(exit_code);
            }
            n => n as u32,
        }
    };
    environment.procs.borrow_mut().insert(pid, None);
    unsafe {
        if let Some(stdin) = stdin {
            cvt(libc::close(stdin))?;
        }
        if let Some(stdout) = stdout {
            cvt(libc::close(stdout))?;
        }
    }
    Ok(pid)
}

pub fn dup_fd(fd: i32) -> Result<i32, LispError> {
    Ok(unsafe { cvt(libc::dup(fd))? })
}

pub fn replace_fd(new_fd: i32, fd: i32) -> Result<i32, LispError> {
    Ok(unsafe {
        let old = cvt(libc::dup(fd))?;
        cvt(libc::dup2(new_fd, fd))?;
        cvt(libc::close(new_fd))?;
        old
    })
}

pub fn replace_stdin(new_stdin: i32) -> Result<i32, LispError> {
    replace_fd(new_stdin, 0)
}

pub fn replace_stdout(new_stdout: i32) -> Result<i32, LispError> {
    replace_fd(new_stdout, 1)
}

pub fn replace_stderr(new_stderr: i32) -> Result<i32, LispError> {
    replace_fd(new_stderr, 2)
}

pub fn dup_stdin(new_stdin: i32) -> Result<(), LispError> {
    unsafe {
        cvt(libc::dup2(new_stdin, 0))?;
        cvt(libc::close(new_stdin))?;
    }
    Ok(())
}

pub fn dup_stdout(new_stdout: i32) -> Result<(), LispError> {
    unsafe {
        cvt(libc::dup2(new_stdout, 1))?;
        cvt(libc::close(new_stdout))?;
    }
    Ok(())
}

pub fn dup_stderr(new_stderr: i32) -> Result<(), LispError> {
    unsafe {
        cvt(libc::dup2(new_stderr, 2))?;
        cvt(libc::close(new_stderr))?;
    }
    Ok(())
}

pub fn close_fd(fd: i32) -> Result<(), LispError> {
    unsafe {
        cvt(libc::close(fd))?;
    }
    Ok(())
}

pub fn fd_to_file(fd: i32) -> File {
    unsafe { File::from_raw_fd(fd) }
}

fn os2c(s: &OsStr, saw_nul: &mut bool) -> CString {
    CString::new(s.as_bytes()).unwrap_or_else(|_e| {
        *saw_nul = true;
        CString::new("<string-with-nul>").unwrap()
    })
}

pub fn exec<I, S, P>(program: P, args: I) -> LispError
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
    P: AsRef<OsStr>,
{
    let mut saw_nul = false;
    let program = os2c(program.as_ref(), &mut saw_nul);
    let mut argv = vec![program.as_ptr(), ptr::null()];
    let mut args_t = vec![program.clone()];
    for arg in args {
        // Overwrite the trailing NULL pointer in `argv` and then add a new null
        // pointer.
        let arg = os2c(arg.as_ref(), &mut saw_nul);
        argv[args_t.len()] = arg.as_ptr();
        argv.push(ptr::null());
        // Also make sure we keep track of the owned value to schedule a
        // destructor for this memory.
        args_t.push(arg);
    }

    unsafe {
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
        libc::execvp(program.as_ptr(), argv.as_ptr());
    }
    io::Error::last_os_error().into()
}

pub fn fork_exec<I, S, P>(
    environment: &mut Environment,
    stdin: Option<i32>,
    stdout: Option<i32>,
    program: P,
    args: I,
) -> Result<u32, LispError>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
    P: AsRef<OsStr>,
{
    let result = unsafe { cvt(libc::fork())? };

    let pid = unsafe {
        match result {
            0 => {
                if let Some(stdin) = stdin {
                    if let Err(err) = cvt(libc::dup2(stdin, 0)) {
                        eprintln!("Error setting up stdin (dup) in pipe: {}", err);
                        libc::_exit(10);
                    }
                    if let Err(err) = cvt(libc::close(stdin)) {
                        eprintln!("Error setting up stdin (close) in pipe: {}", err);
                        libc::_exit(10);
                    }
                    environment.root_scope.borrow_mut().insert(
                        "*stdin*",
                        ExpEnum::File(Rc::new(RefCell::new(FileState::Stdin))).into(),
                    );
                    if environment.dynamic_scope.contains_key("*stdin*") {
                        environment.dynamic_scope.remove("*stdin*");
                    }
                }
                if let Some(stdout) = stdout {
                    if let Err(err) = cvt(libc::dup2(stdout, 1)) {
                        eprintln!("Error setting up stdout (dup) in pipe: {}", err);
                        libc::_exit(10);
                    }
                    if let Err(err) = cvt(libc::close(stdout)) {
                        eprintln!("Error setting up stdout (close) in pipe: {}", err);
                        libc::_exit(10);
                    }
                    environment.root_scope.borrow_mut().insert(
                        "*stdout*",
                        ExpEnum::File(Rc::new(RefCell::new(FileState::Stdout))).into(),
                    );

                    if environment.dynamic_scope.contains_key("*stdout*") {
                        environment.dynamic_scope.remove("*stdout*");
                    }
                }
                let pgid = environment.pipe_pgid;
                if environment.do_job_control {
                    let pid = unistd::getpid();
                    let pgid = match pgid {
                        Some(pgid) => Pid::from_raw(pgid as i32),
                        None => unistd::getpid(),
                    };
                    if let Err(_err) = unistd::setpgid(pid, pgid) {
                        // Ignore, do in parent and child.
                        //let msg = format!("Error setting pgid for {}: {}", pid, err);
                    }
                    if !environment.run_background {
                        if let Err(_err) = unistd::tcsetpgrp(environment.terminal_fd, pgid) {
                            // Ignore, do in parent and child.
                            //let msg = format!("Error making {} foreground: {}", pid, err);
                        }
                    }
                }

                let err = exec(program, args);
                eprintln!("Error executing, {}", err);
                libc::_exit(1);
            }
            n => n as u32,
        }
    };
    unsafe {
        if let Some(stdin) = stdin {
            cvt(libc::close(stdin))?;
        }
        if let Some(stdout) = stdout {
            cvt(libc::close(stdout))?;
        }
    }
    Ok(pid)
}
