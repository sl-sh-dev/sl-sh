use std::cell::RefCell;
use std::convert::TryInto;
use std::ffi::{CString, OsStr};
use std::fs::File;
use std::io::{self, BufWriter, Read, Write};
use std::os::unix::ffi::OsStrExt;
use std::os::unix::io::FromRawFd;
use std::ptr;
use std::rc::Rc;

use nix::libc;
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

fn fork_job_name(environment: &mut Environment, exp: &Expression) -> Result<String, LispError> {
    let res = match &exp.get().data {
        ExpEnum::Pair(car, cdr) => {
            if car.to_string() == "syscall" {
                if let ExpEnum::Pair(cadr, _) = &cdr.get().data {
                    eval(environment, cadr)?.to_string()
                } else {
                    fork_job_name(environment, cdr)?
                }
            } else {
                car.to_string()
            }
        }
        ExpEnum::Vector(v) if !v.is_empty() => {
            if v[0].to_string() == "syscall" {
                if let Some(n) = v.get(1) {
                    eval(environment, n)?.to_string()
                } else {
                    v[0].to_string()
                }
            } else {
                v[0].to_string()
            }
        }
        _ => {
            let exp_name = exp.to_string();
            if exp_name.len() > 30 {
                exp_name[0..30].to_string()
            } else {
                exp_name
            }
        }
    };
    Ok(res)
}

fn setup_job(environment: &mut Environment, proc: u32, command: &str) {
    let pgid = environment.pipe_pgid;
    if environment.do_job_control {
        let pid = Pid::from_raw(proc as i32);
        let pgid_raw = match pgid {
            Some(pgid) => Pid::from_raw(pgid as i32),
            None => Pid::from_raw(proc as i32),
        };
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
                eprintln!("WARNING: Something in job control is amiss, probably a command not part of pipe or a bug!");
            }
        }
        if let Err(_err) = unistd::setpgid(pid, pgid_raw) {
            // Ignore, do in parent and child.
        }
        if let Err(_err) = unistd::tcsetpgrp(libc::STDIN_FILENO, pgid_raw) {
            // Ignore, do in parent and child.
        }
    }
}

pub fn fork(
    environment: &mut Environment,
    exp: Expression,
    stdin: Option<i32>,
    stdout: Option<i32>,
    stderr: Option<i32>,
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
                }
                if let Some(stdout) = stdout {
                    if let Err(err) = cvt(libc::dup2(stdout, 1)) {
                        eprintln!("Error setting up stdout (dup) in pipe: {}", err);
                        libc::_exit(10);
                    }
                    let mut close_out = true;
                    if let Some(stderr) = stderr {
                        if stdout == stderr {
                            close_out = false;
                        }
                    }
                    if close_out {
                        if let Err(err) = cvt(libc::close(stdout)) {
                            eprintln!("Error setting up stdout (close) in pipe: {}", err);
                            libc::_exit(10);
                        }
                    }
                    environment.root_scope.borrow_mut().insert(
                        "*stdout*",
                        ExpEnum::File(Rc::new(RefCell::new(FileState::Stdout))).into(),
                    );
                }
                if let Some(stderr) = stderr {
                    if let Err(err) = cvt(libc::dup2(stderr, 2)) {
                        eprintln!("Error setting up stderr (dup) in pipe: {}", err);
                        libc::_exit(10);
                    }
                    if let Err(err) = cvt(libc::close(stderr)) {
                        eprintln!("Error setting up stderr (close) in pipe: {}", err);
                        libc::_exit(10);
                    }
                    environment.root_scope.borrow_mut().insert(
                        "*stderr*",
                        ExpEnum::File(Rc::new(RefCell::new(FileState::Stderr))).into(),
                    );
                }
                if environment.do_job_control {
                    let pid = unistd::getpid();
                    let pgid = match environment.pipe_pgid {
                        Some(pgid) => Pid::from_raw(pgid as i32),
                        None => pid,
                    };
                    if let Err(_err) = unistd::setpgid(pid, pgid) {
                        // Ignore, do in parent and child.
                    }
                    if let Err(_err) = unistd::tcsetpgrp(libc::STDIN_FILENO, pgid) {
                        // Ignore, do in parent and child.
                    }
                }
                environment.eval_level = 0;
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
                let mut exit_code: i32 = 0;
                match eval(environment, exp) {
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
                        if let ExpEnum::Int(code) = &exp.get().data {
                            if *code < i32::MAX as i64 {
                                exit_code = *code as i32;
                            }
                        }
                    }
                    Ok(exp) => {
                        if let ExpEnum::Int(code) = &exp.get().data {
                            if *code < i32::MAX as i64 {
                                exit_code = *code as i32;
                            }
                        }
                    }
                    Err(_) => exit_code = 1,
                };
                if let Err(err) = reap_procs(environment) {
                    eprintln!("Error reaping procs in a pipe process: {}", err);
                }
                if let Some(ec) = environment.exit_code {
                    libc::_exit(ec);
                }
                libc::_exit(exit_code);
            }
            n => n,
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
    let pid = if pid < 0 {
        return Err(LispError::new(format!("Invalid pid in fork {}", pid)));
    } else {
        pid as u32
    };
    let job_name = fork_job_name(environment, &exp)?;
    setup_job(environment, pid, &job_name);
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

pub fn exec<I, S, P>(program: P, args: I) -> io::Error
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
    io::Error::last_os_error()
}

pub fn fork_exec<I, S>(
    environment: &mut Environment,
    stdin: Option<i32>,
    stdout: Option<i32>,
    stderr: Option<i32>,
    program: &str,
    args: I,
) -> Result<u32, LispError>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    const CLOEXEC_MSG_FOOTER: [u8; 4] = *b"NOEX";
    let (input, output) = anon_pipe()?;
    let result = unsafe { cvt(libc::fork())? };

    let pid = unsafe {
        match result {
            0 => {
                libc::close(input);
                if let Some(stdin) = stdin {
                    if let Err(err) = cvt(libc::dup2(stdin, 0)) {
                        eprintln!("Error setting up stdin (dup) in pipe: {}", err);
                        libc::_exit(10);
                    }
                    if let Err(err) = cvt(libc::close(stdin)) {
                        eprintln!("Error setting up stdin (close) in pipe: {}", err);
                        libc::_exit(10);
                    }
                }
                if let Some(stdout) = stdout {
                    if let Err(err) = cvt(libc::dup2(stdout, 1)) {
                        eprintln!("Error setting up stdout (dup) in pipe: {}", err);
                        libc::_exit(10);
                    }
                }
                if let Some(stderr) = stderr {
                    if let Err(err) = cvt(libc::dup2(stderr, 2)) {
                        eprintln!("Error setting up stderr (dup) in pipe: {}", err);
                        libc::_exit(10);
                    }
                }
                if let Some(stdout) = stdout {
                    if let Err(err) = cvt(libc::close(stdout)) {
                        eprintln!("Error setting up stdout (close) in pipe: {}", err);
                        libc::_exit(10);
                    }
                    if let Some(stderr) = stderr {
                        if stderr != stdout {
                            if let Err(err) = cvt(libc::close(stderr)) {
                                eprintln!("Error setting up stderr (close) in pipe: {}", err);
                                libc::_exit(10);
                            }
                        }
                    }
                } else if let Some(stderr) = stderr {
                    if let Err(err) = cvt(libc::close(stderr)) {
                        eprintln!("Error setting up stderr (close) in pipe: {}", err);
                        libc::_exit(10);
                    }
                }
                if environment.do_job_control {
                    let pid = unistd::getpid();
                    let pgid = match environment.pipe_pgid {
                        Some(pgid) => Pid::from_raw(pgid as i32),
                        None => pid,
                    };
                    if let Err(_err) = unistd::setpgid(pid, pgid) {
                        // Ignore, do in parent and child.
                    }
                    if let Err(_err) = unistd::tcsetpgrp(libc::STDIN_FILENO, pgid) {
                        // Ignore, do in parent and child.
                    }
                }

                let err = exec(program, args);
                let errno = err.raw_os_error().unwrap_or(libc::EINVAL) as u32;
                let errno = errno.to_be_bytes();
                let bytes = [
                    errno[0],
                    errno[1],
                    errno[2],
                    errno[3],
                    CLOEXEC_MSG_FOOTER[0],
                    CLOEXEC_MSG_FOOTER[1],
                    CLOEXEC_MSG_FOOTER[2],
                    CLOEXEC_MSG_FOOTER[3],
                ];
                // pipe I/O up to PIPE_BUF bytes should be atomic, and then
                // we want to be sure we *don't* run at_exit destructors as
                // we're being torn down regardless
                File::from_raw_fd(output).write_all(&bytes)?;
                libc::_exit(1);
            }
            n => n as u32,
        }
    };
    unsafe {
        libc::close(output);
        if let Some(stdin) = stdin {
            cvt(libc::close(stdin))?;
        }
        if let Some(stdout) = stdout {
            cvt(libc::close(stdout))?;
        }
    }
    let mut bytes = [0; 8];

    let mut input = unsafe { File::from_raw_fd(input) };
    // loop to handle EINTR
    loop {
        match input.read(&mut bytes) {
            Ok(0) => {
                setup_job(environment, pid, program);
                return Ok(pid);
            }
            Ok(8) => {
                let (errno, footer) = bytes.split_at(4);
                assert_eq!(
                    CLOEXEC_MSG_FOOTER, footer,
                    "Validation on the CLOEXEC pipe failed: {:?}",
                    bytes
                );
                let errno = i32::from_be_bytes(errno.try_into().unwrap());
                //assert!(p.wait().is_ok(), "wait() should either return Ok or panic");
                return Err(io::Error::from_raw_os_error(errno).into());
            }
            Err(ref e) if e.kind() == io::ErrorKind::Interrupted => {}
            Err(e) => {
                panic!("the CLOEXEC pipe failed: {:?}", e)
            }
            Ok(..) => {
                // pipe I/O up to PIPE_BUF bytes should be atomic
                panic!("short read on the CLOEXEC pipe")
            }
        }
    }
}
