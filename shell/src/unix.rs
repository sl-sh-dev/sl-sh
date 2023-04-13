use std::convert::TryInto;
use std::ffi::{c_char, CString, OsStr};
use std::fs::File;
use std::io::{self, Read, Write};
use std::os::unix::ffi::OsStrExt;
use std::os::unix::io::FromRawFd;
use std::ptr;

use crate::glob::{expand_glob, GlobOutput};
use crate::jobs::Job;
use crate::parse::ParsedJob;
use crate::signals::test_clear_sigint;
use nix::libc;
use nix::sys::signal::{self, kill, SigHandler, Signal};
use nix::sys::termios;
use nix::sys::wait::{self, WaitPidFlag, WaitStatus};
use nix::unistd::{self, Pid};

trait IsMinusOne {
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

fn cvt<T: IsMinusOne>(t: T) -> Result<T, io::Error> {
    if t.is_minus_one() {
        Err(io::Error::last_os_error())
    } else {
        Ok(t)
    }
}

pub fn anon_pipe() -> Result<(i32, i32), io::Error> {
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
            unsafe {
                cvt(libc::pipe(fds.as_mut_ptr()))?;
                cvt(libc::fcntl(fds[0], libc::F_SETFD, libc::O_CLOEXEC))?;
                cvt(libc::fcntl(fds[1], libc::F_SETFD, libc::O_CLOEXEC))?;
            }
            Ok((fds[0], fds[1]))
        }
    }
}

/*
fn replace_fd(new_fd: i32, fd: i32) -> Result<i32, io::Error> {
    Ok(unsafe {
        let old = cvt(libc::dup(fd))?;
        cvt(libc::dup2(new_fd, fd))?;
        cvt(libc::close(new_fd))?;
        old
    })
}

fn replace_stdin(new_stdin: i32) -> Result<i32, io::Error> {
    replace_fd(new_stdin, 0)
}

fn replace_stdout(new_stdout: i32) -> Result<i32, io::Error> {
    replace_fd(new_stdout, 1)
}

fn replace_stderr(new_stderr: i32) -> Result<i32, io::Error> {
    replace_fd(new_stderr, 2)
}

fn dup_stdin(new_stdin: i32) -> Result<(), io::Error> {
    unsafe {
        cvt(libc::dup2(new_stdin, 0))?;
        cvt(libc::close(new_stdin))?;
    }
    Ok(())
}

fn dup_stdout(new_stdout: i32) -> Result<(), io::Error> {
    unsafe {
        cvt(libc::dup2(new_stdout, 1))?;
        cvt(libc::close(new_stdout))?;
    }
    Ok(())
}

fn dup_stderr(new_stderr: i32) -> Result<(), io::Error> {
    unsafe {
        cvt(libc::dup2(new_stderr, 2))?;
        cvt(libc::close(new_stderr))?;
    }
    Ok(())
}

fn close_fd(fd: i32) -> Result<(), io::Error> {
    unsafe {
        cvt(libc::close(fd))?;
    }
    Ok(())
}

fn fd_to_file(fd: i32) -> File {
    unsafe { File::from_raw_fd(fd) }
}
 */

fn os2c(s: &OsStr, saw_nul: &mut bool) -> CString {
    CString::new(s.as_bytes()).unwrap_or_else(|_e| {
        *saw_nul = true;
        CString::new("<string-with-nul>").unwrap()
    })
}

fn arg_into_args<S: AsRef<OsStr>>(
    arg: S,
    argv: &mut Vec<*const c_char>,
    args_t: &mut Vec<CString>,
    saw_nul: &mut bool,
) {
    // Overwrite the trailing NULL pointer in `argv` and then add a new null
    // pointer.
    let arg = os2c(arg.as_ref(), saw_nul);
    argv[args_t.len()] = arg.as_ptr();
    argv.push(ptr::null());
    // Also make sure we keep track of the owned value to schedule a
    // destructor for this memory.
    args_t.push(arg);
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
        match expand_glob(arg.as_ref()) {
            GlobOutput::Arg(arg) => arg_into_args(arg, &mut argv, &mut args_t, &mut saw_nul),
            GlobOutput::Args(args) => {
                for arg in args {
                    arg_into_args(arg, &mut argv, &mut args_t, &mut saw_nul)
                }
            }
        }
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

pub fn fork_pipe(
    stdin: Option<i32>,
    stdout: Option<i32>,
    stderr: Option<i32>,
    new_job: ParsedJob,
    job: &mut Job,
) -> Result<(), io::Error> {
    let progs = new_job.len();
    let mut fds: [i32; 2] = [0; 2];
    unsafe {
        cvt(libc::pipe(fds.as_mut_ptr()))?;
    }
    let mut next_in = stdin;
    let mut next_out = Some(fds[1]);
    let mut upcoming_in = Some(fds[0]);
    for (i, program) in new_job.commands().iter().enumerate() {
        if let Some(command) = program.command() {
            let args = program.args();
            if i == (progs - 1) {
                fork_exec(next_in, stdout, stderr, command, args, job)?;
            } else {
                fork_exec(next_in, next_out, None, command, args, job)?;
                next_in = upcoming_in;
                if i < (progs - 1) {
                    unsafe {
                        cvt(libc::pipe(fds.as_mut_ptr()))?;
                    }
                    next_out = Some(fds[1]);
                    upcoming_in = Some(fds[0]);
                }
            }
        }
    }
    if job.is_empty() {
        Err(io::Error::new(io::ErrorKind::Other, "no processes execed"))
    } else {
        Ok(())
    }
}

pub fn fork_exec<I, S>(
    stdin: Option<i32>,
    stdout: Option<i32>,
    stderr: Option<i32>,
    program: &str,
    args: I,
    job: &mut Job,
) -> Result<(), io::Error>
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
                //XXXX if jobs.job_control() {
                let pid = unistd::getpid();
                let pgid = if job.is_empty() {
                    pid
                } else {
                    Pid::from_raw(job.pgid())
                };
                if let Err(_err) = unistd::setpgid(pid, pgid) {
                    // Ignore, do in parent and child.
                }
                // XXXX only if foreground
                if let Err(_err) = unistd::tcsetpgrp(libc::STDIN_FILENO, pgid) {
                    // Ignore, do in parent and child.
                }
                //}

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
            n => n,
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
                job.add_process(pid, program);
                return Ok(());
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
                return Err(io::Error::from_raw_os_error(errno));
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

pub fn try_wait_pid(pid: i32, job: &mut Job) -> (bool, Option<i32>) {
    let mut opts = WaitPidFlag::WUNTRACED;
    opts.insert(WaitPidFlag::WCONTINUED);
    opts.insert(WaitPidFlag::WNOHANG);
    match wait::waitpid(Pid::from_raw(pid), Some(opts)) {
        Err(nix::errno::Errno::ECHILD) => {
            // Does not exist.
            (true, None)
        }
        Err(err) => {
            eprintln!("Error waiting for pid {pid}, {err}");
            job.process_error(pid);
            (true, None)
        }
        Ok(WaitStatus::Exited(_, status)) => {
            job.process_done(pid, status);
            (true, Some(status))
        }
        Ok(WaitStatus::Stopped(..)) => {
            job.mark_stopped();
            (true, None)
        }
        Ok(WaitStatus::Signaled(pid, signal, _core_dumped)) => {
            job.process_signaled(pid.into(), signal);
            (true, None)
        }
        Ok(WaitStatus::Continued(_)) => (false, None),
        Ok(WaitStatus::PtraceEvent(_pid, _signal, _)) => (false, None),
        Ok(WaitStatus::PtraceSyscall(_pid)) => (false, None),
        Ok(WaitStatus::StillAlive) => (false, None),
    }
}

pub fn wait_job(
    job: &mut Job,
    term_settings: Option<&termios::Termios>,
    terminal_fd: i32,
) -> Option<i32> {
    let mut result: Option<i32> = None;
    let mut int_cnt = 0;
    let pgid = job.pgid();
    let mut i = 0;
    while let Some(pid) = job.pids().get(i) {
        i += 1;
        let pid = pid.pid();
        loop {
            if test_clear_sigint() {
                if int_cnt == 0 {
                    if let Err(err) = kill(Pid::from_raw(-pgid), Signal::SIGINT) {
                        eprintln!("ERROR sending SIGINT to child process group {pgid}, {err}");
                    }
                } else if int_cnt == 1 {
                    if let Err(err) = kill(Pid::from_raw(-pgid), Signal::SIGTERM) {
                        eprintln!("ERROR sending SIGTERM to child process group {pgid}, {err}");
                    }
                } else if let Err(err) = kill(Pid::from_raw(-pgid), Signal::SIGKILL) {
                    eprintln!("ERROR sending SIGKILL to child process group {pgid}, {err}");
                }
                int_cnt += 1;
            }
            let (stop, status) = try_wait_pid(pid, job);
            if stop {
                result = status;
                break;
            }
            std::thread::sleep(std::time::Duration::from_millis(100));
        }
    }
    restore_terminal(term_settings, terminal_fd, job);
    result
}

pub fn restore_terminal(term_settings: Option<&termios::Termios>, terminal_fd: i32, job: &Job) {
    // If we were given terminal settings restore them.
    if let Some(settings) = term_settings {
        if let Err(err) = termios::tcsetattr(terminal_fd, termios::SetArg::TCSANOW, settings) {
            eprintln!("Error resetting shell terminal settings: {}", err);
        }
    }
    // Move the shell back into the foreground.
    if job.is_tty() {
        let pid = unistd::getpid();
        if let Err(err) = unistd::tcsetpgrp(terminal_fd, pid) {
            // XXX TODO- be more specific with this (ie only turn off tty if that is the error)?
            eprintln!(
                "Error making shell (stop pretending to be a tty?) {} foreground: {}",
                pid, err
            );
        }
    }
}

pub fn terminal_fd() -> i32 {
    if let Ok(fd) = unsafe { cvt(libc::dup(0)) } {
        fd
    } else {
        0
    }
}
