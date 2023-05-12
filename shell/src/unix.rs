use std::convert::TryInto;
use std::ffi::{c_char, CString, OsStr};
use std::fs::File;
use std::io::{self, ErrorKind, Read, Write};
use std::os::unix::ffi::OsStrExt;
use std::os::unix::io::FromRawFd;
use std::ptr;

use crate::command_data::{CommandWithArgs, Run};
use crate::glob::{expand_glob, GlobOutput};
use crate::jobs::{Job, Jobs};
use crate::run_job;
use crate::signals::test_clear_sigint;
use nix::libc;
use nix::sys::signal::{self, kill, SigHandler, Signal};
use nix::sys::termios;
use nix::sys::termios::Termios;
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

fn anon_pipe() -> Result<(i32, i32), io::Error> {
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

/// Close a raw Unix file descriptor.
pub fn close_fd(fd: i32) -> Result<(), io::Error> {
    unsafe {
        cvt(libc::close(fd))?;
    }
    Ok(())
}

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

fn exec<I, S, P>(program: P, args: I) -> io::Error
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
    new_job: &[Run],
    job: &mut Job,
    jobs: &mut Jobs,
    term_settings: Termios,
    terminal_fd: i32,
) -> Result<bool, io::Error> {
    let progs = new_job.len();
    let mut fds: [i32; 2] = [0; 2];
    unsafe {
        cvt(libc::pipe(fds.as_mut_ptr()))?;
    }
    let mut next_in = Some(fds[0]);
    let mut next_out = None;
    let mut upcoming_out = fds[1];
    let mut background = false;
    for (i, program) in new_job.iter().rev().enumerate() {
        match program {
            Run::Command(command) => {
                let mut command = command.clone();
                command.push_stdin_front(next_in);
                command.push_stdout_front(next_out);
                fork_exec(&command, job)?;
            }
            Run::BackgroundCommand(command) => {
                let mut command = command.clone();
                command.push_stdin_front(next_in);
                command.push_stdout_front(next_out);
                if i == 0 {
                    background = true;
                }
                fork_exec(&command, job)?;
            }
            Run::Subshell(_) => {
                let mut program = program.clone();
                program.push_stdin_front(next_in);
                program.push_stdout_front(next_out);
                if let Run::Subshell(sub_run) = &mut program {
                    match fork_run(
                        &*sub_run,
                        job,
                        jobs,
                        term_settings.clone(),
                        terminal_fd,
                        true,
                    ) {
                        Ok(()) => {
                            restore_terminal(Some(&term_settings), terminal_fd, job);
                        }
                        Err(err) => {
                            // Make sure we restore the terminal...
                            restore_terminal(Some(&term_settings), terminal_fd, job);
                            return Err(err);
                        }
                    }
                }
            }
            Run::Pipe(_) | Run::Sequence(_) | Run::And(_) | Run::Or(_) => {
                // Don't think this is expressible with the parser and maybe should be an error?
                let mut program = program.clone();
                program.push_stdin_front(next_in);
                program.push_stdout_front(next_out);
                run_job(&program, jobs, term_settings.clone(), terminal_fd, true)?;
            }
            Run::Empty => {}
        }
        next_out = Some(upcoming_out);
        if i < (progs - 1) {
            unsafe {
                cvt(libc::pipe(fds.as_mut_ptr()))?;
            }
            upcoming_out = fds[1];
            next_in = Some(fds[0]);
        } else {
            next_in = None;
        }
    }
    if job.is_empty() {
        Err(io::Error::new(io::ErrorKind::Other, "no processes started"))
    } else {
        job.reverse();
        Ok(background)
    }
}

/// Setup the child process stdios (stdin/out/err).  Dups any handles into 0, 1, 2 and closes other
/// higher number handles in prep.
/// This is ONLY to be called shortly after forking a child process.
/// SAFETY: This is entire function is a bunch of libc calls, hence the unsafe.
/// If this function returns an err then the fork MUST notify the parent and exit.
unsafe fn setup_child_stdio(
    stdin: Option<i32>,
    stdout: Option<i32>,
    stderr: Option<i32>,
) -> Result<(), io::Error> {
    if let Some(stdin) = stdin {
        if stdin != 0 {
            // Sanity check we are not already stdin...
            cvt(libc::dup2(stdin, 0))?;
            // This would be very odd for stdin to be less than or equal to 2 but don't close if it is (?)...
            if stdin > 2 {
                cvt(libc::close(stdin))?;
            }
        }
    }
    if let Some(stdout) = stdout {
        if stdout != 1 {
            // Sanity check we are not already stdout...
            cvt(libc::dup2(stdout, 1))?;
        }
    }
    if let Some(stderr) = stderr {
        if stderr != 2 {
            // Sanity check we are not already stderr...
            cvt(libc::dup2(stderr, 2))?;
        }
    }
    if let Some(stdout) = stdout {
        if stdout > 2 {
            // Make sure we don't close a std IO by accident.
            cvt(libc::close(stdout))?;
        }
        if let Some(stderr) = stderr {
            // Don't close an existing stdio and don't double close stdout if equal stdout.
            if stderr > 2 && stderr != stdout {
                cvt(libc::close(stderr))?;
            }
        }
    } else if let Some(stderr) = stderr {
        if stderr > 2 {
            // Make sure we don't close a std IO by accident.
            cvt(libc::close(stderr))?;
        }
    }
    Ok(())
}

/// If the parent (shell) has any open FDs for child stdio then close them.
/// Log errors, we should not get any but if we do probably want to keep going- we have a child now...
unsafe fn close_parent_stdio(stdin: Option<i32>, stdout: Option<i32>, stderr: Option<i32>) {
    if let Some(stdin) = stdin {
        if stdin > 2 {
            // Don't close a stdio by accident.
            if let Err(err) = cvt(libc::close(stdin)) {
                eprintln!("Error closing child stdin ({stdin}) file descriptor: {err}");
            }
        }
    }
    // It's possible that stdout and stderr share a file descriptor so this weirdness accounts for that.
    if let Some(stdout) = stdout {
        if stdout > 2 {
            // Don't close a stdio by accident.
            if let Err(err) = cvt(libc::close(stdout)) {
                eprintln!("Error closing child stdout ({stdout}) file descriptor: {err}");
            }
        }
        if let Some(stderr) = stderr {
            if stderr > 2 && stderr != stdout {
                // Don't close a stdio by accident or double close stdout.
                if let Err(err) = cvt(libc::close(stderr)) {
                    eprintln!("Error closing child stderr ({stderr}) file descriptor: {err}");
                }
            }
        }
    } else if let Some(stderr) = stderr {
        if stderr > 2 {
            // Don't close a stdio by accident.
            if let Err(err) = cvt(libc::close(stderr)) {
                eprintln!("Error closing child stderr ({stderr}) file descriptor: {err}");
            }
        }
    }
}

fn close_parent_run_stdios(run: &Run) {
    match run {
        Run::Command(command) => {
            let (stdin, stdout, stderr) = command.stdios();
            unsafe {
                close_parent_stdio(stdin, stdout, stderr);
            }
        }
        Run::BackgroundCommand(command) => {
            let (stdin, stdout, stderr) = command.stdios();
            unsafe {
                close_parent_stdio(stdin, stdout, stderr);
            }
        }
        Run::Pipe(seq) | Run::Sequence(seq) | Run::And(seq) | Run::Or(seq) => {
            for run in seq {
                close_parent_run_stdios(run);
            }
        }
        Run::Subshell(run) => close_parent_run_stdios(run),
        Run::Empty => {}
    }
}

const CLOEXEC_MSG_FOOTER: [u8; 4] = *b"NOEX";

/// Send an error code back tot he parent from a child process indicating it failed to fork.
/// This function exits the child when done (i.e. never returns).
/// ONLY call on error in a child process.
/// SAFETY: this is doing "unsafe" libc/process stuff...
unsafe fn send_error_to_parent(output: i32, err: io::Error) {
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
    if let Err(e) = File::from_raw_fd(output).write_all(&bytes) {
        eprintln!("Error on child reporting error (err) to parent: {e}");
    }
    libc::_exit(1);
}

pub fn fork_run(
    run: &Run,
    job: &mut Job,
    jobs: &mut Jobs,
    term_settings: Termios,
    terminal_fd: i32,
    force_background: bool,
) -> Result<(), io::Error> {
    let result = unsafe { cvt(libc::fork())? };
    let pid = unsafe {
        match result {
            0 => {
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

                match run_job(run, jobs, term_settings, terminal_fd, force_background) {
                    Ok(status) => libc::_exit(status),
                    Err(e) => {
                        eprintln!("Error running subshell: {e}");
                        libc::_exit(1);
                    }
                }
            }
            n => n,
        }
    };
    close_parent_run_stdios(run);
    job.add_process(pid, format!("{run}"));
    Ok(())
}

pub fn fork_exec(command: &CommandWithArgs, job: &mut Job) -> Result<(), io::Error> {
    let (stdin, stdout, stderr) = command.stdios();
    let program = if let Some(program) = command.command() {
        program
    } else {
        return Err(io::Error::new(ErrorKind::Other, "no program to execute"));
    };
    let args = command.args_iter();
    let (input, output) = anon_pipe()?;
    let result = unsafe { cvt(libc::fork())? };

    let pid = unsafe {
        match result {
            0 => {
                libc::close(input);
                if let Err(err) = setup_child_stdio(stdin, stdout, stderr) {
                    // This call won't return.
                    send_error_to_parent(output, err);
                }
                let fd_max = libc::sysconf(libc::_SC_OPEN_MAX) as i32;
                for fd in 4..fd_max {
                    libc::close(fd);
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
                // This call won't return.
                send_error_to_parent(output, err);
                0
            }
            n => n,
        }
    };
    unsafe {
        libc::close(output);
        // Close any FD for child stdio we don't care about.
        close_parent_stdio(stdin, stdout, stderr);
    }
    let mut bytes = [0; 8];

    // Wait for a status message or closed pipe from the child.
    let mut input = unsafe { File::from_raw_fd(input) };
    // loop to handle EINTR
    loop {
        match input.read(&mut bytes) {
            Ok(0) => {
                job.add_process(pid, program.to_string_lossy());
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

/// Duplicate a raw file descriptor.
pub fn dup_fd(fd: i32) -> Result<i32, io::Error> {
    unsafe { cvt(libc::dup(fd)) }
}

/// Make an anon pipe, (read, write).
pub fn pipe() -> Result<(i32, i32), io::Error> {
    let mut fds: [i32; 2] = [0; 2];
    unsafe {
        cvt(libc::pipe(fds.as_mut_ptr()))?;
    }
    Ok((fds[0], fds[1]))
}
