use std::collections::HashSet;
use std::convert::TryInto;
use std::ffi::{c_char, CString, OsStr};
use std::fs::File;
use std::io::{self, ErrorKind, Read, Write};
use std::os::unix::ffi::OsStrExt;
use std::os::unix::io::FromRawFd;
use std::ptr;

use crate::command_data::{Arg, CommandWithArgs, Run};
use crate::glob::{expand_glob, GlobOutput};
use crate::jobs::{Job, Jobs};
use crate::run_job;
use crate::signals::test_clear_sigint;
use nix::libc;
use nix::sys::signal::{self, kill, SigHandler, Signal};
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

/// Return the input and output file descriptors for an anonymous pipe.
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

fn exec<'arg, I, P>(program: P, args: I, jobs: &mut Jobs) -> Result<(), io::Error>
where
    I: IntoIterator<Item = &'arg Arg>,
    P: AsRef<OsStr>,
{
    let mut saw_nul = false;
    let program = os2c(program.as_ref(), &mut saw_nul);
    let mut argv = vec![program.as_ptr(), ptr::null()];
    let mut args_t = vec![program.clone()];
    for arg in args {
        match expand_glob(arg.resolve_arg(jobs)?) {
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
    Err(io::Error::last_os_error())
}

pub fn fork_pipe(new_job: &[Run], job: &mut Job, jobs: &mut Jobs) -> Result<bool, io::Error> {
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
                fork_exec(&command, job, jobs)?;
            }
            Run::BackgroundCommand(command) => {
                let mut command = command.clone();
                command.push_stdin_front(next_in);
                command.push_stdout_front(next_out);
                if i == 0 {
                    background = true;
                }
                fork_exec(&command, job, jobs)?;
            }
            Run::Subshell(_) => {
                let mut program = program.clone();
                program.push_stdin_front(next_in);
                program.push_stdout_front(next_out);
                if let Run::Subshell(sub_run) = &mut program {
                    match fork_run(&*sub_run, job, jobs) {
                        Ok(()) => {
                            jobs.restore_terminal();
                        }
                        Err(err) => {
                            // Make sure we restore the terminal...
                            jobs.restore_terminal();
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
                run_job(&program, jobs, true)?;
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

unsafe fn close_extra_fds(opened: &HashSet<i32>) {
    let fd_max = libc::sysconf(libc::_SC_OPEN_MAX) as i32;
    for fd in 3..fd_max {
        if !opened.contains(&fd) {
            libc::close(fd);
        }
    }
}

pub fn fork_run(run: &Run, job: &mut Job, jobs: &mut Jobs) -> Result<(), io::Error> {
    let result = unsafe { cvt(libc::fork())? };
    let pid = unsafe {
        match result {
            0 => {
                job.setup_group_term(unistd::getpid().into());

                let redir_fds = run.get_internal_fds();
                close_extra_fds(&redir_fds);
                jobs.set_interactive(false);
                jobs.set_no_tty();
                match run_job(run, jobs, false) {
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
    job.setup_group_term(pid);
    // Close any internal FDs (from pipes for instance) in this process.
    let redir_fds = run.get_internal_fds();
    for fd in redir_fds {
        if fd > 3 {
            let _ = close_fd(fd);
        }
    }
    job.add_process(pid, format!("{run}"));
    Ok(())
}

pub fn fork_exec(
    command: &CommandWithArgs,
    job: &mut Job,
    jobs: &mut Jobs,
) -> Result<(), io::Error> {
    let program = if let Some(program) = command.command(jobs) {
        program?
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
                jobs.set_no_tty();
                jobs.set_interactive(false);
                match command.process_redirects(jobs) {
                    Ok(fds) => {
                        close_extra_fds(&fds);
                    }
                    Err(err) => send_error_to_parent(output, err), // This call won't return.
                }
                job.setup_group_term(unistd::getpid().into());

                let err = exec(&program, args, jobs);
                // This call won't return.
                // If exec returns then it is an error so can unwrap it...
                send_error_to_parent(output, err.unwrap_err());
                0
            }
            n => n,
        }
    };
    job.setup_group_term(pid);
    unsafe {
        libc::close(output);
        // Close any FD for child stdio we don't care about.
        // This means any FDs we opened for pipes etc.
        let redir_fds = command.get_internal_fds();
        for fd in redir_fds {
            if fd > 3 {
                let _ = close_fd(fd);
            }
        }
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

pub fn wait_job(job: &mut Job) -> Option<i32> {
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
    result
}

/// Duplicate a raw file descriptor to another file descriptor.
pub fn dup2_fd(src_fd: i32, dst_fd: i32) -> Result<i32, io::Error> {
    unsafe { cvt(libc::dup2(src_fd, dst_fd)) }
}

/// Make an anon pipe, (read, write).
pub fn pipe() -> Result<(i32, i32), io::Error> {
    let mut fds: [i32; 2] = [0; 2];
    unsafe {
        cvt(libc::pipe(fds.as_mut_ptr()))?;
    }
    Ok((fds[0], fds[1]))
}
