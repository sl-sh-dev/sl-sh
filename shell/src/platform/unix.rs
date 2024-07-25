use std::collections::HashSet;
use std::ffi::{c_char, CString, OsStr, OsString};
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::{self, Error, ErrorKind, Read, Write};
use std::os::fd::{AsFd, BorrowedFd, IntoRawFd, RawFd};
use std::os::unix::ffi::OsStrExt;
use std::os::unix::io::FromRawFd;
use std::ptr;
use std::str::FromStr;

use crate::command_data::{Arg, CommandWithArgs, Run};
use crate::jobs::{Job, JobStatus, Jobs};
pub use crate::platform::unix::umask::mode_t;
use crate::platform::{FromFileDesc, Platform, RLimit, RLimitVals};
use crate::run::run_job;
use crate::signals::test_clear_sigint;
use nix::libc;
use nix::sys::signal::{self, kill, SigHandler, Signal};
use nix::sys::termios;
use nix::sys::wait::{self, WaitPidFlag, WaitStatus};
use nix::unistd::{self, Uid};

mod umask;

// macos does not define __rlimit_resource_t...
#[cfg(not(any(target_env = "gnu", target_env = "uclibc")))]
pub type RlimitResource = nix::libc::c_int;
#[cfg(any(target_env = "gnu", target_env = "uclibc"))]
pub type RlimitResource = nix::libc::__rlimit_resource_t;

pub struct Sys {}
impl Platform for Sys {
    type Pid = UnixPid;
    type FileDesc = UnixFileDesc;
    type TermSettings = UnixTermSettings;

    /// If terminal is a terminal then get it's term settings.
    fn get_term_settings(terminal: UnixFileDesc) -> Result<UnixTermSettings, io::Error> {
        Ok(UnixTermSettings(termios::tcgetattr(terminal)?))
    }

    /// Restore terminal settings and put the shell back into the foreground.
    fn restore_terminal(
        term_settings: &UnixTermSettings,
        shell_pid: UnixPid,
    ) -> Result<(), io::Error> {
        termios::tcsetattr(UnixFileDesc(0), termios::SetArg::TCSANOW, &term_settings.0)?;
        // XXX TODO- be more specific if the next line fails (ie only turn off tty if that is the error)?
        unistd::tcsetpgrp(0, unistd::Pid::from_raw(shell_pid.0))?;
        Ok(())
    }

    /// Put terminal in the foreground, loop until this succeeds.
    /// Used during shell startup.
    fn terminal_foreground(terminal: UnixFileDesc) {
        /* Loop until we are in the foreground.  */
        let mut shell_pgid = unistd::getpgrp();
        while unistd::tcgetpgrp(terminal.0) != Ok(shell_pgid) {
            if let Err(err) = signal::kill(shell_pgid, Signal::SIGTTIN) {
                eprintln!("Error sending sigttin: {}.", err);
            }
            shell_pgid = unistd::getpgrp();
        }
    }

    /// Puts the running process into its own process group.
    /// Do this during shell initialization.
    fn set_self_pgroup() -> Result<(), io::Error> {
        /* Put ourselves in our own process group.  */
        let pgid = unistd::getpid();
        if let Err(err) = unistd::setpgid(pgid, pgid) {
            match err {
                nix::errno::Errno::EPERM => { /* ignore */ }
                _ => return Err(err.into()),
            }
        }
        Ok(())
    }

    /// Grab control of terminal.
    /// Used for shell startup.
    fn grab_terminal(terminal: UnixFileDesc) -> Result<(), io::Error> {
        /* Grab control of the terminal.  */
        let pgid = unistd::getpid();
        Ok(unistd::tcsetpgrp(terminal.0, pgid)?)
    }

    /// Return the input and output file descriptors for an anonymous pipe.
    fn anon_pipe() -> Result<(UnixFileDesc, UnixFileDesc), io::Error> {
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
            Ok((UnixFileDesc(fds[0]), UnixFileDesc(fds[1])))
        } else {
            unsafe {
                cvt(libc::pipe(fds.as_mut_ptr()))?;
                cvt(libc::fcntl(fds[0], libc::F_SETFD, libc::FD_CLOEXEC))?;
                cvt(libc::fcntl(fds[1], libc::F_SETFD, libc::FD_CLOEXEC))?;
            }
            Ok((UnixFileDesc(fds[0]), UnixFileDesc(fds[1])))
        }
        }
    }

    /// Close a raw Unix file descriptor.
    fn close_fd(fd: UnixFileDesc) -> Result<(), io::Error> {
        unsafe {
            cvt(libc::close(fd.0))?;
        }
        Ok(())
    }

    fn fork_run(run: &Run, job: &mut Job, jobs: &mut Jobs) -> Result<(), io::Error> {
        let result = unsafe { cvt(libc::fork())? };
        let pid = unsafe {
            match result {
                0 => {
                    setup_group_term(UnixPid(unistd::getpid().into()), job);

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
        setup_group_term(UnixPid(pid), job);
        // Close any internal FDs (from pipes for instance) in this process.
        let redir_fds = run.get_internal_fds();
        for fd in redir_fds {
            if fd > STDERR_FILENO {
                let _ = Self::close_fd(fd);
            }
        }
        job.add_process(UnixPid(pid), format!("{run}"));
        Ok(())
    }

    fn fork_exec(
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
        let (UnixFileDesc(input), UnixFileDesc(output)) = Sys::anon_pipe()?;
        let result = unsafe { cvt(libc::fork())? };

        let pid = unsafe {
            match result {
                0 => {
                    libc::close(input);
                    jobs.set_no_tty();
                    jobs.set_interactive(false);
                    match command.process_redirects(jobs) {
                        Ok(mut fds) => {
                            fds.insert(UnixFileDesc(output));
                            close_extra_fds(&fds);
                        }
                        Err(err) => send_error_to_parent(output, err), // This call won't return.
                    }
                    setup_group_term(UnixPid(unistd::getpid().into()), job);

                    let err = exec(&program, args, jobs);
                    // This call won't return.
                    // If exec returns then it is an error so can unwrap it...
                    send_error_to_parent(output, err.unwrap_err());
                    0
                }
                n => n,
            }
        };
        setup_group_term(UnixPid(pid), job);
        unsafe {
            libc::close(output);
            // Close any FD for child stdio we don't care about.
            // This means any FDs we opened for pipes etc.
            let redir_fds = command.get_internal_fds();
            for fd in redir_fds {
                if fd > STDERR_FILENO {
                    let _ = Self::close_fd(fd);
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
                    job.add_process(UnixPid(pid), program.to_string_lossy());
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
                Ok(_n) => {
                    // pipe I/O up to PIPE_BUF bytes should be atomic
                    panic!("short read on the CLOEXEC pipe")
                }
            }
        }
    }

    fn try_wait_pid(pid: UnixPid, job: &mut Job) -> (bool, Option<i32>) {
        let mut opts = WaitPidFlag::WUNTRACED;
        opts.insert(WaitPidFlag::WCONTINUED);
        opts.insert(WaitPidFlag::WNOHANG);
        match wait::waitpid(unistd::Pid::from_raw(pid.0), Some(opts)) {
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
                job.process_signaled(UnixPid(pid.into()), signal as i32);
                (true, None)
            }
            Ok(WaitStatus::Continued(_)) => (false, None),
            #[cfg(any(target_os = "linux", target_os = "android"))]
            Ok(WaitStatus::PtraceEvent(_pid, _signal, _)) => (false, None),
            #[cfg(any(target_os = "linux", target_os = "android"))]
            Ok(WaitStatus::PtraceSyscall(_pid)) => (false, None),
            Ok(WaitStatus::StillAlive) => (false, None),
        }
    }

    fn wait_job(job: &mut Job) -> Option<i32> {
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
                        if let Err(err) = kill(unistd::Pid::from_raw(-pgid.0), Signal::SIGINT) {
                            eprintln!("ERROR sending SIGINT to child process group {pgid}, {err}");
                        }
                    } else if int_cnt == 1 {
                        if let Err(err) = kill(unistd::Pid::from_raw(-pgid.0), Signal::SIGTERM) {
                            eprintln!("ERROR sending SIGTERM to child process group {pgid}, {err}");
                        }
                    } else if let Err(err) = kill(unistd::Pid::from_raw(-pgid.0), Signal::SIGKILL) {
                        eprintln!("ERROR sending SIGKILL to child process group {pgid}, {err}");
                    }
                    int_cnt += 1;
                }
                let (stop, status) = Self::try_wait_pid(pid, job);
                if stop {
                    result = status;
                    break;
                }
                std::thread::sleep(std::time::Duration::from_millis(100));
            }
        }
        result
    }

    /// Move the job for job_num to te foreground.
    fn foreground_job(
        job: &mut Job,
        term_settings: &Option<UnixTermSettings>,
    ) -> Result<(), io::Error> {
        let pgid = job.pgid();
        if let JobStatus::Stopped = job.status() {
            let ppgid = unistd::Pid::from_raw(-pgid.0);
            signal::kill(ppgid, Signal::SIGCONT)?;
            let ppgid = unistd::Pid::from_raw(pgid.0);
            unistd::tcsetpgrp(libc::STDIN_FILENO, ppgid)?;
            job.mark_running();
            Self::wait_job(job);
            if let Some(term_settings) = term_settings {
                Self::restore_terminal(term_settings, job.shell_pid())?;
            }
        } else {
            let ppgid = unistd::Pid::from_raw(pgid.0);
            // The job is running, so no sig cont needed...
            unistd::tcsetpgrp(libc::STDIN_FILENO, ppgid)?;
            Self::wait_job(job);
            if let Some(term_settings) = term_settings {
                Self::restore_terminal(term_settings, job.shell_pid())?;
            }
        }
        Ok(())
    }

    /// Move the job for job_num to te background and running (start a stopped job in the background).
    fn background_job(job: &mut Job) -> Result<(), io::Error> {
        let pgid = job.pgid();
        if let JobStatus::Stopped = job.status() {
            let ppgid = unistd::Pid::from_raw(-pgid.0);
            signal::kill(ppgid, Signal::SIGCONT)?;
            job.mark_running();
        }
        Ok(())
    }

    /// Duplicate a raw file descriptor to another file descriptor.
    fn dup2_fd(src_fd: UnixFileDesc, dst_fd: UnixFileDesc) -> Result<UnixFileDesc, io::Error> {
        Ok(UnixFileDesc(unsafe {
            cvt(libc::dup2(src_fd.0, dst_fd.0))?
        }))
    }

    /// Get the current PID.
    fn getpid() -> UnixPid {
        UnixPid(unistd::getpid().into())
    }

    /// Get the current machines hostname if available.
    fn gethostname() -> Option<OsString> {
        nix::unistd::gethostname().ok()
    }

    /// Get current UID of the process.
    fn current_uid() -> u32 {
        Uid::current().into()
    }

    /// Get effective UID of the process.
    fn effective_uid() -> u32 {
        Uid::effective().into()
    }

    fn is_tty(terminal: UnixFileDesc) -> bool {
        unistd::isatty(terminal.0).unwrap_or(false)
    }

    fn set_rlimit(rlimit: RLimit, values: RLimitVals) -> Result<(), io::Error> {
        let val = libc::rlimit {
            rlim_cur: values.current,
            rlim_max: values.max,
        };
        unsafe {
            cvt(libc::setrlimit(
                rlimit_to_c(rlimit)?,
                &val as *const libc::rlimit,
            ))?;
        }
        Ok(())
    }

    fn get_rlimit(rlimit: RLimit) -> Result<RLimitVals, io::Error> {
        let mut val = libc::rlimit {
            rlim_cur: 0,
            rlim_max: 0,
        };
        unsafe {
            cvt(libc::getrlimit(
                rlimit_to_c(rlimit)?,
                &mut val as *mut libc::rlimit,
            ))?;
        }
        Ok(RLimitVals {
            current: val.rlim_cur,
            max: val.rlim_max,
        })
    }

    fn merge_and_set_umask(current_umask: mode_t, mask_string: &str) -> Result<mode_t, Error> {
        umask::merge_and_set_umask(current_umask, mask_string)
    }

    fn get_and_clear_umask() -> mode_t {
        umask::get_and_clear_umask()
    }

    fn set_umask(umask: mode_t) -> Result<(), Error> {
        umask::set_umask(umask)
    }
}

/// Process ID for the target platform.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Hash, Debug)]
pub struct UnixPid(i32);

impl Display for UnixPid {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Raw file descriptor for the target platform.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Hash, Debug)]
pub struct UnixFileDesc(RawFd);

impl FromStr for UnixFileDesc {
    type Err = io::Error;

    fn from_str(fd_str: &str) -> Result<Self, Self::Err> {
        match fd_str.parse::<i32>() {
            Ok(fd) => Ok(Self(fd)),
            Err(err) => Err(io::Error::new(ErrorKind::Other, err)),
        }
    }
}

impl Display for UnixFileDesc {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl From<File> for UnixFileDesc {
    fn from(file: File) -> Self {
        Self(file.into_raw_fd())
    }
}
impl FromFileDesc for File {
    unsafe fn from_file_desc(fd: UnixFileDesc) -> Self {
        File::from_raw_fd(fd.0)
    }
}

impl AsFd for UnixFileDesc {
    fn as_fd(&self) -> BorrowedFd<'_> {
        unsafe { BorrowedFd::borrow_raw(self.0) }
    }
}

/// Saved terminal settings.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct UnixTermSettings(termios::Termios);

pub const STDIN_FILENO: UnixFileDesc = UnixFileDesc(0);
pub const STDOUT_FILENO: UnixFileDesc = UnixFileDesc(1);
pub const STDERR_FILENO: UnixFileDesc = UnixFileDesc(2);

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

/// An OS signal.
pub type OsSignal = i32;

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
        arg_into_args(arg.resolve_arg(jobs)?, &mut argv, &mut args_t, &mut saw_nul);
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

unsafe fn close_extra_fds(opened: &HashSet<UnixFileDesc>) {
    let fd_max = libc::sysconf(libc::_SC_OPEN_MAX) as i32;
    for fd in 3..fd_max {
        if !opened.contains(&UnixFileDesc(fd)) {
            libc::close(fd);
        }
    }
}

/// Setup the process group for the current pid as well term if interactive.
/// Call from both the parent and child proc to avoid race conditions.
fn setup_group_term(pid: UnixPid, job: &Job) {
    let pid = unistd::Pid::from_raw(pid.0);
    let pgid = if job.is_empty() {
        pid
    } else {
        unistd::Pid::from_raw(job.pgid().0)
    };
    if job.interactive() {
        if let Err(_err) = unistd::setpgid(pid, pgid) {
            // Ignore, do in parent and child.
        }
        // XXXX only if foreground
        if let Err(_err) = unistd::tcsetpgrp(libc::STDIN_FILENO, pgid) {
            // Ignore, do in parent and child.
        }
    } else {
        // If not interactive then put all procs into the shells process group.
        if let Err(_err) = unistd::setpgid(pid, unistd::Pid::from_raw(job.shell_pid().0)) {
            // Ignore, do in parent and child.
        }
    }
}

fn rlimit_to_c(rlimit: RLimit) -> Result<RlimitResource, io::Error> {
    match rlimit {
        #[cfg(any(target_os = "freebsd", target_os = "dragonfly"))]
        RLimit::SocketBufferSize => Ok(libc::RLIMIT_SBSIZE),
        #[cfg(not(any(target_os = "freebsd", target_os = "dragonfly")))]
        RLimit::SocketBufferSize => Err(Error::new(ErrorKind::Unsupported, "not on platform")),
        RLimit::CoreSize => Ok(libc::RLIMIT_CORE),
        RLimit::DataSize => Ok(libc::RLIMIT_DATA),
        #[cfg(any(target_os = "android", target_os = "linux"))]
        RLimit::Nice => Ok(libc::RLIMIT_NICE),
        #[cfg(not(any(target_os = "android", target_os = "linux")))]
        RLimit::Nice => Err(Error::new(ErrorKind::Unsupported, "not on platform")),
        RLimit::FileSize => Ok(libc::RLIMIT_FSIZE),
        #[cfg(any(target_os = "android", target_os = "linux"))]
        RLimit::SigPending => Ok(libc::RLIMIT_SIGPENDING),
        #[cfg(not(any(target_os = "android", target_os = "linux")))]
        RLimit::SigPending => Err(Error::new(ErrorKind::Unsupported, "not on platform")),
        #[cfg(target_os = "freebsd")]
        RLimit::KQueues => Ok(libc::RLIMIT_KQUEUES),
        #[cfg(not(target_os = "freebsd"))]
        RLimit::KQueues => Err(Error::new(ErrorKind::Unsupported, "not on platform")),
        #[cfg(any(
            target_os = "android",
            target_os = "freebsd",
            target_os = "openbsd",
            target_os = "linux",
            target_os = "netbsd"
        ))]
        RLimit::MemLock => Ok(libc::RLIMIT_MEMLOCK),
        #[cfg(not(any(
            target_os = "android",
            target_os = "freebsd",
            target_os = "openbsd",
            target_os = "linux",
            target_os = "netbsd"
        )))]
        RLimit::MemLock => Err(Error::new(ErrorKind::Unsupported, "not on platform")),
        #[cfg(any(
            target_os = "android",
            target_os = "freebsd",
            target_os = "openbsd",
            target_os = "linux",
            target_os = "netbsd"
        ))]
        RLimit::RSS => Ok(libc::RLIMIT_RSS),
        #[cfg(not(any(
            target_os = "android",
            target_os = "freebsd",
            target_os = "openbsd",
            target_os = "linux",
            target_os = "netbsd"
        )))]
        RLimit::RSS => Err(Error::new(ErrorKind::Unsupported, "not on platform")),
        RLimit::MaxFiles => Ok(libc::RLIMIT_NOFILE),
        //RLimit::PipeBufferSize => {}
        #[cfg(any(target_os = "android", target_os = "linux"))]
        RLimit::MessageQueueByte => Ok(libc::RLIMIT_MSGQUEUE),
        #[cfg(not(any(target_os = "android", target_os = "linux")))]
        RLimit::MessageQueueByte => Err(Error::new(ErrorKind::Unsupported, "not on platform")),
        #[cfg(any(target_os = "android", target_os = "linux"))]
        RLimit::RealTimePriority => Ok(libc::RLIMIT_RTPRIO),
        #[cfg(not(any(target_os = "android", target_os = "linux")))]
        RLimit::RealTimePriority => Err(Error::new(ErrorKind::Unsupported, "not on platform")),
        RLimit::StackSize => Ok(libc::RLIMIT_STACK),
        RLimit::CpuTime => Ok(libc::RLIMIT_CPU),
        #[cfg(any(
            target_os = "android",
            target_os = "freebsd",
            target_os = "openbsd",
            target_os = "linux",
            target_os = "netbsd"
        ))]
        RLimit::MaxProcs => Ok(libc::RLIMIT_NPROC),
        #[cfg(not(any(
            target_os = "android",
            target_os = "freebsd",
            target_os = "openbsd",
            target_os = "linux",
            target_os = "netbsd"
        )))]
        RLimit::MaxProcs => Err(Error::new(ErrorKind::Unsupported, "not on platform")),
        #[cfg(not(any(target_os = "freebsd", target_os = "netbsd", target_os = "openbsd")))]
        RLimit::MaxMemory => Ok(libc::RLIMIT_AS),
        #[cfg(target_os = "freebsd")]
        RLimit::MaxMemory => Ok(libc::RLIMIT_VMEM),
        #[cfg(any(target_os = "netbsd", target_os = "openbsd"))]
        RLimit::MaxMemory => Err(Error::new(ErrorKind::Unsupported, "not on platform")),
        #[cfg(any(target_os = "android", target_os = "linux"))]
        RLimit::MaxFileLocks => Ok(libc::RLIMIT_LOCKS),
        #[cfg(not(any(target_os = "android", target_os = "linux")))]
        RLimit::MaxFileLocks => Err(Error::new(ErrorKind::Unsupported, "not on platform")),
        #[cfg(target_os = "freebsd")]
        RLimit::MaxPtty => Ok(libc::RLIMIT_NPTS),
        #[cfg(not(target_os = "freebsd"))]
        RLimit::MaxPtty => Err(Error::new(ErrorKind::Unsupported, "not on platform")),
        #[cfg(target_os = "linux")]
        RLimit::MaxRealTime => Ok(libc::RLIMIT_RTTIME),
        #[cfg(not(any(target_os = "linux")))]
        RLimit::MaxRealTime => Err(Error::new(ErrorKind::Unsupported, "not on platform")),
        #[cfg(target_os = "linux")]
        RLimit::MaxThreads => Ok(libc::RLIMIT_NPROC),
        #[cfg(not(any(target_os = "linux")))]
        RLimit::MaxThreads => Err(Error::new(ErrorKind::Unsupported, "not on platform")),
    }
}
