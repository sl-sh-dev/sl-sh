use std::collections::HashSet;
use std::convert::TryInto;
use std::ffi::{c_char, CString, OsStr, OsString};
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::{self, ErrorKind, Read, Write};
use std::os::fd::{IntoRawFd, RawFd};
use std::os::unix::ffi::OsStrExt;
use std::os::unix::io::FromRawFd;
use std::ptr;
use std::str::FromStr;

use crate::command_data::{Arg, CommandWithArgs, Run};
use crate::glob::{expand_glob, GlobOutput};
use crate::jobs::{Job, JobStatus, Jobs};
use crate::run::run_job;
use crate::signals::test_clear_sigint;
use nix::libc;
use nix::sys::signal::{self, kill, SigHandler, Signal};
use nix::sys::termios;
use nix::sys::wait::{self, WaitPidFlag, WaitStatus};
use nix::unistd::{self, Uid};

/// Process ID for the target platform.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Hash, Debug)]
pub struct Pid(i32);

impl Display for Pid {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Raw file descriptor for the target platform.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Hash, Debug)]
pub struct FileDesc(RawFd);

impl FromStr for FileDesc {
    type Err = io::Error;

    fn from_str(fd_str: &str) -> Result<Self, Self::Err> {
        match fd_str.parse::<i32>() {
            Ok(fd) => Ok(Self(fd)),
            Err(err) => Err(io::Error::new(ErrorKind::Other, err)),
        }
    }
}

/// Trait to turn a FileDesc into another object (like File).
pub trait FromFileDesc {
    /// Constructs a new instance of Self from the given FileDesc.
    /// # Safety
    /// The fd passed in must be a valid and open file descriptor.
    unsafe fn from_file_desc(fd: FileDesc) -> Self;
}

impl Display for FileDesc {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl From<File> for FileDesc {
    fn from(file: File) -> Self {
        Self(file.into_raw_fd())
    }
}
impl FromFileDesc for File {
    unsafe fn from_file_desc(fd: FileDesc) -> Self {
        File::from_raw_fd(fd.0)
    }
}

pub const STDIN_FILENO: FileDesc = FileDesc(0);
pub const STDOUT_FILENO: FileDesc = FileDesc(1);
pub const STDERR_FILENO: FileDesc = FileDesc(2);

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

/// Saved terminal settings.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TermSettings(termios::Termios);

/// If terminal is a terminal then get it's term settings.
pub fn get_term_settings(terminal: FileDesc) -> Result<TermSettings, io::Error> {
    Ok(TermSettings(termios::tcgetattr(terminal.0)?))
}

/// Restore terminal settings and put the shell back into the foreground.
pub fn restore_terminal(term_settings: &TermSettings, shell_pid: Pid) -> Result<(), io::Error> {
    termios::tcsetattr(0, termios::SetArg::TCSANOW, &term_settings.0)?;
    // XXX TODO- be more specific if the next line fails (ie only turn off tty if that is the error)?
    unistd::tcsetpgrp(0, unistd::Pid::from_raw(shell_pid.0))?;
    Ok(())
}

/// Put terminal in the foreground, loop until this succeeds.
/// Used during shell startup.
pub fn terminal_foreground(terminal: FileDesc) {
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
pub fn set_self_pgroup() -> Result<(), io::Error> {
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
pub fn grab_terminal(terminal: FileDesc) -> Result<(), io::Error> {
    /* Grab control of the terminal.  */
    let pgid = unistd::getpid();
    Ok(unistd::tcsetpgrp(terminal.0, pgid)?)
}

/// Return the input and output file descriptors for an anonymous pipe.
pub fn anon_pipe() -> Result<(FileDesc, FileDesc), io::Error> {
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
            Ok((FileDesc(fds[0]), FileDesc(fds[1])))
        } else {
            unsafe {
                cvt(libc::pipe(fds.as_mut_ptr()))?;
                cvt(libc::fcntl(fds[0], libc::F_SETFD, libc::O_CLOEXEC))?;
                cvt(libc::fcntl(fds[1], libc::F_SETFD, libc::O_CLOEXEC))?;
            }
            Ok((FileDesc(fds[0]), FileDesc(fds[1])))
        }
    }
}

/// Close a raw Unix file descriptor.
pub fn close_fd(fd: FileDesc) -> Result<(), io::Error> {
    unsafe {
        cvt(libc::close(fd.0))?;
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

unsafe fn close_extra_fds(opened: &HashSet<FileDesc>) {
    let fd_max = libc::sysconf(libc::_SC_OPEN_MAX) as i32;
    for fd in 3..fd_max {
        if !opened.contains(&FileDesc(fd)) {
            libc::close(fd);
        }
    }
}

pub fn fork_run(run: &Run, job: &mut Job, jobs: &mut Jobs) -> Result<(), io::Error> {
    let result = unsafe { cvt(libc::fork())? };
    let pid = unsafe {
        match result {
            0 => {
                setup_group_term(Pid(unistd::getpid().into()), job);

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
    setup_group_term(Pid(pid), job);
    // Close any internal FDs (from pipes for instance) in this process.
    let redir_fds = run.get_internal_fds();
    for fd in redir_fds {
        if fd > STDERR_FILENO {
            let _ = close_fd(fd);
        }
    }
    job.add_process(Pid(pid), format!("{run}"));
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
    let (FileDesc(input), FileDesc(output)) = anon_pipe()?;
    let result = unsafe { cvt(libc::fork())? };

    let pid = unsafe {
        match result {
            0 => {
                libc::close(input);
                jobs.set_no_tty();
                jobs.set_interactive(false);
                match command.process_redirects(jobs) {
                    Ok(mut fds) => {
                        fds.insert(FileDesc(output));
                        close_extra_fds(&fds);
                    }
                    Err(err) => send_error_to_parent(output, err), // This call won't return.
                }
                setup_group_term(Pid(unistd::getpid().into()), job);

                let err = exec(&program, args, jobs);
                // This call won't return.
                // If exec returns then it is an error so can unwrap it...
                send_error_to_parent(output, err.unwrap_err());
                0
            }
            n => n,
        }
    };
    setup_group_term(Pid(pid), job);
    unsafe {
        libc::close(output);
        // Close any FD for child stdio we don't care about.
        // This means any FDs we opened for pipes etc.
        let redir_fds = command.get_internal_fds();
        for fd in redir_fds {
            if fd > STDERR_FILENO {
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
                job.add_process(Pid(pid), program.to_string_lossy());
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

pub fn try_wait_pid(pid: Pid, job: &mut Job) -> (bool, Option<i32>) {
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
            job.process_signaled(Pid(pid.into()), signal as i32);
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

/// Move the job for job_num to te foreground.
pub fn foreground_job(
    job: &mut Job,
    term_settings: &Option<TermSettings>,
) -> Result<(), io::Error> {
    let pgid = job.pgid();
    if let JobStatus::Stopped = job.status() {
        let ppgid = unistd::Pid::from_raw(-pgid.0);
        signal::kill(ppgid, Signal::SIGCONT)?;
        let ppgid = unistd::Pid::from_raw(pgid.0);
        unistd::tcsetpgrp(libc::STDIN_FILENO, ppgid)?;
        job.mark_running();
        wait_job(job);
        if let Some(term_settings) = term_settings {
            restore_terminal(term_settings, job.shell_pid())?;
        }
    } else {
        let ppgid = unistd::Pid::from_raw(pgid.0);
        // The job is running, so no sig cont needed...
        unistd::tcsetpgrp(libc::STDIN_FILENO, ppgid)?;
        wait_job(job);
        if let Some(term_settings) = term_settings {
            restore_terminal(term_settings, job.shell_pid())?;
        }
    }
    Ok(())
}

/// Move the job for job_num to te background and running (start a stopped job in the background).
pub fn background_job(job: &mut Job) -> Result<(), io::Error> {
    let pgid = job.pgid();
    if let JobStatus::Stopped = job.status() {
        let ppgid = unistd::Pid::from_raw(-pgid.0);
        signal::kill(ppgid, Signal::SIGCONT)?;
        job.mark_running();
    }
    Ok(())
}

/// Duplicate a raw file descriptor to another file descriptor.
pub fn dup2_fd(src_fd: FileDesc, dst_fd: FileDesc) -> Result<FileDesc, io::Error> {
    Ok(FileDesc(unsafe { cvt(libc::dup2(src_fd.0, dst_fd.0))? }))
}

/// Get the current PID.
pub fn getpid() -> Pid {
    Pid(unistd::getpid().into())
}

/// Get the current machines hostname if available.
pub fn gethostname() -> Option<OsString> {
    nix::unistd::gethostname().ok()
}

/// Get current UID of the process.
pub fn current_uid() -> u32 {
    Uid::current().into()
}

/// Get effective UID of the process.
pub fn effective_uid() -> u32 {
    Uid::effective().into()
}

pub fn is_tty(terminal: FileDesc) -> bool {
    unistd::isatty(terminal.0).unwrap_or(false)
}

/// Setup the process group for the current pid as well term if interactive.
/// Call from both the parent and child proc to avoid race conditions.
fn setup_group_term(pid: Pid, job: &Job) {
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
