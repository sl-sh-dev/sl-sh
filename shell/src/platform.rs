pub mod unix;

use crate::command_data::{CommandWithArgs, Run};
use crate::jobs::{Job, Jobs};
pub use crate::platform::unix::*;
use std::ffi::OsString;
use std::io;

/// Abstraction for a "platform" (for instance Unix or Windows).
pub trait Platform {
    type Pid;
    type FileDesc;
    type TermSettings;

    /// If terminal is a terminal then get it's term settings.
    fn get_term_settings(terminal: UnixFileDesc) -> Result<TermSettings, io::Error>;
    /// Restore terminal settings and put the shell back into the foreground.
    fn restore_terminal(term_settings: &TermSettings, shell_pid: UnixPid) -> Result<(), io::Error>;
    /// Put terminal in the foreground, loop until this succeeds.
    /// Used during shell startup.
    fn terminal_foreground(terminal: UnixFileDesc);
    /// Puts the running process into its own process group.
    /// Do this during shell initialization.
    fn set_self_pgroup() -> Result<(), io::Error>;
    /// Grab control of terminal.
    /// Used for shell startup.
    fn grab_terminal(terminal: UnixFileDesc) -> Result<(), io::Error>;
    /// Return the input and output file descriptors for an anonymous pipe.
    fn anon_pipe() -> Result<(UnixFileDesc, UnixFileDesc), io::Error>;
    /// Close a raw Unix file descriptor.
    fn close_fd(fd: UnixFileDesc) -> Result<(), io::Error>;

    fn fork_run(run: &Run, job: &mut Job, jobs: &mut Jobs) -> Result<(), io::Error>;
    fn fork_exec(
        command: &CommandWithArgs,
        job: &mut Job,
        jobs: &mut Jobs,
    ) -> Result<(), io::Error>;
    fn try_wait_pid(pid: UnixPid, job: &mut Job) -> (bool, Option<i32>);
    fn wait_job(job: &mut Job) -> Option<i32>;
    /// Move the job for job_num to te foreground.
    fn foreground_job(job: &mut Job, term_settings: &Option<TermSettings>)
        -> Result<(), io::Error>;
    /// Move the job for job_num to te background and running (start a stopped job in the background).
    fn background_job(job: &mut Job) -> Result<(), io::Error>;
    /// Duplicate a raw file descriptor to another file descriptor.
    fn dup2_fd(src_fd: UnixFileDesc, dst_fd: UnixFileDesc) -> Result<UnixFileDesc, io::Error>;
    /// Get the current PID.
    fn getpid() -> UnixPid;
    /// Get the current machines hostname if available.
    fn gethostname() -> Option<OsString>;
    /// Get current UID of the process.
    fn current_uid() -> u32;
    /// Get effective UID of the process.
    fn effective_uid() -> u32;
    fn is_tty(terminal: UnixFileDesc) -> bool;
}

/// Trait to turn a UnixFileDesc into another object (like File).
pub trait FromFileDesc {
    /// Constructs a new instance of Self from the given UnixFileDesc.
    /// # Safety
    /// The fd passed in must be a valid and open file descriptor.
    unsafe fn from_file_desc(fd: UnixFileDesc) -> Self;
}

pub type Pid = <Sys as Platform>::Pid;
pub type FileDesc = <Sys as Platform>::FileDesc;
pub type TermSettings = <Sys as Platform>::TermSettings;
