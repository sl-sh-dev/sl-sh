mod unix;

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
    fn get_term_settings(terminal: FileDesc) -> Result<TermSettings, io::Error>;
    /// Restore terminal settings and put the shell back into the foreground.
    fn restore_terminal(term_settings: &TermSettings, shell_pid: Pid) -> Result<(), io::Error>;
    /// Put terminal in the foreground, loop until this succeeds.
    /// Used during shell startup.
    fn terminal_foreground(terminal: FileDesc);
    /// Puts the running process into its own process group.
    /// Do this during shell initialization.
    fn set_self_pgroup() -> Result<(), io::Error>;
    /// Grab control of terminal.
    /// Used for shell startup.
    fn grab_terminal(terminal: FileDesc) -> Result<(), io::Error>;
    /// Return the input and output file descriptors for an anonymous pipe.
    fn anon_pipe() -> Result<(FileDesc, FileDesc), io::Error>;
    /// Close a raw file descriptor.
    fn close_fd(fd: FileDesc) -> Result<(), io::Error>;

    fn fork_run(run: &Run, job: &mut Job, jobs: &mut Jobs) -> Result<(), io::Error>;
    fn fork_exec(
        command: &CommandWithArgs,
        job: &mut Job,
        jobs: &mut Jobs,
    ) -> Result<(), io::Error>;
    fn try_wait_pid(pid: Pid, job: &mut Job) -> (bool, Option<i32>);
    fn wait_job(job: &mut Job) -> Option<i32>;
    /// Move the job for job_num to te foreground.
    fn foreground_job(job: &mut Job, term_settings: &Option<TermSettings>)
    -> Result<(), io::Error>;
    /// Move the job for job_num to te background and running (start a stopped job in the background).
    fn background_job(job: &mut Job) -> Result<(), io::Error>;
    /// Duplicate a raw file descriptor to another file descriptor.
    fn dup2_fd(src_fd: FileDesc, dst_fd: FileDesc) -> Result<FileDesc, io::Error>;
    /// Get the current PID.
    fn getpid() -> Pid;
    /// Get the current machines hostname if available.
    fn gethostname() -> Option<OsString>;
    /// Get current UID of the process.
    fn current_uid() -> u32;
    /// Get effective UID of the process.
    fn effective_uid() -> u32;
    fn is_tty(terminal: FileDesc) -> bool;

    fn set_rlimit(rlimit: RLimit, values: RLimitVals) -> Result<(), io::Error>;
    fn get_rlimit(rlimit: RLimit) -> Result<RLimitVals, io::Error>;

    // umask operations, these can be NoOps if platform does not have umasks.
    /// If mask_string is a mode string then merge it with umask and set the current umask.
    /// If mask_string is an int then treat it as a umask and set the current umask (no merge)).
    fn merge_and_set_umask(current_umask: mode_t, mask_string: &str) -> Result<mode_t, io::Error>;
    /// Clears the current umask and returns the previous umask.
    fn get_and_clear_umask() -> mode_t;
    /// Set current umask to umask.
    fn set_umask(umask: mode_t) -> Result<(), io::Error>;
    /// Convert mode to the octal string umask format.
    fn to_octal_string(mode: mode_t) -> Result<String, io::Error> {
        let mut octal = format!("{:o}", mode);
        if octal.len() < 4 {
            while octal.len() < 4 {
                octal = "0".to_owned() + &octal;
            }
            Ok(octal)
        } else {
            let msg = format!("encountered invalid umask {octal}.");
            Err(io::Error::other(msg))
        }
    }
}

pub type Pid = <Sys as Platform>::Pid;
pub type FileDesc = <Sys as Platform>::FileDesc;
pub type TermSettings = <Sys as Platform>::TermSettings;

/// Trait to turn a UnixFileDesc into another object (like File).
pub trait FromFileDesc {
    /// Constructs a new instance of Self from the given UnixFileDesc.
    /// # Safety
    /// The fd passed in must be a valid and open file descriptor.
    unsafe fn from_file_desc(fd: FileDesc) -> Self;
}

/// Holder for setting or getting rlimits (get/setrlimit).
#[derive(Copy, Clone, Debug)]
pub struct RLimitVals {
    pub current: u64,
    pub max: u64,
}

/// Abstraction over the various rlimits.
pub enum RLimit {
    ///-b: The maximum socket buffer size. RLIMIT_SBSIZE (freebsd, dragonfly)
    SocketBufferSize,
    /// -c; The maximum size of core files created. RLIMIT_CORE
    /// The maximum size core file that this process can create. If the process terminates and would
    /// dump a core file larger than this, then no core file is created. So setting this limit to zero
    /// prevents core files from ever being created.
    CoreSize,
    /// -d: The maximum size of a process’s data segment. RLIMIT_DATA
    /// The maximum size of data memory for the process. If the process tries to allocate data memory
    /// beyond this amount, the allocation function fails.
    DataSize,
    /// -e: The maximum scheduling priority ("nice"). RLIMIT_NICE
    /// Specifies a ceiling to which the process's nice value can be raised using setpriority(2) or
    /// nice(2). The actual ceiling for the nice value is calculated as 20 - rlim_cur. (This
    /// strangeness occurs because negative numbers cannot be specified as resource limit values,
    /// since they typically have special meanings. For example, RLIM_INFINITY typically is the
    /// same as -1.)
    Nice,
    /// -f: The maximum size of files written by the shell and its children. RLIMIT_FSIZE
    /// The maximum size of file the process can create. Trying to write a larger file causes a signal: SIGXFSZ.
    FileSize,
    /// -i: The maximum number of pending signals.RLIMIT_SIGPENDING
    /// Specifies the limit on the number of signals that may be queued for the real user ID of the
    /// calling process. Both standard and real-time signals are counted for the purpose of checking
    /// this limit. However, the limit is only enforced for sigqueue(3); it is always possible to use
    /// kill(2) to queue one instance of any of the signals that are not already queued to the process.
    SigPending,
    /// -k: The maximum number of kqueues that may be allocated. RLIMIT_KQUEUES (freebsd)
    KQueues,
    /// -l: The maximum size that may be locked into memory. RLIMIT_MEMLOCK,
    /// The maximum size (in bytes) which a process may lock into memory
    /// using the mlock(2) system call.
    MemLock,
    /// -m: The maximum resident set size (many systems do not honor this limit). RLIMIT_RSS
    /// When there is memory pressure and swap is available, prioritize
    /// eviction of a process' resident pages beyond this amount (in bytes).
    RSS,
    /// -n: The maximum number of open file descriptors (most systems do not allow this value to be set). RLIMIT_NOFILE
    MaxFiles,
    // -p: The pipe buffer size.
    //PipeBufferSize,
    /// -q: The maximum number of bytes in POSIX message queues. RLIMIT_MSGQUEUE
    /// A limit on the number of bytes that can be allocated for POSIX
    /// message queues  for  the  real  user  ID  of  the  calling process.
    MessageQueueByte,
    /// -r: The maximum real-time scheduling priority. RLIMIT_RTPRIO
    RealTimePriority,
    /// -s: The maximum stack size. RLIMIT_STACK
    StackSize,
    /// -t: The maximum amount of cpu time in seconds. RLIMIT_CPU
    CpuTime,
    /// -u: The maximum number of processes available to a single user. RLIMIT_NPROC
    MaxProcs,
    /// -v: The maximum amount of virtual memory available to the shell, and, on some systems, to its children. RLIMIT_AS
    MaxMemory,
    /// -x: The maximum number of file locks. RLIMIT_LOCKS
    MaxFileLocks,
    /// -P: The maximum number of pseudoterminals. RLIMIT_NPTS
    MaxPtty,
    /// -R: The maximum time a real-time process can run before blocking, in microseconds. RLIMIT_RTTIME
    MaxRealTime,
    /// -T: The maximum number of threads. RLIMIT_NPROC (Same as MaxProcs- Linux)
    MaxThreads,
}
