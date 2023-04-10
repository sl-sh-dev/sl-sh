use crate::unix::{terminal_fd, try_wait_pid, wait_job};
use nix::{
    libc,
    sys::{
        signal::{self, Signal},
        termios,
    },
    unistd::{self, Pid},
};
use std::fmt;
use std::fmt::{Display, Formatter};

/// Status of a Job.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum JobStatus {
    /// New job, not initialized.
    New,
    /// Running job.
    Running,
    /// Job is paused in background.
    Stopped,
    /// Job is done.
    Done,
}

impl fmt::Display for JobStatus {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            JobStatus::New => write!(f, "New"),
            JobStatus::Running => write!(f, "Running"),
            JobStatus::Stopped => write!(f, "Stopped"),
            JobStatus::Done => write!(f, "Done"),
        }
    }
}

/// Status of a process that is part of a job.
#[derive(Copy, Clone, Debug)]
pub enum PidStatus {
    /// Process is running, contains the pid.
    Running(i32),
    /// Process is done, contains the pid and status code.
    Done(i32, i32), // pid, status
    /// Process had an error, no status code available.  Contains the pid.
    Error(i32),
    /// Process was stopped due to a signal.  Contains the pid and signal that stopped it.
    Signaled(i32, Signal),
}

impl PidStatus {
    pub fn pid(&self) -> i32 {
        match self {
            PidStatus::Running(pid) => *pid,
            PidStatus::Done(pid, _) => *pid,
            PidStatus::Error(pid) => *pid,
            PidStatus::Signaled(pid, _) => *pid,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Job {
    id: u32,
    pgid: i32,
    pids: Vec<PidStatus>,
    names: Vec<String>,
    status: JobStatus,
    tty: bool,
}

impl Job {
    /// Create a new empty job with id.
    fn new(id: u32, tty: bool) -> Self {
        Self {
            id,
            pgid: 1,
            pids: vec![],
            names: vec![],
            status: JobStatus::New,
            tty,
        }
    }

    /// Job id/number.  Unique while job is active but will be reused once not active.
    pub fn id(&self) -> u32 {
        self.id
    }

    /// Number of processes in this job.
    pub fn len(&self) -> usize {
        self.pids.len()
    }

    /// True if this job is empty (contains no processes).
    pub fn is_empty(&self) -> bool {
        self.pids.is_empty()
    }

    /// The process group id for the job.  Should be equal to the first pid.
    pub fn pgid(&self) -> i32 {
        self.pgid
    }

    /// Pids with status that make up a job.
    pub fn pids(&self) -> &[PidStatus] {
        &self.pids[..]
    }

    /// THe names of the processes (without args) that make up the job.
    pub fn names(&self) -> &[String] {
        &self.names[..]
    }

    /// Status of this job.
    pub fn status(&self) -> JobStatus {
        self.status
    }

    /// True if this job was stated with a tty.
    pub fn is_tty(&self) -> bool {
        self.tty
    }

    /// Mark the job as stopped.
    pub fn mark_stopped(&mut self) {
        self.status = JobStatus::Stopped;
    }

    /// Mark the job as running.
    pub fn mark_running(&mut self) {
        self.status = JobStatus::Running;
    }

    /// Mark the job status to done.
    pub fn mark_done(&mut self) {
        self.status = JobStatus::Done;
    }

    /// Add pid to list of pids as Running with name.
    /// If the pid list is currently empty record this pid as the pgid (process group) for the job.
    pub fn add_process(&mut self, pid: i32, name: impl Into<String>) {
        if self.pids.is_empty() {
            self.pgid = pid;
        }
        self.pids.push(PidStatus::Running(pid));
        self.names.push(name.into());
    }

    /// Move the process pid to the done state with status.
    /// Will panic if pid is not part of job.
    pub fn process_done(&mut self, pid: i32, status: i32) {
        for proc in self.pids.iter_mut() {
            if proc.pid() == pid {
                *proc = PidStatus::Done(pid, status);
                return;
            }
        }
        panic!("process {pid} not part of job");
    }

    /// Move the process pid to the error state.
    /// Will panic if pid is not part of job.
    pub fn process_error(&mut self, pid: i32) {
        for proc in self.pids.iter_mut() {
            if proc.pid() == pid {
                *proc = PidStatus::Error(pid);
                return;
            }
        }
        panic!("process {pid} not part of job");
    }

    /// Move the process pid to the signaled state.
    /// Will panic if pid is not part of job.
    pub fn process_signaled(&mut self, pid: i32, signal: Signal) {
        for proc in self.pids.iter_mut() {
            if proc.pid() == pid {
                *proc = PidStatus::Signaled(pid, signal);
                return;
            }
        }
        panic!("process {pid} not part of job");
    }
}

impl Display for Job {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "[{}]\t{}\t{:?}\t{:?}",
            self.id, self.status, self.pids, self.names
        )
    }
}

pub struct Jobs {
    next_job: u32,
    jobs: Vec<Job>,
    job_control: bool,
    is_tty: bool,
}

impl Jobs {
    pub fn new() -> Self {
        Self {
            next_job: 0,
            jobs: vec![],
            job_control: true,
            is_tty: true,
        }
    }

    /// Create a new job with the next job number.
    /// Note, this new job is NOT in the jobs list.
    pub fn new_job(&mut self) -> Job {
        if self.jobs.is_empty() {
            self.next_job = 0;
        }
        // Job numbers/ids always start at 1.
        self.next_job += 1;
        Job::new(self.next_job, self.is_tty)
    }

    /// Push job onto the list of jobs.
    pub fn push_job(&mut self, job: Job) {
        self.jobs.push(job);
    }

    /// Are we running on a tty?
    pub fn is_tty(&self) -> bool {
        self.is_tty
    }

    /// Sets whether we are on a tty or not.
    pub fn set_tty(&mut self, is_tty: bool) {
        self.is_tty = is_tty;
    }

    /// Is job control active?  Note that Jobs will be used for bookkeeping even if job control is off.
    pub fn job_control(&self) -> bool {
        self.job_control
    }

    /// Get the job for job_id if it exists.
    pub fn get_job(&self, job_id: u32) -> Option<&Job> {
        self.jobs.iter().find(|job| job.id == job_id)
    }

    /// Get the mutable job for job_id if it exists.
    pub fn get_job_mut(&mut self, job_id: u32) -> Option<&mut Job> {
        self.jobs.iter_mut().find(|job| job.id == job_id)
    }

    /// Check any pids in a job by calling wait and updating the books.
    pub fn reap_procs(&mut self) {
        for job in self.jobs.iter_mut() {
            if let JobStatus::Running = job.status() {
                let pids: Vec<i32> = job.pids().iter().map(|s| s.pid()).collect();
                for pid in &pids {
                    try_wait_pid(*pid, job);
                }
                let mut done = true;
                for pid_status in job.pids() {
                    if let PidStatus::Running(_) = pid_status {
                        done = false;
                    }
                }
                if done {
                    job.mark_done();
                }
            }
        }
        // Remove any Done jobs.
        let jobs_len = self.jobs.len();
        for i in (0..jobs_len).rev() {
            if let JobStatus::Done = self.jobs[i].status() {
                eprintln!("{}", self.jobs[i]);
                self.jobs.remove(i);
            }
        }
    }

    /// Move the job for job_num to te foreground.
    pub fn foreground_job(&mut self, job_num: u32) {
        let job = self
            .get_job_mut(job_num)
            .expect("job number {job_num} is invalid");
        let pgid = job.pgid();
        if let JobStatus::Stopped = job.status() {
            let term_settings = termios::tcgetattr(libc::STDIN_FILENO).unwrap();
            let ppgid = Pid::from_raw(-pgid);
            if let Err(err) = signal::kill(ppgid, Signal::SIGCONT) {
                eprintln!("Error sending sigcont to wake up process: {}.", err);
            } else {
                let ppgid = Pid::from_raw(pgid);
                if let Err(err) = unistd::tcsetpgrp(libc::STDIN_FILENO, ppgid) {
                    eprintln!("Error making {pgid} foreground in parent: {err}");
                }
                job.mark_running();
                wait_job(job, Some(&term_settings), terminal_fd());
            }
        } else {
            let ppgid = Pid::from_raw(pgid);
            // The job is running, so no sig cont needed...
            let term_settings = termios::tcgetattr(libc::STDIN_FILENO).unwrap();
            if let Err(err) = unistd::tcsetpgrp(libc::STDIN_FILENO, ppgid) {
                eprintln!("Error making {pgid} foreground in parent: {err}");
            }
            wait_job(job, Some(&term_settings), terminal_fd());
        }
    }

    /// Move the job for job_num to te background and running (start a stopped job in the background).
    pub fn background_job(&mut self, job_num: u32) {
        let job = self
            .get_job_mut(job_num)
            .expect("job number {job_num} is invalid");
        let pgid = job.pgid();
        if let JobStatus::Stopped = job.status() {
            let ppgid = Pid::from_raw(-pgid);
            if let Err(err) = signal::kill(ppgid, Signal::SIGCONT) {
                eprintln!("Error sending sigcont to wake up process: {err}.");
            } else {
                job.mark_running();
            }
        }
    }
}

impl Display for Jobs {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for job in &self.jobs {
            writeln!(f, "{job}")?;
        }
        Ok(())
    }
}

impl Default for Jobs {
    fn default() -> Self {
        Self::new()
    }
}
