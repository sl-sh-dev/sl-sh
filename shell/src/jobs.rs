use crate::command_data::Run;
use crate::parse::parse_line;
use crate::platform::{OsSignal, Pid, Platform, Sys, TermSettings, STDIN_FILENO};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::{fmt, io};

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
    Running(Pid),
    /// Process is done, contains the pid and status code.
    Done(Pid, i32), // pid, status
    /// Process had an error, no status code available.  Contains the pid.
    Error(Pid),
    /// Process was stopped due to a signal.  Contains the pid and signal that stopped it.
    Signaled(Pid, OsSignal),
}

impl PidStatus {
    pub fn pid(&self) -> Pid {
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
    shell_pid: Pid,
    pgid: Pid,
    pids: Vec<PidStatus>,
    names: Vec<String>,
    status: JobStatus,
    interactive: bool,
    stealth: bool, // If true don't report when background job ends.
}

impl Job {
    /// Create a new empty job with id.
    fn new(id: u32, shell_pid: Pid, interactive: bool) -> Self {
        Self {
            id,
            shell_pid,
            pgid: shell_pid,
            pids: vec![],
            names: vec![],
            status: JobStatus::New,
            interactive,
            stealth: false,
        }
    }

    /// True if this job is empty (contains no processes).
    pub fn is_empty(&self) -> bool {
        self.pids.is_empty()
    }

    /// The process group id for the job.  Should be equal to the first pid.
    pub fn pgid(&self) -> Pid {
        self.pgid
    }

    /// Pids with status that make up a job.
    pub fn pids(&self) -> &[PidStatus] {
        &self.pids[..]
    }

    /// Status of this job.
    pub fn status(&self) -> JobStatus {
        self.status
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
    pub fn add_process(&mut self, pid: Pid, name: impl Into<String>) {
        if self.pids.is_empty() {
            self.pgid = pid;
        }
        self.pids.push(PidStatus::Running(pid));
        self.names.push(name.into());
    }

    /// Move the process pid to the done state with status.
    /// Will panic if pid is not part of job.
    pub fn process_done(&mut self, pid: Pid, status: i32) {
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
    pub fn process_error(&mut self, pid: Pid) {
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
    pub fn process_signaled(&mut self, pid: Pid, signal: OsSignal) {
        for proc in self.pids.iter_mut() {
            if proc.pid() == pid {
                *proc = PidStatus::Signaled(pid, signal);
                return;
            }
        }
        panic!("process {pid} not part of job");
    }

    /// Reverse the list of pids in the job.
    pub fn reverse(&mut self) {
        self.pids.reverse();
        self.names.reverse();
    }

    /// Set the stealth flag, if true then do NOT print out when a background job ends (be stealthy...).
    pub fn set_stealth(&mut self, stealth: bool) {
        self.stealth = stealth;
    }

    /// Is this a stealth job (do not report when complete if in the background)?
    pub fn stealth(&self) -> bool {
        self.stealth
    }

    /// Is this job running in an interactive shell?
    pub fn interactive(&self) -> bool {
        self.interactive
    }

    /// The PID of the shell running this job.
    pub fn shell_pid(&self) -> Pid {
        self.shell_pid
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
    shell_pid: Pid,
    interactive: bool,
    term_settings: Option<TermSettings>,
    alias: HashMap<String, Run>,
}

impl Jobs {
    pub fn new(interactive: bool) -> Self {
        let shell_pid = Sys::getpid();
        let term_settings = if interactive {
            if let Ok(term_setting) = Sys::get_term_settings(STDIN_FILENO) {
                Some(term_setting)
            } else {
                None
            }
        } else {
            None
        };
        Self {
            next_job: 0,
            jobs: vec![],
            shell_pid,
            interactive,
            term_settings,
            alias: HashMap::new(),
        }
    }

    pub fn cap_term(&mut self) {
        self.term_settings = if let Ok(term_setting) = Sys::get_term_settings(STDIN_FILENO) {
            Some(term_setting)
        } else {
            None
        };
    }

    /// Create a new job with the next job number.
    /// Note, this new job is NOT in the jobs list.
    pub fn new_job(&mut self) -> Job {
        if self.jobs.is_empty() {
            self.next_job = 0;
        }
        // Job numbers/ids always start at 1.
        self.next_job += 1;
        Job::new(self.next_job, self.shell_pid, self.interactive)
    }

    /// Push job onto the list of jobs.
    pub fn push_job(&mut self, job: Job) {
        self.jobs.push(job);
    }

    /// Set not on a tty.
    pub fn set_no_tty(&mut self) {
        self.term_settings = None;
    }

    /// Sets whether we are interactive or not.
    pub fn set_interactive(&mut self, interactive: bool) {
        self.interactive = interactive;
    }

    /// Get the mutable job for job_id if it exists.
    pub fn get_job_mut(&mut self, job_id: u32) -> Option<&mut Job> {
        self.jobs.iter_mut().find(|job| job.id == job_id)
    }

    /// Check any pids in a job by calling wait and updating the books.
    pub fn reap_procs(&mut self) {
        for job in self.jobs.iter_mut() {
            if let JobStatus::Running = job.status() {
                let pids: Vec<Pid> = job.pids().iter().map(|s| s.pid()).collect();
                for pid in &pids {
                    Sys::try_wait_pid(*pid, job);
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
                if !self.jobs[i].stealth() {
                    eprintln!("{}", self.jobs[i]);
                }
                self.jobs.remove(i);
            }
        }
    }

    /// Move the job for job_num to te foreground.
    pub fn foreground_job(&mut self, job_num: u32) {
        let term_settings = self.term_settings.clone();
        if let Some(job) = self.get_job_mut(job_num) {
            if let Err(err) = Sys::foreground_job(job, &term_settings) {
                eprintln!("Error making job {job_num} foreground in parent: {err}");
            }
        } else {
            eprintln!("job number {job_num} is invalid");
        }
    }

    /// Move the job for job_num to te background and running (start a stopped job in the background).
    pub fn background_job(&mut self, job_num: u32) {
        if let Some(job) = self.get_job_mut(job_num) {
            if let Err(err) = Sys::background_job(job) {
                eprintln!("Error making job {job_num} background in parent: {err}");
            }
        } else {
            eprintln!("job number {job_num} is invalid");
        }
    }

    /// Restore terminal settings and put the shell back into the foreground.
    pub fn restore_terminal(&self) {
        // This assumes file descriptor 0 (STDIN) is the terminal.
        // Move the shell back into the foreground.
        if let Some(term_settings) = &self.term_settings {
            // Restore terminal settings.
            if let Err(err) = Sys::restore_terminal(term_settings, self.shell_pid) {
                eprintln!("Error resetting shell terminal settings: {}", err);
            }
        }
    }

    /// Gets an alias.
    pub fn get_alias<S: AsRef<str>>(&self, name: S) -> Option<Run> {
        self.alias.get(name.as_ref()).cloned()
    }

    /// Clears all the existing aliases.
    pub fn clear_aliases(&mut self) {
        self.alias.clear();
    }

    /// Removes the alias name.
    pub fn remove_alias<S: AsRef<str>>(&mut self, name: S) {
        if self.alias.remove(name.as_ref()).is_none() {
            eprintln!("not found {}", name.as_ref());
        }
    }

    /// Add an alias.
    pub fn add_alias(&mut self, name: String, value: String) -> Result<(), io::Error> {
        let runj = parse_line(&value)?;
        self.alias.insert(name, runj.commands().clone());
        Ok(())
    }

    /// Print a the alias for name if set.
    pub fn print_alias(&self, name: String) {
        if let Some(value) = self.alias.get(&name) {
            println!("alias {name}='{value}'");
        } else {
            eprintln!("no alias set for: {name}");
        }
    }

    /// Print all the defined aliases.
    pub fn print_all_alias(&self) {
        for (name, value) in &self.alias {
            println!("alias {name}='{value}'");
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
