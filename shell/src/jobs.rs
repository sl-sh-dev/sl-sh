use crate::unix::{terminal_fd, try_wait_pid, wait_pid};
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum JobStatus {
    Running,
    Stopped,
}

impl fmt::Display for JobStatus {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            JobStatus::Running => write!(f, "Running"),
            JobStatus::Stopped => write!(f, "Stopped"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Job {
    pub id: u32,
    pub pids: Vec<u32>,
    pub names: Vec<String>,
    pub status: JobStatus,
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
            next_job: 1,
            jobs: vec![],
            job_control: true,
            is_tty: true,
        }
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

    /// Add proc to the job list with name command.  If pgid is Some then add a new proc to the last
    /// job (for pipeline bookkeeping).
    pub fn add_proc(&mut self, proc: u32, command: &str, pgid: Option<u32>) {
        let pid = Pid::from_raw(proc as i32);
        let pgid_raw = match pgid {
            Some(pgid) => Pid::from_raw(pgid as i32),
            None => Pid::from_raw(proc as i32),
        };
        if pgid.is_none() {
            if self.jobs.is_empty() {
                self.next_job = 1;
            }
            let mut job = Job {
                id: self.next_job,
                pids: Vec::new(),
                names: Vec::new(),
                status: JobStatus::Running,
            };
            self.next_job += 1;
            job.pids.push(proc);
            job.names.push(command.to_string());
            self.jobs.push(job);
        } else {
            let job = self.jobs.pop();
            if let Some(mut job) = job {
                job.pids.push(proc);
                job.names.push(command.to_string());
                self.jobs.push(job);
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

    /// Remove pid from any jobs and if it is the only pid in a job then remove the job.
    pub fn remove_pid(&mut self, pid: u32) {
        let mut idx: Option<usize> = None;
        'outer: for (i, j) in self.jobs.iter_mut().enumerate() {
            for p in &j.pids {
                if *p == pid {
                    idx = Some(i);
                    break 'outer;
                }
            }
        }
        if let Some(i) = idx {
            self.jobs[i].pids.retain(|p| *p != pid);
            if self.jobs[i].pids.is_empty() {
                self.jobs.remove(i);
            }
        }
    }

    /// Mark the job containing pid as stopped.
    pub fn mark_job_stopped(&mut self, pid: u32) {
        'outer: for mut j in self.jobs.iter_mut() {
            for p in &j.pids {
                if *p == pid {
                    j.status = JobStatus::Stopped;
                    break 'outer;
                }
            }
        }
    }

    /// Mark the job containing pid as running.
    pub fn mark_job_running(&mut self, pid: u32) {
        'outer: for mut j in self.jobs.iter_mut() {
            for p in &j.pids {
                if *p == pid {
                    j.status = JobStatus::Running;
                    break 'outer;
                }
            }
        }
    }

    /// Check any pids in a job by calling wait and updating the books.
    pub fn reap_procs(&mut self) {
        let pids: Vec<u32> = self
            .jobs
            .iter()
            .flat_map(|j| j.pids.iter().copied())
            .collect();
        for pid in pids {
            // try_wait_pid removes them and tracks exit status
            try_wait_pid(pid, self);
        }
    }

    /// Return the pid for job_num if job_num is stopped.
    pub fn pid_for_stopped_job(&self, job_num: u32) -> Option<u32> {
        for job in &self.jobs {
            if job.id == job_num {
                return if job.status == JobStatus::Stopped {
                    job.pids.first().copied()
                } else {
                    None
                };
            }
        }
        None
    }

    /// Return the pid for job_num, any status.
    pub fn pid_for_job(&self, job_num: u32) -> Option<u32> {
        for job in &self.jobs {
            if job.id == job_num {
                return job.pids.first().copied();
            }
        }
        None
    }

    /// Move the job for job_num to te foreground.
    pub fn foreground_job(&mut self, job_num: u32) {
        if let Some(pid) = self.pid_for_stopped_job(job_num) {
            let term_settings = termios::tcgetattr(libc::STDIN_FILENO).unwrap();
            let ppid = Pid::from_raw(pid as i32);
            if let Err(err) = signal::kill(ppid, Signal::SIGCONT) {
                eprintln!("Error sending sigcont to wake up process: {}.", err);
            } else {
                if let Err(err) = unistd::tcsetpgrp(libc::STDIN_FILENO, ppid) {
                    eprintln!("Error making {pid} foreground in parent: {err}");
                }
                self.mark_job_running(pid);
                wait_pid(pid, Some(&term_settings), terminal_fd(), self);
            }
        } else if let Some(pid) = self.pid_for_job(job_num) {
            let ppid = Pid::from_raw(pid as i32);
            // The job is running, so no sig cont needed...
            let term_settings = termios::tcgetattr(libc::STDIN_FILENO).unwrap();
            if let Err(err) = unistd::tcsetpgrp(libc::STDIN_FILENO, ppid) {
                eprintln!("Error making {pid} foreground in parent: {err}");
            }
            wait_pid(pid, Some(&term_settings), terminal_fd(), self);
        }
    }

    /// Move the job for job_num to te background and running (start a stopped job in the background).
    pub fn background_job(&mut self, job_num: u32) {
        if let Some(pid) = self.pid_for_stopped_job(job_num) {
            let ppid = Pid::from_raw(pid as i32);
            if let Err(err) = signal::kill(ppid, Signal::SIGCONT) {
                eprintln!("Error sending sigcont to wake up process: {}.", err);
            } else {
                self.mark_job_running(pid);
            }
        }
    }
}

impl Display for Jobs {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for job in &self.jobs {
            writeln!(
                f,
                "[{}]\t{}\t{:?}\t{:?}",
                job.id, job.status, job.pids, job.names
            )?;
        }
        Ok(())
    }
}

impl Default for Jobs {
    fn default() -> Self {
        Self::new()
    }
}
