use crate::unix::try_wait_pid;
use nix::libc;
use nix::unistd::{self, Pid};
use std::fmt;
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
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
    pub pids: Vec<u32>,
    pub names: Vec<String>,
    pub status: JobStatus,
}

pub struct Jobs {
    jobs: Vec<Job>,
    job_control: bool,
}

impl Jobs {
    pub fn new() -> Self {
        Self {
            jobs: vec![],
            job_control: true,
        }
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
            let mut job = Job {
                pids: Vec::new(),
                names: Vec::new(),
                status: JobStatus::Running,
            };
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
}

impl Display for Jobs {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (i, job) in self.jobs.iter().enumerate() {
            writeln!(
                f,
                "[{}]\t{}\t{:?}\t{:?}",
                i, job.status, job.pids, job.names
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
