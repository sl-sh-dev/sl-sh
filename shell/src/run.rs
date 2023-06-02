use crate::builtins::run_builtin;
use crate::command_data::{CommandWithArgs, Run};
use crate::jobs::{Job, Jobs};
use crate::parse::parse_line;
use crate::signals::{install_sigint_handler, mask_signals};
use crate::unix::{fork_exec, fork_pipe, fork_run, wait_job};
use nix::{
    sys::signal::{self, Signal},
    unistd,
};
use std::{env, io};

pub fn setup_shell_tty(shell_terminal: i32) {
    /* Loop until we are in the foreground.  */
    let mut shell_pgid = unistd::getpgrp();
    while unistd::tcgetpgrp(shell_terminal) != Ok(shell_pgid) {
        if let Err(err) = signal::kill(shell_pgid, Signal::SIGTTIN) {
            eprintln!("Error sending sigttin: {}.", err);
        }
        shell_pgid = unistd::getpgrp();
    }

    mask_signals();

    /* Put ourselves in our own process group.  */
    let pgid = unistd::getpid();
    if let Err(err) = unistd::setpgid(pgid, pgid) {
        match err {
            nix::errno::Errno::EPERM => { /* ignore */ }
            _ => {
                eprintln!("Couldn't put the shell in its own process group: {}\n", err)
            }
        }
    }
    /* Grab control of the terminal.  */
    if let Err(err) = unistd::tcsetpgrp(shell_terminal, pgid) {
        let msg = format!("Couldn't grab control of terminal: {}\n", err);
        eprintln!("{}", msg);
        return;
    }

    if !install_sigint_handler() {
        std::process::exit(1)
    }
}

fn finish_run(background: bool, mut job: Job, jobs: &mut Jobs) -> i32 {
    job.mark_running();
    let status = if !background {
        if let Some(status) = wait_job(&mut job) {
            env::set_var("LAST_STATUS", format!("{}", status));
            status
        } else {
            env::set_var("LAST_STATUS", format!("{}", -66));
            jobs.push_job(job);
            -66
        }
    } else {
        env::set_var("LAST_STATUS", format!("{}", 0));
        jobs.push_job(job);
        0
    };
    jobs.restore_terminal();
    status
}

fn run_command(
    command: &CommandWithArgs,
    jobs: &mut Jobs,
    background: bool,
    stealth: bool,
) -> Result<i32, io::Error> {
    Ok(if let Some(command_name) = command.command(jobs) {
        let command_name = command_name?;
        if let Some(mut alias_run) = jobs.get_alias(command_name.to_string_lossy()) {
            for arg in command.args_iter() {
                alias_run.push_arg_end(arg.clone());
            }
            if let Some(stdios) = command.stdios() {
                alias_run.extend_redirs_end(stdios)
            }
            run_job(&alias_run, jobs, background)?
        } else {
            let mut args = command.args_iter();
            if !run_builtin(&command_name, &mut args, jobs) {
                let mut job = jobs.new_job();
                job.set_stealth(stealth);
                match fork_exec(command, &mut job, jobs) {
                    Ok(()) => finish_run(background, job, jobs),
                    Err(err) => {
                        // Make sure we restore the terminal...
                        jobs.restore_terminal();
                        return Err(err);
                    }
                }
            } else {
                0
            }
        }
    } else {
        0
    })
}

pub fn run_job(run: &Run, jobs: &mut Jobs, force_background: bool) -> Result<i32, io::Error> {
    let status = match run {
        Run::Command(command) => run_command(command, jobs, force_background, force_background)?,
        Run::BackgroundCommand(command) => run_command(command, jobs, true, false)?,
        Run::Pipe(pipe) => {
            let mut job = jobs.new_job();
            job.set_stealth(force_background);
            match fork_pipe(&pipe[..], &mut job, jobs) {
                Ok(background) => finish_run(background, job, jobs),
                Err(err) => {
                    // Make sure we restore the terminal...
                    jobs.restore_terminal();
                    return Err(err);
                }
            }
        }
        Run::Sequence(seq) => {
            let mut status = 0;
            for r in seq {
                status = run_job(r, jobs, force_background)?;
            }
            status
        }
        Run::And(seq) => {
            // XXXX should background == true be an error?
            let mut status = 0;
            for r in seq {
                status = run_job(r, jobs, force_background)?;
                if status != 0 {
                    break;
                }
            }
            status
        }
        Run::Or(seq) => {
            // XXXX should background == true be an error?
            let mut status = 0;
            for r in seq {
                status = run_job(r, jobs, force_background)?;
                if status == 0 {
                    break;
                }
            }
            status
        }
        Run::Subshell(sub_run) => {
            let mut job = jobs.new_job();
            job.set_stealth(force_background);
            match fork_run(sub_run, &mut job, jobs) {
                Ok(()) => finish_run(false, job, jobs),
                Err(err) => {
                    // Make sure we restore the terminal...
                    jobs.restore_terminal();
                    return Err(err);
                }
            }
        }
        Run::Empty => 0,
    };
    Ok(status)
}

pub fn run_one_command(command: &str, jobs: &mut Jobs) -> Result<i32, io::Error> {
    // Try to make sense out of whatever crap we get (looking at you fzf-tmux)
    // and make it work.
    let commands = parse_line(command)?;

    run_job(commands.commands(), jobs, false)
}
