mod builtins;
mod command_data;
mod config;
mod glob;
mod jobs;
mod parse;
mod signals;
mod unix;

//extern crate jemallocator;

//#[global_allocator]
//static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

use crate::builtins::run_builtin;
use crate::command_data::{CommandWithArgs, Run};
use crate::config::get_config;
use crate::jobs::{Job, Jobs};
use crate::parse::parse_line;
use crate::signals::{install_sigint_handler, mask_signals};
use crate::unix::{fork_exec, fork_pipe, fork_run, wait_job};
use nix::{
    sys::signal::{self, Signal},
    unistd::{self, gethostname, Uid},
};
use sl_liner::Prompt;
use std::ffi::OsString;
use std::io::ErrorKind;
use std::{env, io};

fn setup_shell_tty(shell_terminal: i32) {
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

fn main() {
    if let Some(config) = get_config() {
        let shell_terminal = 0;
        // See if we are running interactively.
        let is_tty = unistd::isatty(shell_terminal).unwrap_or(false);
        if config.command.is_none() && config.script.is_none() {
            if is_tty {
                setup_shell_tty(shell_terminal);
                let code = start_interactive();
                std::process::exit(code);
            } else {
                // No tty, just read stdin and do something with it..
                /* XXXX TODO
                jobs.reap_procs();
                if let Err(err) = run_one_command(&res, &mut jobs) {
                    eprintln!("ERROR executing {res}: {err}");
                }
                std::process::exit(code);
                */
            }
        } else if config.command.is_some() {
            let mut command = config.command.unwrap();
            command.push(' ');
            command.push_str(&config.args.join(" "));
            let mut jobs = Jobs::new(false);

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

            if let Err(err) = run_one_command(&command, &mut jobs) {
                eprintln!("Error running {}: {}", command, err);
                return;
            }
        }
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
) -> Result<i32, io::Error> {
    Ok(if let Some(command_name) = command.command(jobs) {
        let command_name = command_name?;
        let mut args = command.args_iter();
        if !run_builtin(&command_name, &mut args, jobs) {
            let mut job = jobs.new_job();
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
    } else {
        0
    })
}

pub fn run_job(run: &Run, jobs: &mut Jobs, force_background: bool) -> Result<i32, io::Error> {
    let status = match run {
        Run::Command(command) => run_command(command, jobs, force_background)?,
        Run::BackgroundCommand(command) => run_command(command, jobs, true)?,
        Run::Pipe(pipe) => {
            let mut job = jobs.new_job();
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

pub fn run_one_command(command: &str, jobs: &mut Jobs) -> Result<(), io::Error> {
    // Try to make sense out of whatever crap we get (looking at you fzf-tmux)
    // and make it work.
    let commands = parse_line(command)?;

    run_job(commands.commands(), jobs, false)?;
    Ok(())
}

pub fn start_interactive() -> i32 {
    let uid = Uid::current();
    let euid = Uid::effective();
    env::set_var("UID", format!("{}", uid));
    env::set_var("EUID", format!("{}", euid));
    // Initialize the HOST variable
    let host: OsString = gethostname().ok().unwrap_or_else(|| "???".into());
    env::set_var("HOST", &host);
    if let Ok(dir) = env::current_dir() {
        env::set_var("PWD", dir);
    }
    /*let mut home = match env::var("HOME") {
        Ok(val) => val,
        Err(_) => ".".to_string(),
    };
    if home.ends_with('/') {
        home = home[..home.len() - 1].to_string();
    }*/

    let mut con = sl_liner::Context::new();

    if let Err(e) = con.history.set_file_name_and_load_history(".shell-history") {
        eprintln!("Error loading history: {e}");
    }
    let mut jobs = Jobs::new(true);
    loop {
        let res = match con.read_line(Prompt::from("shell> "), None) {
            Ok(input) => input,
            Err(err) => match err.kind() {
                ErrorKind::UnexpectedEof => {
                    break;
                }
                ErrorKind::Interrupted => {
                    continue;
                }
                _ => {
                    // Usually can just restore the tty and be back in action.
                    jobs.restore_terminal();
                    continue;
                }
            },
        };

        if res.is_empty() {
            continue;
        }

        con.history.push(&res).expect("Failed to push history.");
        jobs.reap_procs();
        if let Err(err) = run_one_command(&res, &mut jobs) {
            eprintln!("ERROR executing {res}: {err}");
        }
    }
    0
}
