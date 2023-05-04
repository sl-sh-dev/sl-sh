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
use crate::unix::{fork_exec, fork_pipe, restore_terminal, terminal_fd, wait_job};
use nix::sys::termios;
use nix::sys::termios::Termios;
use nix::{
    libc,
    sys::signal::{self, Signal},
    unistd::{self, gethostname, Uid},
};
use sl_liner::Prompt;
use std::ffi::OsString;
use std::io::ErrorKind;
use std::{env, io};

fn main() {
    if let Some(config) = get_config() {
        let shell_terminal = libc::STDIN_FILENO;
        // See if we are running interactively.
        let is_tty = unistd::isatty(shell_terminal).unwrap_or(false);
        if config.command.is_none() && config.script.is_none() {
            if is_tty {
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

                let code = start_interactive(true);
                std::process::exit(code);
            } else {
                // No tty, just read stdin and do something with it..
                let code = start_interactive(false);
                std::process::exit(code);
            }
        } else if config.command.is_some() {
            let mut command = config.command.unwrap();
            command.push(' ');
            command.push_str(&config.args.join(" "));
            let mut jobs = Jobs::new();
            if let Err(err) = run_one_command(&command, &mut jobs) {
                eprintln!("Error running {}: {}", command, err);
                return;
            }
            /*} else if config.script.is_some() {
               let script = config.script.unwrap();
               let code = run_one_script(&script, &config.args);
               std::process::exit(code);
            */
        }
    }
}

fn finish_run(
    background: bool,
    mut job: Job,
    jobs: &mut Jobs,
    term_settings: Option<&termios::Termios>,
    terminal_fd: i32,
) -> i32 {
    job.mark_running();
    if !background {
        if let Some(status) = wait_job(&mut job, term_settings, terminal_fd) {
            env::set_var("LAST_STATUS", format!("{}", status));
            status
        } else {
            env::set_var("LAST_STATUS", format!("{}", -66));
            jobs.push_job(job);
            -66
        }
    } else {
        restore_terminal(term_settings, terminal_fd, &job);
        env::set_var("LAST_STATUS", format!("{}", 0));
        jobs.push_job(job);
        0
    }
}

fn run_command(
    command: &CommandWithArgs,
    jobs: &mut Jobs,
    background: bool,
    term_settings: Termios,
    terminal_fd: i32,
) -> Result<i32, io::Error> {
    Ok(if let Some(command_name) = command.command() {
        let mut args = command.args_iter();
        if !run_builtin(command_name, &mut args, jobs) {
            let mut job = jobs.new_job();
            match fork_exec(command, &mut job) {
                Ok(()) => finish_run(background, job, jobs, Some(&term_settings), terminal_fd),
                Err(err) => {
                    // Make sure we restore the terminal...
                    restore_terminal(Some(&term_settings), terminal_fd, &job);
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

pub fn run_job(
    run: &Run,
    jobs: &mut Jobs,
    term_settings: Termios,
    terminal_fd: i32,
) -> Result<i32, io::Error> {
    let status = match run {
        Run::Command(command) => run_command(command, jobs, false, term_settings, terminal_fd)?,
        Run::BackgroundCommand(command) => {
            run_command(command, jobs, true, term_settings, terminal_fd)?
        }
        Run::Pipe(pipe) => {
            let mut job = jobs.new_job();
            match fork_pipe(
                &pipe[..],
                &mut job,
                jobs,
                term_settings.clone(),
                terminal_fd,
            ) {
                Ok(background) => {
                    finish_run(background, job, jobs, Some(&term_settings), terminal_fd)
                }
                Err(err) => {
                    // Make sure we restore the terminal...
                    restore_terminal(Some(&term_settings), terminal_fd, &job);
                    return Err(err);
                }
            }
        }
        Run::Sequence(seq) => {
            let mut status = 0;
            for r in seq {
                status = run_job(r, jobs, term_settings.clone(), terminal_fd)?;
            }
            status
        }
        Run::And(seq) => {
            // XXXX should background == true be an error?
            let mut status = 0;
            for r in seq {
                status = run_job(r, jobs, term_settings.clone(), terminal_fd)?;
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
                status = run_job(r, jobs, term_settings.clone(), terminal_fd)?;
                if status == 0 {
                    break;
                }
            }
            status
        }
        Run::Empty => 0,
    };
    Ok(status)
}

pub fn run_one_command(command: &str, jobs: &mut Jobs) -> Result<(), io::Error> {
    // Try to make sense out of whatever crap we get (looking at you fzf-tmux)
    // and make it work.
    let commands = parse_line(command);
    let term_settings = termios::tcgetattr(libc::STDIN_FILENO).unwrap();
    let terminal_fd = terminal_fd();

    run_job(commands.commands(), jobs, term_settings, terminal_fd)?;
    Ok(())
}

pub fn start_interactive(is_tty: bool) -> i32 {
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
    let mut jobs = Jobs::new();
    jobs.set_tty(is_tty);
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
                    panic!("Error on input: {err}");
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
