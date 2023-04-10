mod builtins;
mod config;
mod jobs;
mod parse;
mod signals;
mod unix;

//extern crate jemallocator;

//#[global_allocator]
//static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

use crate::builtins::cd;
use crate::config::get_config;
use crate::jobs::Jobs;
use crate::parse::parse_line;
use crate::signals::{install_sigint_handler, mask_signals};
use crate::unix::{fork_exec, fork_pipe, restore_terminal, terminal_fd, wait_job};
use nix::sys::termios;
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

pub fn run_one_command(command: &str, jobs: &mut Jobs) -> Result<(), io::Error> {
    // Try to make sense out of whatever crap we get (looking at you fzf-tmux)
    // and make it work.
    let commands = parse_line(command);
    let term_settings = termios::tcgetattr(libc::STDIN_FILENO).unwrap();
    let terminal_fd = terminal_fd();

    match commands.len() {
        0 => {} // Nothing to do.
        1 => {
            if let Some(command) = commands.commands()[0].command() {
                let args = commands.commands()[0].args();
                if command == "cd" {
                    let _r = match args.len() {
                        0 => cd(None),
                        1 => cd(Some(args[0].clone())),
                        _ => {
                            eprintln!("cd: too many arguments!");
                            -1
                        }
                    };
                } else if command == "fg" {
                    let _r = if args.len() != 1 {
                        eprintln!("fg: takes one argument!");
                        -1
                    } else if let Ok(job_num) = args[0].parse() {
                        jobs.foreground_job(job_num);
                        0
                    } else {
                        eprintln!("fg: argument not a number!");
                        -1
                    };
                } else if command == "bg" {
                    let _r = if args.len() != 1 {
                        eprintln!("fg: takes one argument!");
                        -1
                    } else if let Ok(job_num) = args[0].parse() {
                        jobs.background_job(job_num);
                        0
                    } else {
                        eprintln!("fg: argument not a number!");
                        -1
                    };
                } else if command == "jobs" {
                    if !args.is_empty() {
                        eprintln!("jobs: too many arguments!");
                    } else {
                        println!("{jobs}");
                    }
                } else {
                    let background = commands.background();
                    let mut job = jobs.new_job();
                    fork_exec(
                        None,
                        None,
                        None,
                        command,
                        commands.commands()[0].args().iter(),
                        &mut job,
                    )?;
                    job.mark_running();
                    if !background {
                        if let Some(status) = wait_job(&mut job, Some(&term_settings), terminal_fd)
                        {
                            env::set_var("LAST_STATUS", format!("{}", status));
                        } else {
                            env::set_var("LAST_STATUS", format!("{}", -66));
                            jobs.push_job(job);
                        }
                    } else {
                        restore_terminal(Some(&term_settings), terminal_fd, &job);
                        env::set_var("LAST_STATUS", format!("{}", 0));
                        jobs.push_job(job);
                    }
                }
            }
        }
        _ => {
            let background = commands.background();
            let mut job = jobs.new_job();
            fork_pipe(None, None, None, commands, &mut job)?;
            job.mark_running();
            if !background {
                if let Some(status) = wait_job(&mut job, Some(&term_settings), terminal_fd) {
                    env::set_var("LAST_STATUS", format!("{}", status));
                } else {
                    env::set_var("LAST_STATUS", format!("{}", -66));
                    jobs.push_job(job);
                }
            } else {
                restore_terminal(Some(&term_settings), terminal_fd, &job);
                env::set_var("LAST_STATUS", format!("{}", 0));
                jobs.push_job(job);
            }
        }
    }
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
                    eprintln!("Error on input: {err}");
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
