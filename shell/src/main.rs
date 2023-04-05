mod builtins;
mod config;
mod jobs;
mod signals;
mod unix;

//extern crate jemallocator;

//#[global_allocator]
//static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

use crate::builtins::cd;
use crate::config::get_config;
use crate::jobs::Jobs;
use crate::signals::{install_sigint_handler, mask_signals};
use crate::unix::{fork_exec, fork_pipe, terminal_fd, wait_pid};
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
            if let Err(err) = run_one_command(&command, is_tty, &mut jobs) {
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

fn parse_one_run_command_line(input: &str) -> Vec<Vec<String>> {
    let mut ret = vec![];
    let mut in_string = false;
    let mut in_stringd = false;
    let mut token = String::new();
    let mut last_ch = ' ';
    let mut args = vec![];
    for ch in input.chars() {
        if ch == '\'' && last_ch != '\\' {
            in_string = !in_string;
            if !in_string {
                args.push(token);
                token = String::new();
            }
            last_ch = ch;
            continue;
        }
        if ch == '"' && last_ch != '\\' {
            in_stringd = !in_stringd;
            if !in_stringd {
                args.push(token);
                token = String::new();
            }
            last_ch = ch;
            continue;
        }
        if ch == '|' && last_ch != '\\' {
            if !token.is_empty() {
                args.push(token);
                token = String::new();
            }
            ret.push(args);
            args = vec![];
            continue;
        }
        if in_string || in_stringd {
            token.push(ch);
        } else if ch == ' ' {
            if !token.is_empty() {
                args.push(token);
                token = String::new();
            }
        } else {
            token.push(ch);
        }
        last_ch = ch;
    }
    if !token.is_empty() {
        args.push(token);
    }
    ret.push(args);
    ret
}

pub fn run_one_command(command: &str, is_tty: bool, jobs: &mut Jobs) -> Result<(), io::Error> {
    // Try to make sense out of whatever crap we get (looking at you fzf-tmux)
    // and make it work.
    let commands = parse_one_run_command_line(command);
    let term_settings = termios::tcgetattr(libc::STDIN_FILENO).unwrap();
    let terminal_fd = terminal_fd();

    match commands.len() {
        0 => {} // Nothing to do.
        1 => {
            if !commands[0].is_empty() {
                if &commands[0][0] == "cd" {
                    let _r = if commands[0].len() > 2 {
                        eprintln!("cd: too many arguments!");
                        -1
                    } else if commands[0].len() == 1 {
                        cd(None)
                    } else {
                        cd(Some(commands[0][1].clone()))
                    };
                } else if &commands[0][0] == "jobs" {
                    if commands[0].len() > 1 {
                        eprintln!("jobs: too many arguments!");
                    } else {
                        println!("{jobs}");
                    }
                } else {
                    let pid = fork_exec(
                        None,
                        None,
                        None,
                        &commands[0][0],
                        commands[0][1..].iter(),
                        None,
                        jobs,
                    )?;
                    wait_pid(pid, Some(&term_settings), terminal_fd, is_tty, jobs);
                }
            }
        }
        _ => {
            let pid = fork_pipe(None, None, None, commands, jobs)?;
            wait_pid(pid, Some(&term_settings), terminal_fd, is_tty, jobs);
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
    loop {
        jobs.reap_procs();
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
        if let Err(err) = run_one_command(&res, is_tty, &mut jobs) {
            eprintln!("ERROR executing {res}: {err}");
        }
    }
    0
}
