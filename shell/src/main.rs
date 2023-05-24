mod builtins;
mod command_data;
mod config;
mod glob;
mod jobs;
mod parse;
mod run;
mod signals;
mod unix;

//extern crate jemallocator;

//#[global_allocator]
//static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

use crate::config::get_config;
use crate::jobs::Jobs;
use crate::run::{run_one_command, setup_shell_tty};
use nix::unistd::{self, gethostname, Uid};
use sl_liner::Prompt;
use std::env;
use std::ffi::OsString;
use std::io::ErrorKind;

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
