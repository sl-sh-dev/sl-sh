use shell::config::get_config;
use shell::jobs::Jobs;
use shell::platform::{Platform, Sys, STDIN_FILENO};
use shell::run::{run_one_command, setup_shell_tty};
use sl_liner::Prompt;
use std::env;
use std::ffi::OsString;
use std::io::ErrorKind;

fn main() {
    if let Some(config) = get_config() {
        let shell_terminal = STDIN_FILENO;
        // See if we are running interactively.
        let is_tty = Sys::is_tty(shell_terminal);
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
            if let Err(err) = Sys::set_self_pgroup() {
                eprintln!("Couldn't put the shell in its own process group: {err}")
            }

            if let Err(err) = run_one_command(&command, &mut jobs) {
                eprintln!("Error running {}: {}", command, err);
                return;
            }
        }
    }
}

pub fn start_interactive() -> i32 {
    let uid = Sys::current_uid();
    let euid = Sys::effective_uid();
    unsafe { env::set_var("UID", format!("{}", uid)); }
    unsafe { env::set_var("EUID", format!("{}", euid)); }
    // Initialize the HOST variable
    let host: OsString = Sys::gethostname().unwrap_or_else(|| "???".into());
    unsafe { env::set_var("HOST", host); }
    if let Ok(dir) = env::current_dir() {
        unsafe { env::set_var("PWD", dir); }
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
