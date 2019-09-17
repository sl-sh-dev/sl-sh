use std::io;

use nix::{
    sys::signal::{self, SigHandler, Signal},
    unistd,
};

use ::slsh::*;

use std::io::Write;
fn main() -> io::Result<()> {
    println!("This on!");
    let config = get_config();
    if let Ok(config) = config {
        if config.command.is_none() {
            //block_signals();
            /* See if we are running interactively.  */
            let mut writer = std::fs::File::create("/home/sstanf/slsh.errors")?;
            writer.write_all("Trying to start interactive\n".as_bytes())?;
            println!("Trying to start interactive");
            let shell_terminal = nix::libc::STDIN_FILENO;
            if let Ok(true) = unistd::isatty(shell_terminal) {
                writer.write_all("Is a TTY\n".as_bytes())?;
                /* Loop until we are in the foreground.  */
                let mut shell_pgid = unistd::getpgrp();
                while unistd::tcgetpgrp(shell_terminal) != Ok(shell_pgid) {
                    //kill (- shell_pgid, SIGTTIN);
                    if let Err(err) = signal::kill(shell_pgid, Signal::SIGTTIN) {
                        eprintln!("Error sending sigttin: {}.", err);
                        let msg = format!("Error sending sigttin: {}.\n", err);
                        writer.write_all(msg.as_bytes())?;
                    }
                    shell_pgid = unistd::getpgrp();
                }

                writer.write_all("Killing signals\n".as_bytes())?;
                /* Ignore interactive and job-control signals.  */
                unsafe {
                    signal::signal(Signal::SIGINT, SigHandler::SigIgn).unwrap();
                    signal::signal(Signal::SIGQUIT, SigHandler::SigIgn).unwrap();
                    signal::signal(Signal::SIGTSTP, SigHandler::SigIgn).unwrap();
                    signal::signal(Signal::SIGTTIN, SigHandler::SigIgn).unwrap();
                    signal::signal(Signal::SIGTTOU, SigHandler::SigIgn).unwrap();
                    signal::signal(Signal::SIGCHLD, SigHandler::SigIgn).unwrap();
                }

                /* Put ourselves in our own process group.  */
                let pgid = unistd::getpid();
                if let Err(err) = unistd::setpgid(pgid, pgid) {
                    eprintln!("Couldn't put the shell in its own process group: {}", err);
                    let msg = format!("Couldn't put the shell in its own process group: {}\n", err);
                    writer.write_all(msg.as_bytes())?;
                    //return Err(io::Error::new(
                    //    io::ErrorKind::Other,
                    //    "Couldn't put the shell in its own process group",
                    //));
                }
                /* Grab control of the terminal.  */
                if let Err(err) = unistd::tcsetpgrp(shell_terminal, pgid) {
                    eprintln!("Couldn't grab control of terminal: {}", err);
                    let msg = format!("Couldn't grab control of terminal: {}\n", err);
                    writer.write_all(msg.as_bytes())?;
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "Couldn't grab control of terminal.",
                    ));
                }
                /* Save default terminal attributes for shell.  */
                //tcgetattr (shell_terminal, &shell_tmodes);
            }

            writer.write_all("Going interactive\n".as_bytes())?;
            start_interactive();
        } else {
            let command = config.command.unwrap();
            if let Err(err) = run_one_command(&command, &config.args) {
                if let Err(err2) = run_one_script(&command, &config.args) {
                    eprintln!("Error running {}: {}, {}", command, err, err2);
                    return Err(err2);
                }
            }
        }
    }
    Ok(())
}
