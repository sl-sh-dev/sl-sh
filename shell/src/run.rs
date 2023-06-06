use crate::builtins::run_builtin;
use crate::command_data::{CommandWithArgs, Run};
use crate::jobs::{Job, Jobs};
use crate::parse::parse_line;
use crate::platform::{
    anon_pipe, fork_exec, fork_run, grab_terminal, set_self_pgroup, terminal_foreground, wait_job,
    FileDesc,
};
use crate::signals::{install_sigint_handler, mask_signals};
use std::{env, io};

pub fn setup_shell_tty(shell_terminal: FileDesc) {
    terminal_foreground(shell_terminal);

    mask_signals();

    if let Err(err) = set_self_pgroup() {
        eprintln!("Couldn't put the shell in its own process group: {}\n", err)
    }
    /* Grab control of the terminal.  */
    if let Err(err) = grab_terminal(shell_terminal) {
        eprintln!("Couldn't grab control of terminal: {err}");
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
            match run_builtin(&command_name, &mut args, jobs) {
                Some(status) => status,
                None => {
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
                }
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
            match run_pipe(&pipe[..], &mut job, jobs) {
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

fn pipe_command(
    command: &CommandWithArgs,
    next_in: Option<FileDesc>,
    next_out: Option<FileDesc>,
    job: &mut Job,
    jobs: &mut Jobs,
) -> Result<(), io::Error> {
    let mut command = command.clone();
    if let Some(command_name) = command.command(jobs) {
        let command_name = command_name?;
        if let Some(mut alias_run) = jobs.get_alias(command_name.to_string_lossy()) {
            alias_run.push_stdin_front(next_in);
            alias_run.push_stdout_front(next_out);
            for arg in command.args_iter() {
                alias_run.push_arg_end(arg.clone());
            }
            if let Some(stdios) = command.stdios() {
                alias_run.extend_redirs_end(stdios)
            }
            match alias_run {
                Run::Command(command) | Run::BackgroundCommand(command) => {
                    fork_exec(&command, job, jobs)?;
                }
                _ => {
                    fork_run(&alias_run, job, jobs)?;
                }
            }
        } else {
            command.push_stdin_front(next_in);
            command.push_stdout_front(next_out);
            fork_exec(&command, job, jobs)?;
        }
    }
    Ok(())
}

fn run_pipe(new_job: &[Run], job: &mut Job, jobs: &mut Jobs) -> Result<bool, io::Error> {
    let progs = new_job.len();
    let (p_in, p_out) = anon_pipe()?;
    let mut next_in = Some(p_in);
    let mut next_out = None;
    let mut upcoming_out = p_out;
    let mut background = false;
    for (i, program) in new_job.iter().rev().enumerate() {
        match program {
            Run::Command(command) => {
                pipe_command(command, next_in, next_out, job, jobs)?;
            }
            Run::BackgroundCommand(command) => {
                pipe_command(command, next_in, next_out, job, jobs)?;
                if i == 0 {
                    background = true;
                }
            }
            Run::Subshell(_) => {
                let mut program = program.clone();
                program.push_stdin_front(next_in);
                program.push_stdout_front(next_out);
                if let Run::Subshell(sub_run) = &mut program {
                    match fork_run(&*sub_run, job, jobs) {
                        Ok(()) => {
                            jobs.restore_terminal();
                        }
                        Err(err) => {
                            // Make sure we restore the terminal...
                            jobs.restore_terminal();
                            return Err(err);
                        }
                    }
                }
            }
            Run::Pipe(_) | Run::Sequence(_) | Run::And(_) | Run::Or(_) => {
                // Don't think this is expressible with the parser and maybe should be an error?
                let mut program = program.clone();
                program.push_stdin_front(next_in);
                program.push_stdout_front(next_out);
                run_job(&program, jobs, true)?;
            }
            Run::Empty => {}
        }
        next_out = Some(upcoming_out);
        if i < (progs - 1) {
            let (p_in, p_out) = anon_pipe()?;
            upcoming_out = p_out;
            next_in = Some(p_in);
        } else {
            next_in = None;
        }
    }
    if job.is_empty() {
        Err(io::Error::new(io::ErrorKind::Other, "no processes started"))
    } else {
        job.reverse();
        Ok(background)
    }
}
