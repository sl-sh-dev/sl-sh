use std::env;
use std::ffi::CStr;
use std::fs::create_dir_all;
use std::io;
use std::os::unix::process::CommandExt;
use std::process::{Command, Stdio};
use std::sync::atomic::AtomicBool;
use std::sync::Arc;

use libc::uid_t;
use nix::sys::signal::{self, SigHandler, Signal};
use nix::unistd::{gethostname, Uid};

use crate::builtins::load;
use crate::environment::*;
use crate::eval::*;
use crate::gc::*;
use crate::reader::*;
use crate::types::*;

fn load_user_env(environment: &mut Environment, home: &str, loadrc: bool, repl: bool) {
    let uid = Uid::current();
    let euid = Uid::effective();
    env::set_var("UID", format!("{}", uid));
    env::set_var("EUID", format!("{}", euid));
    let mut interned_sym = environment.interner.intern("*uid*");
    let data = Expression::alloc_data(ExpEnum::Int(uid_t::from(uid) as i64));
    environment.insert_into_root_scope(interned_sym, data);
    interned_sym = environment.interner.intern("*euid*");
    let data = Expression::alloc_data(ExpEnum::Int(uid_t::from(euid) as i64));
    environment.insert_into_root_scope(interned_sym, data);

    let mut load_path = Vec::new();
    load_path.push(
        Expression::alloc_data(ExpEnum::String(
            environment
                .interner
                .intern(&format!("{}/.config/sl-sh", home))
                .into(),
            None,
        ))
        .handle_no_root(),
    );
    let data = Expression::with_list(load_path);
    environment.root_scope.borrow_mut().insert_with_doc(
        environment.interner.intern("*load-path*"),
        data,
        Some(
            "Usage: (set '*load-path* '(\"/path/one\" \"/path/two\"))

Set the a list of paths to search for loading scripts with the load form.

Section: scripting

Example:
;(set '*load-path '(\"/path\"))
;(load \"script-in-path\")
t
"
            .to_string(),
        ),
    );
    if loadrc {
        environment.root_scope.borrow_mut().insert(
            environment.interner.intern("*load-slshrc*"),
            Expression::make_true(),
        );
    }
    if repl {
        environment.root_scope.borrow_mut().insert(
            environment.interner.intern("*interactive*"),
            Expression::make_true(),
        );
    }
    if let Err(err) = load(environment, "slsh-std.lisp") {
        eprintln!(
            "WARNING: Failed to load standard lib script slsh-std.lisp: {}",
            err
        );
    }
    gc_mut().clean();
}

pub fn start_interactive(sig_int: Arc<AtomicBool>) -> i32 {
    // Initialize the HOST variable
    let mut hostname = [0_u8; 512];
    env::set_var(
        "HOST",
        &gethostname(&mut hostname)
            .ok()
            .map_or_else(|| "?".into(), CStr::to_string_lossy)
            .as_ref(),
    );
    if let Ok(dir) = env::current_dir() {
        env::set_var("PWD", dir);
    }
    let mut home = match env::var("HOME") {
        Ok(val) => val,
        Err(_) => ".".to_string(),
    };
    if home.ends_with('/') {
        home = home[..home.len() - 1].to_string();
    }
    let config_dir = format!("{}/.config/sl-sh", home);
    if let Err(err) = create_dir_all(&config_dir) {
        eprintln!(
            "WARNING: Unable to create config directory: {}- {}",
            config_dir, err
        );
    }
    let share_dir = format!("{}/.local/share/sl-sh", home);
    if let Err(err) = create_dir_all(&share_dir) {
        eprintln!(
            "WARNING: Unable to create share directory: {}- {}",
            share_dir, err
        );
    }
    let mut environment = build_default_environment(sig_int);
    load_user_env(&mut environment, &home, true, true);
    if environment.exit_code.is_some() {
        environment.exit_code.unwrap()
    } else {
        0
    }
}

pub fn read_stdin() -> i32 {
    let mut home = match env::var("HOME") {
        Ok(val) => val,
        Err(_) => ".".to_string(),
    };
    if home.ends_with('/') {
        home = home[..home.len() - 1].to_string();
    }
    let share_dir = format!("{}/.local/share/sl-sh", home);
    if let Err(err) = create_dir_all(&share_dir) {
        eprintln!(
            "WARNING: Unable to create share directory: {}- {}",
            share_dir, err
        );
    }
    let mut environment = build_default_environment(Arc::new(AtomicBool::new(false)));
    environment.do_job_control = false;
    environment.is_tty = false;
    load_user_env(&mut environment, &home, true, false);

    let mut input = String::new();
    loop {
        match io::stdin().read_line(&mut input) {
            Ok(0) => return 0,
            Ok(_n) => {
                let input = input.trim();
                environment.state.stdout_status = None;
                let ast = read(&mut environment, input, None, true);
                match ast {
                    Ok(ast) => {
                        environment.loose_symbols = true;
                        match eval(&mut environment, ast) {
                            Ok(exp) => {
                                match &exp.get().data {
                                    ExpEnum::Nil => { /* don't print nil */ }
                                    ExpEnum::Process(_) => { /* should have used stdout */ }
                                    _ => {
                                        if let Err(err) = exp.write(&mut environment) {
                                            eprintln!("Error writing result: {}", err);
                                        }
                                    }
                                }
                            }
                            Err(err) => eprintln!("{}", err),
                        }
                        environment.loose_symbols = false;
                    }
                    Err(err) => eprintln!("{:?}", err),
                }
                environment.state.stderr_status = None;
            }
            Err(error) => {
                eprintln!("ERROR reading stdin: {}", error);
                return 66;
            }
        }
        if environment.exit_code.is_some() {
            break;
        }
    }
    if environment.exit_code.is_some() {
        environment.exit_code.unwrap()
    } else {
        0
    }
}

fn parse_one_run_command_line(input: &str, nargs: &mut Vec<String>) -> Result<(), LispError> {
    let mut in_string = false;
    let mut in_stringd = false;
    let mut token = String::new();
    let mut last_ch = ' ';
    for ch in input.chars() {
        if ch == '\'' && last_ch != '\\' {
            in_string = !in_string;
            if !in_string {
                nargs.push(token);
                token = String::new();
            }
            last_ch = ch;
            continue;
        }
        if ch == '"' && last_ch != '\\' {
            in_stringd = !in_stringd;
            if !in_stringd {
                nargs.push(token);
                token = String::new();
            }
            last_ch = ch;
            continue;
        }
        if in_string || in_stringd {
            token.push(ch);
        } else if ch == ' ' {
            if !token.is_empty() {
                nargs.push(token);
                token = String::new();
            }
        } else {
            token.push(ch);
        }
        last_ch = ch;
    }
    if !token.is_empty() {
        nargs.push(token);
    }
    Ok(())
}

pub fn run_one_command(command: &str, args: &[String]) -> Result<(), LispError> {
    // Try to make sense out of whatever crap we get (looking at you fzf-tmux)
    // and make it work.
    let mut nargs: Vec<String> = Vec::new();
    parse_one_run_command_line(command, &mut nargs)?;
    for arg in args {
        parse_one_run_command_line(&arg, &mut nargs)?;
    }

    if !nargs.is_empty() {
        let mut com = Command::new(&nargs[0]);
        if nargs.len() > 1 {
            com.args(&nargs[1..]);
        }
        com.stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .stdin(Stdio::inherit());

        unsafe {
            com.pre_exec(|| -> io::Result<()> {
                signal::signal(Signal::SIGINT, SigHandler::SigDfl).unwrap();
                signal::signal(Signal::SIGHUP, SigHandler::SigDfl).unwrap();
                signal::signal(Signal::SIGTERM, SigHandler::SigDfl).unwrap();
                Ok(())
            });
        }

        let mut proc = com.spawn()?;
        proc.wait()?;
    }
    Ok(())
}

pub fn run_one_script(command: &str, args: &[String]) -> i32 {
    let mut environment = build_default_environment(Arc::new(AtomicBool::new(false)));
    environment.do_job_control = false;

    let mut home = match env::var("HOME") {
        Ok(val) => val,
        Err(_) => ".".to_string(),
    };
    if home.ends_with('/') {
        home = home[..home.len() - 1].to_string();
    }
    environment.root_scope.borrow_mut().insert(
        environment.interner.intern("*run-script*"),
        Expression::alloc_data(ExpEnum::String(
            environment.interner.intern(command).into(),
            None,
        )),
    );

    let mut exp_args: Vec<Handle> = Vec::with_capacity(args.len());
    for a in args {
        exp_args.push(
            Expression::alloc_data(ExpEnum::String(environment.interner.intern(a).into(), None))
                .handle_no_root(),
        );
    }
    let data = Expression::with_list(exp_args);
    environment
        .root_scope
        .borrow_mut()
        .insert(environment.interner.intern("args"), data);
    load_user_env(&mut environment, &home, false, false);
    if environment.exit_code.is_some() {
        environment.exit_code.unwrap()
    } else {
        0
    }
}
