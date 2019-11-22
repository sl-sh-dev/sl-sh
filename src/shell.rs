use std::cell::RefCell;
use std::collections::HashMap;
use std::env;
use std::ffi::CStr;
use std::fs;
use std::fs::create_dir_all;
use std::io::{self, ErrorKind};
use std::os::unix::process::CommandExt;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use liner::ColorClosure;
use liner::Context;

use nix::sys::signal::{self, SigHandler, Signal};
use nix::unistd::gethostname;

use crate::completions::*;
use crate::environment::*;
use crate::eval::*;
use crate::reader::*;
use crate::types::*;

fn load_user_env(environment: &mut Environment, home: &str) {
    let mut load_path = Vec::new();
    load_path.push(Expression::Atom(Atom::String(format!(
        "{}/.config/sl-sh",
        home
    ))));
    environment.root_scope.borrow_mut().data.insert(
        "*load-path*".to_string(),
        Rc::new(Expression::with_list(load_path)),
    );
    let mut script = format!("{}/.config/sl-sh/slsh-std.lisp", home);
    if let Err(err) = run_script(&script, environment) {
        eprintln!(
            "WARNING: Failed to load standard macros script {}: {}",
            script, err
        );
    }
    let dname = build_new_namespace(environment, "user");
    match dname {
        Ok(scope) => {
            let settings = Rc::new(RefCell::new(HashMap::new()));
            settings.borrow_mut().insert(
                "keybindings".to_string(),
                Rc::new(Expression::Atom(Atom::Symbol("emacs".to_string()))),
            );
            scope.borrow_mut().data.insert(
                "*repl-settings*".to_string(),
                Rc::new(Expression::HashMap(settings)),
            );
            environment.current_scope.push(scope);
        }
        Err(msg) => eprintln!(
            "ERROR: Failed to create default namespace \"user\": {}",
            msg
        ),
    }
    script = format!("{}/.config/sl-sh/slshrc", home);
    if let Err(err) = run_script(&script, environment) {
        eprintln!("WARNING: Failed to load init script {}: {}", script, err);
    }
}

fn get_prompt(environment: &mut Environment) -> String {
    if let Some(exp) = get_expression(environment, "__prompt") {
        let exp = match *exp {
            Expression::Atom(Atom::Lambda(_)) => {
                let mut v = Vec::with_capacity(1);
                v.push(Expression::Atom(Atom::Symbol("__prompt".to_string())));
                Rc::new(Expression::with_list(v))
            }
            _ => exp,
        };
        environment.save_exit_status = false; // Do not overwrite last exit status with prompt commands.
        let res = eval(environment, &exp);
        environment.save_exit_status = true;
        res.unwrap_or_else(|e| Expression::Atom(Atom::String(format!("ERROR: {}", e).to_string())))
            .as_string(environment)
            .unwrap_or_else(|_| "ERROR".to_string())
    } else {
        // Nothing set, use a default.
        let hostname = match env::var("HOST") {
            Ok(val) => val,
            Err(_) => "UNKNOWN".to_string(),
        };
        let pwd = match env::current_dir() {
            Ok(val) => val,
            Err(_) => {
                let mut p = PathBuf::new();
                p.push("/");
                p
            }
        };
        let namespace = if let Some(exp) = get_expression(environment, "*ns*") {
            match &*exp {
                Expression::Atom(Atom::String(s)) => s.to_string(),
                _ => "NO_NAME".to_string(),
            }
        } else {
            "NO_NAME".to_string()
        };
        format!(
            "\x1b[32m{}:\x1b[34m{}\x1b[37m(sl-sh::{})\x1b[32m>\x1b[39m ",
            hostname,
            pwd.display(),
            namespace,
        )
    }
}

fn get_color_closure(environment: Rc<RefCell<Environment>>) -> Option<ColorClosure> {
    let mut has_handle = false;
    let mut exp = Rc::new(Expression::Atom(Atom::Nil));
    if let Some(lexp) = get_expression(&environment.borrow(), "__line_handler") {
        has_handle = true;
        exp = lexp.clone();
    }
    if has_handle {
        let line_color = move |input: &str| -> String {
            let exp = match *exp {
                Expression::Atom(Atom::Lambda(_)) => {
                    let mut v = Vec::with_capacity(1);
                    v.push(Expression::Atom(Atom::Symbol("__line_handler".to_string())));
                    v.push(Expression::Atom(Atom::String(input.to_string())));
                    Rc::new(Expression::with_list(v))
                }
                _ => return input.to_string(),
            };
            environment.borrow_mut().save_exit_status = false; // Do not overwrite last exit status with line_handler.
            let res = eval(&mut environment.borrow_mut(), &exp);
            environment.borrow_mut().save_exit_status = true;
            res.unwrap_or_else(|e| {
                Expression::Atom(Atom::String(format!("ERROR: {}", e).to_string()))
            })
            .as_string(&environment.borrow())
            .unwrap_or_else(|_| "ERROR".to_string())
        };
        Some(Box::new(line_color))
    } else {
        None
    }
}

fn handle_result(
    environment: &mut Environment,
    res: io::Result<Expression>,
    con: &mut Context,
    input: &str,
) {
    match res {
        Ok(exp) => {
            if !input.is_empty() {
                if let Err(err) = con.history.push(input.into()) {
                    eprintln!("Error saving history: {}", err);
                }
            }
            match exp {
                Expression::Atom(Atom::Nil) => { /* don't print nil */ }
                Expression::File(_) => { /* don't print file contents */ }
                Expression::Process(_) => { /* should have used stdout */ }
                Expression::Atom(Atom::String(_)) => {
                    if let Err(err) = exp.write(environment) {
                        eprintln!("Error writing result: {}", err);
                    }
                }
                _ => {
                    if let Err(err) = exp.pretty_print(environment) {
                        eprintln!("Error writing result: {}", err);
                    }
                }
            }
        }
        Err(err) => {
            if !input.is_empty() {
                if let Err(err) = con.history.push_throwaway(input.into()) {
                    eprintln!("Error saving temp history: {}", err);
                }
            }
            eprintln!("{}", err);
        }
    }
}

fn apply_repl_settings(repl_settings: Rc<Expression>, con: &mut Context) {
    if let Expression::HashMap(repl_settings) = &*repl_settings {
        if let Some(keybindings) = repl_settings.borrow().get("keybindings") {
            let keybindings = keybindings.clone();
            if let Expression::Atom(Atom::Symbol(keybindings)) = &*keybindings {
                match &keybindings[..] {
                    "vi" => con.key_bindings = liner::KeyBindings::Vi,
                    "emacs" => con.key_bindings = liner::KeyBindings::Emacs,
                    _ => eprintln!("Invalid keybinding setting: {}", keybindings),
                }
            }
        }
    }
}

pub fn start_interactive(sig_int: Arc<AtomicBool>) -> i32 {
    let mut con = Context::new();
    con.history.append_duplicate_entries = false;
    con.history.inc_append = true;
    con.history.load_duplicates = false;
    con.history.share = true;
    con.key_bindings = liner::KeyBindings::Emacs;
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
    let share_dir = format!("{}/.local/share/sl-sh", home);
    if let Err(err) = create_dir_all(&share_dir) {
        eprintln!(
            "WARNING: Unable to create share directory: {}- {}",
            share_dir, err
        );
    }
    if let Err(err) = con
        .history
        .set_file_name_and_load_history(format!("{}/history", share_dir))
    {
        eprintln!("WARNING: Unable to load history: {}", err);
    }
    let environment = Rc::new(RefCell::new(build_default_environment(sig_int)));
    load_user_env(&mut environment.borrow_mut(), &home);
    let repl_settings = get_expression(&environment.borrow(), "*repl-settings*").unwrap();
    environment
        .borrow_mut()
        .root_scope
        .borrow_mut()
        .data
        .insert(
            "*last-status*".to_string(),
            Rc::new(Expression::Atom(Atom::Int(0))),
        );
    loop {
        apply_repl_settings(repl_settings.clone(), &mut con);
        environment.borrow_mut().state.stdout_status = None;
        environment.borrow_mut().state.stderr_status = None;
        // Clear the SIGINT if one occured.
        environment
            .borrow()
            .sig_int
            .compare_and_swap(true, false, Ordering::Relaxed);
        let prompt = get_prompt(&mut environment.borrow_mut());
        if let Err(err) = reap_procs(&environment.borrow()) {
            eprintln!("Error reaping processes: {}", err);
        }
        let mut shell_completer = ShellCompleter::new(environment.clone());
        con.history.search_context = if let Ok(cur_dir) = env::current_dir() {
            Some(cur_dir.to_string_lossy().to_string())
        } else {
            None
        };
        let color_closure = get_color_closure(environment.clone());
        match con.read_line(prompt, color_closure, &mut shell_completer) {
            Ok(input) => {
                let input = input.trim();
                if input.is_empty() {
                    continue;
                }
                let add_parens = !(input.starts_with('(')
                    || input.starts_with('\'')
                    || input.starts_with('`')
                    || input.starts_with('#'));
                // Clear the last status once something new is entered.
                env::set_var("LAST_STATUS".to_string(), format!("{}", 0));
                environment
                    .borrow_mut()
                    .root_scope
                    .borrow_mut()
                    .data
                    .insert(
                        "*last-status*".to_string(),
                        Rc::new(Expression::Atom(Atom::Int(i64::from(0)))),
                    );
                let ast = read(input, add_parens);
                match ast {
                    Ok(ast) => {
                        environment.borrow_mut().loose_symbols = true;
                        let res = eval(&mut environment.borrow_mut(), &ast);
                        handle_result(&mut environment.borrow_mut(), res, &mut con, input);
                        environment.borrow_mut().loose_symbols = false;
                    }
                    Err(err) => {
                        if !input.is_empty() {
                            if let Err(err) = con.history.push_throwaway(input.into()) {
                                eprintln!("Error saving temp history: {}", err);
                            }
                        }
                        eprintln!("{:?}", err);
                    }
                }
            }
            Err(err) => match err.kind() {
                ErrorKind::UnexpectedEof => return 0,
                ErrorKind::Interrupted => {}
                _ => println!("Error on input: {}", err),
            },
        }
        if environment.borrow().exit_code.is_some() {
            break;
        }
    }
    if environment.borrow().exit_code.is_some() {
        environment.borrow().exit_code.unwrap()
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
    load_user_env(&mut environment, &home);

    let mut input = String::new();
    loop {
        match io::stdin().read_line(&mut input) {
            Ok(0) => return 0,
            Ok(_n) => {
                let input = input.trim();
                environment.state.stdout_status = None;
                let add_parens =
                    !(input.starts_with('(') || input.starts_with('\'') || input.starts_with('`'));
                let ast = read(input, add_parens);
                match ast {
                    Ok(ast) => {
                        environment.loose_symbols = true;
                        match eval(&mut environment, &ast) {
                            Ok(exp) => {
                                match exp {
                                    Expression::Atom(Atom::Nil) => { /* don't print nil */ }
                                    Expression::Process(_) => { /* should have used stdout */ }
                                    _ => {
                                        if let Err(err) = exp.write(&environment) {
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

fn parse_one_run_command_line(input: &str, nargs: &mut Vec<String>) -> io::Result<()> {
    let mut in_string = false;
    let mut in_stringd = false;
    let mut token = String::new();
    let mut last_ch = ' ';
    for ch in input.chars() {
        if ch == '\'' && last_ch != '\\' {
            // Kakoune bug "
            in_string = !in_string;
            if !in_string {
                nargs.push(token);
                token = String::new();
            }
            last_ch = ch;
            continue;
        }
        if ch == '"' && last_ch != '\\' {
            // Kakoune bug "
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

pub fn run_one_command(command: &str, args: &[String]) -> io::Result<()> {
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

fn run_script(file_name: &str, environment: &mut Environment) -> io::Result<()> {
    let contents = fs::read_to_string(file_name)?;
    let ast = read(&contents, false);
    match ast {
        Ok(Expression::Vector(list)) => {
            for exp in list.borrow().iter() {
                match eval(environment, &exp) {
                    Ok(_exp) => {}
                    Err(err) => {
                        eprintln!("{}", err);
                        return Err(err);
                    }
                }
            }
            Ok(())
        }
        Ok(ast) => match eval(environment, &ast) {
            Ok(_exp) => Ok(()),
            Err(err) => {
                eprintln!("{}", err);
                Err(err)
            }
        },
        Err(err) => {
            eprintln!("{:?}", err);
            Err(io::Error::new(io::ErrorKind::Other, err.reason))
        }
    }
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
    load_user_env(&mut environment, &home);

    let mut exp_args: Vec<Expression> = Vec::with_capacity(args.len());
    for a in args {
        exp_args.push(Expression::Atom(Atom::String(a.clone())));
    }
    environment
        .root_scope
        .borrow_mut()
        .data
        .insert("args".to_string(), Rc::new(Expression::with_list(exp_args)));
    if let Err(err) = run_script(command, &mut environment) {
        eprintln!("Error running {}: {}", command, err);
        if environment.exit_code.is_none() {
            return 1;
        }
    }
    if environment.exit_code.is_some() {
        environment.exit_code.unwrap()
    } else {
        0
    }
}
