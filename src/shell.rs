use std::cell::RefCell;
use std::collections::HashMap;
use std::env;
use std::ffi::CStr;
use std::fs::create_dir_all;
use std::io::{self, ErrorKind};
use std::os::unix::process::CommandExt;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use liner::{keymap, Buffer, ColorClosure, Context, Prompt};

use libc::uid_t;
use nix::sys::signal::{self, SigHandler, Signal};
use nix::unistd::{gethostname, Uid};

use crate::builtins::load;
use crate::completions::*;
use crate::environment::*;
use crate::eval::*;
use crate::reader::*;
use crate::types::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Keys {
    Vi,
    Emacs,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ReplSettings {
    key_bindings: Keys,
    max_history: usize,
    vi_esc_sequence: Option<(char, char, u32)>,
    vi_normal_prompt_prefix: Option<String>,
    vi_normal_prompt_suffix: Option<String>,
    vi_insert_prompt_prefix: Option<String>,
    vi_insert_prompt_suffix: Option<String>,
}

fn load_user_env(environment: &mut Environment, home: &str, loadrc: bool) {
    let mut load_path = Vec::new();
    load_path.push(Expression::Atom(Atom::StringRef(
        environment
            .interner
            .intern(&format!("{}/.config/sl-sh", home)),
    )));
    environment.root_scope.borrow_mut().insert_exp(
        environment.interner.intern("*load-path*"),
        Expression::with_list(load_path),
    );
    if let Err(err) = load(environment, "slsh-std.lisp") {
        eprintln!(
            "WARNING: Failed to load standard macros script slsh-std.lisp: {}",
            err
        );
    }
    let dname = build_new_namespace(environment, "user");
    match dname {
        Ok(scope) => {
            let settings = Rc::new(RefCell::new(HashMap::new()));
            settings.borrow_mut().insert(
                "keybindings".to_string(),
                Rc::new(Expression::Atom(Atom::Symbol(
                    environment.interner.intern("emacs"),
                ))),
            );
            scope.borrow_mut().insert_exp(
                environment.interner.intern("*repl-settings*"),
                Expression::HashMap(settings),
            );
            environment.current_scope.push(scope);
        }
        Err(msg) => eprintln!(
            "ERROR: Failed to create default namespace \"user\": {}",
            msg
        ),
    }
    if loadrc {
        if let Err(err) = load(environment, "slshrc") {
            eprintln!("WARNING: Failed to load init script slshrc: {}", err);
        }
    }
}

fn get_prompt(environment: &mut Environment) -> Prompt {
    if let Some(exp) = get_expression(environment, "__prompt") {
        let exp = match exp.exp {
            Expression::Atom(Atom::Lambda(_)) => {
                let mut v = Vec::with_capacity(1);
                v.push(Expression::Atom(Atom::Symbol(
                    environment.interner.intern("__prompt"),
                )));
                Rc::new(Expression::with_list(v))
            }
            _ => Rc::new(exp.exp.clone()),
        };
        environment.save_exit_status = false; // Do not overwrite last exit status with prompt commands.
        let res = eval(environment, &exp);
        environment.save_exit_status = true;
        let ptext = res
            .unwrap_or_else(|e| Expression::Atom(Atom::String(format!("ERROR: {}", e))))
            .as_string(environment)
            .unwrap_or_else(|_| "ERROR".to_string());
        Prompt::from(ptext)
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
            match &exp.exp {
                Expression::Atom(Atom::String(s)) => s.to_string(),
                _ => "NO_NAME".to_string(),
            }
        } else {
            "NO_NAME".to_string()
        };
        let ptext = format!(
            "\x1b[32m{}:\x1b[34m{}\x1b[37m(sl-sh::{})\x1b[32m>\x1b[39m ",
            hostname,
            pwd.display(),
            namespace,
        );
        Prompt::from(ptext)
    }
}

fn get_color_closure(environment: Rc<RefCell<Environment>>) -> Option<ColorClosure> {
    let mut has_handle = false;
    let mut exp = Rc::new(Expression::nil());
    if let Some(lexp) = get_expression(&environment.borrow(), "__line_handler") {
        has_handle = true;
        exp = Rc::new(lexp.exp.clone());
    }
    if has_handle {
        let line_color = move |input: &str| -> String {
            let mut environment = environment.borrow_mut();
            let exp = match *exp {
                Expression::Atom(Atom::Lambda(_)) => {
                    let mut v = Vec::with_capacity(1);
                    v.push(Expression::Atom(Atom::Symbol(
                        environment.interner.intern("__line_handler"),
                    )));
                    v.push(Expression::Atom(Atom::String(input.to_string())));
                    Rc::new(Expression::with_list(v))
                }
                _ => return input.to_string(),
            };
            environment.save_exit_status = false; // Do not overwrite last exit status with line_handler.
            environment.str_ignore_expand = true;
            let res = eval(&mut environment, &exp);
            environment.str_ignore_expand = false;
            environment.save_exit_status = true;
            res.unwrap_or_else(|e| Expression::Atom(Atom::String(format!("ERROR: {}", e))))
                .as_string(&environment)
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
    save_history: bool,
) {
    match res {
        Ok(exp) => {
            if !input.is_empty() {
                if save_history {
                    if let Err(err) = con.history.push(input.into()) {
                        eprintln!("Error saving history: {}", err);
                    }
                }
                environment.root_scope.borrow_mut().insert_exp(
                    environment.interner.intern("*last-command*"),
                    Expression::Atom(Atom::String(input.to_string())),
                );
            }
            match exp {
                Expression::Pair(_) if exp.is_nil() => { /* don't print nil */ }
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
            if save_history && !input.is_empty() {
                if let Err(err) = con.history.push_throwaway(input.into()) {
                    eprintln!("Error saving temp history: {}", err);
                }
            }
            if !environment.stack_on_error {
                if let Some(exp) = &environment.error_expression {
                    let exp = exp.clone();
                    eprintln!("Error evaluting:");
                    let stderr = io::stderr();
                    let mut handle = stderr.lock();
                    if let Err(err) = exp.pretty_printf(environment, &mut handle) {
                        eprintln!("\nGOT SECONDARY ERROR PRINTING EXPRESSION: {}", err);
                    }
                    eprintln!("");
                }
                eprintln!("{}", err);
            } else {
                eprintln!("{}", err);
            }
        }
    }
}

fn apply_repl_settings(repl_settings: &Expression) -> ReplSettings {
    let mut ret = ReplSettings {
        key_bindings: Keys::Emacs,
        max_history: 1000,
        vi_esc_sequence: None,
        vi_normal_prompt_prefix: None,
        vi_normal_prompt_suffix: None,
        vi_insert_prompt_prefix: None,
        vi_insert_prompt_suffix: None,
    };
    if let Expression::HashMap(repl_settings) = repl_settings {
        if let Some(keybindings) = repl_settings.borrow().get(":keybindings") {
            let keybindings = keybindings.clone();
            if let Expression::Atom(Atom::Symbol(keybindings)) = &*keybindings {
                match &keybindings[..] {
                    ":vi" => ret.key_bindings = Keys::Vi,
                    ":emacs" => ret.key_bindings = Keys::Emacs,
                    _ => eprintln!("Invalid keybinding setting: {}", keybindings),
                }
            }
        }
        if let Some(max) = repl_settings.borrow().get(":max-history") {
            let max = max.clone();
            if let Expression::Atom(Atom::Int(max)) = &*max {
                if *max >= 0 {
                    ret.max_history = *max as usize;
                } else {
                    eprintln!("Max history must be positive: {}", max);
                }
            } else {
                eprintln!("Max history must be a positive integer: {}", max);
            }
        }
        if let Some(vi_esc) = repl_settings.borrow().get(":vi_esc_sequence") {
            let vi_esc = vi_esc.clone();
            let vl_i;
            let mut i = match &*vi_esc {
                Expression::Vector(vl) => {
                    vl_i = vl.borrow();
                    Box::new(vl_i.iter())
                }
                _ => vi_esc.iter(),
            };
            if let Some(Expression::Atom(Atom::String(keys))) = i.next() {
                if let Some(Expression::Atom(Atom::Int(ms))) = i.next() {
                    if keys.len() == 2 {
                        let mut chars = keys.chars();
                        ret.vi_esc_sequence =
                            Some((chars.next().unwrap(), chars.next().unwrap(), *ms as u32));
                    } else {
                        eprintln!(":vi_esc_sequence first value should be a string of two characters (two key sequence for escape)");
                    }
                } else {
                    eprintln!(":vi_esc_sequence second value should be number (ms delay)");
                }
            } else {
                eprintln!(
                    ":vi_esc_sequence first value should be a string (two key sequence for escape)"
                );
            }
        }
        if let Some(prefix) = repl_settings.borrow().get(":vi-normal-prompt-prefix") {
            let prefix = prefix.clone();
            if let Expression::Atom(Atom::String(prefix)) = &*prefix {
                ret.vi_normal_prompt_prefix = Some(prefix.to_string());
            }
        }
        if let Some(suffix) = repl_settings.borrow().get(":vi-normal-prompt-suffix") {
            let suffix = suffix.clone();
            if let Expression::Atom(Atom::String(suffix)) = &*suffix {
                ret.vi_normal_prompt_suffix = Some(suffix.to_string());
            }
        }
        if let Some(prefix) = repl_settings.borrow().get(":vi-insert-prompt-prefix") {
            let prefix = prefix.clone();
            if let Expression::Atom(Atom::String(prefix)) = &*prefix {
                ret.vi_insert_prompt_prefix = Some(prefix.to_string());
            }
        }
        if let Some(suffix) = repl_settings.borrow().get(":vi-insert-prompt-suffix") {
            let suffix = suffix.clone();
            if let Expression::Atom(Atom::String(suffix)) = &*suffix {
                ret.vi_insert_prompt_suffix = Some(suffix.to_string());
            }
        }
    }
    ret
}

fn exec_hook(environment: &mut Environment, input: &str) -> Result<Expression, ParseError> {
    fn read_add_parens(
        environment: &mut Environment,
        input: &str,
    ) -> Result<Expression, ParseError> {
        let add_parens = !(input.starts_with('(')
            || input.starts_with('\'')
            || input.starts_with('`')
            || input.starts_with('#'));
        read(environment, input, add_parens)
    }
    if let Some(exec_exp) = get_expression(&environment, "__exec_hook") {
        let exp = match exec_exp.exp {
            Expression::Atom(Atom::Lambda(_)) => {
                let mut v = Vec::with_capacity(2);
                v.push(Expression::Atom(Atom::Symbol(
                    environment.interner.intern("__exec_hook"),
                )));
                v.push(Expression::Atom(Atom::String(input.to_string())));
                Rc::new(Expression::with_list(v))
            }
            _ => {
                eprintln!("WARNING: __exec_hook not a lambda, ignoring.");
                return read_add_parens(environment, input);
            }
        };
        match eval(environment, &exp) {
            Ok(res) => match res {
                Expression::Atom(Atom::String(s)) => read_add_parens(environment, &s),
                Expression::Atom(Atom::StringRef(s)) => read_add_parens(environment, s),
                Expression::Atom(Atom::StringBuf(s)) => read_add_parens(environment, &s.borrow()),
                _ => Ok(res),
            },
            Err(err) => {
                eprintln!("ERROR calling __exec_hook: {}", err);
                read_add_parens(environment, input)
            }
        }
    } else {
        read_add_parens(environment, input)
    }
}

// Like the liner default but make '(' and ')' their own words for cleaner completions.
fn get_liner_words(buf: &Buffer) -> Vec<(usize, usize)> {
    let mut res = Vec::new();

    let mut word_start = None;
    let mut just_had_backslash = false;

    for (i, &c) in buf.chars().enumerate() {
        if c == '\\' {
            just_had_backslash = true;
            continue;
        }

        if let Some(start) = word_start {
            if (c == ' ' || c == '(' || c == ')') && !just_had_backslash {
                res.push((start, i));
                if c == '(' || c == ')' {
                    res.push((i, i + 1));
                }
                word_start = None;
            }
        } else if c == '(' || c == ')' {
            res.push((i, i + 1));
        } else if c != ' ' {
            word_start = Some(i);
        }

        just_had_backslash = false;
    }

    if let Some(start) = word_start {
        res.push((start, buf.num_chars()));
    }

    res
}

pub fn start_interactive(sig_int: Arc<AtomicBool>) -> i32 {
    let mut con = Context::new();
    con.set_word_divider(Box::new(get_liner_words));
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
    let uid = Uid::current();
    let euid = Uid::effective();
    env::set_var("UID", format!("{}", uid));
    env::set_var("EUID", format!("{}", euid));
    let mut env = environment.borrow_mut();
    let mut interned_sym = env.interner.intern("*uid*");
    env.root_scope.borrow_mut().insert_exp(
        interned_sym,
        Expression::Atom(Atom::Int(uid_t::from(uid) as i64)),
    );
    interned_sym = env.interner.intern("*euid*");
    env.root_scope.borrow_mut().insert_exp(
        interned_sym,
        Expression::Atom(Atom::Int(uid_t::from(euid) as i64)),
    );
    load_user_env(&mut env, &home, true);
    let repl_settings = get_expression(&env, "*repl-settings*").unwrap();
    interned_sym = env.interner.intern("*last-status*");
    env.root_scope
        .borrow_mut()
        .insert_exp(interned_sym, Expression::Atom(Atom::Int(0)));
    interned_sym = env.interner.intern("*last-command*");
    let interned_sym2 = env.interner.intern("");
    env.root_scope.borrow_mut().insert_exp(
        interned_sym,
        Expression::Atom(Atom::StringRef(interned_sym2)),
    );
    let mut current_repl_settings = ReplSettings {
        key_bindings: Keys::Emacs,
        max_history: 1000,
        vi_esc_sequence: None,
        vi_normal_prompt_prefix: None,
        vi_normal_prompt_suffix: None,
        vi_insert_prompt_prefix: None,
        vi_insert_prompt_suffix: None,
    };
    drop(env);
    con.set_completer(Box::new(ShellCompleter::new(environment.clone())));
    loop {
        let new_repl_settings = apply_repl_settings(&repl_settings.exp);
        if current_repl_settings != new_repl_settings {
            let keymap: Box<dyn keymap::KeyMap> = match new_repl_settings.key_bindings {
                Keys::Vi => {
                    let mut vi = keymap::Vi::new();
                    if let Some((ch1, ch2, timeout)) = new_repl_settings.vi_esc_sequence {
                        vi.set_esc_sequence(ch1, ch2, timeout);
                    }
                    vi.set_normal_prompt_prefix(new_repl_settings.vi_normal_prompt_prefix.clone());
                    vi.set_normal_prompt_suffix(new_repl_settings.vi_normal_prompt_suffix.clone());
                    vi.set_insert_prompt_prefix(new_repl_settings.vi_insert_prompt_prefix.clone());
                    vi.set_insert_prompt_suffix(new_repl_settings.vi_insert_prompt_suffix.clone());
                    Box::new(vi)
                }
                Keys::Emacs => Box::new(keymap::Emacs::new()),
            };
            con.set_keymap(keymap);
            con.history
                .set_max_history_size(new_repl_settings.max_history);
        };
        current_repl_settings = new_repl_settings.clone();
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
        con.history
            .set_search_context(if let Ok(cur_dir) = env::current_dir() {
                Some(cur_dir.to_string_lossy().to_string())
            } else {
                None
            });
        let color_closure = get_color_closure(environment.clone());
        match con.read_line(prompt, color_closure) {
            Ok(input) => {
                let input = input.trim();
                if input.is_empty() {
                    continue;
                }
                // Clear the last status once something new is entered.
                env::set_var("LAST_STATUS".to_string(), format!("{}", 0));
                let mut environment = environment.borrow_mut();
                interned_sym = environment.interner.intern("*last-status*");
                environment
                    .root_scope
                    .borrow_mut()
                    .insert_exp(interned_sym, Expression::Atom(Atom::Int(i64::from(0))));
                let ast = exec_hook(&mut environment, &input);
                match ast {
                    Ok(ast) => {
                        if let Err(err) = con.history.push(input.into()) {
                            eprintln!("Error saving history: {}", err);
                        }
                        environment.loose_symbols = true;
                        environment.error_expression = None;
                        let res = eval(&mut environment, &ast);
                        handle_result(&mut environment, res, &mut con, &input, false);
                        environment.loose_symbols = false;
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
    load_user_env(&mut environment, &home, true);

    let mut input = String::new();
    loop {
        match io::stdin().read_line(&mut input) {
            Ok(0) => return 0,
            Ok(_n) => {
                let input = input.trim();
                environment.state.stdout_status = None;
                let add_parens =
                    !(input.starts_with('(') || input.starts_with('\'') || input.starts_with('`'));
                let ast = read(&mut environment, input, add_parens);
                match ast {
                    Ok(ast) => {
                        environment.loose_symbols = true;
                        match eval(&mut environment, &ast) {
                            Ok(exp) => {
                                match exp {
                                    Expression::Pair(_) if exp.is_nil() => { /* don't print nil */ }
                                    Expression::Process(_) => { /* should have used stdout */ }
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
    load_user_env(&mut environment, &home, false);

    let mut exp_args: Vec<Expression> = Vec::with_capacity(args.len());
    for a in args {
        exp_args.push(Expression::Atom(Atom::StringRef(
            environment.interner.intern(a),
        )));
    }
    environment.root_scope.borrow_mut().insert_exp(
        environment.interner.intern("args"),
        Expression::with_list(exp_args),
    );
    if let Err(err) = load(&mut environment, command) {
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
