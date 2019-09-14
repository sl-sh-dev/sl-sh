use liner::Context;
use std::env;
use std::fs;
use std::io::{self, ErrorKind, Write};
use std::os::unix::process::CommandExt;
use std::path::PathBuf;
use std::process::{ChildStdin, ChildStdout, Command, Stdio};

use glob::glob;
use nix::sys::signal::{self, SigHandler, Signal};

use crate::builtins_util::*;
use crate::completions::*;
use crate::environment::*;
use crate::script::*;
use crate::types::*;

fn run_command(
    environment: &mut Environment,
    command: &str,
    args: &mut Vec<Expression>,
    stdin: Stdio,
    stdout: Stdio,
    stderr: Stdio,
    data_in: Option<Atom>,
    wait: bool,
) -> io::Result<Expression> {
    let mut new_args: Vec<String> = Vec::new();
    for a in args {
        new_args.push(a.make_string(environment)?);
    }
    let mut com_obj = Command::new(command);
    com_obj
        .args(new_args)
        .stdin(stdin)
        .stdout(stdout)
        .stderr(stderr);

    unsafe {
        com_obj.pre_exec(|| -> io::Result<()> {
            // XXX TODO, do better with these unwraps.
            /*let pgid = unistd::getpid();
            if pgid != unistd::getpgrp() {
                unistd::setpgid(pgid, pgid).unwrap();
            }*/
            signal::signal(Signal::SIGINT, SigHandler::SigDfl).unwrap();
            signal::signal(Signal::SIGHUP, SigHandler::SigDfl).unwrap();
            signal::signal(Signal::SIGTERM, SigHandler::SigDfl).unwrap();
            Ok(())
        });
    }

    let proc = com_obj.spawn();

    let mut result = Expression::Atom(Atom::Nil);
    match proc {
        Ok(mut proc) => {
            if wait {
                if let Err(err) = proc.wait() {
                    eprintln!("Failed to wait for {}: {}", command, err);
                }
            }
            if let Some(data_in) = data_in {
                if proc.stdin.is_some() {
                    let mut input: Option<ChildStdin> = None;
                    std::mem::swap(&mut proc.stdin, &mut input);
                    let mut input = input.unwrap();
                    input.write_all(data_in.to_string().as_bytes())?;
                }
            }
            let pid = add_process(environment, proc);
            /*let pid = if !wait {
                add_process(environment, proc)
            } else {
                proc.id()
            };*/
            result = Expression::Process(pid);
        }
        Err(e) => {
            eprintln!("Failed to execute {}: {}", command, e);
        }
    };
    Ok(result)
}

fn call_lambda(
    environment: &mut Environment,
    lambda: &Lambda,
    args: &[Expression],
) -> io::Result<Expression> {
    let mut new_environment = build_new_scope(environment);
    setup_args(&mut new_environment, &lambda.params, args, true)?;
    eval(
        &mut new_environment,
        &lambda.body,
        Expression::Atom(Atom::Nil),
        false,
    )
}

fn expand_macro(
    environment: &mut Environment,
    sh_macro: &Lambda,
    args: &[Expression],
) -> io::Result<Expression> {
    let mut new_environment = build_new_scope(environment);
    setup_args(&mut new_environment, &sh_macro.params, args, false)?;
    let expansion = eval(
        &mut new_environment,
        &sh_macro.body,
        Expression::Atom(Atom::Nil),
        false,
    )?;
    eval(environment, &expansion, Expression::Atom(Atom::Nil), false)
}

pub fn eval(
    environment: &mut Environment,
    expression: &Expression,
    data_in: Expression,
    use_stdout: bool,
) -> io::Result<Expression> {
    match expression {
        Expression::List(parts) => {
            let (command, parts) = match parts.split_first() {
                Some((c, p)) => (c, p),
                None => {
                    eprintln!("No valid command.");
                    return Err(io::Error::new(io::ErrorKind::Other, "No valid command."));
                }
            };
            let command = match command {
                Expression::Atom(Atom::Symbol(s)) => s,
                _ => {
                    eprintln!(
                        "Not a valid command {}, must be a symbol.",
                        command.make_string(environment)?
                    );
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "Not a valid command, must be a symbol.",
                    ));
                }
            };
            if command.is_empty() {
                return Ok(Expression::Atom(Atom::Nil));
            }

            if let Some(exp) = get_expression(environment, &command) {
                if let Expression::Func(f) = exp {
                    f(environment, &parts)
                } else if let Expression::Atom(Atom::Lambda(f)) = exp {
                    call_lambda(environment, &f, parts)
                } else if let Expression::Atom(Atom::Macro(m)) = exp {
                    expand_macro(environment, &m, parts)
                } else {
                    let exp = exp.clone();
                    eval(environment, &exp, data_in, use_stdout)
                }
            } else {
                match &command[..] {
                    "nil" => Ok(Expression::Atom(Atom::Nil)),
                    "|" | "pipe" => {
                        environment.in_pipe = true;
                        let mut out = data_in;
                        for p in parts {
                            out = eval(environment, p, out, false)?;
                        }
                        environment.in_pipe = false;
                        if use_stdout {
                            out.write(environment)?;
                            Ok(Expression::Atom(Atom::Nil))
                        } else {
                            Ok(out)
                        }
                    }
                    //"exit" => return,
                    command => {
                        let mut data = None;
                        let stdin = match data_in {
                            Expression::Atom(Atom::Nil) => Stdio::inherit(),
                            Expression::Atom(atom) => {
                                data = Some(atom);
                                Stdio::piped()
                            }
                            Expression::Process(pid) => {
                                let procs = environment.procs.clone();
                                let mut procs = procs.borrow_mut();
                                if let Some(proc) = procs.get_mut(&pid) {
                                    if proc.stdout.is_some() {
                                        let mut out: Option<ChildStdout> = None;
                                        std::mem::swap(&mut proc.stdout, &mut out);
                                        Stdio::from(out.unwrap())
                                    } else {
                                        Stdio::inherit()
                                    }
                                } else {
                                    Stdio::inherit()
                                }
                            }
                            Expression::Func(_) => {
                                return Err(io::Error::new(
                                    io::ErrorKind::Other,
                                    "Invalid expression state before command (special form).",
                                ))
                            }
                            Expression::List(_) => {
                                return Err(io::Error::new(
                                    io::ErrorKind::Other,
                                    "Invalid expression state before command (form).",
                                ))
                            }
                        };
                        let stdout = if use_stdout {
                            Stdio::inherit()
                        } else {
                            Stdio::piped()
                        };
                        let mut args = to_args(environment, parts, false)?;
                        let mut nargs: Vec<Expression> = Vec::with_capacity(args.len());
                        for arg in args.drain(..) {
                            if let Expression::Atom(Atom::String(s)) = &arg {
                                let s = match expand_tilde(&s) {
                                    Some(p) => p,
                                    None => s.to_string(), // XXX not great.
                                };
                                if s.contains('*')
                                    || s.contains('?')
                                    || s.contains('[')
                                    || s.contains('{')
                                {
                                    match glob(&s) {
                                        Ok(paths) => {
                                            let mut i = 0;
                                            for p in paths {
                                                match p {
                                                    Ok(p) => {
                                                        i += 1;
                                                        if let Some(p) = p.to_str() {
                                                            nargs.push(Expression::Atom(
                                                                Atom::String(p.to_string()),
                                                            ));
                                                        }
                                                    }
                                                    Err(err) => {
                                                        let msg = format!(
                                                            "glob error on while iterating {}, {}",
                                                            s, err
                                                        );
                                                        return Err(io::Error::new(
                                                            io::ErrorKind::Other,
                                                            msg,
                                                        ));
                                                    }
                                                }
                                            }
                                            if i == 0 {
                                                nargs.push(Expression::Atom(Atom::String(s)));
                                            }
                                        }
                                        Err(_err) => {
                                            nargs.push(Expression::Atom(Atom::String(s)));
                                        }
                                    }
                                } else {
                                    nargs.push(Expression::Atom(Atom::String(s)));
                                }
                            } else {
                                nargs.push(arg);
                            }
                        }
                        let stderr = if environment.err_null {
                            Stdio::null()
                        } else {
                            Stdio::inherit()
                        };
                        run_command(
                            environment,
                            command,
                            &mut nargs,
                            stdin,
                            stdout,
                            stderr,
                            data,
                            use_stdout || !environment.in_pipe,
                        )
                    }
                }
            }
        }
        Expression::Atom(Atom::Symbol(s)) => {
            if s.starts_with('$') {
                match env::var(&s[1..]) {
                    Ok(val) => Ok(Expression::Atom(Atom::String(val))),
                    Err(_) => Ok(Expression::Atom(Atom::String("".to_string()))),
                }
            } else if let Some(exp) = get_expression(environment, &s[..]) {
                if let Expression::Func(_) = exp {
                    Ok(Expression::Atom(Atom::String(s.clone())))
                } else {
                    Ok(exp)
                }
            } else {
                Ok(Expression::Atom(Atom::String(s.clone())))
            }
        }
        Expression::Atom(atom) => Ok(Expression::Atom(atom.clone())),
        Expression::Func(_) => Ok(Expression::Atom(Atom::Nil)),
        Expression::Process(pid) => Ok(Expression::Atom(Atom::Int(i64::from(*pid)))),
    }
}

pub fn start_interactive() {
    let mut con = Context::new();
    con.history.append_duplicate_entries = false;
    con.history.inc_append = true;
    con.history.load_duplicates = false;
    con.key_bindings = liner::KeyBindings::Vi;
    if let Err(err) = con.history.set_file_name_and_load_history("tmp_history") {
        eprintln!("Error loading history: {}", err);
    }
    let mut environment = build_default_environment();
    let home = match env::var("HOME") {
        Ok(val) => val,
        Err(_) => ".".to_string(),
    };
    let init_script = format!("{}/.config/slsh/slshrc", home);
    if let Err(err) = run_script(&init_script, &mut environment) {
        eprintln!("Failed to run init script {}: {}", init_script, err);
    }

    loop {
        let hostname = match env::var("HOSTNAME") {
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
        let prompt = if environment.data.contains_key("__prompt") {
            let mut exp = environment.data.get("__prompt").unwrap().clone();
            exp = match exp {
                Expression::Atom(Atom::Lambda(_)) => {
                    let mut v = Vec::with_capacity(1);
                    v.push(Expression::Atom(Atom::Symbol("__prompt".to_string())));
                    Expression::List(v)
                }
                _ => exp,
            };
            let res = eval(&mut environment, &exp, Expression::Atom(Atom::Nil), false);
            res.unwrap_or_else(|e| {
                Expression::Atom(Atom::String(format!("ERROR: {}", e).to_string()))
            })
            .make_string(&environment)
            .unwrap_or_else(|_| "ERROR".to_string())
        } else {
            format!(
                "\x1b[32m{}:\x1b[34m{}\x1b[37m(slsh)\x1b[32m>\x1b[39m ",
                hostname,
                pwd.display()
            )
        };
        let prompt = prompt.replace("\\x1b", "\x1b"); // Patch escape codes.
        if let Err(err) = reap_procs(&environment) {
            eprintln!("Error reaping processes: {}", err);
        }
        match con.read_line(prompt, None, &mut ShellCompleter) {
            Ok(input) => {
                if input.is_empty() {
                    continue;
                }
                let mod_input = if input.starts_with('(') {
                    format!("(use-stdout {})", input)
                } else {
                    format!("(use-stdout ({}))", input)
                };
                let tokens = tokenize(&mod_input);
                let ast = parse(&tokens);
                //println!("{:?}", ast);
                match ast {
                    Ok(ast) => {
                        match eval(&mut environment, &ast, Expression::Atom(Atom::Nil), true) {
                            Ok(_exp) => {
                                //println!("{}", s);
                                if !input.is_empty() {
                                    if let Err(err) = con.history.push(input.into()) {
                                        eprintln!("Error saving history: {}", err);
                                    }
                                }
                                /*if let Err(err) = exp.write(&environment) {
                                    eprintln!("Error writing result: {}", err);
                                }*/
                            }
                            Err(err) => eprintln!("{}", err),
                        }
                    }
                    Err(err) => eprintln!("{:?}", err),
                }
            }
            Err(err) => match err.kind() {
                ErrorKind::UnexpectedEof => return,
                ErrorKind::Interrupted => {}
                _ => println!("Error on input: {}", err),
            },
        }
    }
}

pub fn run_one_command(command: &str, args: &[String]) -> io::Result<()> {
    let mut com = Command::new(command);
    com.args(args)
        .stdout(Stdio::inherit())
        .stdin(Stdio::inherit())
        .stderr(Stdio::inherit());

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
    Ok(())
}

fn run_script(file_name: &str, environment: &mut Environment) -> io::Result<()> {
    let contents = fs::read_to_string(file_name)?;
    let tokens = tokenize(&contents);
    let ast = parse(&tokens);
    match ast {
        Ok(Expression::List(list)) => {
            for exp in list {
                match eval(environment, &exp, Expression::Atom(Atom::Nil), true) {
                    Ok(_exp) => {}
                    Err(err) => {
                        eprintln!("{}", err);
                        return Err(err);
                    }
                }
            }
            Ok(())
        }
        Ok(ast) => match eval(environment, &ast, Expression::Atom(Atom::Nil), true) {
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

pub fn run_one_script(command: &str, args: &[String]) -> io::Result<()> {
    let mut environment = build_default_environment();
    let mut exp_args: Vec<Expression> = Vec::with_capacity(args.len());
    for a in args {
        exp_args.push(Expression::Atom(Atom::String(a.clone())));
    }
    environment
        .data
        .insert("args".to_string(), Expression::List(exp_args));
    run_script(command, &mut environment)
}
