use liner::Context;
use std::cell::RefCell;
use std::collections::HashMap;
use std::env;
use std::io::{self, ErrorKind, Write};
use std::path::PathBuf;
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};
use std::rc::Rc;

use crate::builtins::*;
use crate::builtins_math::*;
use crate::builtins_util::*;
use crate::completions::*;
use crate::script::*;
use crate::types::*;

fn run_command(
    environment: &mut Environment,
    command: &str,
    args: &mut Vec<Expression>,
    stdin: Stdio,
    stdout: Stdio,
    data_in: Option<Atom>,
    wait: bool,
) -> io::Result<Expression> {
    let mut new_args: Vec<String> = Vec::new();
    for a in args {
        new_args.push(a.make_string(environment)?);
    }
    let proc = Command::new(command)
        .args(new_args)
        .stdin(stdin)
        .stdout(stdout)
        .spawn();

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
            let pid = if !wait {
                add_process(environment, proc)
            } else {
                proc.id()
            };
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
    if let Expression::List(l) = &*lambda.params {
        let var_names = to_args_str(&mut new_environment, &l, false)?;
        let mut vars = to_args(&mut new_environment, args, false)?;
        if var_names.len() != vars.len() {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "wrong number of parameters",
            ));
        } else {
            for (k, v) in var_names.iter().zip(vars.iter_mut()) {
                new_environment.data.insert(k.clone(), v.clone());
            }
        }
    }
    eval(
        &mut new_environment,
        &lambda.body,
        Expression::Atom(Atom::Nil),
        false,
    )
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
                    eprintln!("Not a valid command, must be a symbol.");
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
                } else {
                    let exp = exp.clone();
                    eval(environment, &exp, data_in, use_stdout)
                }
            } else {
                match &command[..] {
                    "nil" => Ok(Expression::Atom(Atom::Nil)),
                    "|" | "pipe" => {
                        let mut out = data_in;
                        for p in parts {
                            out = eval(environment, p, out, false)?;
                        }
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
                        run_command(
                            environment,
                            command,
                            &mut args,
                            stdin,
                            stdout,
                            data,
                            use_stdout,
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
                    let exp = exp.clone();
                    eval(environment, &exp, data_in, use_stdout)
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

fn build_default_environment<'a>() -> Environment<'a> {
    let mut data: HashMap<String, Expression> = HashMap::new();
    let procs: Rc<RefCell<HashMap<u32, Child>>> = Rc::new(RefCell::new(HashMap::new()));
    add_builtins(&mut data);
    add_math_builtins(&mut data);
    Environment {
        data,
        procs,
        outer: None,
    }
}

pub fn start_interactive() {
    let mut con = Context::new();
    con.history.append_duplicate_entries = false;
    con.history.inc_append = true;
    con.history.load_duplicates = false;
    if let Err(err) = con.history.set_file_name_and_load_history("tmp_history") {
        eprintln!("Error loading history: {}", err);
    }
    let mut environment = build_default_environment();

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
            res.unwrap_or_else(|_| Expression::Atom(Atom::String("ERROR".to_string())))
                .make_string(&environment)
                .unwrap_or_else(|_| "ERROR".to_string())
        } else {
            format!(
                "\x1b[32m{}:\x1b[34m{}\x1b[37m(TEST SHELL)\x1b[32m>\x1b[39m ",
                hostname,
                pwd.display()
            )
        };
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
                            Ok(_) => {
                                //println!("{}", s);
                                if !input.is_empty() {
                                    if let Err(err) = con.history.push(input.into()) {
                                        eprintln!("Error saving history: {}", err);
                                    }
                                }
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
