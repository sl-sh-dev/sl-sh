use liner::Context;
use std::collections::HashMap;
use std::env;
use std::io::{self, ErrorKind, Write};
use std::path::PathBuf;
use std::process::{Command, Stdio};

use crate::builtins::*;
use crate::builtins_math::*;
use crate::builtins_util::*;
use crate::completions::*;
use crate::script::*;
use crate::types::*;

fn run_command(
    command: &str,
    args: &mut Vec<EvalResult>,
    stdin: Stdio,
    stdout: Stdio,
    data_in: Option<Atom>,
    wait: bool,
) -> io::Result<EvalResult> {
    let mut new_args: Vec<String> = Vec::new();
    for a in args {
        new_args.push(a.make_string()?);
    }
    let output = Command::new(command)
        .args(new_args)
        .stdin(stdin)
        .stdout(stdout)
        .spawn();

    let mut result = EvalResult::Atom(Atom::Nil);
    match output {
        Ok(mut output) => {
            if wait {
                if let Err(err) = output.wait() {
                    eprintln!("Failed to wait for {}: {}", command, err);
                }
            }
            if let Some(data_in) = data_in {
                if let Some(mut input) = output.stdin {
                    input.write_all(data_in.to_string().as_bytes())?;
                }
            }
            if let Some(out) = output.stdout {
                result = EvalResult::Stdout(out);
            }
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
) -> io::Result<EvalResult> {
    let mut new_environment = build_new_scope(environment);
    if let Expression::List(l) = &*lambda.params {
        let var_names = to_args_str(&mut new_environment, &l, false)?;
        let mut vars = to_args(&mut new_environment, args, false)?;
        if var_names.len() != vars.len() {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "wrong number of parameters",
            ))
        } else {
            for (k, v) in var_names.iter().zip(vars.iter_mut()) {
                new_environment.data.insert(k.clone(), v.make_expression()?);
            }
            eval(
                &mut new_environment,
                &lambda.body,
                EvalResult::Atom(Atom::Nil),
                false,
            )
        }
    } else {
        eval(
            &mut new_environment,
            &lambda.body,
            EvalResult::Atom(Atom::Nil),
            false,
        )
    }
}

pub fn eval(
    environment: &mut Environment,
    expression: &Expression,
    data_in: EvalResult,
    use_stdout: bool,
) -> io::Result<EvalResult> {
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
                return Ok(EvalResult::Atom(Atom::Nil));
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
                    "nil" => Ok(EvalResult::Atom(Atom::Nil)),
                    "|" | "pipe" => {
                        let mut out = data_in;
                        for p in parts {
                            out = eval(environment, p, out, false)?;
                        }
                        if use_stdout {
                            out.write()?;
                            Ok(EvalResult::Atom(Atom::Nil))
                        } else {
                            Ok(out)
                        }
                    }
                    //"exit" => return,
                    command => {
                        let mut data = None;
                        let stdin = match data_in {
                            EvalResult::Atom(Atom::Nil) => Stdio::inherit(),
                            EvalResult::Atom(atom) => {
                                data = Some(atom);
                                Stdio::piped()
                            }
                            EvalResult::Stdout(out) => Stdio::from(out),
                        };
                        let stdout = if use_stdout {
                            Stdio::inherit()
                        } else {
                            Stdio::piped()
                        };
                        let mut args = to_args(environment, parts, false)?;
                        run_command(command, &mut args, stdin, stdout, data, use_stdout)
                    }
                }
            }
        }
        Expression::Atom(Atom::Symbol(s)) => {
            if s.starts_with('$') {
                match env::var(&s[1..]) {
                    Ok(val) => Ok(EvalResult::Atom(Atom::String(val))),
                    Err(_) => Ok(EvalResult::Atom(Atom::String("".to_string()))),
                }
            } else if let Some(exp) = get_expression(environment, &s[..]) {
                if let Expression::Func(_) = exp {
                    Ok(EvalResult::Atom(Atom::String(s.clone())))
                } else {
                    let exp = exp.clone();
                    eval(environment, &exp, data_in, use_stdout)
                }
            } else {
                Ok(EvalResult::Atom(Atom::String(s.clone())))
            }
        }
        Expression::Atom(atom) => Ok(EvalResult::Atom(atom.clone())),
        Expression::Func(_) => Ok(EvalResult::Atom(Atom::Nil)),
    }
}

fn build_default_environment<'a>() -> Environment<'a> {
    let mut data: HashMap<String, Expression> = HashMap::new();
    add_builtins(&mut data);
    add_math_builtins(&mut data);
    Environment { data, outer: None }
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
            let res = eval(&mut environment, &exp, EvalResult::Atom(Atom::Nil), false);
            res.unwrap_or_else(|_| EvalResult::Atom(Atom::String("ERROR".to_string())))
                .make_string()
                .unwrap_or_else(|_| "ERROR".to_string())
        } else {
            format!(
                "\x1b[32m{}:\x1b[34m{}\x1b[37m(TEST SHELL)\x1b[32m>\x1b[39m ",
                hostname,
                pwd.display()
            )
        };
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
                        match eval(&mut environment, &ast, EvalResult::Atom(Atom::Nil), true) {
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
