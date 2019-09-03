use liner::Context;
use std::collections::HashMap;
use std::env;
use std::io::{self, ErrorKind, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

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

    let mut result = EvalResult::Empty;
    match output {
        Ok(mut output) => {
            if wait {
                if let Err(err) = output.wait() {
                    eprintln!("{}", err);
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
            eprintln!("{}", e);
        }
    };
    Ok(result)
}

fn print(args: Vec<EvalResult>, add_newline: bool) -> io::Result<EvalResult> {
    for a in args {
        a.write()?;
    }
    if add_newline {
        println!();
    }
    Ok(EvalResult::Empty)
}

fn to_args(
    env: &mut Environment,
    parts: &[Expression],
    use_stdout: bool,
) -> io::Result<Vec<EvalResult>> {
    let mut args: Vec<EvalResult> = Vec::new();
    for a in parts {
        args.push(eval(env, a, EvalResult::Empty, use_stdout)?);
    }
    Ok(args)
}

fn to_args_str(
    env: &mut Environment,
    parts: &[Expression],
    use_stdout: bool,
) -> io::Result<Vec<String>> {
    let mut args: Vec<String> = Vec::new();
    for a in parts {
        args.push(eval(env, a, EvalResult::Empty, use_stdout)?.make_string()?);
    }
    Ok(args)
}

fn builtin_cd(env: &mut Environment, parts: &[Expression]) -> io::Result<EvalResult> {
    let home = match env::var("HOME") {
        Ok(val) => val,
        Err(_) => "/".to_string(),
    };
    let args = to_args_str(env, parts, false)?;
    let args = args.iter();
    let new_dir = args.peekable().peek().map_or(&home[..], |x| *x);
    let root = Path::new(new_dir);
    if let Err(e) = env::set_current_dir(&root) {
        eprintln!("{}", e);
    }

    Ok(EvalResult::Empty)
}

fn builtin_print(env: &mut Environment, parts: &[Expression]) -> io::Result<EvalResult> {
    print(to_args(env, parts, false)?, false)
}

fn builtin_println(env: &mut Environment, parts: &[Expression]) -> io::Result<EvalResult> {
    print(to_args(env, parts, false)?, true)
}

fn builtin_use_stdout(env: &mut Environment, parts: &[Expression]) -> io::Result<EvalResult> {
    for a in parts {
        eval(env, a, EvalResult::Empty, true)?;
    }
    Ok(EvalResult::Empty)
}

fn builtin_export(env: &mut Environment, parts: &[Expression]) -> io::Result<EvalResult> {
    if parts.len() != 2 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "export can only have two expressions",
        ))
    } else {
        let mut parts = parts.iter();
        let key = eval(env, parts.next().unwrap(), EvalResult::Empty, false)?.make_string()?;
        let val = eval(env, parts.next().unwrap(), EvalResult::Empty, false)?.make_string()?;
        if !val.is_empty() {
            env::set_var(key, val);
        } else {
            env::remove_var(key);
        }
        Ok(EvalResult::Empty)
    }
}

fn builtin_let(env: &mut Environment, parts: &[Expression]) -> io::Result<EvalResult> {
    if parts.len() != 2 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "let can only have two expressions",
        ))
    } else {
        let mut parts = parts.iter();
        let key = parts.next().unwrap();
        let key = match key {
            Expression::Atom(Atom::Symbol(s)) => s.clone(),
            _ => {
                return Err(io::Error::new(io::ErrorKind::Other, "invalid lvalue"));
            }
        };
        if key.starts_with('$') {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "use export to set environment variables",
            ));
        }
        let mut val = eval(env, parts.next().unwrap(), EvalResult::Empty, false)?;
        if let EvalResult::Atom(atom) = val {
            env.global.insert(key, Expression::Atom(atom));
        } else {
            env.global
                .insert(key, Expression::Atom(Atom::String(val.make_string()?)));
        }
        Ok(EvalResult::Empty)
    }
}

fn eval(
    env: &mut Environment,
    expression: &Expression,
    data_in: EvalResult,
    use_stdout: bool,
) -> io::Result<EvalResult> {
    match expression {
        Expression::List(parts) => {
            let (command, parts) = match parts.split_first() {
                Some((c, p)) => (eval(env, c, EvalResult::Empty, false)?.make_string()?, p),
                None => {
                    eprintln!("No valid command.");
                    return Err(io::Error::new(io::ErrorKind::Other, "No valid command."));
                }
            };

            if env.global.contains_key(&command) {
                let exp = env.global.get(&command).unwrap();
                if let Expression::Func(f) = exp {
                    f(env, parts)
                } else {
                    let exp = exp.clone();
                    eval(env, &exp, data_in, use_stdout)
                }
            } else {
                match &command[..] {
                    "|" | "pipe" => {
                        let mut out = data_in;
                        for p in parts {
                            out = eval(env, p, out, false)?;
                        }
                        if use_stdout {
                            out.write()?;
                            Ok(EvalResult::Empty)
                        //print!("{}", out.to_string()?);
                        } else {
                            Ok(out)
                        }
                    }
                    //"exit" => return,
                    command => {
                        let mut data = None;
                        let stdin = match data_in {
                            EvalResult::Atom(atom) => {
                                data = Some(atom);
                                Stdio::piped()
                            }
                            EvalResult::Stdout(out) => Stdio::from(out),
                            EvalResult::Empty => Stdio::inherit(),
                        };
                        let stdout = if use_stdout {
                            Stdio::inherit()
                        } else {
                            Stdio::piped()
                        };
                        let mut args = to_args(env, parts, false)?;
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
            } else if env.global.contains_key(&s[..]) {
                let exp = env.global.get(&s[..]).unwrap();
                if let Expression::Func(_) = exp {
                    Ok(EvalResult::Atom(Atom::String(s.clone())))
                } else {
                    let exp = exp.clone();
                    eval(env, &exp, data_in, use_stdout)
                }
            } else {
                Ok(EvalResult::Atom(Atom::String(s.clone())))
            }
        }
        Expression::Atom(atom) => Ok(EvalResult::Atom(atom.clone())),
        Expression::Func(_) => Ok(EvalResult::Empty),
    }
}

fn build_env() -> Environment {
    let mut global: HashMap<String, Expression> = HashMap::new();
    global.insert("cd".to_string(), Expression::Func(builtin_cd));
    global.insert("print".to_string(), Expression::Func(builtin_print));
    global.insert("println".to_string(), Expression::Func(builtin_println));
    global.insert(
        "use-stdout".to_string(),
        Expression::Func(builtin_use_stdout),
    );
    global.insert("export".to_string(), Expression::Func(builtin_export));
    global.insert("let".to_string(), Expression::Func(builtin_let));
    Environment { global }
}

pub fn start_interactive() {
    let mut con = Context::new();
    con.history.append_duplicate_entries = false;
    con.history.inc_append = true;
    con.history.load_duplicates = false;
    if let Err(err) = con.history.set_file_name_and_load_history("tmp_history") {
        eprintln!("Error loading history: {}", err);
    }
    let mut env = build_env();

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
        let prompt = format!(
            "\x1b[32m{}:\x1b[34m{}\x1b[37m(TEST SHELL)\x1b[32m>\x1b[39m ",
            hostname,
            pwd.display()
        );
        match con.read_line(prompt, None, &mut ShellCompleter) {
            Ok(input) => {
                let mod_input = if input.starts_with('(') {
                    format!("(use-stdout {})", input)
                } else {
                    format!("(use-stdout ({}))", input)
                };
                let tokens = tokenize(&mod_input);
                let ast = parse(&tokens);
                //println!("{:?}", ast);
                match ast {
                    Ok(ast) => match eval(&mut env, &ast, EvalResult::Empty, true) {
                        Ok(_) => {
                            //println!("{}", s);
                            if !input.is_empty() {
                                if let Err(err) = con.history.push(input.into()) {
                                    eprintln!("Error saving history: {}", err);
                                }
                            }
                        }
                        Err(err) => eprintln!("{}", err),
                    },
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
