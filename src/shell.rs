use liner::Context;
use std::env;
use std::io::{self, ErrorKind, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{ChildStdout, Command, Stdio};

use crate::completions::*;
use crate::script::*;

enum EvalOut {
    String(String),
    Stdout(ChildStdout),
    Empty,
}

impl EvalOut {
    fn to_string(&mut self) -> io::Result<String> {
        match self {
            EvalOut::String(s) => Ok(s.clone()),
            EvalOut::Stdout(out) => {
                let mut buffer = String::new();
                out.read_to_string(&mut buffer)?;
                Ok(buffer)
            }
            EvalOut::Empty => Ok("".to_string()),
        }
    }

    fn write(self) -> io::Result<()> {
        match self {
            EvalOut::String(s) => print!("{}", s),
            EvalOut::Empty => {}
            EvalOut::Stdout(mut out) => {
                let mut buf = [0; 1024];
                let stdout = io::stdout();
                let mut handle = stdout.lock();
                loop {
                    match out.read(&mut buf) {
                        Ok(0) => break,
                        Ok(_) => handle.write_all(&buf)?,
                        Err(err) => return Err(err),
                    }
                }
            }
        }
        Ok(())
    }
}

fn run_command(
    command: &str,
    args: &mut Vec<EvalOut>,
    stdin: Stdio,
    stdout: Stdio,
    data_in: Option<String>,
    wait: bool,
) -> io::Result<EvalOut> {
    let mut new_args: Vec<String> = Vec::new();
    for a in args {
        new_args.push(a.to_string()?);
    }
    let output = Command::new(command)
        .args(new_args)
        .stdin(stdin)
        .stdout(stdout)
        .spawn();

    let mut result = EvalOut::Empty;
    match output {
        Ok(mut output) => {
            if wait {
                if let Err(err) = output.wait() {
                    eprintln!("{}", err);
                }
            }
            if let Some(data_in) = data_in {
                if let Some(mut input) = output.stdin {
                    input.write_all(data_in.as_bytes())?;
                }
            }
            if let Some(out) = output.stdout {
                result = EvalOut::Stdout(out);
            }
        }
        Err(e) => {
            eprintln!("{}", e);
        }
    };
    Ok(result)
}

fn print(args: Vec<EvalOut>, add_newline: bool) -> io::Result<EvalOut> {
    for a in args {
        a.write()?;
    }
    if add_newline {
        println!("");
    }
    Ok(EvalOut::String("".to_string()))
}

fn to_args(parts: &[Expression], use_stdout: bool) -> io::Result<Vec<EvalOut>> {
    let mut args: Vec<EvalOut> = Vec::new();
    for a in parts {
        args.push(eval(a, EvalOut::Empty, use_stdout)?);
    }
    Ok(args)
}

fn to_args_str(parts: &[Expression], use_stdout: bool) -> io::Result<Vec<String>> {
    let mut args: Vec<String> = Vec::new();
    for a in parts {
        args.push(eval(a, EvalOut::Empty, use_stdout)?.to_string()?);
    }
    Ok(args)
}

fn eval(expression: &Expression, data_in: EvalOut, use_stdout: bool) -> io::Result<EvalOut> {
    match expression {
        Expression::List(parts) => {
            let (command, parts) = match parts.split_first() {
                Some((c, p)) => (eval(c, EvalOut::Empty, false)?.to_string()?, p),
                None => {
                    eprintln!("No valid command.");
                    return Err(io::Error::new(io::ErrorKind::Other, "No valid command."));
                }
            };

            let home = match env::var("HOME") {
                Ok(val) => val,
                Err(_) => "/".to_string(),
            };
            match &command[..] {
                "cd" => {
                    let args = to_args_str(parts, false)?;
                    let args = args.iter();
                    let new_dir = args.peekable().peek().map_or(&home[..], |x| *x);
                    let root = Path::new(new_dir);
                    if let Err(e) = env::set_current_dir(&root) {
                        eprintln!("{}", e);
                    }

                    Ok(EvalOut::String("".to_string()))
                }
                "print" => print(to_args(parts, false)?, false),
                "println" => print(to_args(parts, false)?, true),
                "use-stdout" => {
                    for a in parts {
                        eval(a, EvalOut::Empty, true)?;
                    }
                    Ok(EvalOut::String("".to_string()))
                }
                "|" | "pipe" => {
                    let mut out = data_in;
                    for p in parts {
                        out = eval(p, out, false)?;
                    }
                    if use_stdout {
                        out.write()?;
                        Ok(EvalOut::Empty)
                    //print!("{}", out.to_string()?);
                    } else {
                        Ok(out)
                    }
                }
                //"exit" => return,
                command => {
                    let mut data = None;
                    let stdin = match data_in {
                        EvalOut::String(s) => {
                            data = Some(s);
                            Stdio::piped()
                        }
                        EvalOut::Stdout(out) => Stdio::from(out),
                        EvalOut::Empty => Stdio::inherit(),
                    };
                    let stdout = if use_stdout {
                        Stdio::inherit()
                    } else {
                        Stdio::piped()
                    };
                    let mut args = to_args(parts, false)?;
                    run_command(command, &mut args, stdin, stdout, data, use_stdout)
                }
            }
        }
        Expression::Number(Number::Int(i)) => Ok(EvalOut::String(format!("{}", i))),
        Expression::Number(Number::Float(f)) => Ok(EvalOut::String(format!("{}", f))),
        Expression::String(s) => Ok(EvalOut::String(s.clone())),
        Expression::Symbol(s) => Ok(EvalOut::String(s.clone())),
        Expression::Nil => Ok(EvalOut::String("".to_string())),
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
                    Ok(ast) => match eval(&ast, EvalOut::Empty, true) {
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
