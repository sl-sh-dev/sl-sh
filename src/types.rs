use std::collections::HashMap;
use std::io::{self, Read, Write};
use std::process::ChildStdout;

#[derive(Clone)]
pub enum Atom {
    Nil,
    True,
    False,
    Float(f64),
    Int(i64),
    Symbol(String),
    String(String),
    Quote(String),
}

impl Atom {
    pub fn to_string(&self) -> String {
        match self {
            Atom::Nil => "nil".to_string(),
            Atom::True => "true".to_string(),
            Atom::False => "false".to_string(),
            Atom::Float(f) => format!("{}", f),
            Atom::Int(i) => format!("{}", i),
            Atom::Symbol(s) => s.clone(),
            Atom::String(s) => s.clone(),
            Atom::Quote(q) => q.clone(),
        }
    }
}

#[derive(Clone)]
pub enum Expression {
    Atom(Atom),
    List(Vec<Expression>),
    Func(fn(&mut Environment, &[Expression]) -> io::Result<EvalResult>),
}

#[derive(Clone, Debug)]
pub struct ParseError {
    pub reason: String,
}

pub enum EvalResult {
    Atom(Atom),
    Stdout(ChildStdout),
    Empty,
}

impl EvalResult {
    pub fn make_string(&mut self) -> io::Result<String> {
        match self {
            EvalResult::Atom(a) => Ok(a.to_string()),
            EvalResult::Stdout(out) => {
                let mut buffer = String::new();
                out.read_to_string(&mut buffer)?;
                Ok(buffer)
            }
            EvalResult::Empty => Ok("".to_string()),
        }
    }

    pub fn write(self) -> io::Result<()> {
        match self {
            EvalResult::Atom(a) => print!("{}", a.to_string()),
            EvalResult::Empty => {}
            EvalResult::Stdout(mut out) => {
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

pub struct Environment {
    pub global: HashMap<String, Expression>,
}
