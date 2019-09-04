use std::collections::HashMap;
use std::fmt;
use std::io::{self, Read, Write};
use std::num::{ParseFloatError, ParseIntError};
use std::process::ChildStdout;

#[derive(Clone, Debug)]
pub struct Lambda {
    pub params: Box<Expression>,
    pub body: Box<Expression>,
}

#[derive(Clone, Debug)]
pub enum Atom {
    Nil,
    True,
    False,
    Float(f64),
    Int(i64),
    Symbol(String),
    String(String),
    Quote(String),
    Lambda(Lambda),
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
            Atom::Lambda(_) => "lambda".to_string(),
        }
    }
}

#[derive(Clone)]
pub enum Expression {
    Atom(Atom),
    List(Vec<Expression>),
    Func(fn(&mut Environment, &[Expression]) -> io::Result<EvalResult>),
}

impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Atom(a) => write!(f, "Expression ( Atom: {:?} )", a),
            Expression::List(l) => write!(f, "Expression ( List: {:?} )", l),
            Expression::Func(_) => write!(f, "Expression ( Func )"),
        }
    }
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

    pub fn make_float(&mut self) -> io::Result<f64> {
        match self {
            EvalResult::Atom(Atom::Float(f)) => Ok(*f),
            EvalResult::Atom(Atom::Int(i)) => Ok(*i as f64),
            EvalResult::Atom(_) => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
            EvalResult::Stdout(out) => {
                let mut buffer = String::new();
                out.read_to_string(&mut buffer)?;
                let potential_float: Result<f64, ParseFloatError> = buffer.parse();
                match potential_float {
                    Ok(v) => Ok(v),
                    Err(_) => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
                }
            }
            EvalResult::Empty => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
        }
    }

    pub fn make_int(&mut self) -> io::Result<i64> {
        match self {
            EvalResult::Atom(Atom::Int(i)) => Ok(*i),
            EvalResult::Atom(_) => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
            EvalResult::Stdout(out) => {
                let mut buffer = String::new();
                out.read_to_string(&mut buffer)?;
                let potential_int: Result<i64, ParseIntError> = buffer.parse();
                match potential_int {
                    Ok(v) => Ok(v),
                    Err(_) => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
                }
            }
            EvalResult::Empty => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
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

#[derive(Clone)]
pub struct Environment<'a> {
    pub data: HashMap<String, Expression>,
    pub outer: Option<&'a Environment<'a>>,
}
