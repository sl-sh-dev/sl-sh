use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::io::{self, Read, Write};
use std::num::{ParseFloatError, ParseIntError};
use std::process::Child;
use std::rc::Rc;

use crate::builtins_util::wait_process;

#[derive(Clone, Debug)]
pub struct ParseError {
    pub reason: String,
}

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
    Func(fn(&mut Environment, &[Expression]) -> io::Result<Expression>),
    Process(u32),
}

impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Atom(a) => write!(f, "Expression::Atom({:?})", a),
            Expression::List(l) => write!(f, "Expression::List({:?})", l),
            Expression::Func(_) => write!(f, "Expression::Func(_)"),
            Expression::Process(pid) => write!(f, "Expression::Process({})", pid),
        }
    }
}

impl Expression {
    fn pid_to_string(
        &self,
        procs: Rc<RefCell<HashMap<u32, Child>>>,
        pid: u32,
    ) -> io::Result<String> {
        match procs.borrow_mut().get_mut(&pid) {
            Some(child) => {
                //match &child.stdout {
                if child.stdout.is_some() {
                    let mut buffer = String::new();
                    child.stdout.as_mut().unwrap().read_to_string(&mut buffer)?;
                    Ok(buffer)
                } else {
                    Ok("".to_string())
                }
            }
            None => Ok("".to_string()),
        }
    }

    pub fn make_string(&self, environment: &Environment) -> io::Result<String> {
        match self {
            Expression::Atom(a) => Ok(a.to_string()),
            Expression::Process(pid) => self.pid_to_string(environment.procs.clone(), *pid),
            Expression::Func(_) => Ok("".to_string()),
            Expression::List(list) => {
                let mut res = String::new();
                res.push('(');
                let mut first = true;
                for exp in list {
                    if !first {
                        res.push_str(", ");
                    }
                    res.push_str(&exp.make_string(environment)?);
                    first = false;
                }
                res.push(')');
                Ok(res)
            }
        }
    }

    pub fn make_float(&self, environment: &Environment) -> io::Result<f64> {
        match self {
            Expression::Atom(Atom::Float(f)) => Ok(*f),
            Expression::Atom(Atom::Int(i)) => Ok(*i as f64),
            Expression::Atom(_) => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
            Expression::Process(pid) => {
                let buffer = self.pid_to_string(environment.procs.clone(), *pid)?;
                let potential_float: Result<f64, ParseFloatError> = buffer.parse();
                match potential_float {
                    Ok(v) => Ok(v),
                    Err(_) => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
                }
            }
            Expression::Func(_) => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
            Expression::List(_) => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
        }
    }

    pub fn make_int(&self, environment: &Environment) -> io::Result<i64> {
        match self {
            Expression::Atom(Atom::Int(i)) => Ok(*i),
            Expression::Atom(_) => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
            Expression::Process(pid) => {
                let buffer = self.pid_to_string(environment.procs.clone(), *pid)?;
                let potential_int: Result<i64, ParseIntError> = buffer.parse();
                match potential_int {
                    Ok(v) => Ok(v),
                    Err(_) => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
                }
            }
            Expression::Func(_) => Err(io::Error::new(io::ErrorKind::Other, "Not a integer")),
            Expression::List(_) => Err(io::Error::new(io::ErrorKind::Other, "Not a integer")),
        }
    }

    pub fn write(self, environment: &Environment) -> io::Result<()> {
        match self {
            Expression::Atom(a) => print!("{}", a.to_string()),
            Expression::Process(pid) => {
                let procs = environment.procs.clone();
                let mut procs = procs.borrow_mut();
                match procs.get_mut(&pid) {
                    Some(child) => {
                        if child.stdout.is_some() {
                            let out = child.stdout.as_mut().unwrap();
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
                        } else {
                            return Err(io::Error::new(
                                io::ErrorKind::Other,
                                "Failed to get process out to write to.",
                            ));
                        }
                    }
                    None => {
                        return Err(io::Error::new(
                            io::ErrorKind::Other,
                            "Failed to get process to write to.",
                        ));
                    }
                }
                drop(procs);
                wait_process(environment, pid)?;
            }
            Expression::Func(_) => {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "Can not write a function",
                ))
            }
            Expression::List(list) => {
                for exp in list {
                    exp.write(environment)?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct Environment<'a> {
    pub data: HashMap<String, Expression>,
    pub procs: Rc<RefCell<HashMap<u32, Child>>>,
    pub outer: Option<&'a Environment<'a>>,
}
