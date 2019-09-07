use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::io::{self, Read, Write};
use std::num::{ParseFloatError, ParseIntError};
use std::process::Child;
use std::rc::Rc;

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

#[derive(Clone, Debug)]
pub enum EvalResult {
    Atom(Atom),
    Process(u32),
}

impl EvalResult {
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

    pub fn make_expression(&mut self, environment: &Environment) -> io::Result<Expression> {
        match self {
            EvalResult::Atom(a) => Ok(Expression::Atom(a.clone())),
            _ => Ok(Expression::Atom(Atom::String(
                self.make_string(environment)?,
            ))),
        }
    }

    pub fn make_string(&self, environment: &Environment) -> io::Result<String> {
        match self {
            EvalResult::Atom(a) => Ok(a.to_string()),
            EvalResult::Process(pid) => self.pid_to_string(environment.procs.clone(), *pid),
        }
    }

    pub fn make_float(&self, environment: &Environment) -> io::Result<f64> {
        match self {
            EvalResult::Atom(Atom::Float(f)) => Ok(*f),
            EvalResult::Atom(Atom::Int(i)) => Ok(*i as f64),
            EvalResult::Atom(_) => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
            EvalResult::Process(pid) => {
                let buffer = self.pid_to_string(environment.procs.clone(), *pid)?;
                let potential_float: Result<f64, ParseFloatError> = buffer.parse();
                match potential_float {
                    Ok(v) => Ok(v),
                    Err(_) => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
                }
            }
        }
    }

    pub fn make_int(&self, environment: &Environment) -> io::Result<i64> {
        match self {
            EvalResult::Atom(Atom::Int(i)) => Ok(*i),
            EvalResult::Atom(_) => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
            EvalResult::Process(pid) => {
                let buffer = self.pid_to_string(environment.procs.clone(), *pid)?;
                let potential_int: Result<i64, ParseIntError> = buffer.parse();
                match potential_int {
                    Ok(v) => Ok(v),
                    Err(_) => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
                }
            }
        }
    }

    pub fn write(self, environment: &Environment) -> io::Result<()> {
        match self {
            EvalResult::Atom(a) => print!("{}", a.to_string()),
            EvalResult::Process(pid) => {
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
            }
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct Environment<'a> {
    pub data: HashMap<String, Expression>,
    pub procs: Rc<RefCell<HashMap<u32, Child>>>,
    pub outer: Option<&'a Environment<'a>>,
}
