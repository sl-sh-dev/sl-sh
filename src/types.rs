use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::io::{self, Read, Write};
use std::num::{ParseFloatError, ParseIntError};
use std::process::Child;
use std::rc::Rc;

use crate::environment::*;
use crate::process::*;

#[derive(Clone, Debug)]
pub struct ParseError {
    pub reason: String,
}

#[derive(Clone, Debug)]
pub struct Lambda {
    pub params: Box<Expression>,
    pub body: Box<Expression>,
    pub capture: Rc<RefCell<Scope>>,
}

#[derive(Clone, Debug)]
pub struct Macro {
    pub params: Box<Expression>,
    pub body: Box<Expression>,
}

#[derive(Clone, Debug)]
pub enum Atom {
    Nil,
    True,
    Float(f64),
    Int(i64),
    Symbol(String),
    String(String),
    Lambda(Lambda),
    Macro(Macro),
}

impl Atom {
    pub fn to_string(&self) -> String {
        match self {
            Atom::Nil => "nil".to_string(),
            Atom::True => "true".to_string(),
            Atom::Float(f) => format!("{}", f),
            Atom::Int(i) => format!("{}", i),
            Atom::Symbol(s) => s.clone(),
            Atom::String(s) => s.clone(),
            Atom::Lambda(l) => {
                format!("Lambda ({}) ({})", l.params.to_string(), l.body.to_string())
            }
            Atom::Macro(m) => format!("Macro ({}) ({})", m.params.to_string(), m.body.to_string()),
        }
    }

    pub fn display_type(&self) -> String {
        match self {
            Atom::Nil => "Nil".to_string(),
            Atom::True => "True".to_string(),
            Atom::Float(_) => "Float".to_string(),
            Atom::Int(_) => "Int".to_string(),
            Atom::Symbol(_) => "Symbol".to_string(),
            Atom::String(_) => "String".to_string(),
            Atom::Lambda(_) => "Lambda".to_string(),
            Atom::Macro(_) => "Macro".to_string(),
        }
    }
}

#[derive(Clone, Copy)]
pub enum ProcessState {
    Running(u32),   // pid
    Over(u32, i32), // pid and exit status
}

#[derive(Clone)]
pub struct ExpList {
    pub list: Rc<RefCell<Vec<Expression>>>,
    pub range: Option<(usize, usize)>,
}

impl ExpList {
    pub fn new(list: Rc<RefCell<Vec<Expression>>>) -> ExpList {
        ExpList { list, range: None }
    }

    pub fn new_range(exp_list: &ExpList, start: usize, end: usize) -> ExpList {
        let range = if let Some(r) = exp_list.range {
            (r.0 + start, r.1 + end)
        } else {
            (start, end)
        };
        ExpList { list: exp_list.list.clone(), range: Some(range) }
    }

    pub fn insert(&self, idx: usize, item: Expression) {
        if let Some(r) = self.range() {
            self.list.borrow_mut().insert(r.0 + idx, item);
        } else {
            self.list.borrow_mut().insert(idx, item);
        }
    }

    pub fn remove(&self, idx: usize) {
        if let Some(r) = self.range() {
            self.list.borrow_mut().remove(r.0 + idx);
        } else {
            self.list.borrow_mut().remove(idx);
        }
    }

    pub fn len(&self) -> usize {
        if let Some(r) = self.range {
            r.1 - r.0
        } else {
            self.list.borrow().len()
        }
    }
}

#[macro_export]
macro_rules! list_to_slice {
    ($list:expr, $exp_list:expr) => {{
        $list = $exp_list.list.borrow();
        if let Some(range) = $exp_list.range {
            //println!("XXXX range {} to {}", range.0, range.1);
            let sl = $list.get(range.0..range.1);
            match sl {
                Some(l) => l,
                None => &*$list,
            }
        } else {
            &*$list
        }
    }};
}

#[macro_export]
macro_rules! list_to_slice_mut {
    ($list:expr, $exp_list:expr) => {{
        $list = $exp_list.list.borrow_mut();
        if let Some(range) = $exp_list.range {
            //println!("XXXX range {} to {}", range.0, range.1);
            let sl = $list.get_mut(range.0..range.1);
            match sl {
                Some(l) => l,
                None => &mut *$list,
            }
        } else {
            &mut *$list
        }
    }};
}

#[derive(Clone)]
pub enum Expression {
    Atom(Atom),
    // RefCell the vector to allow destructive forms.
    List(ExpList),//Rc<RefCell<Vec<Expression>>>),
    Func(fn(&mut Environment, &[Expression]) -> io::Result<Expression>),
    Process(ProcessState),
}

impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Atom(a) => write!(f, "Expression::Atom({:?})", a),
            Expression::List(l) => {
                let lt;
                let l = list_to_slice!(lt, l);
                write!(f, "Expression::List({:?})", l)
            }
            Expression::Func(_) => write!(f, "Expression::Func(_)"),
            Expression::Process(ProcessState::Running(pid)) => {
                write!(f, "Expression::Process(ProcessStats::Running({}))", pid)
            }
            Expression::Process(ProcessState::Over(pid, exit_status)) => write!(
                f,
                "Expression::Process(ProcessState::Over({}, {}))",
                pid, exit_status
            ),
        }
    }
}

impl Expression {
    pub fn with_list(list: Vec<Expression>) -> Expression {
        Expression::List(ExpList::new(Rc::new(RefCell::new(list))))
    }

    pub fn with_list_range(exp_list: &ExpList, start: usize, end: usize) -> Expression {
        Expression::List(ExpList::new_range(exp_list, start, end))
    }

    pub fn to_string(&self) -> String {
        match self {
            Expression::Atom(a) => a.to_string(),
            Expression::Process(ProcessState::Running(pid)) => format!("{}", pid).to_string(),
            Expression::Process(ProcessState::Over(pid, _exit_status)) => {
                format!("{}", pid).to_string()
            }
            Expression::Func(_) => "Func".to_string(),
            Expression::List(list) => {
                let mut res = String::new();
                res.push_str("( ");
                let lt;
                let list = list_to_slice!(lt, list);
                for exp in list.iter() {
                    res.push_str(&exp.to_string());
                    res.push_str(" ");
                }
                res.push(')');
                res
            }
        }
    }

    pub fn display_type(&self) -> String {
        match self {
            Expression::Atom(a) => a.display_type(),
            Expression::Process(_) => "Process".to_string(),
            Expression::Func(_) => "Func".to_string(),
            Expression::List(_) => "List".to_string(),
        }
    }

    fn pid_to_string(
        &self,
        procs: Rc<RefCell<HashMap<u32, Child>>>,
        pid: u32,
    ) -> io::Result<String> {
        match procs.borrow_mut().get_mut(&pid) {
            Some(child) => {
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
            Expression::Process(ProcessState::Running(_pid)) => Ok("".to_string()),
            Expression::Process(ProcessState::Over(pid, _exit_status)) => {
                self.pid_to_string(environment.procs.clone(), *pid)
            }
            Expression::Func(_) => Ok("".to_string()),
            Expression::List(list) => {
                let mut res = String::new();
                res.push_str("( ");
                let lt;
                let list = list_to_slice!(lt, list);
                for exp in list.iter() {
                    res.push_str(&exp.make_string(environment)?);
                    res.push_str(" ");
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
            Expression::Process(ProcessState::Running(_pid)) => Err(io::Error::new(
                io::ErrorKind::Other,
                "Not a number (process still running!)",
            )),
            Expression::Process(ProcessState::Over(pid, _exit_status)) => {
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
            Expression::Process(ProcessState::Running(_pid)) => Err(io::Error::new(
                io::ErrorKind::Other,
                "Not an integer (process still running!)",
            )),
            Expression::Process(ProcessState::Over(pid, _exit_status)) => {
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

    pub fn writef(&self, environment: &Environment, writer: &mut dyn Write) -> io::Result<()> {
        match self {
            Expression::Atom(a) => write!(writer, "{}", a.to_string())?,
            Expression::Process(ProcessState::Running(_pid)) => {
                // Maybe should write anything available?
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "Can not write, process not complete",
                ));
            }
            Expression::Process(ProcessState::Over(pid, _exit_status)) => {
                let procs = environment.procs.clone();
                let mut procs = procs.borrow_mut();
                match procs.get_mut(&pid) {
                    Some(child) => {
                        if child.stdout.is_some() {
                            let out = child.stdout.as_mut().unwrap();
                            let mut buf = [0; 1024];
                            loop {
                                match out.read(&mut buf) {
                                    Ok(0) => break,
                                    Ok(n) => writer.write_all(&buf[..n])?,
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
                wait_pid(environment, *pid, None);
            }
            Expression::Func(_) => {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "Can not write a function",
                ))
            }
            Expression::List(list) => {
                write!(writer, "( ")?;
                let lt;
                let list = list_to_slice!(lt, list);
                for exp in list.iter() {
                    exp.writef(environment, writer)?;
                    write!(writer, " ")?;
                }
                write!(writer, ")")?;
            }
        }
        writer.flush()?;
        Ok(())
    }

    pub fn write(&self, environment: &Environment) -> io::Result<()> {
        let stdout = io::stdout();
        let mut handle = stdout.lock();
        self.writef(environment, &mut handle)
    }
}
