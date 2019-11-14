use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::fs::File;
use std::io::{self, BufReader, BufWriter, Read, Write};
use std::iter;
use std::marker;
use std::num::{ParseFloatError, ParseIntError};
use std::process::Child;
use std::rc::Rc;

use crate::builtins_util::is_proper_list;
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

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Atom::Nil => write!(f, "nil"),
            Atom::True => write!(f, "true"),
            Atom::Float(n) => write!(f, "{}", n),
            Atom::Int(i) => write!(f, "{}", i),
            Atom::Symbol(s) => write!(f, "{}", s),
            Atom::String(s) => write!(f, "\"{}\"", s),
            Atom::Lambda(l) => write!(f, "(fn {} {})", l.params.to_string(), l.body.to_string()),
            Atom::Macro(m) => write!(f, "(macro {} {})", m.params.to_string(), m.body.to_string()),
        }
    }
}

impl Atom {
    // Like to_string but don't put quotes around strings.
    pub fn as_string(&self) -> String {
        if let Atom::String(s) = self {
            s.to_string()
        } else {
            self.to_string()
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
pub enum FileState {
    Stdin,
    Stdout,
    Stderr,
    Read(Rc<RefCell<BufReader<File>>>),
    Write(Rc<RefCell<BufWriter<File>>>),
    Closed,
}

pub struct PairIter<'a> {
    current: Option<Expression>,
    started: bool,
    _marker: marker::PhantomData<&'a Expression>,
}

impl<'a> PairIter<'a> {
    fn new(exp: Expression) -> PairIter<'a> {
        PairIter {
            current: Some(exp),
            started: false,
            _marker: marker::PhantomData,
        }
    }
}

impl<'a> Iterator for PairIter<'a> {
    type Item = &'a Expression;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.started {
            self.started = true;
        } else {
            self.current = if let Some(current) = &self.current {
                if let Expression::Pair(_e1, e2) = current {
                    Some(e2.borrow().clone())
                } else {
                    None
                }
            } else {
                None
            };
        }
        if let Some(current) = &self.current {
            if let Expression::Pair(e1, _e2) = current {
                unsafe {
                    // Need an unbound lifetime to get 'a
                    Some(&*e1.as_ptr())
                }
            } else {
                None
            }
        } else {
            None
        }
    }
}

type CallFunc =
    fn(&mut Environment, &mut dyn Iterator<Item = &Expression>) -> io::Result<Expression>;

#[derive(Clone)]
pub struct Callable {
    pub func: CallFunc,
    pub doc_str: String,
    pub is_special_form: bool,
}

impl Callable {
    pub fn new(func: CallFunc, doc_str: String, is_special_form: bool) -> Callable {
        Callable {
            func,
            doc_str,
            is_special_form,
        }
    }
}

#[derive(Clone)]
pub enum Expression {
    Atom(Atom),
    // RefCell the vector to allow destructive forms.
    Vector(Rc<RefCell<Vec<Expression>>>),
    Pair(Rc<RefCell<Expression>>, Rc<RefCell<Expression>>),
    HashMap(Rc<RefCell<HashMap<String, Rc<Expression>>>>),
    // Func is depricated use Function for new code.
    Func(fn(&mut Environment, &[Expression]) -> io::Result<Expression>),
    Function(Callable),
    Process(ProcessState),
    File(FileState),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn list_out(res: &mut String, itr: &mut dyn Iterator<Item = &Expression>) {
            let mut first = true;
            let mut last_exp = &Expression::Atom(Atom::Nil);
            for p in itr {
                if !first {
                    if let Expression::Atom(Atom::Symbol(sym)) = last_exp {
                        if sym != "," && sym != ",@" {
                            res.push_str(" ");
                        }
                    } else {
                        res.push_str(" ");
                    }
                } else {
                    first = false;
                }
                res.push_str(&p.to_string());
                last_exp = p;
            }
        }

        match self {
            Expression::Atom(a) => write!(f, "{}", a),
            Expression::Process(ProcessState::Running(pid)) => write!(f, "#<PID: {} Running>", pid),
            Expression::Process(ProcessState::Over(pid, exit_status)) => write!(
                f,
                "#<PID: {}, EXIT STATUS: {},  Complete>",
                pid, exit_status
            ),
            Expression::Func(_) => write!(f, "#<Function>"),
            Expression::Function(_) => write!(f, "#<Function>"),
            Expression::Vector(list) => {
                let mut res = String::new();
                res.push_str("#(");
                list_out(&mut res, &mut list.borrow().iter());
                res.push(')');
                write!(f, "{}", res)
            }
            Expression::Pair(e1, e2) => {
                if is_proper_list(self) {
                    match &*e1.borrow() {
                        Expression::Atom(Atom::Symbol(sym)) if sym == "quote" => {
                            f.write_str("'")?;
                            // This will be a two element list or something is wrong...
                            if let Expression::Pair(a2, _is_nil) = &*e2.borrow() {
                                f.write_str(&a2.borrow().to_string())
                            } else {
                                f.write_str(&e2.borrow().to_string())
                            }
                        }
                        Expression::Atom(Atom::Symbol(sym)) if sym == "bquote" => {
                            f.write_str("`")?;
                            // This will be a two element list or something is wrong...
                            if let Expression::Pair(a2, _is_nil) = &*e2.borrow() {
                                f.write_str(&a2.borrow().to_string())
                            } else {
                                f.write_str(&e2.borrow().to_string())
                            }
                        }
                        _ => {
                            let mut res = String::new();
                            res.push_str("(");
                            list_out(&mut res, &mut self.iter());
                            res.push(')');
                            write!(f, "{}", res)
                        }
                    }
                } else {
                    write!(
                        f,
                        "({} . {})",
                        e1.borrow().to_string(),
                        e2.borrow().to_string()
                    )
                }
            }
            Expression::HashMap(map) => {
                let mut res = String::new();
                res.push_str("(make-hash (");
                for (key, val) in map.borrow().iter() {
                    res.push_str(&format!("({} . {})", key, val));
                }
                res.push_str("))");
                write!(f, "{}", res)
            }
            Expression::File(FileState::Stdout) => write!(f, "#<STDOUT>"),
            Expression::File(FileState::Stderr) => write!(f, "#<STDERR>"),
            Expression::File(FileState::Stdin) => write!(f, "#<STDIN>"),
            Expression::File(FileState::Closed) => write!(f, "#<CLOSED FILE>"),
            Expression::File(FileState::Read(_file)) => write!(f, "#<READ FILE>"),
            Expression::File(FileState::Write(_file)) => write!(f, "#<WRITE FILE>"),
        }
    }
}

impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Atom(a) => write!(f, "Expression::Atom({:?})", a),
            Expression::Vector(l) => write!(f, "Expression::Vector({:?})", l.borrow()),
            Expression::Pair(e1, e2) => {
                write!(f, "Expression::Pair({:?} . {:?})", e1.borrow(), e2.borrow())
            }
            Expression::HashMap(map) => write!(f, "Expression::HashMap({:?})", map.borrow()),
            Expression::Func(_) => write!(f, "Expression::Func(_)"),
            Expression::Function(_) => write!(f, "Expression::Function(_)"),
            Expression::Process(ProcessState::Running(pid)) => {
                write!(f, "Expression::Process(ProcessStats::Running({}))", pid)
            }
            Expression::Process(ProcessState::Over(pid, exit_status)) => write!(
                f,
                "Expression::Process(ProcessState::Over({}, {}))",
                pid, exit_status
            ),
            Expression::File(_) => write!(f, "Expression::File(_)"),
        }
    }
}

impl Expression {
    pub fn iter(&self) -> Box<dyn Iterator<Item = &Expression>> {
        match self {
            Expression::Pair(e1, e2) => {
                Box::new(PairIter::new(Expression::Pair(e1.clone(), e2.clone())))
            }
            //Expression::Vector(list) => {
            //    Box::new(list.clone().borrow().iter())
            //}
            _ => Box::new(iter::empty()),
        }
    }

    pub fn make_function(func: CallFunc, doc_str: &str) -> Expression {
        Expression::Function(Callable::new(func, doc_str.to_string(), false))
    }

    pub fn make_special(func: CallFunc, doc_str: &str) -> Expression {
        Expression::Function(Callable::new(func, doc_str.to_string(), true))
    }

    pub fn with_list(list: Vec<Expression>) -> Expression {
        Expression::Vector(Rc::new(RefCell::new(list)))
    }

    pub fn cons_from_vec(v: &mut Vec<Expression>) -> Expression {
        let mut last_pair = Expression::Atom(Atom::Nil);
        if !v.is_empty() {
            let mut i = v.len() - 1;
            loop {
                last_pair = Expression::Pair(
                    Rc::new(RefCell::new(v.remove(i))),
                    Rc::new(RefCell::new(last_pair.clone())),
                );
                if i == 0 {
                    break;
                }
                i -= 1;
            }
        }
        last_pair
    }

    pub fn display_type(&self) -> String {
        match self {
            Expression::Atom(a) => a.display_type(),
            Expression::Process(_) => "Process".to_string(),
            Expression::Func(_) => "Function".to_string(),
            Expression::Function(_) => "Function".to_string(),
            Expression::Vector(_) => "Vector".to_string(),
            Expression::Pair(_, _) => "Pair".to_string(),
            Expression::HashMap(_) => "HashMap".to_string(),
            Expression::File(_) => "File".to_string(),
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

    fn pretty_print_int(
        &self,
        environment: &mut Environment,
        indent: usize,
        writer: &mut dyn Write,
    ) -> io::Result<()> {
        fn init_space(indent: usize, writer: &mut dyn Write) -> io::Result<()> {
            let mut i = 0;
            if indent > 0 {
                writer.write_all("\n".as_bytes())?;
            }
            while i < indent {
                writer.write_all(b"    ")?;
                i += 1;
            }
            Ok(())
        }
        match self {
            Expression::Vector(list) => {
                init_space(indent, writer)?;
                let a_str = self.to_string();
                if a_str.len() < 40 || a_str.starts_with('\'') || a_str.starts_with('`') {
                    writer.write_all(a_str.as_bytes())?;
                } else {
                    writer.write_all(b"#(")?;
                    let mut first = true;
                    for exp in list.borrow().iter() {
                        if !first {
                            writer.write_all(b" ")?;
                        } else {
                            first = false;
                        }
                        exp.pretty_print_int(environment, indent + 1, writer)?;
                    }
                    writer.write_all(b")")?;
                }
            }
            Expression::Pair(e1, e2) => {
                init_space(indent, writer)?;
                let a_str = self.to_string();
                if a_str.len() < 40 || a_str.starts_with('\'') || a_str.starts_with('`') {
                    writer.write_all(a_str.as_bytes())?;
                } else if is_proper_list(self) {
                    writer.write_all(b"(")?;
                    let mut first = true;
                    let mut last_p = &Expression::Atom(Atom::Nil);
                    for p in self.iter() {
                        if !first {
                            if let Expression::Atom(Atom::Symbol(sym)) = last_p {
                                if sym != "," && sym != ",@" {
                                    writer.write_all(b" ")?;
                                }
                            } else {
                                writer.write_all(b" ")?;
                            }
                        } else {
                            first = false;
                        }
                        p.pretty_print_int(environment, indent + 1, writer)?;
                        last_p = p;
                    }
                    writer.write_all(b")")?;
                } else {
                    write!(
                        writer,
                        "({} . {})",
                        e1.borrow().to_string(),
                        e2.borrow().to_string()
                    )?;
                }
            }
            Expression::HashMap(map) => {
                init_space(indent, writer)?;
                let a_str = self.to_string();
                if a_str.len() < 40 {
                    writer.write_all(a_str.as_bytes())?;
                } else {
                    writer.write_all(b"(make-hash (")?;
                    for (key, val) in map.borrow().iter() {
                        init_space(indent + 1, writer)?;
                        write!(writer, "({} . {})", key, val)?;
                    }
                    write!(writer, "))")?;
                }
            }
            Expression::Atom(Atom::String(_s)) => {
                write!(writer, "{}", self.to_string())?;
            }
            Expression::Atom(Atom::Lambda(l)) => {
                write!(writer, "(fn {}", l.params.to_string())?;
                l.body.pretty_print_int(environment, indent + 1, writer)?;
                writer.write_all(b")")?;
            }
            Expression::Atom(Atom::Macro(m)) => {
                write!(writer, "(macro {}", m.params.to_string())?;
                m.body.pretty_print_int(environment, indent + 1, writer)?;
                writer.write_all(b")")?;
            }
            _ => self.writef(environment, writer)?,
        }
        Ok(())
    }

    pub fn pretty_printf(
        &self,
        environment: &mut Environment,
        writer: &mut dyn Write,
    ) -> io::Result<()> {
        self.pretty_print_int(environment, 0, writer)
    }

    pub fn pretty_print(&self, environment: &mut Environment) -> io::Result<()> {
        let stdout = io::stdout();
        let mut handle = stdout.lock();
        self.pretty_print_int(environment, 0, &mut handle)
    }

    pub fn make_string(&self, environment: &Environment) -> io::Result<String> {
        match self {
            Expression::Atom(a) => Ok(a.to_string()),
            Expression::Process(ProcessState::Running(_pid)) => Ok(self.to_string()),
            Expression::Process(ProcessState::Over(pid, _exit_status)) => {
                self.pid_to_string(environment.procs.clone(), *pid)
            }
            Expression::Func(_) => Ok(self.to_string()),
            Expression::Function(_) => Ok(self.to_string()),
            Expression::Vector(_list) => Ok(self.to_string()),
            Expression::Pair(_e1, _e2) => Ok(self.to_string()),
            Expression::HashMap(_map) => Ok(self.to_string()),
            Expression::File(FileState::Stdin) => {
                let f = io::stdin();
                let mut f = f.lock();
                let mut out_str = String::new();
                f.read_to_string(&mut out_str)?;
                Ok(out_str)
            }
            Expression::File(FileState::Read(file)) => {
                let mut f = file.borrow_mut();
                let mut out_str = String::new();
                f.read_to_string(&mut out_str)?;
                Ok(out_str)
            }
            Expression::File(_) => Ok(self.to_string()),
        }
    }

    // Like make_string but don't put quotes around strings.
    pub fn as_string(&self, environment: &Environment) -> io::Result<String> {
        if let Expression::Atom(a) = self {
            Ok(a.as_string())
        } else {
            self.make_string(environment)
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
            Expression::Function(_) => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
            Expression::Vector(_) => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
            Expression::Pair(_, _) => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
            Expression::HashMap(_) => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
            Expression::File(_) => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
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
            Expression::Func(_) => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
            Expression::Function(_) => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
            Expression::Vector(_) => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
            Expression::Pair(_, _) => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
            Expression::HashMap(_) => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
            Expression::File(_) => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
        }
    }

    pub fn writef(&self, environment: &Environment, writer: &mut dyn Write) -> io::Result<()> {
        match self {
            Expression::Atom(a) => write!(writer, "{}", a.as_string())?,
            Expression::Process(ps) => {
                let pid = match ps {
                    ProcessState::Running(pid) => pid,
                    ProcessState::Over(pid, _exit_status) => pid,
                };
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
            Expression::Func(_) => write!(writer, "{}", self.to_string())?,
            Expression::Function(_) => write!(writer, "{}", self.to_string())?,
            Expression::Vector(_list) => write!(writer, "{}", self.to_string())?,
            Expression::Pair(_e1, _e2) => write!(writer, "{}", self.to_string())?,
            Expression::HashMap(_map) => write!(writer, "{}", self.to_string())?,
            Expression::File(FileState::Stdin) => {
                let f = io::stdin();
                let mut f = f.lock();
                let mut buf = [0; 1024];
                loop {
                    match f.read(&mut buf) {
                        Ok(0) => break,
                        Ok(n) => writer.write_all(&buf[..n])?,
                        Err(err) => return Err(err),
                    }
                }
            }
            Expression::File(FileState::Read(file)) => {
                let mut f = file.borrow_mut();
                let mut buf = [0; 1024];
                loop {
                    match f.read(&mut buf) {
                        Ok(0) => break,
                        Ok(n) => writer.write_all(&buf[..n])?,
                        Err(err) => return Err(err),
                    }
                }
            }
            Expression::File(_) => write!(writer, "{}", self.to_string())?,
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
