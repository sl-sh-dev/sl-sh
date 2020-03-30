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
use crate::eval::call_lambda;
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
    True,
    Float(f64),
    Int(i64),
    Symbol(&'static str),
    StringRef(&'static str),
    String(String),
    StringBuf(Rc<RefCell<String>>),
    Char(char),
    Lambda(Lambda),
    Macro(Macro),
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Atom::True => write!(f, "true"),
            Atom::Float(n) => write!(f, "{}", n),
            Atom::Int(i) => write!(f, "{}", i),
            Atom::Symbol(s) => write!(f, "{}", s),
            Atom::StringRef(s) => write!(f, "\"{}\"", s),
            Atom::String(s) => write!(f, "\"{}\"", s),
            Atom::StringBuf(s) => write!(f, "\"{}\"", s.borrow()),
            Atom::Char(c) => write!(f, "#\\{}", c),
            Atom::Lambda(l) => write!(f, "(fn {} {})", l.params.to_string(), l.body.to_string()),
            Atom::Macro(m) => write!(f, "(macro {} {})", m.params.to_string(), m.body.to_string()),
        }
    }
}

impl Atom {
    // Like to_string but don't put quotes around strings or #\ in front of chars.
    pub fn as_string(&self) -> String {
        if let Atom::StringRef(s) = self {
            (*s).to_string()
        } else if let Atom::String(s) = self {
            s.to_string()
        } else if let Atom::StringBuf(s) = self {
            s.borrow().to_string()
        } else if let Atom::Char(c) = self {
            c.to_string()
        } else {
            self.to_string()
        }
    }

    pub fn display_type(&self) -> String {
        match self {
            Atom::True => "True".to_string(),
            Atom::Float(_) => "Float".to_string(),
            Atom::Int(_) => "Int".to_string(),
            Atom::Symbol(_) => "Symbol".to_string(),
            Atom::String(_) => "String".to_string(),
            Atom::StringRef(_) => "String".to_string(),
            Atom::StringBuf(_) => "StringBuf".to_string(),
            Atom::Char(_) => "Char".to_string(),
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

pub enum FileState {
    Stdin,
    Stdout,
    Stderr,
    Read(RefCell<BufReader<File>>),
    Write(RefCell<BufWriter<File>>),
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
                if let Expression::Pair(p, _) = current {
                    if let Some((_e1, e2)) = &*p.borrow() {
                        Some(e2.clone())
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            };
        }
        if let Some(current) = &self.current {
            if let Expression::Pair(p, _) = current {
                let pair = unsafe {
                    // Need an unbound lifetime to get 'a
                    &*p.as_ptr()
                };
                if let Some((e1, _e2)) = pair {
                    Some(e1)
                } else {
                    None
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
    pub is_special_form: bool,
}

impl Callable {
    pub fn new(func: CallFunc, is_special_form: bool) -> Callable {
        Callable {
            func,
            is_special_form,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExpMeta {
    pub file: String,
    pub line: usize,
    pub col: usize,
}

#[derive(Clone)]
pub enum Expression {
    Atom(Atom),
    // RefCell the vector to allow destructive forms.
    Vector(Rc<RefCell<Vec<Expression>>>, Option<ExpMeta>),
    // Nil is represented with a Pair that contains None.  May seem odd
    // but allows nil to be transformed into a pair with xar!/xdr! and treated
    // as a true empty list.
    Pair(
        Rc<RefCell<Option<(Expression, Expression)>>>,
        Option<ExpMeta>,
    ),
    HashMap(Rc<RefCell<HashMap<String, Rc<Expression>>>>),
    Function(Callable),
    Process(ProcessState),
    File(Rc<RefCell<FileState>>),
    LazyFn(Lambda, Vec<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn list_out(res: &mut String, itr: &mut dyn Iterator<Item = &Expression>) {
            let mut first = true;
            let mut last_exp = &Expression::nil();
            for p in itr {
                if !first {
                    if let Expression::Atom(Atom::Symbol(sym)) = last_exp {
                        if sym != &"," && sym != &",@" {
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
            Expression::Function(_) => write!(f, "#<Function>"),
            Expression::Vector(list, _) => {
                let mut res = String::new();
                res.push_str("#(");
                list_out(&mut res, &mut list.borrow().iter());
                res.push(')');
                write!(f, "{}", res)
            }
            Expression::Pair(p, _) => {
                if let Some((e1, e2)) = &*p.borrow() {
                    if is_proper_list(self) {
                        match e1 {
                            Expression::Atom(Atom::Symbol(sym)) if sym == &"quote" => {
                                f.write_str("'")?;
                                // This will be a two element list or something is wrong...
                                if let Expression::Pair(p, _) = e2 {
                                    if let Some((a2, _is_nil)) = &*p.borrow() {
                                        f.write_str(&a2.to_string())
                                    } else {
                                        f.write_str(&e2.to_string())
                                    }
                                } else {
                                    f.write_str(&e2.to_string())
                                }
                            }
                            Expression::Atom(Atom::Symbol(sym)) if sym == &"bquote" => {
                                f.write_str("`")?;
                                // This will be a two element list or something is wrong...
                                if let Expression::Pair(p, _) = e2 {
                                    if let Some((a2, _is_nil)) = &*p.borrow() {
                                        f.write_str(&a2.to_string())
                                    } else {
                                        f.write_str(&e2.to_string())
                                    }
                                } else {
                                    f.write_str(&e2.to_string())
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
                        write!(f, "({} . {})", e1.to_string(), e2.to_string())
                    }
                } else {
                    f.write_str("nil")
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
            Expression::File(file) => match &*file.borrow() {
                FileState::Stdout => write!(f, "#<STDOUT>"),
                FileState::Stderr => write!(f, "#<STDERR>"),
                FileState::Stdin => write!(f, "#<STDIN>"),
                FileState::Closed => write!(f, "#<CLOSED FILE>"),
                FileState::Read(_file) => write!(f, "#<READ FILE>"),
                FileState::Write(_file) => write!(f, "#<WRITE FILE>"),
            },
            Expression::LazyFn(_, args) => {
                let mut res = String::new();
                res.push_str("#<LAZYFN<");
                list_out(&mut res, &mut args.iter());
                res.push_str(">>");
                write!(f, "{}", res)
            }
        }
    }
}

impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Atom(a) => write!(f, "Expression::Atom({:?})", a),
            Expression::Vector(l, _) => write!(f, "Expression::Vector({:?})", l.borrow()),
            Expression::Pair(p, _) => {
                if let Some((e1, e2)) = &*p.borrow() {
                    write!(f, "Expression::Pair({:?} . {:?})", e1, e2)
                } else {
                    write!(f, "Expression::Nil")
                }
            }
            Expression::HashMap(map) => write!(f, "Expression::HashMap({:?})", map.borrow()),
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
            Expression::LazyFn(_, exp) => write!(f, "Expression::LazyFn({:?})", exp),
        }
    }
}

impl Expression {
    // If the expression is a lazy fn then resolve it to concrete expression.
    pub fn resolve(self, environment: &mut Environment) -> io::Result<Self> {
        let mut res = self;
        while let Expression::LazyFn(lambda, parts) = &res {
            let ib = Box::new(parts.iter());
            res = call_lambda(environment, &lambda, ib, false)?;
        }
        Ok(res)
    }

    pub fn nil() -> Expression {
        Expression::Pair(Rc::new(RefCell::new(None)), None)
    }

    pub fn is_nil(&self) -> bool {
        if let Expression::Pair(p, _) = self {
            p.borrow().is_none()
        } else {
            false
        }
    }

    pub fn iter(&self) -> Box<dyn Iterator<Item = &Expression>> {
        match self {
            Expression::Pair(p, _) => {
                if p.borrow().is_some() {
                    Box::new(PairIter::new(Expression::Pair(p.clone(), None)))
                } else {
                    Box::new(iter::empty())
                }
            }
            //Expression::Vector(list) => {
            //    Box::new(list.clone().borrow().iter())
            //}
            _ => Box::new(iter::empty()),
        }
    }

    pub fn make_function(func: CallFunc, doc_str: &str, namespace: &'static str) -> Reference {
        Reference {
            exp: Expression::Function(Callable::new(func, false)),
            meta: RefMetaData {
                namespace: Some(namespace),
                doc_string: Some(doc_str.to_string()),
            },
        }
    }

    pub fn make_special(func: CallFunc, doc_str: &str, namespace: &'static str) -> Reference {
        Reference {
            exp: Expression::Function(Callable::new(func, true)),
            meta: RefMetaData {
                namespace: Some(namespace),
                doc_string: Some(doc_str.to_string()),
            },
        }
    }

    pub fn with_list(list: Vec<Expression>) -> Expression {
        Expression::Vector(Rc::new(RefCell::new(list)), None)
    }

    pub fn with_list_meta(list: Vec<Expression>, meta: Option<ExpMeta>) -> Expression {
        Expression::Vector(Rc::new(RefCell::new(list)), meta)
    }

    pub fn cons_from_vec(v: &mut Vec<Expression>, meta: Option<ExpMeta>) -> Expression {
        let mut last_pair = Expression::nil();
        if !v.is_empty() {
            let mut i = v.len() - 1;
            loop {
                if i == 0 {
                    last_pair = Expression::Pair(
                        Rc::new(RefCell::new(Some((v.remove(i), last_pair.clone())))),
                        meta,
                    );
                    break;
                } else {
                    last_pair = Expression::Pair(
                        Rc::new(RefCell::new(Some((v.remove(i), last_pair.clone())))),
                        None,
                    );
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
            Expression::Function(f) => {
                if f.is_special_form {
                    "SpecialForm".to_string()
                } else {
                    "Function".to_string()
                }
            }
            Expression::Vector(_, _) => "Vector".to_string(),
            Expression::Pair(p, _) => {
                if let Some((_, _)) = &*p.borrow() {
                    "Pair".to_string()
                } else {
                    "Nil".to_string()
                }
            }
            Expression::HashMap(_) => "HashMap".to_string(),
            Expression::File(_) => "File".to_string(),
            Expression::LazyFn(_, _) => "Lambda".to_string(),
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
                writer.write_all(b"\n")?;
            }
            while i < indent {
                writer.write_all(b"    ")?;
                i += 1;
            }
            Ok(())
        }
        match self {
            Expression::Vector(list, _) => {
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
            Expression::Pair(p, _) => {
                if let Some((e1, e2)) = &*p.borrow() {
                    init_space(indent, writer)?;
                    let a_str = self.to_string();
                    if a_str.len() < 40 || a_str.starts_with('\'') || a_str.starts_with('`') {
                        writer.write_all(a_str.as_bytes())?;
                    } else if is_proper_list(self) {
                        writer.write_all(b"(")?;
                        let mut first = true;
                        let mut last_p = &Expression::nil();
                        for p in self.iter() {
                            if !first {
                                if let Expression::Atom(Atom::Symbol(sym)) = last_p {
                                    if sym != &"," && sym != &",@" {
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
                        write!(writer, "({} . {})", e1.to_string(), e2.to_string())?;
                    }
                } else {
                    write!(writer, "nil")?;
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
            Expression::Atom(Atom::StringBuf(_s)) => {
                write!(writer, "(str-buf {})", self.to_string())?;
            }
            Expression::Atom(Atom::Char(_c)) => {
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
            Expression::Function(_) => Ok(self.to_string()),
            Expression::Vector(_, _) => Ok(self.to_string()),
            Expression::Pair(_, _) => Ok(self.to_string()),
            Expression::HashMap(_map) => Ok(self.to_string()),
            Expression::File(file) => match &*file.borrow_mut() {
                FileState::Stdin => {
                    let f = io::stdin();
                    let mut f = f.lock();
                    let mut out_str = String::new();
                    f.read_to_string(&mut out_str)?;
                    Ok(out_str)
                }
                FileState::Read(f) => {
                    let mut out_str = String::new();
                    f.borrow_mut().read_to_string(&mut out_str)?;
                    Ok(out_str)
                }
                _ => Ok(self.to_string()),
            },
            Expression::LazyFn(_, _) => Ok(self.to_string()),
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
            Expression::Function(_) => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
            Expression::Vector(_, _) => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
            Expression::Pair(_, _) => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
            Expression::HashMap(_) => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
            Expression::File(_) => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
            Expression::LazyFn(_, _) => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
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
            Expression::Function(_) => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
            Expression::Vector(_, _) => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
            Expression::Pair(_, _) => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
            Expression::HashMap(_) => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
            Expression::File(_) => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
            Expression::LazyFn(_, _) => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
        }
    }

    pub fn writef(&self, environment: &mut Environment, writer: &mut dyn Write) -> io::Result<()> {
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
            Expression::Function(_) => write!(writer, "{}", self.to_string())?,
            Expression::Vector(_, _) => write!(writer, "{}", self.to_string())?,
            Expression::Pair(_, _) => write!(writer, "{}", self.to_string())?,
            Expression::HashMap(_map) => write!(writer, "{}", self.to_string())?,
            Expression::File(file) => match &*file.borrow_mut() {
                FileState::Stdin => {
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
                FileState::Read(f) => {
                    let mut buf = [0; 1024];
                    loop {
                        match f.borrow_mut().read(&mut buf) {
                            Ok(0) => break,
                            Ok(n) => writer.write_all(&buf[..n])?,
                            Err(err) => return Err(err),
                        }
                    }
                }
                _ => write!(writer, "{}", self.to_string())?,
            },
            Expression::LazyFn(_, _) => write!(writer, "{}", self.to_string())?,
        }
        writer.flush()?;
        Ok(())
    }

    pub fn write(&self, environment: &mut Environment) -> io::Result<()> {
        let stdout = io::stdout();
        let mut handle = stdout.lock();
        self.writef(environment, &mut handle)
    }
}
