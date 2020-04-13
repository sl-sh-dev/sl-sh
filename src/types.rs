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

use broom::prelude::*;

use crate::builtins_util::is_proper_list;
use crate::environment::*;
use crate::eval::call_lambda;
use crate::process::*;

pub type GC = Heap<ExpObj>;

#[derive(Clone, Debug)]
pub struct Lambda {
    pub params: Expression,
    pub body: Expression,
    pub capture: Rc<RefCell<Scope>>,
}

#[derive(Clone, Debug)]
pub struct Macro {
    pub params: Expression,
    pub body: Expression,
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
    _marker: marker::PhantomData<&'a Expression>,
}

impl<'a> PairIter<'a> {
    fn new(exp: Expression) -> PairIter<'a> {
        PairIter {
            current: Some(exp),
            _marker: marker::PhantomData,
        }
    }
}

impl<'a> Iterator for PairIter<'a> {
    type Item = &'a Expression;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(current) = &mut self.current {
            let current = unsafe {
                // Need an unbound lifetime to get 'a
                &*current.as_ptr()
            };
            if let ExpEnum::Pair(e1, e2) = current.data {
                self.current = Some(e2);
                let e1 = unsafe {
                    let e: *const Expression = &e1;
                    &*e
                };
                Some(e1)
            } else {
                None
            }
        } else {
            None
        }
    }
}

pub struct PairIterMut<'a> {
    current: Option<Expression>,
    _marker: marker::PhantomData<&'a mut Expression>,
}

impl<'a> PairIterMut<'a> {
    fn new(exp: Expression) -> PairIterMut<'a> {
        PairIterMut {
            current: Some(exp),
            _marker: marker::PhantomData,
        }
    }
}

impl<'a> Iterator for PairIterMut<'a> {
    type Item = &'a mut Expression;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(current) = &mut self.current {
            let current = unsafe {
                // Need an unbound lifetime to get 'a
                &*current.as_ptr()
            };
            if let ExpEnum::Pair(mut e1, e2) = current.data {
                self.current = Some(e2);
                let e1 = unsafe {
                    let e: *mut Expression = &mut e1;
                    &mut *e
                };
                Some(e1)
            } else {
                None
            }
        } else {
            None
        }
    }
}

type CallFunc =
    fn(&mut Environment, &mut dyn Iterator<Item = &mut Expression>) -> io::Result<Expression>;

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
    pub file: &'static str,
    pub line: usize,
    pub col: usize,
}

#[derive(Clone)]
pub enum ExpEnum {
    Atom(Atom),
    // RefCell the vector to allow destructive forms.
    Vector(Vec<Expression>),
    Pair(Expression, Expression),
    HashMap(HashMap<String, Expression>),
    Function(Callable),
    Process(ProcessState),
    File(Rc<RefCell<FileState>>),
    LazyFn(Lambda, Vec<Expression>),
    Nil,
}

impl ExpEnum {
    pub fn replace(&mut self, new_data: ExpEnum) {
        *self = new_data;
    }

    pub fn cons_from_vec(gc: &mut GC, v: &mut Vec<Expression>) -> ExpEnum {
        let mut last_pair = ExpEnum::Nil;
        if !v.is_empty() {
            let mut i = v.len() - 1;
            loop {
                if i == 0 {
                    last_pair = ExpEnum::Pair(
                        v.remove(i),
                        Expression::alloc(
                            gc,
                            ExpObj {
                                data: last_pair.clone(),
                                meta: None,
                            },
                        ),
                    );
                    break;
                } else {
                    last_pair = ExpEnum::Pair(
                        v.remove(i),
                        Expression::alloc(
                            gc,
                            ExpObj {
                                data: last_pair.clone(),
                                meta: None,
                            },
                        ),
                    );
                }
                i -= 1;
            }
        }
        last_pair
    }
}

#[derive(Clone)]
pub struct ExpObj {
    pub data: ExpEnum,
    pub meta: Option<ExpMeta>,
}

impl Trace<ExpObj> for ExpEnum {
    fn trace(&self, tracer: &mut Tracer<ExpObj>) {
        match self {
            Self::Vector(list) => list.trace(tracer),
            Self::Pair(p1, p2) => {
                p1.trace(tracer);
                p2.trace(tracer);
            }
            Self::HashMap(map) => map.trace(tracer),
            Self::LazyFn(lambda, exp) => {
                lambda.params.trace(tracer);
                lambda.body.trace(tracer);
                exp.trace(tracer);
            }
            Self::Atom(Atom::Lambda(lambda)) => {
                lambda.params.trace(tracer);
                lambda.body.trace(tracer);
            }
            Self::Atom(Atom::Macro(mac)) => {
                mac.params.trace(tracer);
                mac.body.trace(tracer);
            }
            _ => {}
        }
    }
}

impl Trace<Self> for ExpObj {
    fn trace(&self, tracer: &mut Tracer<Self>) {
        self.data.trace(tracer);
    }
}

#[derive(Clone, Copy)]
pub struct Expression {
    pub obj: Handle<ExpObj>,
}

impl Expression {
    pub fn new(obj: Handle<ExpObj>) -> Expression {
        Expression { obj }
    }

    pub fn alloc(gc: &mut GC, obj: ExpObj) -> Expression {
        let obj = gc.insert_temp(obj);
        Expression { obj }
    }

    pub fn alloc_data(gc: &mut GC, data: ExpEnum) -> Expression {
        let obj = gc.insert_temp(ExpObj { data, meta: None });
        Expression { obj }
    }

    pub fn make_nil(gc: &mut GC) -> Expression {
        let obj = gc.insert_temp(ExpObj {
            data: ExpEnum::Nil,
            meta: None,
        });
        Expression { obj }
    }

    pub fn make_true(gc: &mut GC) -> Expression {
        let obj = gc.insert_temp(ExpObj {
            data: ExpEnum::Atom(Atom::True),
            meta: None,
        });
        Expression { obj }
    }

    pub fn replace(&mut self, new_data: Expression) {
        self.obj = new_data.obj;
    }

    pub fn get(&self) -> &ExpEnum {
        unsafe { &self.obj.get_unchecked().data }
    }

    pub fn get_mut(&mut self) -> &mut ExpEnum {
        unsafe { &mut self.obj.get_mut_unchecked().data }
    }

    pub fn as_ptr(&mut self) -> *mut ExpObj {
        self.obj.as_ptr()
    }

    pub fn meta(&self) -> &Option<ExpMeta> {
        unsafe { &self.obj.get_unchecked().meta }
    }

    //fn box_slice_it<'a>(v: &'a [Expression]) -> Box<dyn Iterator<Item = &Expression> + 'a> {
    //pub fn iter<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Expression>> {
    pub fn iter<'a>(&'a self) -> Box<dyn Iterator<Item = &Expression> + 'a> {
        match self.get() {
            ExpEnum::Pair(_, _) => Box::new(PairIter::new(*self)),
            ExpEnum::Vector(list) => Box::new(list.iter()),
            _ => Box::new(iter::empty()),
        }
    }

    pub fn iter_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &mut Expression> + 'a> {
        let exp = *self;
        match self.get_mut() {
            ExpEnum::Pair(_, _) => Box::new(PairIterMut::new(exp)),
            ExpEnum::Vector(list) => Box::new(list.iter_mut()),
            _ => Box::new(iter::empty()),
        }
    }

    pub fn is_nil(&self) -> bool {
        if let ExpEnum::Nil = self.get() {
            true
        } else {
            false
        }
    }

    // If the expression is a lazy fn then resolve it to concrete expression.
    pub fn resolve(self, environment: &mut Environment) -> io::Result<Self> {
        let mut res = self;
        while let ExpEnum::LazyFn(lambda, parts) = res.get_mut() {
            let mut lambda = lambda.clone();
            let ib = &mut Box::new(parts.iter_mut());
            res = call_lambda(environment, &mut lambda, ib, false)?;
        }
        Ok(res)
    }

    pub fn make_function(
        gc: &mut GC,
        func: CallFunc,
        doc_str: &str,
        namespace: &'static str,
    ) -> Reference {
        Reference::new(
            gc,
            ExpEnum::Function(Callable::new(func, false)),
            RefMetaData {
                namespace: Some(namespace),
                doc_string: Some(doc_str.to_string()),
            },
        )
    }

    pub fn make_special(
        gc: &mut GC,
        func: CallFunc,
        doc_str: &str,
        namespace: &'static str,
    ) -> Reference {
        Reference::new(
            gc,
            ExpEnum::Function(Callable::new(func, true)),
            RefMetaData {
                namespace: Some(namespace),
                doc_string: Some(doc_str.to_string()),
            },
        )
    }

    pub fn with_list(gc: &mut GC, list: Vec<Expression>) -> Expression {
        Expression::alloc(
            gc,
            ExpObj {
                data: ExpEnum::Vector(list),
                meta: None,
            },
        )
    }

    pub fn with_list_meta(gc: &mut GC, list: Vec<Expression>, meta: Option<ExpMeta>) -> Expression {
        Expression::alloc(
            gc,
            ExpObj {
                data: ExpEnum::Vector(list),
                meta,
            },
        )
    }

    pub fn cons_from_vec(
        gc: &mut GC,
        v: &mut Vec<Expression>,
        meta: Option<ExpMeta>,
    ) -> Expression {
        let mut last_pair = ExpEnum::Nil;
        if !v.is_empty() {
            let mut i = v.len() - 1;
            loop {
                if i == 0 {
                    last_pair = ExpEnum::Pair(
                        v.remove(i),
                        Expression::alloc(
                            gc,
                            ExpObj {
                                data: last_pair.clone(),
                                meta: None,
                            },
                        ),
                    );
                    break;
                } else {
                    last_pair = ExpEnum::Pair(
                        v.remove(i),
                        Expression::alloc(
                            gc,
                            ExpObj {
                                data: last_pair.clone(),
                                meta: None,
                            },
                        ),
                    );
                }
                i -= 1;
            }
        }
        Expression::alloc(
            gc,
            ExpObj {
                data: last_pair,
                meta: meta,
            },
        )
    }

    pub fn display_type(&self) -> String {
        match self.get() {
            ExpEnum::Atom(a) => a.display_type(),
            ExpEnum::Process(_) => "Process".to_string(),
            ExpEnum::Function(f) => {
                if f.is_special_form {
                    "SpecialForm".to_string()
                } else {
                    "Function".to_string()
                }
            }
            ExpEnum::Vector(_) => "Vector".to_string(),
            ExpEnum::Pair(_, _) => "Pair".to_string(),
            ExpEnum::HashMap(_) => "HashMap".to_string(),
            ExpEnum::File(_) => "File".to_string(),
            ExpEnum::LazyFn(_, _) => "Lambda".to_string(),
            ExpEnum::Nil => "Nil".to_string(),
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
        match self.get() {
            ExpEnum::Vector(list) => {
                init_space(indent, writer)?;
                let a_str = self.to_string();
                if a_str.len() < 40 || a_str.starts_with('\'') || a_str.starts_with('`') {
                    writer.write_all(a_str.as_bytes())?;
                } else {
                    writer.write_all(b"#(")?;
                    let mut first = true;
                    for exp in list.iter() {
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
            ExpEnum::Pair(e1, e2) => {
                init_space(indent, writer)?;
                let a_str = self.to_string();
                if a_str.len() < 40 || a_str.starts_with('\'') || a_str.starts_with('`') {
                    writer.write_all(a_str.as_bytes())?;
                } else if is_proper_list(self) {
                    writer.write_all(b"(")?;
                    let mut first = true;
                    let mut last_p = &ExpEnum::Nil;
                    for p in self.iter() {
                        if !first {
                            if let ExpEnum::Atom(Atom::Symbol(sym)) = last_p {
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
                        last_p = p.get();
                    }
                    writer.write_all(b")")?;
                } else {
                    write!(writer, "({} . {})", e1.to_string(), e2.to_string())?;
                }
            }
            ExpEnum::Nil => write!(writer, "nil")?,
            ExpEnum::HashMap(map) => {
                init_space(indent, writer)?;
                let a_str = self.to_string();
                if a_str.len() < 40 {
                    writer.write_all(a_str.as_bytes())?;
                } else {
                    writer.write_all(b"(make-hash (")?;
                    for (key, val) in map.iter() {
                        init_space(indent + 1, writer)?;
                        write!(writer, "({} . {})", key, val)?;
                    }
                    write!(writer, "))")?;
                }
            }
            ExpEnum::Atom(Atom::String(_s)) => {
                write!(writer, "{}", self.to_string())?;
            }
            ExpEnum::Atom(Atom::StringBuf(_s)) => {
                write!(writer, "(str-buf {})", self.to_string())?;
            }
            ExpEnum::Atom(Atom::Char(_c)) => {
                write!(writer, "{}", self.to_string())?;
            }
            ExpEnum::Atom(Atom::Lambda(l)) => {
                write!(writer, "(fn {}", l.params.to_string())?;
                l.body.pretty_print_int(environment, indent + 1, writer)?;
                writer.write_all(b")")?;
            }
            ExpEnum::Atom(Atom::Macro(m)) => {
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
        match self.get() {
            ExpEnum::Atom(a) => Ok(a.to_string()),
            ExpEnum::Process(ProcessState::Running(_pid)) => Ok(self.to_string()),
            ExpEnum::Process(ProcessState::Over(pid, _exit_status)) => {
                self.pid_to_string(environment.procs.clone(), *pid)
            }
            ExpEnum::Function(_) => Ok(self.to_string()),
            ExpEnum::Vector(_) => Ok(self.to_string()),
            ExpEnum::Pair(_, _) => Ok(self.to_string()),
            ExpEnum::Nil => Ok(self.to_string()),
            ExpEnum::HashMap(_map) => Ok(self.to_string()),
            ExpEnum::File(file) => match &*file.borrow_mut() {
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
            ExpEnum::LazyFn(_, _) => Ok(self.to_string()),
        }
    }

    // Like make_string but don't put quotes around strings.
    pub fn as_string(&self, environment: &Environment) -> io::Result<String> {
        if let ExpEnum::Atom(a) = self.get() {
            Ok(a.as_string())
        } else {
            self.make_string(environment)
        }
    }

    pub fn make_float(&self, environment: &Environment) -> io::Result<f64> {
        match self.get() {
            ExpEnum::Atom(Atom::Float(f)) => Ok(*f),
            ExpEnum::Atom(Atom::Int(i)) => Ok(*i as f64),
            ExpEnum::Atom(_) => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
            ExpEnum::Process(ProcessState::Running(_pid)) => Err(io::Error::new(
                io::ErrorKind::Other,
                "Not a number (process still running!)",
            )),
            ExpEnum::Process(ProcessState::Over(pid, _exit_status)) => {
                let buffer = self.pid_to_string(environment.procs.clone(), *pid)?;
                let potential_float: Result<f64, ParseFloatError> = buffer.parse();
                match potential_float {
                    Ok(v) => Ok(v),
                    Err(_) => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
                }
            }
            ExpEnum::Function(_) => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
            ExpEnum::Vector(_) => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
            ExpEnum::Pair(_, _) => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
            ExpEnum::Nil => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
            ExpEnum::HashMap(_) => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
            ExpEnum::File(_) => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
            ExpEnum::LazyFn(_, _) => Err(io::Error::new(io::ErrorKind::Other, "Not a number")),
        }
    }

    pub fn make_int(&self, environment: &Environment) -> io::Result<i64> {
        match self.get() {
            ExpEnum::Atom(Atom::Int(i)) => Ok(*i),
            ExpEnum::Atom(_) => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
            ExpEnum::Process(ProcessState::Running(_pid)) => Err(io::Error::new(
                io::ErrorKind::Other,
                "Not an integer (process still running!)",
            )),
            ExpEnum::Process(ProcessState::Over(pid, _exit_status)) => {
                let buffer = self.pid_to_string(environment.procs.clone(), *pid)?;
                let potential_int: Result<i64, ParseIntError> = buffer.parse();
                match potential_int {
                    Ok(v) => Ok(v),
                    Err(_) => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
                }
            }
            ExpEnum::Function(_) => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
            ExpEnum::Vector(_) => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
            ExpEnum::Pair(_, _) => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
            ExpEnum::Nil => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
            ExpEnum::HashMap(_) => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
            ExpEnum::File(_) => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
            ExpEnum::LazyFn(_, _) => Err(io::Error::new(io::ErrorKind::Other, "Not an integer")),
        }
    }

    pub fn writef(&self, environment: &mut Environment, writer: &mut dyn Write) -> io::Result<()> {
        match self.get() {
            ExpEnum::Atom(a) => write!(writer, "{}", a.as_string())?,
            ExpEnum::Process(ps) => {
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
            ExpEnum::Function(_) => write!(writer, "{}", self.to_string())?,
            ExpEnum::Vector(_) => write!(writer, "{}", self.to_string())?,
            ExpEnum::Pair(_, _) => write!(writer, "{}", self.to_string())?,
            ExpEnum::Nil => write!(writer, "{}", self.to_string())?,
            ExpEnum::HashMap(_map) => write!(writer, "{}", self.to_string())?,
            ExpEnum::File(file) => match &*file.borrow_mut() {
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
            ExpEnum::LazyFn(_, _) => write!(writer, "{}", self.to_string())?,
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

impl Trace<ExpObj> for Expression {
    fn trace(&self, tracer: &mut Tracer<ExpObj>) {
        self.obj.trace(tracer);
    }
}

impl AsRef<Handle<ExpObj>> for Expression {
    fn as_ref(&self) -> &Handle<ExpObj> {
        &self.obj
    }
}

impl AsRef<ExpEnum> for Expression {
    fn as_ref(&self) -> &ExpEnum {
        &self.get()
    }
}

impl From<Rooted<ExpObj>> for Expression {
    fn from(rooted: Rooted<ExpObj>) -> Self {
        Expression {
            obj: rooted.handle(),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn list_out(res: &mut String, itr: &mut dyn Iterator<Item = &Expression>) {
            let mut first = true;
            let mut last_exp = &ExpEnum::Nil;
            for p in itr {
                if !first {
                    if let ExpEnum::Atom(Atom::Symbol(sym)) = last_exp {
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
                last_exp = p.get();
            }
        }

        match self.get() {
            ExpEnum::Atom(a) => write!(f, "{}", a),
            ExpEnum::Process(ProcessState::Running(pid)) => write!(f, "#<PID: {} Running>", pid),
            ExpEnum::Process(ProcessState::Over(pid, exit_status)) => write!(
                f,
                "#<PID: {}, EXIT STATUS: {},  Complete>",
                pid, exit_status
            ),
            ExpEnum::Function(_) => write!(f, "#<Function>"),
            ExpEnum::Vector(list) => {
                let mut res = String::new();
                res.push_str("#(");
                list_out(&mut res, &mut list.iter());
                res.push(')');
                write!(f, "{}", res)
            }
            ExpEnum::Pair(e1, e2) => {
                if is_proper_list(self) {
                    match e1.get() {
                        ExpEnum::Atom(Atom::Symbol(sym)) if sym == &"quote" => {
                            f.write_str("'")?;
                            // This will be a two element list or something is wrong...
                            if let ExpEnum::Pair(a2, _) = e2.get() {
                                f.write_str(&a2.to_string())
                            } else {
                                f.write_str(&e2.to_string())
                            }
                        }
                        ExpEnum::Atom(Atom::Symbol(sym)) if sym == &"bquote" => {
                            f.write_str("`")?;
                            // This will be a two element list or something is wrong...
                            if let ExpEnum::Pair(a2, _) = e2.get() {
                                f.write_str(&a2.to_string())
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
            }
            ExpEnum::Nil => f.write_str("nil"),
            ExpEnum::HashMap(map) => {
                let mut res = String::new();
                res.push_str("(make-hash (");
                for (key, val) in map.iter() {
                    res.push_str(&format!("({} . {})", key, val));
                }
                res.push_str("))");
                write!(f, "{}", res)
            }
            ExpEnum::File(file) => match &*file.borrow() {
                FileState::Stdout => write!(f, "#<STDOUT>"),
                FileState::Stderr => write!(f, "#<STDERR>"),
                FileState::Stdin => write!(f, "#<STDIN>"),
                FileState::Closed => write!(f, "#<CLOSED FILE>"),
                FileState::Read(_file) => write!(f, "#<READ FILE>"),
                FileState::Write(_file) => write!(f, "#<WRITE FILE>"),
            },
            ExpEnum::LazyFn(_, args) => {
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
        match self.get() {
            ExpEnum::Atom(a) => write!(f, "ExpEnum::Atom({:?})", a),
            ExpEnum::Vector(l) => write!(f, "ExpEnum::Vector({:?})", l),
            ExpEnum::Pair(e1, e2) => write!(f, "ExpEnum::Pair({:?} . {:?})", e1, e2),
            ExpEnum::HashMap(map) => write!(f, "ExpEnum::HashMap({:?})", map),
            ExpEnum::Function(_) => write!(f, "ExpEnum::Function(_)"),
            ExpEnum::Process(ProcessState::Running(pid)) => {
                write!(f, "ExpEnum::Process(ProcessStats::Running({}))", pid)
            }
            ExpEnum::Process(ProcessState::Over(pid, exit_status)) => write!(
                f,
                "ExpEnum::Process(ProcessState::Over({}, {}))",
                pid, exit_status
            ),
            ExpEnum::File(_) => write!(f, "ExpEnum::File(_)"),
            ExpEnum::LazyFn(_, exp) => write!(f, "ExpEnum::LazyFn({:?})", exp),
            ExpEnum::Nil => write!(f, "ExpEnum::Nil"),
        }
    }
}
