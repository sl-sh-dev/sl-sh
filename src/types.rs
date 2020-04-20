use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::fs::File;
use std::io::{self, BufReader, BufWriter, Read, Write};
use std::iter;
use std::num::{ParseFloatError, ParseIntError};
use std::process::Child;
use std::rc::Rc;
use std::sync::RwLock;

use broom::prelude::*;

use crate::builtins_util::is_proper_list;
use crate::environment::*;
use crate::eval::call_lambda;
use crate::process::*;

pub type GC = Heap<ExpObj>;

static mut STATIC_GC: Option<RwLock<GC>> = None;

pub fn init_gc() {
    unsafe {
        if STATIC_GC.is_none() {
            STATIC_GC = Some(RwLock::new(GC::new()));
        }
    }
}

pub fn gc<'a>() -> &'a GC {
    unsafe { &*(&*STATIC_GC.as_ref().unwrap().read().unwrap() as *const GC) }
}

pub fn gc_mut<'a>() -> &'a mut GC {
    unsafe { &mut *(&mut *STATIC_GC.as_ref().unwrap().write().unwrap() as *mut GC) }
}

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

pub struct PairIter {
    current: Option<Expression>,
}

impl PairIter {
    fn new(exp: Expression) -> PairIter {
        PairIter { current: Some(exp) }
    }
}

impl Iterator for PairIter {
    type Item = Expression;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(current) = self.current {
            if let ExpEnum::Pair(e1, e2) = current.get().data {
                self.current = Some(e2);
                Some(e1)
            } else {
                None
            }
        } else {
            None
        }
    }
}

pub struct ListIter<'a> {
    inner_iter: std::slice::Iter<'a, Expression>,
}

impl<'a> ListIter<'a> {
    pub fn new_list(list: &[Expression]) -> ListIter<'_> {
        ListIter {
            inner_iter: list.iter(),
        }
    }

    pub fn new_slice(list: &'a [Expression]) -> ListIter<'a> {
        ListIter {
            inner_iter: list.iter(),
        }
    }
}

impl<'a> Iterator for ListIter<'a> {
    type Item = Expression;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(exp) = self.inner_iter.next() {
            Some(*exp)
        } else {
            None
        }
    }
}

type CallFunc =
    fn(&mut Environment, &mut dyn Iterator<Item = Expression>) -> io::Result<Expression>;

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

    pub fn cons_from_vec(v: &mut Vec<Expression>) -> ExpEnum {
        let mut last_pair = ExpEnum::Nil;
        if !v.is_empty() {
            let mut i = v.len() - 1;
            loop {
                if i == 0 {
                    last_pair = ExpEnum::Pair(
                        v.remove(i),
                        Expression::alloc(ExpObj {
                            data: last_pair.clone(),
                            meta: None,
                        }),
                    );
                    break;
                } else {
                    last_pair = ExpEnum::Pair(
                        v.remove(i),
                        Expression::alloc(ExpObj {
                            data: last_pair.clone(),
                            meta: None,
                        }),
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
    obj: Handle<ExpObj>,
}

impl Expression {
    pub fn new(obj: Handle<ExpObj>) -> Expression {
        Expression { obj }
    }

    pub fn alloc(obj: ExpObj) -> Expression {
        let obj = gc_mut().insert_temp(obj);
        Expression { obj }
    }

    pub fn alloc_data(data: ExpEnum) -> Expression {
        let obj = gc_mut().insert_temp(ExpObj { data, meta: None });
        Expression { obj }
    }

    pub fn make_nil() -> Expression {
        let obj = gc_mut().insert_temp(ExpObj {
            data: ExpEnum::Nil,
            meta: None,
        });
        Expression { obj }
    }

    pub fn make_true() -> Expression {
        let obj = gc_mut().insert_temp(ExpObj {
            data: ExpEnum::Atom(Atom::True),
            meta: None,
        });
        Expression { obj }
    }

    pub fn duplicate(self) -> Expression {
        Expression::alloc(self.get().clone())
    }

    pub fn replace(&mut self, new_data: Expression) {
        self.obj = new_data.obj;
    }

    pub fn get(&self) -> Obj<'_, ExpObj> {
        gc().get(self.obj).expect("Invalid expression!")
    }

    pub fn get_mut(&self) -> ObjMut<'_, ExpObj> {
        gc().get_mut(self.obj).expect("Invalid expression!")
    }

    pub fn meta(self) -> Option<ExpMeta> {
        self.get().meta.clone()
    }

    pub fn iter(self) -> Box<dyn Iterator<Item = Expression>> {
        let data = self.get();
        match &data.data {
            ExpEnum::Pair(_, _) => Box::new(PairIter::new(self)),
            ExpEnum::Vector(_) => panic!("Can not make a vector iterator this way!"),
            _ => Box::new(iter::empty()),
        }
    }

    pub fn is_nil(self) -> bool {
        let data = &self.get().data;
        if let ExpEnum::Nil = data {
            true
        } else {
            false
        }
    }

    // If the expression is a lazy fn then resolve it to concrete expression.
    pub fn resolve(self, environment: &mut Environment) -> io::Result<Self> {
        let mut res = self;
        while let ExpEnum::LazyFn(lambda, parts) = &self.get().data {
            let mut lambda = lambda.clone();
            let ib = &mut Box::new(ListIter::new_list(parts));
            res = call_lambda(environment, &mut lambda, ib, false)?;
        }
        Ok(res)
    }

    pub fn make_function(func: CallFunc, doc_str: &str, namespace: &'static str) -> Reference {
        Reference::new(
            ExpEnum::Function(Callable::new(func, false)),
            RefMetaData {
                namespace: Some(namespace),
                doc_string: Some(doc_str.to_string()),
            },
        )
    }

    pub fn make_special(func: CallFunc, doc_str: &str, namespace: &'static str) -> Reference {
        Reference::new(
            ExpEnum::Function(Callable::new(func, true)),
            RefMetaData {
                namespace: Some(namespace),
                doc_string: Some(doc_str.to_string()),
            },
        )
    }

    pub fn with_list(list: Vec<Expression>) -> Expression {
        Expression::alloc(ExpObj {
            data: ExpEnum::Vector(list),
            meta: None,
        })
    }

    pub fn with_list_meta(list: Vec<Expression>, meta: Option<ExpMeta>) -> Expression {
        Expression::alloc(ExpObj {
            data: ExpEnum::Vector(list),
            meta,
        })
    }

    pub fn cons_from_vec(v: &mut Vec<Expression>, meta: Option<ExpMeta>) -> Expression {
        let mut last_pair = ExpEnum::Nil;
        if !v.is_empty() {
            let mut i = v.len() - 1;
            loop {
                if i == 0 {
                    last_pair = ExpEnum::Pair(
                        v.remove(i),
                        Expression::alloc(ExpObj {
                            data: last_pair.clone(),
                            meta: None,
                        }),
                    );
                    break;
                } else {
                    last_pair = ExpEnum::Pair(
                        v.remove(i),
                        Expression::alloc(ExpObj {
                            data: last_pair.clone(),
                            meta: None,
                        }),
                    );
                }
                i -= 1;
            }
        }
        Expression::alloc(ExpObj {
            data: last_pair,
            meta,
        })
    }

    pub fn display_type(self) -> String {
        match &self.get().data {
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
        self,
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
        self,
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
        match &self.get().data {
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
                    let mut last_p = Expression::make_nil();
                    for p in self.iter() {
                        if !first {
                            if let ExpEnum::Atom(Atom::Symbol(sym)) = &last_p.get().data {
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
                        last_p = p; //&p.get().data;
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
        self,
        environment: &mut Environment,
        writer: &mut dyn Write,
    ) -> io::Result<()> {
        self.pretty_print_int(environment, 0, writer)
    }

    pub fn pretty_print(self, environment: &mut Environment) -> io::Result<()> {
        let stdout = io::stdout();
        let mut handle = stdout.lock();
        self.pretty_print_int(environment, 0, &mut handle)
    }

    pub fn make_string(self, environment: &Environment) -> io::Result<String> {
        match &self.get().data {
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
    pub fn as_string(self, environment: &Environment) -> io::Result<String> {
        if let ExpEnum::Atom(a) = &self.get().data {
            Ok(a.as_string())
        } else {
            self.make_string(environment)
        }
    }

    pub fn make_float(self, environment: &Environment) -> io::Result<f64> {
        match &self.get().data {
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

    pub fn make_int(self, environment: &Environment) -> io::Result<i64> {
        match &self.get().data {
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

    pub fn writef(self, environment: &mut Environment, writer: &mut dyn Write) -> io::Result<()> {
        match &self.get().data {
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

    pub fn write(self, environment: &mut Environment) -> io::Result<()> {
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

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn list_out(res: &mut String, itr: &mut dyn Iterator<Item = Expression>) {
            let mut first = true;
            let mut last_exp = Expression::make_nil();
            for p in itr {
                if !first {
                    if let ExpEnum::Atom(Atom::Symbol(sym)) = &last_exp.get().data {
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

        match &self.get().data {
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
                let mut ib = Box::new(ListIter::new_list(&list));
                list_out(&mut res, &mut ib);
                res.push(')');
                write!(f, "{}", res)
            }
            ExpEnum::Pair(e1, e2) => {
                if is_proper_list(*self) {
                    match &e1.get().data {
                        ExpEnum::Atom(Atom::Symbol(sym)) if sym == &"quote" => {
                            f.write_str("'")?;
                            // This will be a two element list or something is wrong...
                            if let ExpEnum::Pair(a2, _) = &e2.get().data {
                                f.write_str(&a2.to_string())
                            } else {
                                f.write_str(&e2.to_string())
                            }
                        }
                        ExpEnum::Atom(Atom::Symbol(sym)) if sym == &"bquote" => {
                            f.write_str("`")?;
                            // This will be a two element list or something is wrong...
                            if let ExpEnum::Pair(a2, _) = &e2.get().data {
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
                list_out(&mut res, &mut Box::new(ListIter::new_list(args)));
                res.push_str(">>");
                write!(f, "{}", res)
            }
        }
    }
}

impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.get().data {
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
