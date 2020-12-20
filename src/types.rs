use std::borrow::Cow;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt;
use std::fs::File;
use std::io::{self, BufReader, BufWriter, Read, Write};
use std::iter;
use std::num::{ParseFloatError, ParseIntError};
use std::process::Child;
use std::rc::Rc;

use crate::environment::*;
use crate::eval::call_lambda;
use crate::gc::*;
use crate::process::*;
use crate::symbols::*;

#[derive(Clone, Debug)]
pub struct LispError {
    pub reason: String,
    pub backtrace: Option<Vec<Handle>>,
}

impl Error for LispError {}

impl fmt::Display for LispError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.reason)
    }
}

impl From<io::Error> for LispError {
    fn from(item: io::Error) -> Self {
        LispError {
            reason: item.to_string(),
            backtrace: None,
        }
    }
}

impl LispError {
    pub fn new<S: Into<String>>(reason: S) -> LispError {
        LispError {
            reason: reason.into(),
            backtrace: None,
        }
    }
}

pub trait PeekableIterator: std::iter::Iterator {
    fn peek(&mut self) -> Option<&Self::Item>;
}

impl<I: std::iter::Iterator> PeekableIterator for std::iter::Peekable<I> {
    fn peek(&mut self) -> Option<&Self::Item> {
        std::iter::Peekable::peek(self)
    }
}

pub type CharIter = Box<dyn PeekableIterator<Item = Cow<'static, str>>>;

fn copy_handle(h: &Handle) -> Handle {
    let obj = h.get().copy();
    Expression::alloc_h(obj)
}

#[derive(Clone, Debug)]
pub struct Lambda {
    pub params: Vec<&'static str>,
    pub num_params: usize,
    pub has_rest: bool,
    pub body: Vec<Expression>,
    pub syms: Symbols,
    pub namespace: Rc<RefCell<Namespace>>,
}

impl Lambda {
    pub fn copy(&self) -> Self {
        Lambda {
            params: self.params.iter().copied().collect(),
            num_params: self.num_params,
            has_rest: self.has_rest,
            body: self.body.iter().map(|b| b.copy()).collect(),
            syms: self.syms.clone(), // XXX TODO deep?
            namespace: self.namespace.clone(),
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
    Read(Option<CharIter>, i64),
    ReadBinary(BufReader<File>),
    Write(BufWriter<File>),
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
        if let Some(current) = self.current.clone() {
            if let ExpEnum::Pair(e1, e2) = &current.get().data {
                let e1: Expression = e1.into();
                let e2: Expression = e2.into();
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
    inner_iter: std::slice::Iter<'a, Handle>,
}

impl<'a> ListIter<'a> {
    pub fn new_list(list: &[Handle]) -> ListIter<'_> {
        ListIter {
            inner_iter: list.iter(),
        }
    }

    pub fn new_slice(list: &'a [Handle]) -> ListIter<'a> {
        ListIter {
            inner_iter: list.iter(),
        }
    }
}

impl<'a> Iterator for ListIter<'a> {
    type Item = Expression;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(exp) = self.inner_iter.next() {
            let exp: Expression = exp.into();
            Some(exp)
        } else {
            None
        }
    }
}

type CallFunc =
    fn(&mut Environment, &mut dyn Iterator<Item = Expression>) -> Result<Expression, LispError>;

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

#[derive(Copy, Clone, Debug)]
pub struct ExpMeta {
    pub file: &'static str,
    pub line: usize,
    pub col: usize,
}

#[derive(Clone, Debug)]
pub enum SymLoc {
    None,
    Ref(Binding),
    Namespace(Rc<RefCell<Namespace>>, usize),
    Stack(usize),
}

impl SymLoc {
    pub fn replace(&mut self, new_data: SymLoc) {
        *self = new_data;
    }
}

pub enum ExpEnum {
    // Primatives
    True,
    Nil,
    Float(f64),
    Int(i64),
    Symbol(&'static str, SymLoc),
    // NOTE: String has an invarent to maintain, if Cow ever changes then the iterator must be set
    // to None if it is Some.
    String(Cow<'static, str>, Option<CharIter>),
    Char(Cow<'static, str>),
    CodePoint(char),

    // Lambda and macros and builtings
    Lambda(Lambda),
    Macro(Lambda),
    Function(Callable),
    LazyFn(Handle, Vec<Handle>), // Lambda ready to call- used for tail call optimization

    // Buildin data structures
    Vector(Vec<Handle>),
    Values(Vec<Handle>), // Used for multi value returns
    Pair(Handle, Handle),
    HashMap(HashMap<&'static str, Handle>),

    // Represents a running or completed system process
    Process(ProcessState),

    // A file
    File(Rc<RefCell<FileState>>),

    // Used as part of analyzer (a wrapped thing has already been 'prepped' so
    // when evaluated just unwrap it).
    Wrapper(Handle),

    // Used to help the analyzer reconize things it cares about without
    // doing a lot of extra work.  These are morally equivelent to a Function.
    // If adding to this list be sure to add to builtins_types.rs/builtin_is_builtin.
    DeclareDef,
    DeclareVar,
    DeclareFn,
    DeclareMacro,
    Quote,
    BackQuote,

    // This is a placeholder for an unset variable- error to evaluate.
    Undefined,
}

impl ExpEnum {
    pub fn replace(&mut self, new_data: ExpEnum) {
        *self = new_data;
    }

    pub fn cons_from_vec(v: &mut Vec<Handle>) -> ExpEnum {
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
                            meta_tags: None,
                            analyzed: RefCell::new(false),
                        })
                        .into(),
                    );
                    break;
                } else {
                    last_pair = ExpEnum::Pair(
                        v.remove(i),
                        Expression::alloc(ExpObj {
                            data: last_pair.clone(),
                            meta: None,
                            meta_tags: None,
                            analyzed: RefCell::new(false),
                        })
                        .into(),
                    );
                }
                i -= 1;
            }
        }
        last_pair
    }

    fn copy(&self) -> ExpEnum {
        match self {
            ExpEnum::True => ExpEnum::True,
            ExpEnum::Nil => ExpEnum::Nil,
            ExpEnum::Float(n) => ExpEnum::Float(*n),
            ExpEnum::Int(i) => ExpEnum::Int(*i),
            ExpEnum::Symbol(s, _) => ExpEnum::Symbol(s, SymLoc::None),
            // XXX TODO- make a new Cow (next two)?
            ExpEnum::String(s, _) => ExpEnum::String(s.clone(), None),
            ExpEnum::Char(c) => ExpEnum::Char(c.clone()),
            ExpEnum::CodePoint(c) => ExpEnum::CodePoint(*c),
            ExpEnum::Lambda(l) => ExpEnum::Lambda(l.copy()),
            ExpEnum::Macro(m) => ExpEnum::Macro(m.copy()),
            ExpEnum::Function(c) => ExpEnum::Function(c.clone()),
            ExpEnum::LazyFn(h, v) => {
                ExpEnum::LazyFn(copy_handle(h), v.iter().map(|h| copy_handle(h)).collect())
            }
            ExpEnum::Vector(v) => ExpEnum::Vector(v.iter().map(|h| copy_handle(h)).collect()),
            ExpEnum::Values(v) => ExpEnum::Values(v.iter().map(|h| copy_handle(h)).collect()),
            ExpEnum::Pair(car, cdr) => ExpEnum::Pair(copy_handle(car), copy_handle(cdr)),
            ExpEnum::HashMap(map) => ExpEnum::HashMap(map.clone()), //XXX TODO- deep copy
            ExpEnum::Process(p) => ExpEnum::Process(*p),
            ExpEnum::File(f) => ExpEnum::File(f.clone()),
            ExpEnum::Wrapper(h) => ExpEnum::Wrapper(copy_handle(h)),
            ExpEnum::DeclareDef => ExpEnum::DeclareDef,
            ExpEnum::DeclareVar => ExpEnum::DeclareVar,
            ExpEnum::DeclareFn => ExpEnum::DeclareFn,
            ExpEnum::DeclareMacro => ExpEnum::DeclareMacro,
            ExpEnum::Quote => ExpEnum::Quote,
            ExpEnum::BackQuote => ExpEnum::BackQuote,
            ExpEnum::Undefined => ExpEnum::Undefined,
        }
    }
}

impl Clone for ExpEnum {
    fn clone(&self) -> ExpEnum {
        match self {
            ExpEnum::True => ExpEnum::True,
            ExpEnum::Nil => ExpEnum::Nil,
            ExpEnum::Float(n) => ExpEnum::Float(*n),
            ExpEnum::Int(i) => ExpEnum::Int(*i),
            ExpEnum::Symbol(s, l) => ExpEnum::Symbol(s, l.clone()),
            ExpEnum::String(s, _) => ExpEnum::String(s.clone(), None),
            ExpEnum::Char(c) => ExpEnum::Char(c.clone()),
            ExpEnum::CodePoint(c) => ExpEnum::CodePoint(*c),
            ExpEnum::Lambda(l) => ExpEnum::Lambda(l.clone()),
            ExpEnum::Macro(m) => ExpEnum::Macro(m.clone()),
            ExpEnum::Function(c) => ExpEnum::Function(c.clone()),
            ExpEnum::LazyFn(h, v) => ExpEnum::LazyFn(h.clone(), v.clone()),
            ExpEnum::Vector(v) => ExpEnum::Vector(v.clone()),
            ExpEnum::Values(v) => ExpEnum::Values(v.clone()),
            ExpEnum::Pair(car, cdr) => ExpEnum::Pair(car.clone(), cdr.clone()),
            ExpEnum::HashMap(map) => ExpEnum::HashMap(map.clone()),
            ExpEnum::Process(p) => ExpEnum::Process(*p),
            ExpEnum::File(f) => ExpEnum::File(f.clone()),
            ExpEnum::Wrapper(h) => ExpEnum::Wrapper(h.clone()),
            ExpEnum::DeclareDef => ExpEnum::DeclareDef,
            ExpEnum::DeclareVar => ExpEnum::DeclareVar,
            ExpEnum::DeclareFn => ExpEnum::DeclareFn,
            ExpEnum::DeclareMacro => ExpEnum::DeclareMacro,
            ExpEnum::Quote => ExpEnum::Quote,
            ExpEnum::BackQuote => ExpEnum::BackQuote,
            ExpEnum::Undefined => ExpEnum::Undefined,
        }
    }
}

impl fmt::Debug for ExpEnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            ExpEnum::True => write!(f, "ExpEnum::True"),
            ExpEnum::Float(n) => write!(f, "ExpEnum::Float({})", n),
            ExpEnum::Int(i) => write!(f, "ExpEnum::Int({})", i),
            ExpEnum::Symbol(s, loc) => write!(f, "ExpEnum::Symbol({}, {:?})", s, loc),
            ExpEnum::String(s, _) => write!(f, "ExpEnum::String(\"{}\")", s),
            ExpEnum::Char(c) => write!(f, "ExpEnum::Char(#\\{})", c),
            ExpEnum::CodePoint(c) => write!(f, "ExpEnum::CodePoint(#\\{})", c),
            ExpEnum::Lambda(_) => {
                let exp: Expression = self.into();
                write!(f, "ExpEnum::Lambda({})", exp)
            }
            ExpEnum::Macro(_) => {
                let exp: Expression = self.into();
                write!(f, "ExpEnum::Macro({})", exp)
            }
            ExpEnum::Vector(l) => write!(f, "ExpEnum::Vector({:?})", l),
            ExpEnum::Values(v) => write!(f, "ExpEnum::Vector({:?})", v),
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
            ExpEnum::Wrapper(exp) => write!(f, "ExpEnum::Wrapper({:?})", exp),
            ExpEnum::Nil => write!(f, "ExpEnum::Nil"),
            ExpEnum::DeclareDef => write!(f, "ExpEnum::Function(_)"),
            ExpEnum::DeclareVar => write!(f, "ExpEnum::Function(_)"),
            ExpEnum::DeclareFn => write!(f, "ExpEnum::Function(_)"),
            ExpEnum::DeclareMacro => write!(f, "ExpEnum::Macro(_)"),
            ExpEnum::Quote => write!(f, "ExpEnum::Function(_)"),
            ExpEnum::BackQuote => write!(f, "ExpEnum::Function(_)"),
            ExpEnum::Undefined => write!(f, "ExpEnum::Undefined"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExpObj {
    pub data: ExpEnum,
    pub meta: Option<ExpMeta>,
    pub meta_tags: Option<HashSet<&'static str>>,
    pub analyzed: RefCell<bool>,
}

impl ExpObj {
    pub fn copy(&self) -> Self {
        let meta = if let Some(meta) = self.meta {
            Some(ExpMeta {
                file: meta.file,
                line: meta.line,
                col: meta.col,
            })
        } else {
            None
        };
        let meta_tags = if let Some(tags) = &self.meta_tags {
            let mut ntags = HashSet::with_capacity(tags.len());
            for t in tags {
                ntags.insert(*t);
            }
            Some(ntags)
        } else {
            None
        };
        ExpObj {
            data: self.data.copy(),
            meta,
            meta_tags,
            analyzed: RefCell::new(*self.analyzed.borrow()),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Expression {
    obj: Handle,
}

impl Expression {
    pub fn copy(&self) -> Expression {
        Expression {
            obj: Expression::alloc_h(self.obj.get().copy()),
        }
    }

    pub fn new(obj: Handle) -> Expression {
        Expression { obj }
    }

    pub fn alloc_h(obj: ExpObj) -> Handle {
        Handle::new(obj)
    }

    pub fn alloc_data_h(data: ExpEnum) -> Handle {
        Handle::new(ExpObj {
            data,
            meta: None,
            meta_tags: None,
            analyzed: RefCell::new(false),
        })
    }

    pub fn alloc(obj: ExpObj) -> Expression {
        Expression::alloc_h(obj).into()
    }

    pub fn alloc_data(data: ExpEnum) -> Expression {
        Expression::alloc_data_h(data).into()
    }

    pub fn make_nil_h() -> Handle {
        Handle::new(ExpObj {
            data: ExpEnum::Nil,
            meta: None,
            meta_tags: None,
            analyzed: RefCell::new(false),
        })
    }

    pub fn make_true_h() -> Handle {
        Handle::new(ExpObj {
            data: ExpEnum::True,
            meta: None,
            meta_tags: None,
            analyzed: RefCell::new(false),
        })
    }

    pub fn make_nil() -> Expression {
        Expression::make_nil_h().into()
    }

    pub fn make_true() -> Expression {
        Expression::make_true_h().into()
    }

    pub fn handle_no_root(&self) -> Handle {
        self.obj.clone()
    }

    pub fn duplicate(&self) -> Expression {
        Expression::alloc(self.get().clone())
    }

    pub fn get(&self) -> Ref<ExpObj> {
        self.obj.get()
    }

    pub fn get_mut(&self) -> RefMut<ExpObj> {
        self.obj.get_mut()
    }

    pub fn meta(&self) -> Option<ExpMeta> {
        self.obj.get().meta
    }

    pub fn iter(&self) -> Box<dyn Iterator<Item = Expression>> {
        let data = self.get();
        match &data.data {
            ExpEnum::Pair(_, _) => Box::new(PairIter::new(self.clone())),
            ExpEnum::Vector(_) => panic!("Can not make a vector iterator this way!"),
            ExpEnum::Values(_) => panic!("Can not make a values iterator this way!"),
            _ => Box::new(iter::empty()),
        }
    }

    pub fn is_nil(&self) -> bool {
        let data = &self.get().data;
        matches!(data, ExpEnum::Nil)
    }

    // If the expression is a lazy fn then resolve it to concrete expression.
    pub fn resolve(self, environment: &mut Environment) -> Result<Self, LispError> {
        let self_d = self.get();
        if let ExpEnum::LazyFn(lambda, parts) = &self_d.data {
            let ib = &mut ListIter::new_list(parts);
            let res = call_lambda(environment, lambda.clone().into(), ib, false)?;
            drop(self_d);
            res.resolve(environment)
        } else {
            drop(self_d);
            Ok(self)
        }
    }

    pub fn make_function(func: CallFunc, doc_str: &str) -> (Expression, String) {
        (
            ExpEnum::Function(Callable::new(func, false)).into(),
            doc_str.to_string(),
        )
    }

    pub fn make_special(func: CallFunc, doc_str: &str) -> (Expression, String) {
        (
            ExpEnum::Function(Callable::new(func, true)).into(),
            doc_str.to_string(),
        )
    }

    pub fn make_special_fn(doc_str: &str) -> (Expression, String) {
        (ExpEnum::DeclareFn.into(), doc_str.to_string())
    }

    pub fn make_special_macro(doc_str: &str) -> (Expression, String) {
        (ExpEnum::DeclareMacro.into(), doc_str.to_string())
    }

    pub fn make_special_def(doc_str: &str) -> (Expression, String) {
        (ExpEnum::DeclareDef.into(), doc_str.to_string())
    }

    pub fn make_special_var(doc_str: &str) -> (Expression, String) {
        (ExpEnum::DeclareVar.into(), doc_str.to_string())
    }

    pub fn make_special_quote(doc_str: &str) -> (Expression, String) {
        (ExpEnum::Quote.into(), doc_str.to_string())
    }

    pub fn make_special_backquote(doc_str: &str) -> (Expression, String) {
        (ExpEnum::BackQuote.into(), doc_str.to_string())
    }

    pub fn with_list(list: Vec<Handle>) -> Expression {
        Expression::alloc(ExpObj {
            data: ExpEnum::Vector(list),
            meta: None,
            meta_tags: None,
            analyzed: RefCell::new(false),
        })
    }

    pub fn with_list_meta(list: Vec<Handle>, meta: Option<ExpMeta>) -> Expression {
        Expression::alloc(ExpObj {
            data: ExpEnum::Vector(list),
            meta,
            meta_tags: None,
            analyzed: RefCell::new(false),
        })
    }

    pub fn cons_from_vec(v: &[Handle], meta: Option<ExpMeta>) -> Expression {
        let mut last_pair = ExpEnum::Nil;
        if !v.is_empty() {
            let mut i = v.len();
            while i > 0 {
                last_pair = ExpEnum::Pair(
                    v[i - 1].clone(),
                    Expression::alloc(ExpObj {
                        data: last_pair.clone(),
                        meta: None,
                        meta_tags: None,
                        analyzed: RefCell::new(false),
                    })
                    .into(),
                );
                i -= 1;
            }
        }
        Expression::alloc(ExpObj {
            data: last_pair,
            meta,
            meta_tags: None,
            analyzed: RefCell::new(false),
        })
    }

    pub fn display_type(&self) -> String {
        match &self.get().data {
            ExpEnum::True => "True".to_string(),
            ExpEnum::Float(_) => "Float".to_string(),
            ExpEnum::Int(_) => "Int".to_string(),
            ExpEnum::Symbol(_, _) => "Symbol".to_string(),
            ExpEnum::String(_, _) => "String".to_string(),
            ExpEnum::Char(_) => "Char".to_string(),
            ExpEnum::CodePoint(_) => "CodePoint".to_string(),
            ExpEnum::Lambda(_) => "Lambda".to_string(),
            ExpEnum::Macro(_) => "Macro".to_string(),
            ExpEnum::Process(_) => "Process".to_string(),
            ExpEnum::Function(f) => {
                if f.is_special_form {
                    "SpecialForm".to_string()
                } else {
                    "Function".to_string()
                }
            }
            ExpEnum::Vector(_) => "Vector".to_string(),
            ExpEnum::Values(v) => {
                if v.is_empty() {
                    "Nil".to_string()
                } else {
                    let v: Expression = (&v[0]).into();
                    v.display_type()
                }
            }
            ExpEnum::Pair(_, _) => "Pair".to_string(),
            ExpEnum::HashMap(_) => "HashMap".to_string(),
            ExpEnum::File(_) => "File".to_string(),
            ExpEnum::LazyFn(_, _) => "Lambda".to_string(),
            ExpEnum::Wrapper(exp) => {
                let exp: Expression = exp.into();
                exp.display_type()
            }
            ExpEnum::Nil => "Nil".to_string(),
            ExpEnum::DeclareDef => "SpecialForm".to_string(),
            ExpEnum::DeclareVar => "SpecialForm".to_string(),
            ExpEnum::DeclareFn => "SpecialForm".to_string(),
            ExpEnum::DeclareMacro => "SpecialForm".to_string(),
            ExpEnum::Quote => "SpecialForm".to_string(),
            ExpEnum::BackQuote => "SpecialForm".to_string(),
            ExpEnum::Undefined => "Undefined".to_string(), //panic!("Tried to get type for undefined!"),
        }
    }

    fn pid_to_string(
        &self,
        procs: Rc<RefCell<HashMap<u32, Child>>>,
        pid: u32,
    ) -> Result<String, LispError> {
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

    pub fn make_string(&self, environment: &Environment) -> Result<String, LispError> {
        match &self.get().data {
            ExpEnum::Process(ProcessState::Over(pid, _exit_status)) => {
                self.pid_to_string(environment.procs.clone(), *pid)
            }
            ExpEnum::Values(v) => {
                if v.is_empty() {
                    Ok(self.to_string())
                } else {
                    let v: Expression = (&v[0]).into();
                    v.make_string(environment)
                }
            }
            ExpEnum::File(file) => {
                let mut file_mut = file.borrow_mut();
                match &mut *file_mut {
                    FileState::Stdin => {
                        let f = io::stdin();
                        let mut f = f.lock();
                        let mut out_str = String::new();
                        f.read_to_string(&mut out_str)?;
                        Ok(out_str)
                    }
                    FileState::Read(f_iter, _) => {
                        let out_str: String = f_iter.as_mut().unwrap().collect();
                        Ok(out_str)
                    }
                    FileState::ReadBinary(f) => {
                        let mut out_str = String::new();
                        f.read_to_string(&mut out_str)?;
                        Ok(out_str)
                    }
                    _ => {
                        drop(file_mut);
                        Ok(self.to_string())
                    }
                }
            }
            ExpEnum::Wrapper(exp) => {
                let exp: Expression = exp.into();
                exp.make_string(environment)
            }
            _ => Ok(self.to_string()),
        }
    }

    // Like make_string but don't put quotes around strings.
    pub fn as_string(&self, environment: &Environment) -> Result<String, LispError> {
        match &self.get().data {
            ExpEnum::String(s, _) => Ok((*s).to_string()),
            ExpEnum::Char(c) => Ok((*c).to_string()),
            ExpEnum::CodePoint(c) => Ok(c.to_string()),
            _ => self.make_string(environment),
        }
    }

    pub fn make_float(&self, environment: &Environment) -> Result<f64, LispError> {
        match &self.get().data {
            ExpEnum::Float(f) => Ok(*f),
            ExpEnum::Int(i) => Ok(*i as f64),
            ExpEnum::Process(ProcessState::Running(_pid)) => {
                Err(LispError::new("Not a number (process still running!)"))
            }
            ExpEnum::Process(ProcessState::Over(pid, _exit_status)) => {
                let buffer = self.pid_to_string(environment.procs.clone(), *pid)?;
                let potential_float: Result<f64, ParseFloatError> = buffer.parse();
                match potential_float {
                    Ok(v) => Ok(v),
                    Err(_) => Err(LispError::new("Process result not a number")),
                }
            }
            ExpEnum::Function(_) => Err(LispError::new("Function not a number")),
            ExpEnum::Vector(_) => Err(LispError::new("Vector not a number")),
            ExpEnum::Values(v) => {
                if v.is_empty() {
                    Err(LispError::new("Empty values not a number"))
                } else {
                    let v: Expression = (&v[0]).into();
                    v.make_float(environment)
                }
            }
            ExpEnum::Pair(_, _) => Err(LispError::new("Pair not a number")),
            ExpEnum::Nil => Err(LispError::new("Nil not a number")),
            ExpEnum::HashMap(_) => Err(LispError::new("Map not a number")),
            ExpEnum::File(_) => Err(LispError::new("File not a number")),
            ExpEnum::LazyFn(_, _) => Err(LispError::new("Fn call not a number")),
            ExpEnum::Wrapper(_) => Err(LispError::new("Not a number")),
            ExpEnum::DeclareDef => Err(LispError::new("Def not a number")),
            ExpEnum::DeclareVar => Err(LispError::new("Var not a number")),
            ExpEnum::DeclareFn => Err(LispError::new("Fn not a number")),
            ExpEnum::DeclareMacro => Err(LispError::new("Macro not a number")),
            ExpEnum::Quote => Err(LispError::new("Function not a number")),
            ExpEnum::BackQuote => Err(LispError::new("Function not a number")),
            _ => Err(LispError::new("Not a number")),
        }
    }

    pub fn make_int(&self, environment: &Environment) -> Result<i64, LispError> {
        match &self.get().data {
            ExpEnum::Int(i) => Ok(*i),
            ExpEnum::Process(ProcessState::Running(_pid)) => {
                Err(LispError::new("Not an integer (process still running!)"))
            }
            ExpEnum::Process(ProcessState::Over(pid, _exit_status)) => {
                let buffer = self.pid_to_string(environment.procs.clone(), *pid)?;
                let potential_int: Result<i64, ParseIntError> = buffer.parse();
                match potential_int {
                    Ok(v) => Ok(v),
                    Err(_) => Err(LispError::new("Process result not an integer")),
                }
            }
            ExpEnum::Function(_) => Err(LispError::new("Function not an integer")),
            ExpEnum::Vector(_) => Err(LispError::new("Vector not an integer")),
            ExpEnum::Values(v) => {
                if v.is_empty() {
                    Err(LispError::new("Empty values not an integer"))
                } else {
                    let v: Expression = (&v[0]).into();
                    v.make_int(environment)
                }
            }
            ExpEnum::Pair(_, _) => Err(LispError::new("Pair not an integer")),
            ExpEnum::Nil => Err(LispError::new("Nil not an integer")),
            ExpEnum::HashMap(_) => Err(LispError::new("Map not an integer")),
            ExpEnum::File(_) => Err(LispError::new("File not an integer")),
            ExpEnum::LazyFn(_, _) => Err(LispError::new("Fn call not an integer")),
            ExpEnum::Wrapper(_) => Err(LispError::new("Not an integer")),
            ExpEnum::DeclareDef => Err(LispError::new("Def not an integer")),
            ExpEnum::DeclareVar => Err(LispError::new("Var not an integer")),
            ExpEnum::DeclareFn => Err(LispError::new("Fn not an integer")),
            ExpEnum::DeclareMacro => Err(LispError::new("Macro not an integer")),
            ExpEnum::Quote => Err(LispError::new("Function not a integer")),
            ExpEnum::BackQuote => Err(LispError::new("Function not a integer")),
            _ => Err(LispError::new("Not an integer")),
        }
    }

    pub fn writef(
        &self,
        environment: &mut Environment,
        writer: &mut dyn Write,
    ) -> Result<(), LispError> {
        match &self.get().data {
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
                                    Err(err) => return Err(err.into()),
                                }
                            }
                        } else {
                            return Err(LispError::new("Failed to get process out to write to."));
                        }
                    }
                    None => {
                        return Err(LispError::new("Failed to get process to write to."));
                    }
                }
                drop(procs);
                wait_pid(environment, *pid, None);
            }
            ExpEnum::Function(_) => write!(writer, "{}", self.to_string())?,
            ExpEnum::Vector(_) => write!(writer, "{}", self.to_string())?,
            ExpEnum::Values(v) => {
                if v.is_empty() {
                    write!(writer, "{}", self.to_string())?;
                } else {
                    let v: Expression = (&v[0]).into();
                    v.writef(environment, writer)?;
                }
            }
            ExpEnum::Pair(_, _) => write!(writer, "{}", self.to_string())?,
            ExpEnum::Nil => write!(writer, "{}", self.to_string())?,
            ExpEnum::HashMap(_map) => write!(writer, "{}", self.to_string())?,
            ExpEnum::File(file) => match &mut *file.borrow_mut() {
                FileState::Stdin => {
                    let f = io::stdin();
                    let mut f = f.lock();
                    let mut buf = [0; 1024];
                    loop {
                        match f.read(&mut buf) {
                            Ok(0) => break,
                            Ok(n) => writer.write_all(&buf[..n])?,
                            Err(err) => return Err(err.into()),
                        }
                    }
                }
                FileState::Read(f_iter, _) => {
                    for ch in f_iter.as_mut().unwrap() {
                        writer.write_all(ch.as_bytes())?;
                    }
                }
                FileState::ReadBinary(f) => {
                    let mut buf = [0; 1024];
                    loop {
                        match f.read(&mut buf) {
                            Ok(0) => break,
                            Ok(n) => writer.write_all(&buf[..n])?,
                            Err(err) => return Err(err.into()),
                        }
                    }
                }
                _ => write!(writer, "{}", self.to_string())?,
            },
            ExpEnum::LazyFn(_, _) => write!(writer, "{}", self.to_string())?,
            ExpEnum::Wrapper(exp) => {
                let exp: Expression = exp.into();
                exp.writef(environment, writer)?;
            }
            ExpEnum::String(s, _) => write!(writer, "{}", s)?, // Do not quote strings.
            _ => write!(writer, "{}", self.to_string())?,
        }
        writer.flush()?;
        Ok(())
    }

    pub fn write(&self, environment: &mut Environment) -> Result<(), LispError> {
        let stdout = io::stdout();
        let mut handle = stdout.lock();
        self.writef(environment, &mut handle)
    }
}

impl AsRef<Handle> for Expression {
    fn as_ref(&self) -> &Handle {
        &self.obj
    }
}

impl AsRef<Expression> for Expression {
    fn as_ref(&self) -> &Expression {
        &self
    }
}

impl From<Expression> for ExpEnum {
    fn from(item: Expression) -> Self {
        match item.obj.try_unwrap() {
            Ok(data) => data.data,
            Err(handle) => handle.get().data.clone(),
        }
    }
}

impl From<Expression> for Handle {
    fn from(item: Expression) -> Self {
        item.obj
    }
}

impl From<Handle> for Expression {
    fn from(item: Handle) -> Self {
        Expression::new(item)
    }
}

impl From<&Handle> for Expression {
    fn from(item: &Handle) -> Self {
        Expression::new(item.clone())
    }
}

impl From<&mut Handle> for Expression {
    fn from(item: &mut Handle) -> Self {
        Expression::new(item.clone())
    }
}

impl From<ExpEnum> for Expression {
    fn from(data: ExpEnum) -> Self {
        let root = Handle::new(ExpObj {
            data,
            meta: None,
            meta_tags: None,
            analyzed: RefCell::new(false),
        });
        Expression::new(root)
    }
}

impl From<&ExpEnum> for Expression {
    fn from(data: &ExpEnum) -> Self {
        let root = Handle::new(ExpObj {
            data: data.clone(),
            meta: None,
            meta_tags: None,
            analyzed: RefCell::new(false),
        });
        Expression::new(root)
    }
}

impl From<&mut ExpEnum> for Expression {
    fn from(data: &mut ExpEnum) -> Self {
        let root = Handle::new(ExpObj {
            data: data.clone(),
            meta: None,
            meta_tags: None,
            analyzed: RefCell::new(false),
        });
        Expression::new(root)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_one() {
        let s1 = Expression::alloc_data_h(ExpEnum::String("sls".into(), None));
        let n1 = Expression::make_nil_h();
        let _p1 = Expression::alloc_data(ExpEnum::Pair(s1.clone(), n1.clone()));
        let nlist = vec![
            Expression::make_nil_h().clone(),
            Expression::make_nil_h().clone(),
        ];
        let _l1 = Expression::with_list(nlist);
        //println!("XXX {}, {}, {}", p1, s1, n1);
        //println!("XXX {}", l1);
        //assert!(1 == 2);
    }
}
