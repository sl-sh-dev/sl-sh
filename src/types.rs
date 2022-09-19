use regex::Regex;
use std::borrow::Cow;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::error::Error;
use std::fmt;
use std::fs::File;
use std::io::{self, BufReader, BufWriter, Read, Write};
use std::iter;
use std::marker::PhantomData;
use std::num::{ParseFloatError, ParseIntError};
use std::rc::Rc;

use crate::environment::*;
use crate::eval::call_lambda;
use crate::process::*;
use crate::symbols::*;
use crate::unix::fd_to_file;
use crate::{try_inner_float, try_inner_hash_map, try_inner_int, LispResult};

#[derive(Clone, Debug)]
pub struct LispError {
    pub reason: String,
    pub backtrace: Option<Vec<Expression>>,
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

pub trait PeekableIterator: Iterator {
    fn peek(&mut self) -> Option<&Self::Item>;
}

impl<I: Iterator> PeekableIterator for std::iter::Peekable<I> {
    fn peek(&mut self) -> Option<&Self::Item> {
        iter::Peekable::peek(self)
    }
}

pub type CharIter = Box<dyn PeekableIterator<Item = Cow<'static, str>>>;

fn copy_handle(h: &Expression) -> Expression {
    let obj = h.get().copy();
    Expression::alloc(obj)
}

#[derive(Clone, Debug)]
pub enum MultiExpression {
    None,
    Single(Expression),
    Multiple(Vec<Expression>),
}

impl MultiExpression {
    pub fn copy(&self) -> Self {
        match self {
            MultiExpression::None => MultiExpression::None,
            MultiExpression::Single(exp) => MultiExpression::Single(exp.copy()),
            MultiExpression::Multiple(exps) => {
                MultiExpression::Multiple(exps.iter().map(|b| b.copy()).collect())
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Lambda {
    pub params: Vec<&'static str>,
    pub num_params: usize,
    pub has_rest: bool,
    pub body: MultiExpression,
    pub syms: Symbols,
    pub namespace: Rc<RefCell<Namespace>>,
    pub no_recur: bool,
}

impl Lambda {
    pub fn copy(&self) -> Self {
        Lambda {
            params: self.params.to_vec(),
            num_params: self.num_params,
            has_rest: self.has_rest,
            body: self.body.copy(),
            syms: self.syms.clone(), // XXX TODO deep?
            namespace: self.namespace.clone(),
            no_recur: self.no_recur,
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
    dotted: bool,
}

impl PairIter {
    pub fn new(exp: Expression) -> PairIter {
        PairIter {
            current: Some(exp),
            dotted: false,
        }
    }

    pub fn is_dotted(&self) -> bool {
        self.dotted
    }
}

impl Iterator for PairIter {
    type Item = Expression;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(current) = self.current.clone() {
            let current_d = current.get();
            match &current_d.data {
                ExpEnum::Pair(e1, e2) => {
                    self.current = Some(e2.clone());
                    Some(e1.clone())
                }
                ExpEnum::Nil => None,
                _ => {
                    drop(current_d);
                    let cur = Some(current);
                    self.current = None;
                    self.dotted = true;
                    cur
                }
            }
        } else {
            None
        }
    }
}

pub struct ListIter {
    current: Expression,
    index: usize,
}

impl ListIter {
    fn new(exp: Expression) -> ListIter {
        ListIter {
            current: exp,
            index: 0,
        }
    }
}

impl Iterator for ListIter {
    type Item = Expression;

    fn next(&mut self) -> Option<Self::Item> {
        if let ExpEnum::Vector(v) = &self.current.get().data {
            if let Some(exp) = v.get(self.index) {
                self.index += 1;
                Some(exp.clone())
            } else {
                None
            }
        } else {
            panic!("Not a vector, invalid for ListIter!");
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
    // Primitives
    True,
    False,
    Nil,
    Float(f64),
    Int(i64),
    Symbol(&'static str, SymLoc),
    // NOTE: String has an invariant to maintain, if Cow ever changes then the
    // iterator must be set to None if it is Some.
    String(Cow<'static, str>, Option<CharIter>),
    Char(Cow<'static, str>),
    CodePoint(char),
    Regex(Regex),

    // Lambda and macros and builtins
    Lambda(Lambda),
    Macro(Lambda),
    Function(Callable),
    LazyFn(Expression, Vec<Expression>), // Lambda ready to call- used for tail call optimization

    // Buildin data structures
    Vector(Vec<Expression>),
    Values(Vec<Expression>), // Used for multi value returns
    Pair(Expression, Expression),
    HashMap(HashMap<&'static str, Expression>),

    // Represents a running or completed system process
    Process(ProcessState),

    // A file
    File(Rc<RefCell<FileState>>),

    // Used as part of analyzer (a wrapped thing has already been 'prepped' so
    // when evaluated just unwrap it).
    Wrapper(Expression),

    // Used to help the analyzer recognize things it cares about without
    // doing a lot of extra work.  These are morally equivalent to a Function.
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

impl ToString for ExpEnum {
    fn to_string(&self) -> String {
        Expression::display_type_from_enum(self)
    }
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
                last_pair = ExpEnum::Pair(
                    v.remove(i),
                    Expression::alloc(ExpObj {
                        data: last_pair.clone(),
                        meta: None,
                        meta_tags: None,
                        analyzed: RefCell::new(false),
                    }),
                );
                if i == 0 {
                    break;
                }
                i -= 1;
            }
        }
        last_pair
    }

    fn copy(&self) -> ExpEnum {
        match self {
            ExpEnum::True => ExpEnum::True,
            ExpEnum::False => ExpEnum::False,
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
                ExpEnum::LazyFn(copy_handle(h), v.iter().map(copy_handle).collect())
            }
            ExpEnum::Vector(v) => ExpEnum::Vector(v.iter().map(copy_handle).collect()),
            ExpEnum::Values(v) => ExpEnum::Values(v.iter().map(copy_handle).collect()),
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
            ExpEnum::Regex(regex) => ExpEnum::Regex(regex.clone()),
        }
    }
}

impl Clone for ExpEnum {
    fn clone(&self) -> ExpEnum {
        match self {
            ExpEnum::True => ExpEnum::True,
            ExpEnum::False => ExpEnum::False,
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
            ExpEnum::Regex(regex) => ExpEnum::Regex(regex.clone()),
        }
    }
}

impl fmt::Debug for ExpEnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            ExpEnum::True => write!(f, "ExpEnum::True"),
            ExpEnum::False => write!(f, "ExpEnum::False"),
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
            ExpEnum::Regex(regex) => {
                write!(f, "ExpEnum::Regex({:?})", regex)
            }
        }
    }
}

// This is used when getting a process output into a string, puts an upper limit
// on the amount of bytes that can be read before an error is generated.
pub struct TakeN<'a, T> {
    inner: &'a mut T,
    limit: u64,
    pid: u32,
}

impl<'a, T> TakeN<'a, T> {
    pub fn new(inner: &'a mut T, limit: u64, pid: u32) -> Self {
        TakeN { inner, limit, pid }
    }
}

impl<'a, T: Read> Read for TakeN<'a, T> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        // Don't call into inner reader at all at EOF because it may still block
        if self.limit == 0 {
            // Send a sigint to the feeding job so it does not hang on a full output buffer.
            let error = nix::sys::signal::kill(
                nix::unistd::Pid::from_raw(self.pid as i32),
                nix::sys::signal::Signal::SIGINT,
            )
            .is_err();
            return if error {
                Err(io::Error::new(
                    io::ErrorKind::Other,
                    "String from process is to large, failed to send sigint to job.",
                ))
            } else {
                Err(io::Error::new(
                    io::ErrorKind::Other,
                    "String from process is to large, sent sigint to job.",
                ))
            };
        }

        let max = std::cmp::min(buf.len() as u64, self.limit) as usize;
        let n = self.inner.read(&mut buf[..max])?;
        self.limit -= n as u64;
        Ok(n)
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
        let meta = self.meta.map(|meta| ExpMeta {
            file: meta.file,
            line: meta.line,
            col: meta.col,
        });
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
    data: Rc<RefCell<ExpObj>>,
}

impl Expression {
    pub fn copy(&self) -> Expression {
        Expression {
            data: Rc::new(RefCell::new(self.data.borrow().copy())),
        }
    }

    pub fn alloc(obj: ExpObj) -> Expression {
        Expression {
            data: Rc::new(RefCell::new(obj)),
        }
    }

    pub fn alloc_data(data: ExpEnum) -> Expression {
        Expression::alloc(ExpObj {
            data,
            meta: None,
            meta_tags: None,
            analyzed: RefCell::new(false),
        })
    }

    pub fn alloc_data_meta(environment: &Environment, data: ExpEnum) -> Expression {
        let meta = if environment.reader_state.in_read {
            environment.reader_state.file_name.map(|file| ExpMeta {
                file,
                line: environment.reader_state.line,
                col: environment.reader_state.column,
            })
        } else {
            None
        };
        Expression::alloc(ExpObj {
            data,
            meta,
            meta_tags: None,
            analyzed: RefCell::new(false),
        })
    }

    pub fn make_nil() -> Expression {
        Expression::alloc(ExpObj {
            data: ExpEnum::Nil,
            meta: None,
            meta_tags: None,
            analyzed: RefCell::new(false),
        })
    }

    pub fn make_true() -> Expression {
        Expression::alloc(ExpObj {
            data: ExpEnum::True,
            meta: None,
            meta_tags: None,
            analyzed: RefCell::new(false),
        })
    }

    pub fn make_false() -> Expression {
        Expression::alloc(ExpObj {
            data: ExpEnum::False,
            meta: None,
            meta_tags: None,
            analyzed: RefCell::new(false),
        })
    }

    pub fn duplicate(&self) -> Expression {
        Expression::alloc(self.get().clone())
    }

    pub fn get(&self) -> Ref<ExpObj> {
        self.data.borrow()
    }

    pub fn get_mut(&self) -> RefMut<ExpObj> {
        self.data.borrow_mut()
    }

    pub fn try_unwrap(self) -> Result<ExpObj, Expression> {
        match Rc::try_unwrap(self.data) {
            Ok(data) => Ok(data.into_inner()),
            Err(data) => Err(Expression { data }),
        }
    }

    pub fn meta(&self) -> Option<ExpMeta> {
        self.get().meta
    }

    pub fn iter(&self) -> Box<dyn Iterator<Item = Expression>> {
        let data = self.get();
        match &data.data {
            ExpEnum::Pair(_, _) => Box::new(PairIter::new(self.clone())),
            ExpEnum::Vector(_) => Box::new(ListIter::new(self.clone())),
            _ => Box::new(iter::empty()),
        }
    }

    pub fn is_nil(&self) -> bool {
        let data = &self.get().data;
        matches!(data, ExpEnum::Nil)
    }

    pub fn is_falsey(&self) -> bool {
        let data = &self.get().data;
        matches!(data, ExpEnum::Nil | ExpEnum::False)
    }

    // If the expression is a lazy fn then resolve it to concrete expression.
    pub fn resolve(self, environment: &mut Environment) -> Result<Self, LispError> {
        let self_d = self.get();
        if let ExpEnum::LazyFn(lambda, parts) = &self_d.data {
            let ib = &mut parts.iter().cloned();
            let res = call_lambda(environment, lambda.clone(), ib, false)?;
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

    pub fn with_list(list: Vec<Expression>) -> Expression {
        Expression::alloc(ExpObj {
            data: ExpEnum::Vector(list),
            meta: None,
            meta_tags: None,
            analyzed: RefCell::new(false),
        })
    }

    pub fn with_list_meta(list: Vec<Expression>, meta: Option<ExpMeta>) -> Expression {
        Expression::alloc(ExpObj {
            data: ExpEnum::Vector(list),
            meta,
            meta_tags: None,
            analyzed: RefCell::new(false),
        })
    }

    pub fn cons_from_vec(v: &[Expression], meta: Option<ExpMeta>) -> Expression {
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
                    }),
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
        Expression::display_type_from_enum(&self.get().data)
    }

    pub fn display_type_from_enum(exp_enum: &ExpEnum) -> String {
        match exp_enum {
            ExpEnum::True => "True".to_string(),
            ExpEnum::False => "False".to_string(),
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
                    let v: Expression = (v[0]).clone();
                    v.display_type()
                }
            }
            ExpEnum::Pair(_, _) => "Pair".to_string(),
            ExpEnum::HashMap(_) => "HashMap".to_string(),
            ExpEnum::File(_) => "File".to_string(),
            ExpEnum::LazyFn(_, _) => "Lambda".to_string(),
            ExpEnum::Wrapper(exp) => {
                let exp: Expression = exp.clone();
                exp.display_type()
            }
            ExpEnum::Nil => "Nil".to_string(),
            ExpEnum::DeclareDef => "SpecialForm".to_string(),
            ExpEnum::DeclareVar => "SpecialForm".to_string(),
            ExpEnum::DeclareFn => "SpecialForm".to_string(),
            ExpEnum::DeclareMacro => "SpecialForm".to_string(),
            ExpEnum::Quote => "SpecialForm".to_string(),
            ExpEnum::BackQuote => "SpecialForm".to_string(),
            ExpEnum::Regex(_) => "Regex".to_string(),
            ExpEnum::Undefined => "Undefined".to_string(), //panic!("Tried to get type for undefined!"),
        }
    }

    fn pid_to_string(&self, procs: ProcessMap, pid: u32) -> Result<String, LispError> {
        match procs.borrow_mut().get_mut(&pid) {
            Some((_, Some(child))) => {
                let mut childout = BufReader::new(fd_to_file(*child));
                let mut buffer = String::new();
                let mut taken = TakeN {
                    inner: &mut childout,
                    limit: 16_777_216, // 16M limit
                    pid,
                };
                taken.read_to_string(&mut buffer)?;
                Ok(buffer)
            }
            Some((_, None)) => Ok("".to_string()),
            None => Ok("".to_string()),
        }
    }

    pub fn make_string(&self, environment: &Environment) -> Result<String, LispError> {
        match &self.get().data {
            ExpEnum::Process(ProcessState::Over(pid, _exit_status)) => {
                self.pid_to_string(environment.procs.clone(), *pid)
            }
            ExpEnum::Process(ProcessState::Running(pid)) => {
                self.pid_to_string(environment.procs.clone(), *pid)
            }
            ExpEnum::Values(v) => {
                if v.is_empty() {
                    Ok(self.to_string())
                } else {
                    v[0].make_string(environment)
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
                let exp: Expression = exp.clone();
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
            ExpEnum::Values(v) => {
                if v.is_empty() {
                    self.make_string(environment)
                } else {
                    v[0].as_string(environment)
                }
            }
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
                    v[0].make_float(environment)
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
                    v[0].make_int(environment)
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
                match procs.get_mut(pid) {
                    Some((_, Some(read_fd))) => {
                        let mut out = BufReader::new(fd_to_file(*read_fd));
                        let mut buf = [0; 1024];
                        loop {
                            match out.read(&mut buf) {
                                Ok(0) => break,
                                Ok(n) => writer.write_all(&buf[..n])?,
                                Err(err) => return Err(err.into()),
                            }
                        }
                    }
                    Some((_, None)) => {
                        // Got a process with no grabbed output so nothing to write...
                    }
                    None => {
                        // Appear to have a stale process, nothing to write.
                    }
                }
                drop(procs);
                wait_pid(environment, *pid, None);
            }
            ExpEnum::Function(_) => write!(writer, "{}", self)?,
            ExpEnum::Vector(_) => write!(writer, "{}", self)?,
            ExpEnum::Values(v) => {
                if v.is_empty() {
                    write!(writer, "{}", self)?;
                } else {
                    let v: Expression = (v[0]).clone();
                    v.writef(environment, writer)?;
                }
            }
            ExpEnum::Pair(_, _) => write!(writer, "{}", self)?,
            ExpEnum::Nil => write!(writer, "{}", self)?,
            ExpEnum::HashMap(_map) => write!(writer, "{}", self)?,
            ExpEnum::File(file) => {
                let mut file_d = file.try_borrow_mut().map_err(|_| {
                    LispError::new("Invalid file, are you trying to read and write the same file?")
                })?;
                match &mut *file_d {
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
                    _ => {
                        drop(file_d);
                        write!(writer, "{}", self)?;
                    }
                }
            }
            ExpEnum::LazyFn(_, _) => write!(writer, "{}", self)?,
            ExpEnum::Wrapper(exp) => {
                let exp: Expression = exp.clone();
                exp.writef(environment, writer)?;
            }
            ExpEnum::String(s, _) => write!(writer, "{}", s)?, // Do not quote strings.
            ExpEnum::Char(c) => write!(writer, "{}", c)?,
            ExpEnum::CodePoint(c) => write!(writer, "{}", c)?,
            _ => write!(writer, "{}", self)?,
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

impl AsRef<Expression> for Expression {
    fn as_ref(&self) -> &Expression {
        self
    }
}

impl From<Expression> for ExpEnum {
    fn from(item: Expression) -> Self {
        match item.try_unwrap() {
            Ok(data) => data.data,
            Err(item) => item.get().data.clone(),
        }
    }
}

impl From<ExpEnum> for Expression {
    fn from(data: ExpEnum) -> Self {
        Expression::alloc(ExpObj {
            data,
            meta: None,
            meta_tags: None,
            analyzed: RefCell::new(false),
        })
    }
}

impl From<&ExpEnum> for Expression {
    fn from(data: &ExpEnum) -> Self {
        Expression::alloc(ExpObj {
            data: data.clone(),
            meta: None,
            meta_tags: None,
            analyzed: RefCell::new(false),
        })
    }
}

impl From<&mut ExpEnum> for Expression {
    fn from(data: &mut ExpEnum) -> Self {
        Expression::alloc(ExpObj {
            data: data.clone(),
            meta: None,
            meta_tags: None,
            analyzed: RefCell::new(false),
        })
    }
}

pub struct TypedExpression<T>(Expression, PhantomData<T>);

impl<T> TypedExpression<T> {
    pub fn new(exp_obj: Expression) -> TypedExpression<T> {
        TypedExpression(exp_obj, PhantomData::default())
    }
}

pub trait RustProcedureRef<T, F>
where
    Self: Sized,
    F: FnOnce(&mut T) -> LispResult<Expression> + ?Sized,
{
    fn apply_ref_mut(&mut self, fn_name: &str, fun: F) -> LispResult<Expression>;
}

pub trait RustProcedure<T, F>
where
    Self: Sized,
    F: FnOnce(T) -> LispResult<Expression> + ?Sized,
{
    fn apply(&self, fn_name: &str, fun: F) -> LispResult<Expression>;
}

impl<F> RustProcedureRef<BTreeMap<&str, Expression>, F>
    for TypedExpression<BTreeMap<&str, Expression>>
where
    F: FnOnce(&mut BTreeMap<&str, Expression>) -> LispResult<Expression>,
{
    fn apply_ref_mut(&mut self, fn_name: &str, fun: F) -> LispResult<Expression> {
        use crate::ErrorStrings;
        let got = self.0.display_type();
        let mut btreemap = BTreeMap::new();
        let x = match &self.0.get().data {
            ExpEnum::HashMap(map) => {
                map.iter().fold(&mut btreemap, |accum, (k, v)| {
                    accum.insert(*k, v.clone());
                    accum
                });
                fun(&mut btreemap)
            }
            _ => {
                return Err(LispError::new(ErrorStrings::mismatched_type(
                    fn_name,
                    &ExpEnum::HashMap(Default::default()).to_string(),
                    &got,
                )))
            }
        };
        let map = btreemap
            .into_iter()
            .fold(HashMap::new(), |mut accum, (k, v)| {
                accum.insert(k, v);
                accum
            });
        self.0.data.borrow_mut().data.replace(ExpEnum::HashMap(map));
        x
    }
}

impl<F> RustProcedureRef<HashMap<&str, Expression>, F>
    for TypedExpression<HashMap<&str, Expression>>
where
    F: FnOnce(&mut HashMap<&str, Expression>) -> LispResult<Expression>,
{
    fn apply_ref_mut(&mut self, fn_name: &str, fun: F) -> LispResult<Expression> {
        try_inner_hash_map!(fn_name, self.0, arg, fun(arg))
    }
}

impl<F> RustProcedure<HashMap<&str, Expression>, F> for TypedExpression<HashMap<&str, Expression>>
where
    F: FnOnce(HashMap<&str, Expression>) -> LispResult<Expression>,
{
    fn apply(&self, fn_name: &str, fun: F) -> LispResult<Expression> {
        try_inner_hash_map!(fn_name, self.0, arg, fun(arg.clone()))
    }
}

impl<F> RustProcedureRef<Expression, F> for TypedExpression<Expression>
where
    F: FnOnce(&mut Expression) -> LispResult<Expression>,
{
    fn apply_ref_mut(&mut self, _fn_name: &str, fun: F) -> LispResult<Expression> {
        fun(&mut self.0.clone())
    }
}

impl<F> RustProcedure<&Expression, F> for TypedExpression<&Expression>
where
    F: FnOnce(&Expression) -> LispResult<Expression>,
{
    fn apply(&self, _fn_name: &str, fun: F) -> LispResult<Expression> {
        fun(&self.0.clone())
    }
}

impl<F> RustProcedure<Expression, F> for TypedExpression<Expression>
where
    F: FnOnce(Expression) -> LispResult<Expression>,
{
    fn apply(&self, _fn_name: &str, fun: F) -> LispResult<Expression> {
        fun(self.0.clone())
    }
}

impl<F> RustProcedureRef<i64, F> for TypedExpression<i64>
where
    F: FnOnce(&mut i64) -> LispResult<Expression>,
{
    fn apply_ref_mut(&mut self, fn_name: &str, fun: F) -> LispResult<Expression> {
        try_inner_int!(fn_name, self.0, num, fun(num))
    }
}

impl<F> RustProcedure<i64, F> for TypedExpression<i64>
where
    F: FnOnce(i64) -> LispResult<Expression>,
{
    fn apply(&self, fn_name: &str, fun: F) -> LispResult<Expression> {
        try_inner_int!(fn_name, self.0, num, fun(*num))
    }
}

impl<F> RustProcedureRef<f64, F> for TypedExpression<f64>
where
    F: FnOnce(&mut f64) -> LispResult<Expression>,
{
    fn apply_ref_mut(&mut self, fn_name: &str, fun: F) -> LispResult<Expression> {
        try_inner_float!(fn_name, self.0, num, fun(num))
    }
}

impl<F> RustProcedure<f64, F> for TypedExpression<f64>
where
    F: FnOnce(f64) -> LispResult<Expression>,
{
    fn apply(&self, fn_name: &str, fun: F) -> LispResult<Expression> {
        try_inner_float!(fn_name, self.0, num, fun(*num))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_one() {
        let s1 = Expression::alloc_data(ExpEnum::String("sls".into(), None));
        let n1 = Expression::make_nil();
        let _p1 = Expression::alloc_data(ExpEnum::Pair(s1.clone(), n1.clone()));
        let nlist = vec![
            Expression::make_nil().clone(),
            Expression::make_nil().clone(),
        ];
        let _l1 = Expression::with_list(nlist);
        //println!("XXX {}, {}, {}", p1, s1, n1);
        //println!("XXX {}", l1);
        //assert!(1 == 2);
    }
}
