use crate::{
    try_inner_file, try_inner_hash_map, try_inner_hash_map_mut, try_inner_int, try_inner_string,
    ExpEnum, Expression, FileState, LispError, LispResult, SymLoc, TypedWrapper,
};
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::rc::Rc;

pub struct ErrorStrings {}

impl ErrorStrings {
    pub fn mismatched_type(fn_name: &str, expected: &str, got: &str) -> String {
        format!("{fn_name}: mismatched type input, expected {expected}, got {got}.")
    }

    pub fn positive_integer(fn_name: &str, expected: &str, got: &str) -> String {
        format!("{fn_name}: expected positive {expected}, got {got}.")
    }

    pub fn positive_integer_no_fn_name(expected: &str, got: &str) -> String {
        format!("Expected positive {expected}, got {got}.")
    }
}

pub trait TryIntoExpression<T>: Sized
where
    Self: ToString + TryInto<T>,
{
    type Error;

    fn human_readable_dest_type(&self) -> String;

    fn try_into_for(self, fn_name: &str) -> Result<T, LispError> {
        let hr_src_type = self.to_string();
        let hr_dest_type = self.human_readable_dest_type();
        let t = self.try_into();
        match t {
            Ok(t) => Ok(t),
            Err(_) => Err(LispError::new(ErrorStrings::mismatched_type(
                fn_name,
                &hr_dest_type,
                &hr_src_type,
            ))),
        }
    }
}

impl TryIntoExpression<Expression> for Expression {
    type Error = LispError;

    fn human_readable_dest_type(&self) -> String {
        self.display_type()
    }
}

impl From<String> for CharString {
    fn from(value: String) -> Self {
        CharString(Cow::from(value))
    }
}

impl TryIntoExpression<CharString> for Expression {
    type Error = LispError;

    fn human_readable_dest_type(&self) -> String {
        ExpEnum::Char(Cow::from(String::default())).to_string()
    }
}

impl From<CharString> for Expression {
    fn from(src: CharString) -> Self {
        Expression::alloc_data(ExpEnum::Char(src.0.clone()))
    }
}

impl TryFrom<Expression> for CharString {
    type Error = LispError;

    fn try_from(value: Expression) -> Result<Self, Self::Error> {
        let val = value.get();
        match &val.data {
            ExpEnum::Char(char_string) => Ok(CharString(char_string.to_owned())),
            _ => Err(LispError::new("Can only convert Char from ExpEnum::Char")),
        }
    }
}

impl TryIntoExpression<String> for Expression {
    type Error = LispError;

    fn human_readable_dest_type(&self) -> String {
        ExpEnum::String(Cow::from(String::default()), Default::default()).to_string()
    }
}

impl From<String> for Expression {
    fn from(src: String) -> Self {
        Expression::alloc_data(ExpEnum::String(src.into(), None))
    }
}

impl TryFrom<Expression> for String {
    type Error = LispError;

    fn try_from(value: Expression) -> Result<Self, Self::Error> {
        match &value.get().data {
            ExpEnum::String(cow, _) => Ok(cow.to_string()),
            ExpEnum::Symbol(sym, _) => Ok(sym.to_string()),
            ExpEnum::Char(ch) => Ok(ch.to_string()),
            ExpEnum::CodePoint(ch) => Ok(ch.to_string()),
            _ => Err(LispError::new(
                "Can only convert String from ExpEnum::String.",
            )),
        }
    }
}

impl From<bool> for Expression {
    fn from(val: bool) -> Self {
        if val {
            Expression::make_true()
        } else {
            Expression::make_false()
        }
    }
}

impl TryFrom<Expression> for bool {
    type Error = LispError;
    fn try_from(num: Expression) -> Result<Self, Self::Error> {
        match num.get().data {
            ExpEnum::True => Ok(true),
            ExpEnum::False => Ok(false),
            _ => Err(LispError::new(
                "Can only convert bool from ExpEnum::True or ExpEnum::False.",
            )),
        }
    }
}

impl From<()> for Expression {
    fn from(_: ()) -> Self {
        Expression::make_nil()
    }
}

impl TryIntoExpression<bool> for Expression {
    type Error = LispError;

    fn human_readable_dest_type(&self) -> String {
        ExpEnum::True.to_string() + " or " + &ExpEnum::False.to_string()
    }
}

impl From<f64> for Expression {
    fn from(num: f64) -> Self {
        Expression::alloc_data(ExpEnum::Float(num))
    }
}

impl TryFrom<Expression> for f64 {
    type Error = LispError;
    fn try_from(num: Expression) -> Result<Self, Self::Error> {
        match num.get().data {
            ExpEnum::Float(num) => Ok(num),
            ExpEnum::Int(num) => Ok(num as f64),
            _ => Err(LispError::new(
                "Can only convert f64 from ExpEnum::Float or ExpEnum::Int.",
            )),
        }
    }
}

impl TryIntoExpression<f64> for Expression {
    type Error = LispError;

    fn human_readable_dest_type(&self) -> String {
        ExpEnum::Float(Default::default()).to_string()
    }
}

impl From<i64> for Expression {
    fn from(num: i64) -> Self {
        Expression::alloc_data(ExpEnum::Int(num))
    }
}

impl TryFrom<Expression> for i64 {
    type Error = LispError;
    fn try_from(num: Expression) -> Result<Self, Self::Error> {
        match num.get().data {
            ExpEnum::Float(num) => Ok(num as i64),
            ExpEnum::Int(num) => Ok(num),
            _ => Err(LispError::new(
                "Can only convert i64 from ExpEnum::Float or ExpEnum::Int.",
            )),
        }
    }
}

impl TryIntoExpression<i64> for Expression {
    type Error = LispError;

    fn human_readable_dest_type(&self) -> String {
        ExpEnum::Int(Default::default()).to_string()
    }
}

impl<T> From<Vec<T>> for Expression
where
    T: Into<Expression>,
{
    fn from(values: Vec<T>) -> Self {
        let mut vec = vec![];
        for value in values {
            vec.push(value.into())
        }
        Expression::with_list(vec)
    }
}

impl From<usize> for Expression {
    fn from(num: usize) -> Self {
        Expression::alloc_data(ExpEnum::Int(num as i64))
    }
}

impl TryFrom<Expression> for usize {
    type Error = LispError;
    fn try_from(num: Expression) -> Result<Self, Self::Error> {
        match num.get().data {
            ExpEnum::Float(num) => {
                if num >= 0.0 {
                    Ok(num as usize)
                } else {
                    Err(LispError::new(ErrorStrings::positive_integer_no_fn_name(
                        "Number",
                        &num.to_string(),
                    )))
                }
            }
            ExpEnum::Int(num) => {
                if num >= 0 {
                    Ok(num as usize)
                } else {
                    Err(LispError::new(ErrorStrings::positive_integer_no_fn_name(
                        "Number",
                        &num.to_string(),
                    )))
                }
            }
            _ => Err(LispError::new(
                "Can only convert usize from positive ExpEnum::Float or ExpEnum::Int.",
            )),
        }
    }
}

impl TryIntoExpression<usize> for Expression {
    type Error = LispError;

    fn human_readable_dest_type(&self) -> String {
        ExpEnum::Int(Default::default()).to_string()
    }
}

impl TryFrom<Expression> for (Expression, Expression) {
    type Error = LispError;

    fn try_from(value: Expression) -> Result<Self, Self::Error> {
        let data = &value.get().data;
        let data_type = data.to_string();
        match data {
            ExpEnum::Pair(e0, e1) => Ok((e0.clone(), e1.clone())),
            _ => {
                let reason = format!("Expected Pair got {}.", data_type);
                Err(LispError::new(reason))
            }
        }
    }
}

impl From<(Expression, Expression)> for Expression {
    fn from((e0, e1): (Expression, Expression)) -> Self {
        Expression::alloc_data(ExpEnum::Pair(e0, e1))
    }
}

impl TryIntoExpression<(Expression, Expression)> for Expression {
    type Error = LispError;

    fn human_readable_dest_type(&self) -> String {
        ExpEnum::Pair(Expression::make_nil(), Expression::make_nil()).to_string()
    }
}

/// Used by sl_sh_fn macro to embed information at runtime about the parameters of
/// the rust native function, specifically whether it is a normal Type, or some
/// supported wrapped type, e.g. Optional.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TypeHandle {
    Direct,
    Optional,
    VarArgs,
}

/// Used by sl_sh_fn macro to embed information at runtime about the parameters of
/// the rust native function, specifically whether it is going to pass the value (a move),
/// a reference, or mutable reference.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PassingStyle {
    Value,
    Reference,
    MutReference,
}

/// Struct used by sl_sh_fn macro to embed information in an array at runtime about each of
/// the parameters of the rust native function.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Param {
    pub handle: TypeHandle,
    pub passing_style: PassingStyle,
}

pub trait RustProcedure<T, F>
where
    Self: Sized,
    F: FnOnce(T) -> LispResult<Expression> + ?Sized,
{
    fn apply(&self, fn_name: &str, fun: F) -> LispResult<Expression>;
}

pub trait RustProcedureRefMut<T, F>
where
    Self: Sized,
    F: FnOnce(&mut T) -> LispResult<Expression> + ?Sized,
{
    fn apply_ref_mut(&mut self, fn_name: &str, fun: F) -> LispResult<Expression>;
}

pub struct Codepoint(pub char);

impl TryIntoExpression<Codepoint> for Expression {
    type Error = LispError;

    fn human_readable_dest_type(&self) -> String {
        ExpEnum::CodePoint(Default::default()).to_string()
    }
}

impl TryFrom<Expression> for Codepoint {
    type Error = LispError;

    fn try_from(value: Expression) -> Result<Self, Self::Error> {
        let val = value.get();
        match &val.data {
            ExpEnum::CodePoint(char) => Ok(Codepoint(*char)),
            _ => Err(LispError::new(
                "Can only convert char from ExpEnum::CodePoint",
            )),
        }
    }
}

impl From<Codepoint> for Expression {
    fn from(value: Codepoint) -> Self {
        Expression::alloc_data(ExpEnum::CodePoint(value.0))
    }
}

pub struct CharString(pub Cow<'static, str>);

pub struct CharStringRef<'a>(pub &'a Cow<'static, str>);

impl<F> RustProcedure<CharString, F> for TypedWrapper<'_, CharString, Expression>
where
    F: FnOnce(CharString) -> LispResult<Expression>,
{
    fn apply(&self, fn_name: &str, fun: F) -> LispResult<Expression> {
        let got = self.0.display_type();
        match &self.0.get().data {
            ExpEnum::Char(str) => {
                let sym = CharString(str.to_owned());
                fun(sym)
            }
            _ => Err(LispError::new(ErrorStrings::mismatched_type(
                fn_name,
                &ExpEnum::Char(Default::default()).to_string(),
                &got,
            ))),
        }
    }
}

impl<F> RustProcedure<CharStringRef<'_>, F> for TypedWrapper<'_, CharStringRef<'_>, Expression>
where
    F: FnOnce(CharStringRef<'_>) -> LispResult<Expression>,
{
    fn apply(&self, fn_name: &str, fun: F) -> LispResult<Expression> {
        let got = self.0.display_type();
        match &self.0.get().data {
            ExpEnum::Char(str) => {
                let sym = CharStringRef(str);
                fun(sym)
            }
            _ => Err(LispError::new(ErrorStrings::mismatched_type(
                fn_name,
                &ExpEnum::Char(Default::default()).to_string(),
                &got,
            ))),
        }
    }
}

pub struct Symbol(&'static str, SymLoc);

impl<'a, F> RustProcedureRefMut<Symbol, F> for TypedWrapper<'_, Symbol, Expression>
where
    F: FnOnce(&mut Symbol) -> LispResult<Expression>,
{
    fn apply_ref_mut(&mut self, fn_name: &str, fun: F) -> LispResult<Expression> {
        let got = self.0.display_type();
        match &self.0.get().data {
            ExpEnum::Symbol(str, loc) => {
                let mut sym = Symbol(str, loc.clone());
                fun(&mut sym)
            }
            _ => Err(LispError::new(ErrorStrings::mismatched_type(
                fn_name,
                &ExpEnum::Symbol(Default::default(), SymLoc::None).to_string(),
                &got,
            ))),
        }
    }
}

impl<F> RustProcedure<&Symbol, F> for TypedWrapper<'_, Symbol, Expression>
where
    F: FnOnce(&Symbol) -> LispResult<Expression>,
{
    fn apply(&self, fn_name: &str, fun: F) -> LispResult<Expression> {
        let got = self.0.display_type();
        match &self.0.get().data {
            ExpEnum::Symbol(str, loc) => {
                let sym = Symbol(str, loc.clone());
                fun(&sym)
            }
            _ => Err(LispError::new(ErrorStrings::mismatched_type(
                fn_name,
                &ExpEnum::Symbol(Default::default(), SymLoc::None).to_string(),
                &got,
            ))),
        }
    }
}

impl<F> RustProcedure<Symbol, F> for TypedWrapper<'_, Symbol, Expression>
where
    F: FnOnce(Symbol) -> LispResult<Expression>,
{
    fn apply(&self, fn_name: &str, fun: F) -> LispResult<Expression> {
        let got = self.0.display_type();
        match &self.0.get().data {
            ExpEnum::Symbol(str, loc) => {
                let sym = Symbol(str, loc.clone());
                fun(sym)
            }
            _ => Err(LispError::new(ErrorStrings::mismatched_type(
                fn_name,
                &ExpEnum::Symbol(Default::default(), SymLoc::None).to_string(),
                &got,
            ))),
        }
    }
}

impl<F> RustProcedureRefMut<HashMap<&str, Expression>, F>
    for TypedWrapper<'_, HashMap<&str, Expression>, Expression>
where
    F: FnOnce(&mut HashMap<&str, Expression>) -> LispResult<Expression>,
{
    fn apply_ref_mut(&mut self, fn_name: &str, fun: F) -> LispResult<Expression> {
        try_inner_hash_map_mut!(fn_name, self.0, arg, fun(arg))
    }
}

impl<F> RustProcedure<HashMap<&str, Expression>, F>
    for TypedWrapper<'_, HashMap<&str, Expression>, Expression>
where
    F: FnOnce(HashMap<&str, Expression>) -> LispResult<Expression>,
{
    fn apply(&self, fn_name: &str, fun: F) -> LispResult<Expression> {
        try_inner_hash_map!(fn_name, self.0, arg, fun(arg.clone()))
    }
}

impl<F> RustProcedure<&str, F> for TypedWrapper<'_, &str, Expression>
where
    F: FnOnce(&str) -> LispResult<Expression>,
{
    fn apply(&self, fn_name: &str, fun: F) -> LispResult<Expression> {
        try_inner_string!(fn_name, &self.0, arg, fun(arg))
    }
}

impl<F> RustProcedure<String, F> for TypedWrapper<'_, String, Expression>
where
    F: FnOnce(String) -> LispResult<Expression>,
{
    fn apply(&self, fn_name: &str, fun: F) -> LispResult<Expression> {
        try_inner_string!(fn_name, &self.0, arg, {
            let arg = arg.to_string();
            fun(arg)
        })
    }
}

impl<F> RustProcedureRefMut<String, F> for TypedWrapper<'_, String, Expression>
where
    F: FnOnce(&mut String) -> LispResult<Expression>,
{
    fn apply_ref_mut(&mut self, fn_name: &str, fun: F) -> LispResult<Expression> {
        try_inner_string!(fn_name, &self.0, arg, {
            let arg = &mut arg.to_string();
            fun(arg)
        })
    }
}

impl<F> RustProcedure<&Expression, F> for TypedWrapper<'_, Expression, Expression>
where
    F: FnOnce(&Expression) -> LispResult<Expression>,
{
    fn apply(&self, _fn_name: &str, fun: F) -> LispResult<Expression> {
        fun(&mut self.0.clone())
    }
}

impl<F> RustProcedureRefMut<Expression, F> for TypedWrapper<'_, Expression, Expression>
where
    F: FnOnce(&mut Expression) -> LispResult<Expression>,
{
    fn apply_ref_mut(&mut self, _fn_name: &str, fun: F) -> LispResult<Expression> {
        fun(&mut self.0.clone())
    }
}

impl<F> RustProcedure<Expression, F> for TypedWrapper<'_, Expression, Expression>
where
    F: FnOnce(Expression) -> LispResult<Expression>,
{
    fn apply(&self, _fn_name: &str, fun: F) -> LispResult<Expression> {
        fun(self.0.clone())
    }
}

impl<F> RustProcedure<i64, F> for TypedWrapper<'_, i64, Expression>
where
    F: FnOnce(i64) -> LispResult<Expression>,
{
    fn apply(&self, fn_name: &str, fun: F) -> LispResult<Expression> {
        try_inner_int!(fn_name, self.0, num, fun(num))
    }
}

impl<F> RustProcedure<usize, F> for TypedWrapper<'_, usize, Expression>
where
    F: FnOnce(usize) -> LispResult<Expression>,
{
    fn apply(&self, fn_name: &str, fun: F) -> LispResult<Expression> {
        match self.0.get().data {
            ExpEnum::Int(name) => {
                if name < 0 {
                    return Err(LispError::new(ErrorStrings::positive_integer(
                        fn_name,
                        &ExpEnum::Int(Default::default()).to_string(),
                        &self.0.to_string(),
                    )));
                } else {
                    fun(name as usize)
                }
            }
            _ => {
                return Err(LispError::new(ErrorStrings::mismatched_type(
                    fn_name,
                    &ExpEnum::Int(Default::default()).to_string(),
                    &self.0.to_string(),
                )))
            }
        }
    }
}

impl<F> RustProcedure<f64, F> for TypedWrapper<'_, f64, Expression>
where
    F: FnOnce(f64) -> LispResult<Expression>,
{
    fn apply(&self, fn_name: &str, fun: F) -> LispResult<Expression> {
        match &self.0.get().data {
            ExpEnum::Float(f) => fun(*f),
            ExpEnum::Int(i) => fun(*i as f64),
            _ => {
                let expected = ExpEnum::Float(f64::default()).to_string()
                    + ", or "
                    + &ExpEnum::Int(i64::default()).to_string();
                Err(LispError::new(ErrorStrings::mismatched_type(
                    fn_name,
                    &expected,
                    &self.0.to_string(),
                )))
            }
        }
    }
}

impl<F> RustProcedure<Rc<RefCell<FileState>>, F>
    for TypedWrapper<'_, Rc<RefCell<FileState>>, Expression>
where
    F: FnOnce(Rc<RefCell<FileState>>) -> LispResult<Expression>,
{
    fn apply(&self, fn_name: &str, fun: F) -> LispResult<Expression> {
        try_inner_file!(fn_name, self.0, file, fun(file))
    }
}
