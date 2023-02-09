use crate::environment::*;
use crate::eval::*;
use crate::types::*;
use std::borrow::Cow;

use std::convert::{TryFrom, TryInto};
use std::env;

pub fn param_eval_optional(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Option<Expression>, LispError> {
    if let Some(arg) = args.next() {
        Ok(Some(eval(environment, arg)?))
    } else {
        Ok(None)
    }
}

pub fn param_eval(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
    form: &str,
) -> Result<Expression, LispError> {
    if let Some(arg) = param_eval_optional(environment, args)? {
        Ok(arg)
    } else {
        let msg = format!(
            "{}: Missing required argument, see (doc '{}) for usage.",
            form, form
        );
        Err(LispError::new(msg))
    }
}

pub fn params_done(
    args: &mut dyn Iterator<Item = Expression>,
    form: &str,
) -> Result<(), LispError> {
    if args.next().is_none() {
        Ok(())
    } else {
        let msg = format!(
            "{}: Too many arguments, see (doc '{}) for usage.",
            form, form
        );
        Err(LispError::new(msg))
    }
}

pub fn is_proper_list(exp: &Expression) -> bool {
    // does not detect empty (nil) lists on purpose.
    if let ExpEnum::Pair(_e1, e2) = &exp.get().data {
        if e2.is_nil() {
            true
        } else {
            is_proper_list(e2)
        }
    } else {
        false
    }
}

pub fn list_to_args(
    environment: &mut Environment,
    parts: &mut [Expression],
    do_eval: bool,
) -> Result<Vec<Expression>, LispError> {
    if do_eval {
        let mut args: Vec<Expression> = Vec::with_capacity(parts.len());
        for a in parts {
            args.push(eval(environment, a)?);
        }
        Ok(args)
    } else {
        let args: Vec<Expression> = parts.to_vec();
        Ok(args)
    }
}

pub fn parse_list_of_ints(
    environment: &mut Environment,
    args: &mut [Expression],
) -> Result<Vec<i64>, LispError> {
    let mut list: Vec<i64> = Vec::with_capacity(args.len());
    for arg in args {
        list.push(arg.make_int(environment)?);
    }
    Ok(list)
}

pub fn parse_list_of_floats(
    environment: &mut Environment,
    args: &mut [Expression],
) -> Result<Vec<f64>, LispError> {
    let mut list: Vec<f64> = Vec::with_capacity(args.len());
    for arg in args {
        list.push(arg.make_float(environment)?);
    }
    Ok(list)
}

pub fn parse_list_of_strings(
    environment: &mut Environment,
    args: &mut [Expression],
) -> Result<Vec<String>, LispError> {
    let mut list: Vec<String> = Vec::with_capacity(args.len());
    for arg in args {
        list.push(arg.make_string(environment)?);
    }
    Ok(list)
}

pub fn expand_tilde(path: &str) -> Option<String> {
    if path.ends_with('~') || path.contains("~/") {
        let mut home = match env::var("HOME") {
            Ok(val) => val,
            Err(_) => "/".to_string(),
        };
        if home.ends_with('/') {
            home.pop();
        }
        let mut new_path = String::new();
        let mut last_ch = ' ';
        for ch in path.chars() {
            if ch == '~' && (last_ch == ' ' || last_ch == ':') {
                if last_ch == '\\' {
                    new_path.push('~');
                } else {
                    new_path.push_str(&home);
                }
            } else {
                if last_ch == '\\' {
                    new_path.push('\\');
                }
                if ch != '\\' {
                    new_path.push(ch);
                }
            }
            last_ch = ch;
        }
        if last_ch == '\\' {
            new_path.push('\\');
        }
        Some(new_path)
    } else {
        None
    }
}

pub fn compress_tilde(path: &str) -> Option<String> {
    if let Ok(mut home) = env::var("HOME") {
        if home.ends_with('/') {
            home.pop();
        }
        if path.contains(&home) {
            Some(path.replace(&home, "~"))
        } else {
            None
        }
    } else {
        None
    }
}

pub fn make_args_eval_no_values(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Vec<Expression>, LispError> {
    let mut list: Vec<Expression> = Vec::new();
    for arg in args {
        list.push(eval_no_values(environment, arg)?);
    }
    Ok(list)
}

pub fn make_args(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Vec<Expression>, LispError> {
    let mut list: Vec<Expression> = Vec::new();
    for arg in args {
        list.push(eval(environment, arg)?);
    }
    Ok(list)
}

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

#[macro_export]
macro_rules! ret_err_exp_enum {
    ($expression:expr, $(|)? $( $pattern:pat_param )|+ $( if $guard: expr )? $(,)?, $eval:expr, $err:expr) => {
        match $expression {
            $( $pattern )|+ $( if $guard )? => $eval,
            _ => return Err(LispError::new($err))
        }
    }
}

#[macro_export]
macro_rules! try_exp_enum {
    ($expression:expr, $(|)? $( $pattern:pat_param )|+ $( if $guard: expr )? $(,)?, $eval:expr, $err:expr) => {
        match $expression {
            $( $pattern )|+ $( if $guard )? => Ok($eval),
            _ => Err(LispError::new($err))
        }
    }
}

#[macro_export]
macro_rules! try_inner_int {
    ($fn_name:ident, $expression:expr, $name:ident, $eval:expr) => {{
        use $crate::ErrorStrings;
        match $expression.get().data {
            ExpEnum::Int($name) => $eval,
            _ => {
                return Err(LispError::new(ErrorStrings::mismatched_type(
                    $fn_name,
                    &ExpEnum::Int(Default::default()).to_string(),
                    &$expression.to_string(),
                )))
            }
        }
    }};
}

#[macro_export]
macro_rules! try_inner_file {
    ($fn_name:ident, $expression:expr, $name:ident, $eval:expr) => {{
        use $crate::ErrorStrings;
        match &$expression.get().data {
            ExpEnum::File($name) => {
                let $name = $name.clone();
                $eval
            }
            data => {
                return Err(LispError::new(ErrorStrings::mismatched_type(
                    $fn_name,
                    &ExpEnum::File(std::rc::Rc::new(std::cell::RefCell::new(
                        $crate::types::FileState::Closed,
                    )))
                    .to_string(),
                    &data.to_string(),
                )))
            }
        }
    }};
}

#[macro_export]
macro_rules! try_inner_float {
    ($fn_name:ident, $expression:expr, $name:ident, $eval:expr) => {{
        use $crate::ErrorStrings;
        let exp_d = $expression.get();
        match &exp_d.data {
            ExpEnum::Float($name) => $eval,
            ExpEnum::Int($name) => {
                let $name = *$name as i64;
                $eval
            }
            data => {
                let expected = ExpEnum::Float(f64::default()).to_string()
                    + ", or "
                    + &ExpEnum::Int(i64::default()).to_string();
                return Err(LispError::new(ErrorStrings::mismatched_type(
                    $fn_name,
                    &expected,
                    &data.to_string(),
                )));
            }
        }
    }};
}

#[macro_export]
macro_rules! is_sequence {
    ($expression:expr) => {{
        match &$expression.get().data {
            ExpEnum::Pair(_, _) => true,
            ExpEnum::Vector(_) => true,
            ExpEnum::Nil => true,
            _ => false,
        }
    }};
}

#[macro_export]
macro_rules! try_inner_pair {
    ($fn_name:ident, $expression:expr, $name0:ident, $name1:ident, $eval:expr) => {{
        use $crate::ErrorStrings;
        match $expression.get().data {
            ExpEnum::Pair($name0, $name1) => $eval,
            _ => {
                return Err($crate::LispError::new(ErrorStrings::mismatched_type(
                    $fn_name,
                    &ExpEnum::Pair(
                        $crate::Expression::make_nil(),
                        $crate::Expression::make_nil(),
                    )
                    .to_string(),
                    &$expression.to_string(),
                )))
            }
        }
    }};
}

#[macro_export]
macro_rules! try_inner_hash_map {
    ($fn_name:ident, $expression:expr, $name:ident, $eval:expr) => {{
        use $crate::ErrorStrings;
        let exp_d = $expression.get();
        match &exp_d.data {
            ExpEnum::HashMap($name) => $eval,
            _ => {
                return Err($crate::LispError::new(ErrorStrings::mismatched_type(
                    $fn_name,
                    &ExpEnum::HashMap(Default::default()).to_string(),
                    &$expression.to_string(),
                )))
            }
        }
    }};
}

#[macro_export]
macro_rules! try_inner_hash_map_mut {
    ($fn_name:ident, $expression:expr, $name:ident, $eval:expr) => {{
        use $crate::ErrorStrings;
        match &mut $expression.get_mut().data {
            ExpEnum::HashMap(ref mut $name) => $eval,
            _ => {
                return Err($crate::LispError::new(ErrorStrings::mismatched_type(
                    $fn_name,
                    &ExpEnum::HashMap(Default::default()).to_string(),
                    &$expression.to_string(),
                )))
            }
        }
    }};
}

#[macro_export]
macro_rules! try_inner_string {
    ($fn_name:ident, $expression:expr, $name:ident, $eval:expr) => {{
        use $crate::ErrorStrings;
        match &$expression.get().data {
            ExpEnum::String($name, _) => $eval,
            ExpEnum::Symbol($name, _) => $eval,
            ExpEnum::Char($name) => $eval,
            _ => {
                return Err($crate::LispError::new(ErrorStrings::mismatched_type(
                    $fn_name,
                    &format!(
                        "{}, {}, or {}, ",
                        ExpEnum::String(Default::default(), Default::default()).to_string(),
                        ExpEnum::Symbol(Default::default(), Default::default()).to_string(),
                        ExpEnum::Char(Default::default()).to_string()
                    ),
                    &$expression.to_string(),
                )))
            }
        }
    }};
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
