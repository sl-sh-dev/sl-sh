use crate::environment::*;
use crate::eval::*;
use crate::types::*;
use std::borrow::Cow;

use crate::LispResult;
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

pub trait ExpandVecToArgs<Args> {
    type Output;
    fn call_expand_args(&self, args: Args) -> Self::Output;
}

impl<F, T, R> ExpandVecToArgs<[T; 0]> for F
where
    F: Fn() -> R,
{
    type Output = R;
    fn call_expand_args(&self, _args: [T; 0]) -> R {
        self()
    }
}

impl<F, T, R> ExpandVecToArgs<[T; 1]> for F
where
    F: Fn(T) -> R,
{
    type Output = R;
    fn call_expand_args(&self, args: [T; 1]) -> R {
        let [arg0] = args;
        self(arg0)
    }
}

impl<F, T, R> ExpandVecToArgs<[T; 2]> for F
where
    F: Fn(T, T) -> R,
{
    type Output = R;
    fn call_expand_args(&self, args: [T; 2]) -> R {
        let [arg0, arg1] = args;
        self(arg0, arg1)
    }
}

impl<F, T, R> ExpandVecToArgs<[T; 3]> for F
where
    F: Fn(T, T, T) -> R,
{
    type Output = R;
    fn call_expand_args(&self, args: [T; 3]) -> R {
        let [arg0, arg1, arg2] = args;
        self(arg0, arg1, arg2)
    }
}

impl<F, T, R> ExpandVecToArgs<[T; 4]> for F
where
    F: Fn(T, T, T, T) -> R,
{
    type Output = R;
    fn call_expand_args(&self, args: [T; 4]) -> R {
        let [arg0, arg1, arg2, arg3] = args;
        self(arg0, arg1, arg2, arg3)
    }
}

impl<F, T, R> ExpandVecToArgs<[T; 5]> for F
where
    F: Fn(T, T, T, T, T) -> R,
{
    type Output = R;
    fn call_expand_args(&self, args: [T; 5]) -> R {
        let [arg0, arg1, arg2, arg3, arg4] = args;
        self(arg0, arg1, arg2, arg3, arg4)
    }
}

impl<F, T, R> ExpandVecToArgs<[T; 6]> for F
where
    F: Fn(T, T, T, T, T, T) -> R,
{
    type Output = R;
    fn call_expand_args(&self, args: [T; 6]) -> R {
        let [arg0, arg1, arg2, arg3, arg4, arg5] = args;
        self(arg0, arg1, arg2, arg3, arg4, arg5)
    }
}

impl<F, T, R> ExpandVecToArgs<[T; 7]> for F
where
    F: Fn(T, T, T, T, T, T, T) -> R,
{
    type Output = R;
    fn call_expand_args(&self, args: [T; 7]) -> R {
        let [arg0, arg1, arg2, arg3, arg4, arg5, arg6] = args;
        self(arg0, arg1, arg2, arg3, arg4, arg5, arg6)
    }
}

pub struct ErrorStrings {}

impl ErrorStrings {
    pub fn mismatched_type(fn_name: &str, expected: &str, got: &str) -> String {
        format!("{fn_name}: mismatched type input, expected {expected}, got {got}.")
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
        use crate::ErrorStrings;
        match &mut $expression.get_mut().data {
            ExpEnum::Int(ref mut $name)=> $eval,
            _ => return Err( LispError::new(ErrorStrings::mismatched_type(
                $fn_name,
                &ExpEnum::Int(Default::default()).to_string(), 
                &$expression.to_string(),
            )))
        }
    }};
}

#[macro_export]
macro_rules! try_inner_float {
    ($fn_name:ident, $expression:expr, $name:ident, $eval:expr) => {{
        use crate::ErrorStrings;
        match &mut $expression.get_mut().data {
            ExpEnum::Float(ref mut $name)=> $eval,
            _ => return Err( LispError::new(ErrorStrings::mismatched_type(
                $fn_name,
                &ExpEnum::Float(Default::default()).to_string(), 
                &$expression.to_string(),
            )))
        }
    }};
}

#[macro_export]
macro_rules! try_inner_hash_map {
    ($fn_name:ident, $expression:expr, $name:ident, $eval:expr) => {{
        use crate::ErrorStrings;
        match &mut $expression.get_mut().data {
            ExpEnum::HashMap(ref mut $name)=> $eval,
            _ => return Err( LispError::new(ErrorStrings::mismatched_type(
                $fn_name,
                &ExpEnum::HashMap(Default::default()).to_string(), 
                &$expression.to_string(),
            )))
        }
    }};
}

#[derive(Debug, Clone)]
pub enum ArgType {
    Exp(Expression),
    Opt(Option<Expression>),
    VarArgs(Vec<Expression>),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ArgVal {
    Value,
    Optional,
    Vec,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ArgPassingStyle {
    Move,
    Reference,
    MutReference,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Arg {
    pub val: ArgVal,
    pub passing_style: ArgPassingStyle,
}

fn has_optional_params(params: &[Arg]) -> bool {
    for p in params {
        match p.val {
            ArgVal::Value => {}
            ArgVal::Optional => {
                return true;
            }
            ArgVal::Vec => {
                return true;
            }
        }
    }
    false
}

fn num_required_args(params: &[Arg]) -> usize {
    params.iter().fold(0, |accum, nxt| {
        if nxt.val == ArgVal::Value {
            accum + 1
        } else {
            accum
        }
    })
}

/// arrays may be of different sizes, but there is at least 1 param that is optional or vec.
fn get_args_optional_aware(
    fn_name: &str,
    params: Vec<Arg>,
    args: Vec<Expression>,
) -> LispResult<Vec<ArgType>> {
    let mut parsed_args = vec![];
    // if they are equal it should be easy to parse regardless of the types, it's one for one.
    if args.len() == params.len() {
        for (exp, arg) in args.into_iter().zip(params.into_iter()) {
            match arg.val {
                ArgVal::Value => parsed_args.push(ArgType::Exp(exp)),
                ArgVal::Optional => parsed_args.push(ArgType::Opt(Some(exp))),
                ArgVal::Vec => parsed_args.push(ArgType::VarArgs(vec![exp])),
            }
        }
    } else {
        // if there are more args than params for the receiving fn,
        // the last param must be a vector.
        if args.len() > params.len() {
            if let Some(param) = params.iter().rev().next() {
                match param.val {
                    ArgVal::Value | ArgVal::Optional => {
                        return Err(LispError::new(format!(
                            "{} given too many arguments, expected {}, got {}.",
                            fn_name,
                            params.len(),
                            args.len()
                        )));
                    }
                    ArgVal::Vec => {}
                }
            }
        }
        let mut args_iter = args.into_iter();
        for param in params {
            if let Some(exp) = args_iter.next() {
                match param.val {
                    ArgVal::Value => parsed_args.push(ArgType::Exp(exp)),
                    ArgVal::Optional => parsed_args.push(ArgType::Opt(Some(exp))),
                    // There can only be one ArgVal::Vec and it's always the last one, this is
                    // enforced at compile time.
                    ArgVal::Vec => {
                        let mut exps = vec![exp];
                        while let Some(exp) = args_iter.next() {
                            exps.push(exp);
                        }
                        parsed_args.push(ArgType::VarArgs(exps));
                    }
                }
            } else {
                match param.val {
                    ArgVal::Value => {
                        // it can't be the case that we don't have enough required arguments,
                        // we already checked for that.
                    }
                    ArgVal::Optional => parsed_args.push(ArgType::Opt(None)),
                    ArgVal::Vec => parsed_args.push(ArgType::VarArgs(vec![])),
                }
            }
        }
    }
    Ok(parsed_args)
}

pub fn get_arg_types(
    fn_name: &str,
    params: Vec<Arg>,
    args: Vec<Expression>,
) -> LispResult<Vec<ArgType>> {
    let parsed_args = vec![];
    let required_args = num_required_args(params.as_slice());
    let has_optional = has_optional_params(params.as_slice());
    let has_optional_str = if has_optional { "at least " } else { "" };
    match (
        args.len() < required_args,
        has_optional,
        args.len() == required_args,
        args.len() > required_args,
    ) {
        (true, _, _, _) => {
            return Err(LispError::new(format!(
                "{} not given enough arguments, expected {}{}, got {}.",
                fn_name,
                has_optional_str,
                required_args,
                args.len()
            )));
        }
        (_, true, _, _) | (_, _, true, _) => {
            return get_args_optional_aware(fn_name, params, args);
        }
        (_, _, _, true) => {
            // if the length of the passed in args is
            return Err(LispError::new(format!(
                "{} given too many arguments, expected {}, got {}.",
                fn_name,
                required_args,
                args.len()
            )));
        }
        (false, false, false, false) => {}
    }
    Ok(parsed_args)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::LispResult;
    use std::collections::HashMap;
    use std::convert::TryFrom;
    use std::convert::TryInto;

    #[test]
    fn test_from_numerical() {
        let result = Expression::from(42i64);
        assert_eq!(result, Expression::alloc_data(ExpEnum::Int(42i64)));

        let result: Expression = 42i64.into();
        assert_eq!(result, Expression::alloc_data(ExpEnum::Int(42i64)));

        let result = i64::try_from(Expression::alloc_data(ExpEnum::Int(7))).unwrap();
        assert_eq!(7, result);

        let result: f64 = Expression::alloc_data(ExpEnum::Int(7)).try_into().unwrap();
        assert_eq!(7f64, result);

        let result: f64 = Expression::alloc_data(ExpEnum::Float(7f64))
            .try_into()
            .unwrap();
        assert_eq!(7f64, result);

        let result: f64 = Expression::alloc_data(ExpEnum::Float(7f64))
            .try_into()
            .unwrap();
        assert_eq!(7f64, result);

        let result = f64::try_from(Expression::alloc_data(ExpEnum::Int(7i64))).unwrap();
        assert_eq!(7f64, result);
    }

    impl PartialEq<Self> for Expression {
        fn eq(&self, other: &Self) -> bool {
            match (&self.get().data, &other.get().data) {
                (ExpEnum::True, ExpEnum::True) => true,
                (ExpEnum::False, ExpEnum::False) => true,
                (ExpEnum::Nil, ExpEnum::Nil) => true,
                (ExpEnum::Float(lf), ExpEnum::Float(rt)) => lf == rt,
                (ExpEnum::Int(lf), ExpEnum::Int(rt)) => lf == rt,
                (ExpEnum::Char(lf), ExpEnum::Char(rt)) => lf == rt,
                (ExpEnum::CodePoint(lf), ExpEnum::CodePoint(rt)) => lf == rt,
                (_, _) => false,
            }
            //(ExpEnum::String(lf_s, lf_i), ExpEnum::String(rt_s, rt_i) => {}
            //(ExpEnum::Symbol(_, _), ExpEnum::Symbol(_, _)) => {}
            //(ExpEnum::Regex(_), ExpEnum::Regex(_)) => {}
            //ExpEnum::Symbol(_, _) => {}
            //ExpEnum::Lambda(_) => {}
            //ExpEnum::Macro(_) => {}
            //ExpEnum::Function(_) => {}
            //ExpEnum::LazyFn(_, _) => {}
            //ExpEnum::Vector(_) => {}
            //ExpEnum::Values(_) => {}
            //ExpEnum::Pair(_, _) => {}
            //ExpEnum::HashMap(_) => {}
            //ExpEnum::Process(_) => {}
            //ExpEnum::File(_) => {}
            //ExpEnum::Wrapper(_) => {}
            //ExpEnum::DeclareDef => {}
            //ExpEnum::DeclareVar => {}
            //ExpEnum::DeclareFn => {}
            //ExpEnum::DeclareMacro => {}
            //ExpEnum::Quote => {}
            //ExpEnum::BackQuote => {}
            //ExpEnum::Undefined => {}
        }
    }

    static K0: &'static str = "key0";
    static K1: &'static str = "key1";

    #[test]
    fn with_some_hashmaps_test() {
        let mut hash_map = HashMap::new();
        hash_map.insert(K0, Expression::alloc_data(ExpEnum::Int(7)));
        hash_map.insert(K1, Expression::alloc_data(ExpEnum::Int(11)));
        let myint0 = ArgType::Exp(Expression::alloc_data(ExpEnum::HashMap(hash_map)));
        arg_translate_clear_hash_map(myint0).unwrap();
    }

    fn arg_translate_clear_hash_map(arg_0: ArgType) -> crate::LispResult<()> {
        try_exp_enum!(
            arg_0,
            ArgType::Exp(exp),
            arg_unwrap_clear_hash_map(exp)?,
            "err"
        )
    }

    fn arg_unwrap_clear_hash_map(exp_0: Expression) -> crate::LispResult<()> {
        //TODO if you need to finagle some code in to this macro call, maybe
        // inlining (with the procedural macro) the function that can receive
        // the provided hash map would work to establish an interface,
        // the inlined function could obviously take the correct/provided
        // ty: Type argument the function had for that argument, and the
        // art would be getting a reference to an equivalent function
        // and copying some of it's code, e.g. if we had some way
        // of supplying a trait that hooked us up with the name of such
        // a function? then we could steal that functions internals?
        // fn does_hash_map_things() {
        //  //here is code we'd grab and steal;
        //  ret_err_exp_enum!
        // }
        // maybe traits could be used to make sure a given type always has
        // the correct function supplier? the trait would HAVE to be
        // declared on a type that (at compile time) would be checked to
        // make sure it has the correct trait, similar to the static assertions
        // we do now. in theory the trait/subtrait the declare argument has on
        // it would clue  us in to try_inner_hash_map or try_inner_int etc.
        // it relies on whether or not it's impossible to at compile time look
        // at the internals of the functions these functions provide and so
        // something with them. alternatively, what if the trait could return
        // a reference to ret_err_exp_enum!
        //
        // complicate macrO_rules like this: https://stackoverflow.com/questions/50008535/calling-functions-with-different-numbers-of-arguments-in-rust-macros
        // make me wonder if it's possible to write a macro that could just do this but?
        //
        // Since we have the type at compile time, what if we could turn
        // an Expression into a type that wraps the Expression but exposes
        // the inner ExpEnum as a generic type.
        // so
        // ```
        // let my_obj: Wrapper<T> = Expression::into_wrapper();
        // have wrapper impl AsRef, so it can "downcast" to a HashMap
        // ```
        
        try_exp_enum!(
            exp_0.get_mut().data,
            ExpEnum::HashMap(ref mut hash_map),
            clear_hash_map(hash_map)?,
            "Not an int_0!"
        )
    }

    fn clear_hash_map(hash_map: &mut HashMap<&str, Expression>) -> LispResult<()> {
        hash_map.clear();
        Ok(())
    }

    #[test]
    fn with_some_optional_hashmaps_test() {
        let mut hash_map = HashMap::new();
        hash_map.insert(K0, Expression::alloc_data(ExpEnum::Int(7)));
        hash_map.insert(K1, Expression::alloc_data(ExpEnum::Int(11)));
        let myint0 = ArgType::Opt(Some(Expression::alloc_data(ExpEnum::HashMap(hash_map))));
        arg_translate_clear_hash_map_optional(myint0).unwrap();
    }

    fn arg_translate_clear_hash_map_optional(arg_0: ArgType) -> crate::LispResult<()> {
        try_exp_enum!(
            arg_0,
            ArgType::Opt(exp),
            arg_unwrap_clear_hash_map_optional(exp)?,
            "Was not parseable as an option"
        )
    }

    fn arg_unwrap_clear_hash_map_optional(exp_0: Option<Expression>) -> crate::LispResult<()> {
        match exp_0 {
            None => Ok(clear_hash_map_optional(None)?),
            Some(exp_0) => {
                try_exp_enum!(
                    exp_0.get_mut().data,
                    ExpEnum::HashMap(ref mut hash_map),
                    clear_hash_map_optional(Some(hash_map))?,
                    "Not an int_0!"
                )
            }
        }
    }

    fn clear_hash_map_optional(hash_map: Option<&mut HashMap<&str, Expression>>) -> LispResult<()> {
        if let Some(hash_map) = hash_map {
            hash_map.clear();
        }
        Ok(())
    }

    #[test]
    fn int2float_negative_test() {
        let myint0 = ArgType::Exp(Expression::alloc_data(ExpEnum::Float(7.0)));
        let myint1 = ArgType::Exp(Expression::alloc_data(ExpEnum::Int(11)));
        let result = arg_translate_int_2_float(myint0, myint1);
        match result {
            Ok(_) => {
                panic!("result should not be ok!");
            }
            Err(e) => {
                assert_eq!(e.reason, "Not an int_0!");
            }
        }
        let myint0 = ArgType::Exp(Expression::alloc_data(ExpEnum::Int(7)));
        let myint1 = ArgType::Exp(Expression::alloc_data(ExpEnum::Float(11.0)));
        let result = arg_translate_int_2_float(myint0, myint1);
        match result {
            Ok(_) => {
                panic!("result should not be ok!");
            }
            Err(e) => {
                assert_eq!(e.reason, "Not an int_1!");
            }
        }
    }

    #[test]
    fn int2float_test() {
        let myint0 = ArgType::Exp(Expression::alloc_data(ExpEnum::Int(7)));
        let myint1 = ArgType::Exp(Expression::alloc_data(ExpEnum::Int(11)));
        let result = arg_translate_int_2_float(myint0, myint1).unwrap();
        let res_d = &result.get().data;
        match res_d {
            ExpEnum::Float(f) => {
                assert_eq!(18.0, *f);
            }
            _ => {
                panic!("Not a float!")
            }
        }
    }

    fn arg_translate_int_2_float(arg_0: ArgType, arg_1: ArgType) -> crate::LispResult<Expression> {
        try_exp_enum!(
            arg_0,
            ArgType::Exp(exp_0),
            {
                try_exp_enum!(
                    arg_1,
                    ArgType::Exp(exp_1),
                    { arg_unwrap_int_2_float(exp_0, exp_1).map(Into::into) }?,
                    "err"
                )
            }?,
            "err exp_0"
        )
    }

    fn arg_unwrap_int_2_float(exp_0: Expression, exp_1: Expression) -> crate::LispResult<f64> {
        Ok({
            let float = ret_err_exp_enum!(
                exp_0.get().data,
                ExpEnum::Int(int_0),
                {
                    let float = ret_err_exp_enum!(
                        exp_1.get().data,
                        ExpEnum::Int(int_1),
                        { int_2_float(int_0, int_1)? },
                        "Not an int_1!"
                    );
                    float
                },
                "Not an int_0!"
            );
            float
        })
    }

    fn int_2_float(my_int: i64, my_o_int: i64) -> LispResult<f64> {
        Ok((my_int + my_o_int) as f64)
    }

    #[test]
    fn optional_int2float_test() {
        let myint0 = ArgType::Exp(Expression::alloc_data(ExpEnum::Int(7)));
        let myint1 = ArgType::Opt(Some(Expression::alloc_data(ExpEnum::Int(11))));
        let result = arg_translate_optional_int_2_float(myint0, myint1).unwrap();
        let res_d = &result.get().data;
        match res_d {
            ExpEnum::Float(f) => {
                assert_eq!(18.0, *f);
            }
            _ => {
                panic!("Not a float!")
            }
        }

        let myint0 = ArgType::Exp(Expression::alloc_data(ExpEnum::Int(7)));
        let myint1 = ArgType::Opt(None);
        let result = arg_translate_optional_int_2_float(myint0, myint1).unwrap();
        let res_d = &result.get().data;
        match res_d {
            ExpEnum::Float(f) => {
                assert_eq!(7.0, *f);
            }
            _ => {
                panic!("Not a float!")
            }
        }
    }

    fn arg_translate_optional_int_2_float(
        arg_0: ArgType,
        arg_1: ArgType,
    ) -> crate::LispResult<Expression> {
        let exp_0 = ret_err_exp_enum!(arg_0, ArgType::Exp(exp), exp, "err");
        let exp_1 = ret_err_exp_enum!(arg_1, ArgType::Opt(exp), exp, "err");
        arg_unwrap_optional_int_2_float(exp_0, exp_1).map(Into::into)
    }

    fn arg_unwrap_optional_int_2_float(
        exp_0: Expression,
        exp_1: Option<Expression>,
    ) -> crate::LispResult<f64> {
        Ok({
            let float = ret_err_exp_enum!(
                exp_0.get().data,
                ExpEnum::Int(int_0),
                {
                    match exp_1 {
                        None => {
                            let int_1 = None;
                            optional_int_2_float(int_0, int_1)
                        },
                        Some(exp_1) => {
                            let float = ret_err_exp_enum!(
                                exp_1.get().data,
                                ExpEnum::Int(int_1),
                                { optional_int_2_float(int_0, Some(int_1)) },
                                "Not an int_1!"
                            );
                            float
                        }
                    }
                },
                "Not an int_0!"
            );
            float
        })
    }

    fn optional_int_2_float(my_int: i64, my_o_int: Option<i64>) -> f64 {
        if let Some(my_o_int) = my_o_int {
            (my_int + my_o_int) as f64
        } else {
            my_int as f64
        }
    }

    #[test]
    fn ints_2float_test() {
        let myint0 = ArgType::Exp(Expression::alloc_data(ExpEnum::Int(7)));
        let myint1 = ArgType::VarArgs(vec![
            Expression::alloc_data(ExpEnum::Int(11)),
            Expression::alloc_data(ExpEnum::Int(2)),
        ]);
        let result = arg_translate_ints_2_float(myint0, myint1).unwrap();
        let res_d = &result.get().data;
        match res_d {
            ExpEnum::Float(f) => {
                assert_eq!(20.0, *f);
            }
            _ => {
                panic!("Not a float!")
            }
        }
    }

    fn arg_translate_ints_2_float(arg_0: ArgType, arg_1: ArgType) -> crate::LispResult<Expression> {
        let exp_0 = ret_err_exp_enum!(arg_0, ArgType::Exp(exp), exp, "err");
        let exp_1 = ret_err_exp_enum!(arg_1, ArgType::VarArgs(exp), exp, "err");
        arg_unwrap_ints_2_float(exp_0, exp_1).map(Into::into)
    }

    fn arg_unwrap_ints_2_float(
        exp_0: Expression,
        exp_1: Vec<Expression>,
    ) -> crate::LispResult<f64> {
        Ok({
            let float = ret_err_exp_enum!(
                exp_0.get().data,
                ExpEnum::Int(int_0),
                {
                    let iter = exp_1
                        .iter()
                        .map(|exp_1| {
                            let int = ret_err_exp_enum!(
                                exp_1.get().data,
                                ExpEnum::Int(int_1),
                                { int_1 },
                                "Not an int_1!"
                            );
                            Ok(int)
                        })
                        .collect::<crate::LispResult<Vec<i64>>>()?;
                    ints_2_float(int_0, iter)
                },
                "Not an int_0!"
            );
            float
        })
    }

    fn ints_2_float(my_int: i64, my_o_ints: Vec<i64>) -> f64 {
        my_o_ints
            .iter()
            .fold(my_int as f64, |sum, val| sum + *val as f64)
    }

    #[test]
    fn test_vecs() {
        let one_vec = vec![Arg {
            val: ArgVal::Vec,
            passing_style: ArgPassingStyle::MutReference,
        }];
        let args = vec![];
        let args = get_arg_types("foo", one_vec.clone(), args).unwrap();
        assert_eq!(1, args.len());
        try_exp_enum!(
            args.get(0).unwrap(),
            ArgType::VarArgs(val),
            assert!(val.is_empty()),
            "err"
        )
        .unwrap();

        let args = vec![Expression::make_true()];
        let args = get_arg_types("foo", one_vec.clone(), args).unwrap();
        assert_eq!(1, args.len());
        try_exp_enum!(
            args.get(0).unwrap(),
            ArgType::VarArgs(val),
            assert_eq!(1, val.len()),
            "err"
        )
        .unwrap();

        let args = vec![
            Expression::make_true(),
            Expression::make_true(),
            Expression::make_true(),
        ];
        let args = get_arg_types("foo", one_vec.clone(), args).unwrap();
        assert_eq!(1, args.len());
        try_exp_enum!(
            args.get(0).unwrap(),
            ArgType::VarArgs(val),
            assert_eq!(3, val.len()),
            "err"
        )
        .unwrap();

        let val_vec = vec![
            Arg {
                val: ArgVal::Value,
                passing_style: ArgPassingStyle::Reference,
            },
            Arg {
                val: ArgVal::Vec,
                passing_style: ArgPassingStyle::MutReference,
            },
        ];
        let args = vec![
            Expression::make_true(),
            Expression::make_true(),
            Expression::make_true(),
            Expression::make_true(),
        ];
        let args = get_arg_types("foo", val_vec.clone(), args).unwrap();
        assert_eq!(2, args.len());
        try_exp_enum!(
            args.get(1).unwrap(),
            ArgType::VarArgs(val),
            assert_eq!(3, val.len()),
            "err"
        )
        .unwrap();
    }

    #[test]
    fn test_vec_with_optionals() {
        let val_opt_and_vec = vec![
            Arg {
                val: ArgVal::Optional,
                passing_style: ArgPassingStyle::Reference,
            },
            Arg {
                val: ArgVal::Optional,
                passing_style: ArgPassingStyle::MutReference,
            },
            Arg {
                val: ArgVal::Vec,
                passing_style: ArgPassingStyle::Move,
            },
        ];

        // optional arguments manifest as Some
        let args = vec![Expression::make_true(), Expression::make_true()];
        let args = get_arg_types("foo", val_opt_and_vec.clone(), args).unwrap();
        assert_eq!(3, args.len());
        try_exp_enum!(
            args.get(0).unwrap(),
            ArgType::Opt(val),
            assert!(val.is_some()),
            "err"
        )
        .unwrap();
        try_exp_enum!(
            args.get(1).unwrap(),
            ArgType::Opt(val),
            assert!(val.is_some()),
            "err"
        )
        .unwrap();
        try_exp_enum!(
            args.get(2).unwrap(),
            ArgType::VarArgs(val),
            assert!(val.is_empty()),
            "err"
        )
        .unwrap();

        // optional arguments manifest as Some
        let args = vec![];
        let args = get_arg_types("foo", val_opt_and_vec, args).unwrap();
        assert_eq!(3, args.len());
        try_exp_enum!(
            args.get(0).unwrap(),
            ArgType::Opt(val),
            assert!(val.is_none()),
            "err"
        )
        .unwrap();
        try_exp_enum!(
            args.get(1).unwrap(),
            ArgType::Opt(val),
            assert!(val.is_none()),
            "err"
        )
        .unwrap();
        try_exp_enum!(
            args.get(2).unwrap(),
            ArgType::VarArgs(val),
            assert!(val.is_empty()),
            "err"
        )
        .unwrap();
    }

    //TODO need all LispError tests (unreachable!?)
    #[test]
    fn test_all_optional() {
        let two_opts = vec![
            Arg {
                val: ArgVal::Optional,
                passing_style: ArgPassingStyle::Move,
            },
            Arg {
                val: ArgVal::Optional,
                passing_style: ArgPassingStyle::Move,
            },
        ];

        // optional arguments manifest as Some
        let args = vec![Expression::make_true(), Expression::make_true()];
        let args = get_arg_types("foo", two_opts.clone(), args).unwrap();
        assert_eq!(2, args.len());
        try_exp_enum!(
            args.get(0).unwrap(),
            ArgType::Opt(val),
            assert!(val.is_some()),
            "err"
        )
        .unwrap();
        try_exp_enum!(
            args.get(1).unwrap(),
            ArgType::Opt(val),
            assert!(val.is_some()),
            "err"
        )
        .unwrap();

        // optional arguments manifest as Some
        let args = vec![];
        let args = get_arg_types("foo", two_opts, args).unwrap();
        assert_eq!(2, args.len());
        try_exp_enum!(
            args.get(0).unwrap(),
            ArgType::Opt(val),
            assert!(val.is_none()),
            "err"
        )
        .unwrap();
        try_exp_enum!(
            args.get(1).unwrap(),
            ArgType::Opt(val),
            assert!(val.is_none()),
            "err"
        )
        .unwrap();
    }

    #[test]
    fn test_optional() {
        let one_val_one_opt = vec![
            Arg {
                val: ArgVal::Value,
                passing_style: ArgPassingStyle::Move,
            },
            Arg {
                val: ArgVal::Optional,
                passing_style: ArgPassingStyle::Move,
            },
        ];

        // optional arguments manifest as Some
        let args = vec![Expression::make_true(), Expression::make_true()];
        let args = get_arg_types("foo", one_val_one_opt.clone(), args).unwrap();
        assert_eq!(2, args.len());
        try_exp_enum!(args.get(0).unwrap(), ArgType::Exp(_), {}, "err").unwrap();
        try_exp_enum!(
            args.get(1).unwrap(),
            ArgType::Opt(val),
            assert!(val.is_some()),
            "err"
        )
        .unwrap();

        // optional arguments are optional
        let args = vec![Expression::make_true()];
        let args = get_arg_types("foo", one_val_one_opt, args).unwrap();
        assert_eq!(2, args.len());
        try_exp_enum!(args.get(0).unwrap(), ArgType::Exp(_), {}, "err").unwrap();
        try_exp_enum!(
            args.get(1).unwrap(),
            ArgType::Opt(val),
            assert!(val.is_none()),
            "err"
        )
        .unwrap();
    }

    #[test]
    fn test_values() {
        let two_moved_values = vec![
            Arg {
                val: ArgVal::Value,
                passing_style: ArgPassingStyle::Move,
            },
            Arg {
                val: ArgVal::Value,
                passing_style: ArgPassingStyle::Move,
            },
        ];

        // if there is not enough arguments we throw an error.
        let args = vec![Expression::make_true()];
        let args = get_arg_types("foo", two_moved_values.clone(), args);
        assert!(args
            .unwrap_err()
            .reason
            .contains("not given enough arguments"));

        // if there are too many arguments we throw an error.
        let args = vec![
            Expression::make_true(),
            Expression::make_true(),
            Expression::make_true(),
        ];
        let args = get_arg_types("foo", two_moved_values.clone(), args);
        assert!(args.unwrap_err().reason.contains("given too many"));

        // the correct number of arguments succeeds.
        let args = vec![Expression::make_true(), Expression::make_true()];
        let args = get_arg_types("foo", two_moved_values, args).unwrap();
        assert_eq!(2, args.len());
        try_exp_enum!(args.get(0).unwrap(), ArgType::Exp(_), {}, "err").unwrap();
        try_exp_enum!(args.get(1).unwrap(), ArgType::Exp(_), {}, "err").unwrap();
    }

    #[test]
    fn test_parse_int_to_float() {
        let mut exps: Vec<Expression> = vec![];
        exps.push(8i64.into());
        exps.push(8i64.into());
        exps.push(8i64.into());
        exps.push(8i64.into());
        exps.push(8i64.into());
        let float = parse_int_to_float(exps).unwrap();
        let float: f64 = float.try_into().unwrap();
        assert_eq!(40.0, float);
        // in this macro, we are going to have to take lists of Expressions and match them to
        // ArgTypes of a generated function. Because the compiler checked, we know these
        // Expressions have to conform to a known spec. This means,
        //
        // 1. we need a concept of REQUIRED_ARGS_LEN, that way we know what our const param is
        // and can use call_expand_args.
    }

    fn parse_int_to_float(
        args: Vec<crate::types::Expression>,
    ) -> crate::LispResult<crate::types::Expression> {
        // full impl would actually have arguments:
        // ===
        //  fn parse_int_to_float(
        //      environment: &mut crate::environment::Environment,
        //      args: &mut dyn Iterator<Item = crate::types::Expression>,
        //  ) -> crate::LispResult<crate::types::Expression> {
        // ===
        // let args = crate::builtins_util::make_args(environment, args)?;
        let fn_name = "int->float";
        const REQUIRED_ARGS_LEN: usize = 2usize;
        let params = vec![
            Arg {
                val: ArgVal::Value,
                passing_style: ArgPassingStyle::Move,
            },
            Arg {
                val: ArgVal::Vec,
                passing_style: ArgPassingStyle::Reference,
            },
        ];
        let args = get_arg_types(fn_name, params, args)?;
        if args.len() >= REQUIRED_ARGS_LEN {
            match args.try_into() {
                Ok(params) => {
                    let params: [ArgType; REQUIRED_ARGS_LEN] = params;
                    println!("{:?}", params);
                    super_builtin_int_to_float.call_expand_args(params)
                }
                Err(_) => {
                    let err_msg = format!("{fn_name}: is broken and can't parse its arguments.");
                    Err(LispError::new(err_msg))
                }
            }
        } else {
            Err(LispError::new("Too few args."))
        }
    }

    fn super_builtin_int_to_float(
        exp_0: crate::ArgType,
        exp_1: crate::ArgType,
    ) -> crate::LispResult<crate::types::Expression> {
        try_exp_enum!(
            exp_0,
            ArgType::Exp(exp_0),
            {
                try_exp_enum!(
                    exp_1,
                    ArgType::VarArgs(exp_1),
                    //{ builtin_int_to_float(arg0, arg1).map(Into::into) }?,
                    {
                        try_exp_enum!(
                            exp_0.get().data,
                            ExpEnum::Int(exp_0),
                            {
                                let iter = exp_1
                                    .iter()
                                    .map(|exp_1| {
                                        let int = ret_err_exp_enum!(
                                            exp_1.get().data,
                                            ExpEnum::Int(int_1),
                                            { int_1 },
                                            "Not an int_1 in this vec!"
                                        );
                                        Ok(int)
                                    })
                                    .collect::<crate::LispResult<Vec<i64>>>()?;
                                int_to_float(exp_0, &iter).map(Into::into)
                            }?,
                            "err turning first arg to expenum::int"
                        )
                    }?,
                    "err turning exp to varargs"
                )
            }?,
            "err turning exp to exp"
        )
    }

    fn int_to_float(int: i64, ints: &Vec<i64>) -> LispResult<f64> {
        Ok(ints.iter().fold(int as f64, |sum, next| sum + *next as f64))
    }
    
    #[test]
    fn test_builtin_int_2_float() {
        let arg_0 = Expression::alloc_data(ExpEnum::Int(8));
        let arg_1 = Expression::alloc_data(ExpEnum::Int(8));
        let exp = sample_builtin_int_2_float(ArgType::Exp(arg_0), ArgType::Exp(arg_1)).unwrap();
        let exp_d = exp.get();
        match exp_d.data {
            ExpEnum::Float(f) => {
                assert_eq!(16.0, f);
            }
            _ => {
                panic!("Should befloat.");
            }
        }
    }

    fn sample_builtin_int_2_float(arg_0: ArgType, arg_1: ArgType) -> Result<Expression, LispError> {
        // Ok(Expression::make_false());
        match arg_1 { 
            ArgType::Exp (arg_1) => match arg_1.get().data {
                ExpEnum::Int(arg_1) => {
                    match arg_0 { 
                        ArgType::Exp(arg_0) => match arg_0.get().data {
                            ExpEnum::Int (arg_0) => {
                                int_2_float (arg_0 , arg_1).map(Into :: into)
                            },
                            _ => return Err (LispError :: new ("sl_sh_fn macro is broken, apparently ArgType::Exp can't be parsed as ArgType::Exp")) , 
                        },
                        _ => return Err (LispError :: new ("sl_sh_fn macro is broken, apparently ArgType::Exp can't be parsed as ArgType::Exp")) , 
                    }
                },
                _ => return Err (LispError :: new ("sl_sh_fn macro is broken, apparently ArgType::Exp can't be parsed as ArgType::Exp")), 
            },
            _ => return Err (LispError :: new ("sl_sh_fn macro is broken, apparently ArgType::Exp can't be parsed as ArgType::Exp")) , 
        }
    }
}
