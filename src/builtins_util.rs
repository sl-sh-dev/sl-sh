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
macro_rules! try_exp_enum {
    ($expression:expr, $(|)? $( $pattern:pat_param )|+ $( if $guard: expr )? $(,)?, $eval:expr, $err:expr) => {
        match $expression {
            $( $pattern )|+ $( if $guard )? => Ok($eval),
            _ => Err(LispError::new($err))
        }
    }
}

#[macro_export]
macro_rules! try_inner_exp_enum {
    ($expression:expr, $(|)? $( $pattern:pat_param )|+ $( if $guard: expr )? $(,)?, $eval:expr, $err:expr) => {
        match $expression {
            $( $pattern )|+ $( if $guard )? => $eval,
            _ => return Err(LispError::new($err))
        }
    };
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

    #[derive(Debug, Clone)]
    pub enum ArgType {
        Exp(Expression),
        Opt(Option<Expression>),
        VarArgs(Vec<Expression>),
    }

    static K0: &'static str = "key0";
    static K1: &'static str = "key1";

    #[test]
    fn with_some_hashmaps_test() -> LispResult<()> {
        let mut hash_map = HashMap::new();
        hash_map.insert(K0, Expression::alloc_data(ExpEnum::Int(7)));
        hash_map.insert(K1, Expression::alloc_data(ExpEnum::Int(11)));
        let myint0 = ArgType::Exp(Expression::alloc_data(ExpEnum::HashMap(hash_map)));
        arg_translate_clear_hash_map(&myint0).unwrap();
        try_inner_exp_enum!(
            myint0,
            ArgType::Exp(exp),
            {
                try_inner_exp_enum!(
                    &exp.get().data,
                    ExpEnum::HashMap(hm),
                    assert!(hm.is_empty()),
                    "should be a hashmap"
                );
            },
            "err"
        );
        Ok(())
    }

    fn arg_translate_clear_hash_map(arg_0: &ArgType) -> crate::LispResult<()> {
        try_exp_enum!(
            arg_0,
            ArgType::Exp(exp),
            arg_unwrap_clear_hash_map(exp)?,
            "err"
        )
    }

    fn arg_unwrap_clear_hash_map(exp_0: &Expression) -> crate::LispResult<()> {
        try_exp_enum!(
            exp_0.get_mut().data,
            ExpEnum::HashMap(ref mut hash_map),
            clear_hash_map(hash_map)?,
            "Not an int_0!"
        )
    }

    fn clear_hash_map<K, V>(hash_map: &mut HashMap<K, V>) -> LispResult<()> {
        hash_map.clear();
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
            let float = try_inner_exp_enum!(
                exp_0.get().data,
                ExpEnum::Int(int_0),
                {
                    let float = try_inner_exp_enum!(
                        exp_1.get().data,
                        ExpEnum::Int(int_1),
                        { int_2_float(int_0, int_1) },
                        "Not an int_1!"
                    );
                    float
                },
                "Not an int_0!"
            );
            float
        })
    }

    fn int_2_float(my_int: i64, my_o_int: i64) -> f64 {
        (my_int + my_o_int) as f64
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
        let exp_0 = try_inner_exp_enum!(arg_0, ArgType::Exp(exp), exp, "err");
        let exp_1 = try_inner_exp_enum!(arg_1, ArgType::Opt(exp), exp, "err");
        arg_unwrap_optional_int_2_float(exp_0, exp_1).map(Into::into)
    }

    fn arg_unwrap_optional_int_2_float(
        exp_0: Expression,
        exp_1: Option<Expression>,
    ) -> crate::LispResult<f64> {
        Ok({
            let float = try_inner_exp_enum!(
                exp_0.get().data,
                ExpEnum::Int(int_0),
                {
                    match exp_1 {
                        None => optional_int_2_float(int_0, None),
                        Some(exp_1) => {
                            let float = try_inner_exp_enum!(
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
        let exp_0 = try_inner_exp_enum!(arg_0, ArgType::Exp(exp), exp, "err");
        let exp_1 = try_inner_exp_enum!(arg_1, ArgType::VarArgs(exp), exp, "err");
        arg_unwrap_ints_2_float(exp_0, exp_1).map(Into::into)
    }

    fn arg_unwrap_ints_2_float(
        exp_0: Expression,
        exp_1: Vec<Expression>,
    ) -> crate::LispResult<f64> {
        Ok({
            let float = try_inner_exp_enum!(
                exp_0.get().data,
                ExpEnum::Int(int_0),
                {
                    let iter = exp_1
                        .iter()
                        .map(|exp_1| {
                            let int = try_inner_exp_enum!(
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
}
