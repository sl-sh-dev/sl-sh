use crate::environment::*;
use crate::eval::*;
use crate::types::*;
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
