use std::collections::HashMap;
use std::env;
use std::hash::BuildHasher;
use std::io;
use std::path::Path;

use crate::builtins_util::*;
use crate::shell::*;
use crate::types::*;

fn builtin_cd(environment: &mut Environment, args: &[Expression]) -> io::Result<EvalResult> {
    let home = match env::var("HOME") {
        Ok(val) => val,
        Err(_) => "/".to_string(),
    };
    let args = to_args_str(environment, args, false)?;
    let args = args.iter();
    let new_dir = args.peekable().peek().map_or(&home[..], |x| *x);
    let root = Path::new(new_dir);
    if let Err(e) = env::set_current_dir(&root) {
        eprintln!("{}", e);
    }

    Ok(EvalResult::Empty)
}

fn builtin_if(environment: &mut Environment, parts: &[Expression]) -> io::Result<EvalResult> {
    let plen = parts.len();
    if plen != 2 && plen != 3 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "if needs exactly two or three expressions",
        ))
    } else {
        let mut parts = parts.iter();
        match eval(environment, parts.next().unwrap(), EvalResult::Empty, false)? {
            EvalResult::Atom(Atom::True) => {
                eval(environment, parts.next().unwrap(), EvalResult::Empty, false)
            }
            EvalResult::Atom(Atom::False) => {
                if plen == 3 {
                    parts.next().unwrap();
                    eval(environment, parts.next().unwrap(), EvalResult::Empty, false)
                } else {
                    Ok(EvalResult::Empty)
                }
            }
            _ => Err(io::Error::new(
                io::ErrorKind::Other,
                "if must evaluate to true or false",
            )),
        }
    }
}

fn builtin_print(environment: &mut Environment, args: &[Expression]) -> io::Result<EvalResult> {
    let args: Vec<EvalResult> = to_args(environment, args, false)?;
    print(args, false)
}

fn builtin_println(environment: &mut Environment, args: &[Expression]) -> io::Result<EvalResult> {
    let args: Vec<EvalResult> = to_args(environment, args, false)?;
    print(args, true)
}

fn builtin_format(environment: &mut Environment, args: &[Expression]) -> io::Result<EvalResult> {
    let args = to_args_str(environment, args, false)?;
    let mut res = String::new();
    for a in args {
        res.push_str(&a);
    }
    Ok(EvalResult::Atom(Atom::String(res)))
}

fn builtin_use_stdout(
    environment: &mut Environment,
    parts: &[Expression],
) -> io::Result<EvalResult> {
    for a in parts {
        eval(environment, a, EvalResult::Empty, true)?;
    }
    Ok(EvalResult::Empty)
}

fn builtin_export(environment: &mut Environment, args: &[Expression]) -> io::Result<EvalResult> {
    let mut args: Vec<EvalResult> = to_args(environment, &args, false)?;
    if args.len() != 2 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "export can only have two expressions",
        ))
    } else {
        let mut args = args.iter_mut();
        let key = args.next().unwrap().make_string()?;
        let val = args.next().unwrap().make_string()?;
        if !val.is_empty() {
            env::set_var(key, val);
        } else {
            env::remove_var(key);
        }
        Ok(EvalResult::Empty)
    }
}

fn builtin_let(environment: &mut Environment, parts: &[Expression]) -> io::Result<EvalResult> {
    if parts.len() != 2 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "let can only have two expressions",
        ))
    } else {
        let mut parts = parts.iter();
        let key = parts.next().unwrap();
        let key = match key {
            Expression::Atom(Atom::Symbol(s)) => s.clone(),
            _ => {
                return Err(io::Error::new(io::ErrorKind::Other, "invalid lvalue"));
            }
        };
        if key.starts_with('$') {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "use export to set environment variables",
            ));
        }
        let mut val = eval(environment, parts.next().unwrap(), EvalResult::Empty, false)?;
        if let EvalResult::Atom(atom) = val {
            environment.data.insert(key, Expression::Atom(atom));
        } else {
            environment
                .data
                .insert(key, Expression::Atom(Atom::String(val.make_string()?)));
        }
        Ok(EvalResult::Empty)
    }
}

fn builtin_fn(_environment: &mut Environment, parts: &[Expression]) -> io::Result<EvalResult> {
    if parts.len() != 2 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "fn can only have two forms",
        ))
    } else {
        let mut parts = parts.iter();
        let params = parts.next().unwrap();
        let body = parts.next().unwrap();
        Ok(EvalResult::Atom(Atom::Lambda(Lambda {
            params: Box::new(params.clone()),
            body: Box::new(body.clone()),
        })))
    }
}

macro_rules! ensure_tonicity {
    ($check_fn:expr, $values:expr, $type:ty, $type_two:ty) => {{
        let first = $values.first().ok_or(io::Error::new(
            io::ErrorKind::Other,
            "expected at least one value",
        ))?;
        let rest = &$values[1..];
        fn f(prev: $type, xs: &[$type_two]) -> bool {
            match xs.first() {
                Some(x) => $check_fn(prev, x) && f(x, &xs[1..]),
                None => true,
            }
        };
        if f(first, rest) {
            Ok(EvalResult::Atom(Atom::True))
        } else {
            Ok(EvalResult::Atom(Atom::False))
        }
    }};
}

macro_rules! ensure_tonicity_all {
    ($check_fn:expr) => {{
        |environment: &mut Environment, args: &[Expression]| -> io::Result<EvalResult> {
            let mut args: Vec<EvalResult> = to_args(environment, args, false)?;
            if let Ok(ints) = parse_list_of_ints(&mut args) {
                ensure_tonicity!($check_fn, ints, &i64, i64)
            } else if let Ok(floats) = parse_list_of_floats(&mut args) {
                ensure_tonicity!($check_fn, floats, &f64, f64)
            } else {
                let strings = parse_list_of_strings(&mut args)?;
                ensure_tonicity!($check_fn, strings, &str, String)
            }
        }
    }};
}

pub fn add_builtins<S: BuildHasher>(data: &mut HashMap<String, Expression, S>) {
    data.insert("cd".to_string(), Expression::Func(builtin_cd));
    data.insert("if".to_string(), Expression::Func(builtin_if));
    data.insert("print".to_string(), Expression::Func(builtin_print));
    data.insert("println".to_string(), Expression::Func(builtin_println));
    data.insert("format".to_string(), Expression::Func(builtin_format));
    data.insert(
        "use-stdout".to_string(),
        Expression::Func(builtin_use_stdout),
    );
    data.insert("export".to_string(), Expression::Func(builtin_export));
    data.insert("let".to_string(), Expression::Func(builtin_let));
    data.insert("fn".to_string(), Expression::Func(builtin_fn));

    data.insert(
        "=".to_string(),
        Expression::Func(
            |environment: &mut Environment, args: &[Expression]| -> io::Result<EvalResult> {
                let mut args: Vec<EvalResult> = to_args(environment, args, false)?;
                if let Ok(ints) = parse_list_of_ints(&mut args) {
                    ensure_tonicity!(|a, b| a == b, ints, &i64, i64)
                } else if let Ok(floats) = parse_list_of_floats(&mut args) {
                    ensure_tonicity!(|a, b| ((a - b) as f64).abs() < 0.000_001, floats, &f64, f64)
                } else {
                    let strings = parse_list_of_strings(&mut args)?;
                    ensure_tonicity!(|a, b| a == b, strings, &str, String)
                }
            },
        ),
    );
    data.insert(
        ">".to_string(),
        Expression::Func(ensure_tonicity_all!(|a, b| a > b)),
    );
    data.insert(
        ">=".to_string(),
        Expression::Func(ensure_tonicity_all!(|a, b| a >= b)),
    );
    data.insert(
        "<".to_string(),
        Expression::Func(ensure_tonicity_all!(|a, b| a < b)),
    );
    data.insert(
        "<=".to_string(),
        Expression::Func(ensure_tonicity_all!(|a, b| a <= b)),
    );
}
