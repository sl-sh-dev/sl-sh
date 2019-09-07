use std::collections::HashMap;
use std::env;
use std::fs;
use std::hash::BuildHasher;
use std::io;
use std::path::Path;

use crate::builtins_util::*;
use crate::script::*;
use crate::shell::*;
use crate::types::*;

fn builtin_cd(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
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
        Ok(Expression::Atom(Atom::False))
    } else {
        Ok(Expression::Atom(Atom::True))
    }
}

//fn builtin_eval(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {}

fn builtin_load(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let mut args: Vec<Expression> = to_args(environment, args, false)?;
    if args.len() != 1 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "load needs one argument",
        ))
    } else {
        let contents = fs::read_to_string(args.pop().unwrap().make_string(environment)?)?;
        let tokens = tokenize(&contents);
        let ast = parse(&tokens);
        match ast {
            Ok(ast) => {
                let ast = match ast {
                    Expression::List(list) => {
                        if let Some(first) = list.get(0) {
                            match first {
                                Expression::List(_) => {
                                    let mut v = Vec::with_capacity(list.len() + 1);
                                    v.push(Expression::Atom(Atom::Symbol("progn".to_string())));
                                    for l in list {
                                        v.push(l);
                                    }
                                    Expression::List(v)
                                }
                                _ => Expression::List(list),
                            }
                        } else {
                            Expression::List(list)
                        }
                    }
                    _ => ast,
                };
                eval(environment, &ast, Expression::Atom(Atom::Nil), false)
            }
            Err(err) => Err(io::Error::new(io::ErrorKind::Other, err.reason)),
        }
    }
}

fn builtin_if(environment: &mut Environment, parts: &[Expression]) -> io::Result<Expression> {
    let plen = parts.len();
    if plen != 2 && plen != 3 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "if needs exactly two or three expressions",
        ))
    } else {
        let mut parts = parts.iter();
        match eval(
            environment,
            parts.next().unwrap(),
            Expression::Atom(Atom::Nil),
            false,
        )? {
            Expression::Atom(Atom::True) => eval(
                environment,
                parts.next().unwrap(),
                Expression::Atom(Atom::Nil),
                false,
            ),
            Expression::Atom(Atom::False) => {
                if plen == 3 {
                    parts.next().unwrap();
                    eval(
                        environment,
                        parts.next().unwrap(),
                        Expression::Atom(Atom::Nil),
                        false,
                    )
                } else {
                    Ok(Expression::Atom(Atom::Nil))
                }
            }
            _ => Err(io::Error::new(
                io::ErrorKind::Other,
                "if must evaluate to true or false",
            )),
        }
    }
}

fn builtin_print(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args: Vec<Expression> = to_args(environment, args, false)?;
    print(environment, args, false)
}

fn builtin_println(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args: Vec<Expression> = to_args(environment, args, false)?;
    print(environment, args, true)
}

fn builtin_format(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args = to_args_str(environment, args, false)?;
    let mut res = String::new();
    for a in args {
        res.push_str(&a);
    }
    Ok(Expression::Atom(Atom::String(res)))
}

fn builtin_use_stdout(
    environment: &mut Environment,
    parts: &[Expression],
) -> io::Result<Expression> {
    for a in parts {
        eval(environment, a, Expression::Atom(Atom::Nil), true)?;
    }
    Ok(Expression::Atom(Atom::Nil))
}

fn builtin_progn(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let mut args = to_args(environment, args, false)?;
    match args.pop() {
        Some(a) => Ok(a),
        None => Ok(Expression::Atom(Atom::Nil)),
    }
}

fn builtin_export(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let mut args: Vec<Expression> = to_args(environment, &args, false)?;
    if args.len() != 2 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "export can only have two expressions",
        ))
    } else {
        let mut args = args.iter_mut();
        let key = args.next().unwrap().make_string(environment)?;
        let val = args.next().unwrap().make_string(environment)?;
        if !val.is_empty() {
            env::set_var(key, val);
        } else {
            env::remove_var(key);
        }
        Ok(Expression::Atom(Atom::Nil))
    }
}

fn builtin_set(environment: &mut Environment, parts: &[Expression]) -> io::Result<Expression> {
    if parts.len() != 2 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "set can only have two expressions",
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
        let val = eval(
            environment,
            parts.next().unwrap(),
            Expression::Atom(Atom::Nil),
            false,
        )?;
        if let Expression::Atom(atom) = val {
            environment.data.insert(key, Expression::Atom(atom));
        } else {
            environment.data.insert(
                key,
                Expression::Atom(Atom::String(val.make_string(environment)?)),
            );
        }
        Ok(Expression::Atom(Atom::Nil))
    }
}

fn builtin_fn(_environment: &mut Environment, parts: &[Expression]) -> io::Result<Expression> {
    if parts.len() != 2 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "fn can only have two forms",
        ))
    } else {
        let mut parts = parts.iter();
        let params = parts.next().unwrap();
        let body = parts.next().unwrap();
        Ok(Expression::Atom(Atom::Lambda(Lambda {
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
            Ok(Expression::Atom(Atom::True))
        } else {
            Ok(Expression::Atom(Atom::False))
        }
    }};
}

macro_rules! ensure_tonicity_all {
    ($check_fn:expr) => {{
        |environment: &mut Environment, args: &[Expression]| -> io::Result<Expression> {
            let mut args: Vec<Expression> = to_args(environment, args, false)?;
            if let Ok(ints) = parse_list_of_ints(environment, &mut args) {
                ensure_tonicity!($check_fn, ints, &i64, i64)
            } else if let Ok(floats) = parse_list_of_floats(environment, &mut args) {
                ensure_tonicity!($check_fn, floats, &f64, f64)
            } else {
                let strings = parse_list_of_strings(environment, &mut args)?;
                ensure_tonicity!($check_fn, strings, &str, String)
            }
        }
    }};
}

pub fn add_builtins<S: BuildHasher>(data: &mut HashMap<String, Expression, S>) {
    data.insert("cd".to_string(), Expression::Func(builtin_cd));
    data.insert("load".to_string(), Expression::Func(builtin_load));
    data.insert("if".to_string(), Expression::Func(builtin_if));
    data.insert("print".to_string(), Expression::Func(builtin_print));
    data.insert("println".to_string(), Expression::Func(builtin_println));
    data.insert("format".to_string(), Expression::Func(builtin_format));
    data.insert(
        "use-stdout".to_string(),
        Expression::Func(builtin_use_stdout),
    );
    data.insert("progn".to_string(), Expression::Func(builtin_progn));
    data.insert("export".to_string(), Expression::Func(builtin_export));
    data.insert("set".to_string(), Expression::Func(builtin_set));
    data.insert("fn".to_string(), Expression::Func(builtin_fn));

    data.insert(
        "=".to_string(),
        Expression::Func(
            |environment: &mut Environment, args: &[Expression]| -> io::Result<Expression> {
                let mut args: Vec<Expression> = to_args(environment, args, false)?;
                if let Ok(ints) = parse_list_of_ints(environment, &mut args) {
                    ensure_tonicity!(|a, b| a == b, ints, &i64, i64)
                } else if let Ok(floats) = parse_list_of_floats(environment, &mut args) {
                    ensure_tonicity!(|a, b| ((a - b) as f64).abs() < 0.000_001, floats, &f64, f64)
                } else {
                    let strings = parse_list_of_strings(environment, &mut args)?;
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
