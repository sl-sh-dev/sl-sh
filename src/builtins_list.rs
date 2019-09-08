use std::collections::HashMap;
use std::hash::BuildHasher;
use std::io;

use crate::builtins_util::*;
use crate::shell::*;
use crate::types::*;

fn builtin_list(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.is_empty() {
        return Ok(Expression::Atom(Atom::Nil));
    }
    let args = to_args(environment, args, false)?;
    let mut list: Vec<Expression> = Vec::with_capacity(args.len());
    for arg in args {
        list.push(arg);
    }
    Ok(Expression::List(list))
}

fn builtin_list_first(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    if args.len() != 1 {
        return Err(io::Error::new(io::ErrorKind::Other, "first takes one form"));
    }
    let arg = eval(environment, &args[0], Expression::Atom(Atom::Nil), false)?;
    match arg {
        Expression::List(list) => {
            if !list.is_empty() {
                Ok(list.get(0).unwrap().clone())
            } else {
                Ok(Expression::Atom(Atom::Nil))
            }
        }
        _ => Err(io::Error::new(
            io::ErrorKind::Other,
            "first operates on a list",
        )),
    }
}

fn builtin_list_rest(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 1 {
        return Err(io::Error::new(io::ErrorKind::Other, "rest takes one form"));
    }
    let arg = eval(environment, &args[0], Expression::Atom(Atom::Nil), false)?;
    match arg {
        Expression::List(mut list) => {
            if list.len() > 1 {
                let mut rest: Vec<Expression> = Vec::with_capacity(list.len() - 1);
                for a in list.drain(1..) {
                    rest.push(a);
                }
                Ok(Expression::List(rest))
            } else {
                Ok(Expression::Atom(Atom::Nil))
            }
        }
        _ => Err(io::Error::new(
            io::ErrorKind::Other,
            "rest operates on a list",
        )),
    }
}

fn builtin_list_length(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    if args.len() != 1 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "length takes one form",
        ));
    }
    let arg = eval(environment, &args[0], Expression::Atom(Atom::Nil), false)?;
    match arg {
        Expression::Atom(Atom::Nil) => Ok(Expression::Atom(Atom::Int(0))),
        Expression::Atom(_) => Ok(Expression::Atom(Atom::Int(1))),
        Expression::List(list) => Ok(Expression::Atom(Atom::Int(list.len() as i64))),
        _ => Ok(Expression::Atom(Atom::Int(0))),
    }
}

pub fn add_list_builtins<S: BuildHasher>(data: &mut HashMap<String, Expression, S>) {
    data.insert("list".to_string(), Expression::Func(builtin_list));
    data.insert("first".to_string(), Expression::Func(builtin_list_first));
    data.insert("rest".to_string(), Expression::Func(builtin_list_rest));
    data.insert("length".to_string(), Expression::Func(builtin_list_length));
}
