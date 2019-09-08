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

fn builtin_list_last(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 1 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "last takes one form (list)",
        ));
    }
    let arg = eval(environment, &args[0], Expression::Atom(Atom::Nil), false)?;
    match arg {
        Expression::List(mut list) => {
            if !list.is_empty() {
                Ok(list.pop().unwrap().clone())
            } else {
                Ok(Expression::Atom(Atom::Nil))
            }
        }
        _ => Err(io::Error::new(
            io::ErrorKind::Other,
            "last operates on a list",
        )),
    }
}

fn builtin_list_nth(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "nth takes two forms (int and list)",
        ));
    }
    let args = to_args(environment, &args, false)?;
    let idx = if let Expression::Atom(Atom::Int(i)) = args[0] {
        i
    } else {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "nth first form must be an int",
        ));
    };
    match &args[1] {
        Expression::List(list) => {
            if !list.is_empty() {
                if idx < 0 || idx >= list.len() as i64 {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "nth index out of range",
                    ));
                }
                Ok(list.get(idx as usize).unwrap().clone())
            } else {
                Ok(Expression::Atom(Atom::Nil))
            }
        }
        _ => Err(io::Error::new(
            io::ErrorKind::Other,
            "nth second form must be a list",
        )),
    }
}

fn builtin_list_setfirst(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "setfirst takes two forms (form and list)",
        ));
    }
    let mut args = to_args(environment, &args, false)?;
    let old_list = args.pop().unwrap();
    let new_car = args.pop().unwrap();
    match old_list {
        Expression::List(mut list) => {
            let mut nlist: Vec<Expression> = Vec::with_capacity(list.len() + 1);
            nlist.push(new_car);
            for a in list.drain(..) {
                nlist.push(a);
            }
            Ok(Expression::List(nlist))
        }
        _ => Err(io::Error::new(
            io::ErrorKind::Other,
            "setfirst second form must be a list",
        )),
    }
}

fn builtin_list_setrest(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "setrest takes two forms (lists)",
        ));
    }
    let mut args = to_args(environment, &args, false)?;
    let new_cdr = args.pop().unwrap();
    let old_list = args.pop().unwrap();
    if let Expression::List(mut new_cdr) = new_cdr {
        if let Expression::List(old_list) = old_list {
            let mut list: Vec<Expression> = Vec::with_capacity(new_cdr.len() + 1);
            if !old_list.is_empty() {
                list.push(old_list.get(0).unwrap().clone());
            }
            for a in new_cdr.drain(..) {
                list.push(a);
            }
            Ok(Expression::List(list))
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "setrest first form must be a list",
            ))
        }
    } else {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "setrest second form must be a list",
        ))
    }
}

pub fn add_list_builtins<S: BuildHasher>(data: &mut HashMap<String, Expression, S>) {
    data.insert("list".to_string(), Expression::Func(builtin_list));
    data.insert("first".to_string(), Expression::Func(builtin_list_first));
    data.insert("car".to_string(), Expression::Func(builtin_list_first));
    data.insert("rest".to_string(), Expression::Func(builtin_list_rest));
    data.insert("cdr".to_string(), Expression::Func(builtin_list_rest));
    data.insert("length".to_string(), Expression::Func(builtin_list_length));
    data.insert("last".to_string(), Expression::Func(builtin_list_last));
    data.insert("nth".to_string(), Expression::Func(builtin_list_nth));
    data.insert(
        "setfirst".to_string(),
        Expression::Func(builtin_list_setfirst),
    );
    data.insert(
        "setcar".to_string(),
        Expression::Func(builtin_list_setfirst),
    );
    data.insert(
        "setrest".to_string(),
        Expression::Func(builtin_list_setrest),
    );
    data.insert("setcdr".to_string(), Expression::Func(builtin_list_setrest));
}
