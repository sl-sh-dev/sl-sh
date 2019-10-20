use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::BuildHasher;
use std::io;
use std::rc::Rc;

use crate::builtins_util::*;
use crate::environment::*;
use crate::types::*;

fn builtin_join(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() != 2 {
        Err(io::Error::new(io::ErrorKind::Other, "join needs two forms"))
    } else {
        Ok(Expression::Pair(
            Rc::new(RefCell::new(args[0].clone())),
            Rc::new(RefCell::new(args[1].clone())),
        ))
    }
}

fn builtin_list(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.is_empty() {
        return Ok(Expression::Atom(Atom::Nil));
    }
    let head = Expression::Pair(
        Rc::new(RefCell::new(Expression::Atom(Atom::Nil))),
        Rc::new(RefCell::new(Expression::Atom(Atom::Nil))),
    );
    let mut current = head.clone();
    let mut args = list_to_args(environment, args, true)?;
    let elements = args.len() - 1;
    for (i, a) in args.drain(..).enumerate() {
        if let Expression::Pair(e1, e2) = current.clone() {
            e1.replace(a);
            if i == elements {
                e2.replace(Expression::Atom(Atom::Nil));
            } else {
                current = Expression::Pair(
                    Rc::new(RefCell::new(Expression::Atom(Atom::Nil))),
                    Rc::new(RefCell::new(Expression::Atom(Atom::Nil))),
                );
                e2.replace(current.clone());
            }
        }
    }
    Ok(head)
}

fn builtin_car(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() != 1 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "car takes one form (pair)",
        ))
    } else {
        match &args[0] {
            Expression::Pair(e1, _e2) => Ok(e1.borrow().clone()),
            Expression::Atom(Atom::Nil) => Ok(Expression::Atom(Atom::Nil)),
            _ => Err(io::Error::new(io::ErrorKind::Other, "car requires a pair")),
        }
    }
}

fn builtin_cdr(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() != 1 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "cdr takes one form (pair)",
        ))
    } else {
        match &args[0] {
            Expression::Pair(_e1, e2) => Ok(e2.borrow().clone()),
            Expression::Atom(Atom::Nil) => Ok(Expression::Atom(Atom::Nil)),
            _ => Err(io::Error::new(io::ErrorKind::Other, "cdr requires a pair")),
        }
    }
}

// Destructive
fn builtin_xar(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let mut args = list_to_args(environment, args, true)?;
    if args.len() != 2 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "xar takes two forms (pair and expression)",
        ))
    } else {
        let arg1 = args.pop().unwrap();
        let pair = args.pop().unwrap();
        match &pair {
            Expression::Pair(e1, _e2) => {
                e1.replace(arg1);
            }
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "xar requires a pair for it's first form",
                ))
            }
        }
        Ok(pair)
    }
}

// Destructive
fn builtin_xdr(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let mut args = list_to_args(environment, args, true)?;
    if args.len() != 2 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "xdr takes two forms (pair and expression)",
        ))
    } else {
        let arg1 = args.pop().unwrap();
        let pair = args.pop().unwrap();
        match &pair {
            Expression::Pair(_e1, e2) => {
                e2.replace(arg1);
            }
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "xdr requires a pair for it's first form",
                ))
            }
        }
        Ok(pair)
    }
}

pub fn add_pair_builtins<S: BuildHasher>(data: &mut HashMap<String, Rc<Expression>, S>) {
    data.insert("join".to_string(), Rc::new(Expression::Func(builtin_join)));
    data.insert("list".to_string(), Rc::new(Expression::Func(builtin_list)));
    data.insert("car".to_string(), Rc::new(Expression::Func(builtin_car)));
    data.insert("cdr".to_string(), Rc::new(Expression::Func(builtin_cdr)));
    data.insert("xar!".to_string(), Rc::new(Expression::Func(builtin_xar)));
    data.insert("xdr!".to_string(), Rc::new(Expression::Func(builtin_xdr)));
}
