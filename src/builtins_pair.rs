use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::BuildHasher;
use std::io;
use std::rc::Rc;

use crate::environment::*;
use crate::eval::*;
use crate::types::*;

fn builtin_join(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg0) = args.next() {
        if let Some(arg1) = args.next() {
            if args.next().is_none() {
                let arg0 = eval(environment, arg0)?;
                let arg1 = eval(environment, arg1)?;
                return Ok(Expression::Pair(
                    Rc::new(RefCell::new(arg0)),
                    Rc::new(RefCell::new(arg1)),
                ));
            }
        }
    }
    Err(io::Error::new(io::ErrorKind::Other, "join needs two forms"))
}

fn builtin_list(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    let mut head = Expression::Atom(Atom::Nil);
    let mut last = head.clone();
    for a in args {
        let a = eval(environment, a)?;
        let pair = Expression::Pair(
            Rc::new(RefCell::new(a)),
            Rc::new(RefCell::new(Expression::Atom(Atom::Nil))),
        );
        if let Expression::Pair(_e1, e2) = last {
            e2.replace(pair.clone());
        }
        last = pair;
        if let Expression::Atom(Atom::Nil) = head {
            head = last.clone();
        }
    }
    Ok(head)
}

fn builtin_car(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return match &arg {
                Expression::Pair(e1, _e2) => Ok(e1.borrow().clone()),
                Expression::Atom(Atom::Nil) => Ok(Expression::Atom(Atom::Nil)),
                _ => Err(io::Error::new(io::ErrorKind::Other, "car requires a pair")),
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "car takes one form (pair)",
    ))
}

fn builtin_cdr(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return match &arg {
                Expression::Pair(_e1, e2) => Ok(e2.borrow().clone()),
                Expression::Atom(Atom::Nil) => Ok(Expression::Atom(Atom::Nil)),
                _ => Err(io::Error::new(io::ErrorKind::Other, "cdr requires a pair")),
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "cdr takes one form (pair)",
    ))
}

// Destructive
fn builtin_xar(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(pair) = args.next() {
        if let Some(arg) = args.next() {
            if args.next().is_none() {
                let arg = eval(environment, arg)?;
                let pair = eval(environment, pair)?;
                match &pair {
                    Expression::Pair(e1, _e2) => {
                        e1.replace(arg);
                    }
                    _ => {
                        return Err(io::Error::new(
                            io::ErrorKind::Other,
                            "xar requires a pair for it's first form",
                        ))
                    }
                }
                return Ok(pair);
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "xar takes two forms (pair and expression)",
    ))
}

// Destructive
fn builtin_xdr(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    //let pair = args.next();
    //let arg = args.next();
    if let Some(pair) = args.next() {
        if let Some(arg) = args.next() {
            if args.next().is_none() {
                let arg = eval(environment, arg)?;
                let pair = eval(environment, pair)?;
                match &pair {
                    Expression::Pair(_e1, e2) => {
                        e2.replace(arg);
                    }
                    _ => {
                        return Err(io::Error::new(
                            io::ErrorKind::Other,
                            "xdr requires a pair for it's first form",
                        ))
                    }
                }
                return Ok(pair);
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "xdr takes two forms (pair and expression)",
    ))
}

pub fn add_pair_builtins<S: BuildHasher>(data: &mut HashMap<String, Rc<Expression>, S>) {
    data.insert(
        "join".to_string(),
        Rc::new(Expression::make_function(builtin_join, "")),
    );
    data.insert(
        "list".to_string(),
        Rc::new(Expression::make_function(builtin_list, "")),
    );
    data.insert(
        "car".to_string(),
        Rc::new(Expression::make_function(builtin_car, "")),
    );
    data.insert(
        "cdr".to_string(),
        Rc::new(Expression::make_function(builtin_cdr, "")),
    );
    data.insert(
        "xar!".to_string(),
        Rc::new(Expression::make_function(builtin_xar, "")),
    );
    data.insert(
        "xdr!".to_string(),
        Rc::new(Expression::make_function(builtin_xdr, "")),
    );
}
