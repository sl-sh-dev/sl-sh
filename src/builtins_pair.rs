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
                return Ok(Expression::Pair(Rc::new(RefCell::new(Some((arg0, arg1))))));
            }
        }
    }
    Err(io::Error::new(io::ErrorKind::Other, "join needs two forms"))
}

fn builtin_list(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    let mut head = Expression::nil();
    let mut last = head.clone();
    for a in args {
        let a = eval(environment, a)?;
        let pair = Expression::Pair(Rc::new(RefCell::new(Some((a, Expression::nil())))));
        if let Expression::Pair(p) = last {
            let p = &mut *p.borrow_mut();
            if let Some(p) = p {
                p.1 = pair.clone();
            }
        }
        last = pair;
        if head.is_nil() {
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
                Expression::Pair(p) => {
                    if let Some((e1, _e2)) = &*p.borrow() {
                        Ok(e1.clone())
                    } else {
                        // Nil
                        Ok(Expression::nil())
                    }
                }
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
                Expression::Pair(p) => {
                    if let Some((_e1, e2)) = &*p.borrow() {
                        Ok(e2.clone())
                    } else {
                        Ok(Expression::nil())
                    }
                }
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
                if let Expression::Pair(pair) = &pair {
                    let mut new_pair = None;
                    {
                        let pair = &mut *pair.borrow_mut();
                        match pair {
                            None => {
                                new_pair = Some((arg, Expression::nil()));
                            }
                            Some(pair) => {
                                pair.0 = arg;
                            }
                        }
                    }
                    if new_pair.is_some() {
                        pair.replace(new_pair);
                    }
                } else {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "xar! requires a pair for it's first form",
                    ));
                }
                return Ok(pair);
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "xar! takes two forms (pair and expression)",
    ))
}

// Destructive
fn builtin_xdr(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(pair) = args.next() {
        if let Some(arg) = args.next() {
            if args.next().is_none() {
                let arg = eval(environment, arg)?;
                let pair = eval(environment, pair)?;
                if let Expression::Pair(pair) = &pair {
                    let mut new_pair = None;
                    {
                        let pair = &mut *pair.borrow_mut();
                        match pair {
                            None => {
                                new_pair = Some((Expression::nil(), arg));
                            }
                            Some(pair) => {
                                pair.1 = arg;
                            }
                        }
                    }
                    if new_pair.is_some() {
                        pair.replace(new_pair);
                    }
                } else {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "xdr! requires a pair for it's first form",
                    ));
                }
                return Ok(pair);
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "xdr! takes two forms (pair and expression)",
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
