use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::BuildHasher;
use std::io;
use std::iter::FromIterator;
use std::rc::Rc;

use crate::builtins_util::*;
use crate::environment::*;
use crate::eval::*;
use crate::types::*;

fn builtin_vec(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    let mut new_args: Vec<Expression> = Vec::new();
    for a in args {
        new_args.push(eval(environment, a)?);
    }
    Ok(Expression::with_list(new_args))
}

fn builtin_make_vec(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    let list = if let Some(cap) = args.next() {
        let cap = eval(environment, cap)?;
        let cap = if let Expression::Atom(Atom::Int(c)) = cap {
            c
        } else {
            let msg = format!("make-vec first arg must be an integer, found {:?}", cap);
            return Err(io::Error::new(io::ErrorKind::Other, msg));
        };
        let mut list = Vec::with_capacity(cap as usize);
        if let Some(item) = args.next() {
            if args.next().is_some() {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "make-vec takes at most two forms",
                ));
            }
            let item = eval(environment, item)?;
            for _ in 0..cap {
                list.push(item.clone());
            }
        }
        list
    } else {
        return Ok(Expression::Vector(Rc::new(RefCell::new(Vec::new()))));
    };
    Ok(Expression::with_list(list))
}

fn builtin_vec_slice(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    let (vec, start, end, has_end) = if let Some(vec) = args.next() {
        if let Some(start) = args.next() {
            let start = if let Expression::Atom(Atom::Int(i)) = eval(environment, start)? {
                i as usize
            } else {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "vec-slice second arg must be an integer",
                ));
            };
            if let Some(end) = args.next() {
                let end = if let Expression::Atom(Atom::Int(i)) = eval(environment, end)? {
                    i as usize
                } else {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "vec-slice third arg must be an integer",
                    ));
                };
                (eval(environment, vec)?, start, end, true)
            } else {
                (eval(environment, vec)?, start, 0, false)
            }
        } else {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "vec-slice takes two or three forms",
            ));
        }
    } else {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "vec-slice takes two or three forms",
        ));
    };
    match &vec {
        Expression::Vector(list) => {
            let list = list.borrow();
            if !list.is_empty() {
                let len = list.len();
                if start == len {
                    return Ok(Expression::nil());
                }
                if start > (len - 1) || end > len {
                    let msg = format!(
                        "vec-slice index out of range (start  {}, end {}, length {})",
                        start, end, len
                    );
                    return Err(io::Error::new(io::ErrorKind::Other, msg));
                }
                let slice = if has_end {
                    Vec::from_iter(list[start..end].iter().cloned())
                } else {
                    Vec::from_iter(list[start..].iter().cloned())
                };
                Ok(Expression::with_list(slice))
            } else {
                Ok(Expression::nil())
            }
        }
        _ => Err(io::Error::new(
            io::ErrorKind::Other,
            "vec-slice operates on a vector",
        )),
    }
}

fn builtin_vec_nth(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(idx) = args.next() {
        if let Some(list) = args.next() {
            if args.next().is_none() {
                if let Expression::Atom(Atom::Int(idx)) = eval(environment, &idx)? {
                    if let Expression::Vector(list) = eval(environment, &list)? {
                        let list = list.borrow();
                        if idx < 0 || idx >= list.len() as i64 {
                            return Err(io::Error::new(
                                io::ErrorKind::Other,
                                "vec-nth index out of range",
                            ));
                        }
                        return Ok(list.get(idx as usize).unwrap().clone());
                    }
                }
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "vec-nth takes two forms (int and list)",
    ))
}

// Destructive
fn builtin_vec_setnth(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    let mut args = list_to_args(environment, args, true)?;
    if args.len() != 3 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "vec-setnth! takes three forms (index, new element and list)",
        ));
    }
    let old_list = args.pop().unwrap();
    let new_element = args.pop().unwrap();
    let idx = if let Expression::Atom(Atom::Int(i)) = args.pop().unwrap() {
        i
    } else {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "vec-setnth! first form must be an int",
        ));
    };
    match old_list {
        Expression::Vector(list) => {
            if idx < 0 || idx >= list.borrow().len() as i64 {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "vec-setnth! index out of range",
                ));
            }
            list.borrow_mut()[idx as usize] = new_element;
            Ok(Expression::Vector(list))
        }
        _ => Err(io::Error::new(
            io::ErrorKind::Other,
            "vec-setnth! third form must be a list",
        )),
    }
}

// Destructive
fn builtin_vec_push(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let mut args = list_to_args(environment, args, true)?;
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "vec-push! takes two forms (list and form)",
        ));
    }
    let new_item = args.pop().unwrap();
    let old_list = args.pop().unwrap();
    match old_list {
        Expression::Vector(list) => {
            list.borrow_mut().push(new_item);
            Ok(Expression::Vector(list))
        }
        _ => Err(io::Error::new(
            io::ErrorKind::Other,
            "vec-push!'s first form must be a list",
        )),
    }
}

// Destructive
fn builtin_vec_pop(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() != 1 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "vec-pop! takes a list",
        ));
    }
    let old_list = &args[0];
    match old_list {
        Expression::Vector(list) => {
            if let Some(item) = list.borrow_mut().pop() {
                Ok(item)
            } else {
                Ok(Expression::nil())
            }
        }
        _ => Err(io::Error::new(
            io::ErrorKind::Other,
            "vec-pop!'s first form must be a list",
        )),
    }
}

fn builtin_vec_is_empty(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() != 1 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "vec-empty? takes a list",
        ));
    }
    let list = &args[0];
    match list {
        Expression::Vector(list) => {
            if list.borrow().is_empty() {
                Ok(Expression::Atom(Atom::True))
            } else {
                Ok(Expression::nil())
            }
        }
        _ => Err(io::Error::new(
            io::ErrorKind::Other,
            "vec-empty?'s first form must be a list",
        )),
    }
}

// Destructive
fn builtin_vec_vclear(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() != 1 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "vec-clear! takes a vector",
        ));
    }
    let list = &args[0];
    match list {
        Expression::Vector(list) => {
            list.borrow_mut().clear();
            Ok(Expression::nil())
        }
        _ => Err(io::Error::new(
            io::ErrorKind::Other,
            "vec-clear!'s first form must be a list",
        )),
    }
}

// Destructive
fn builtin_vec_remove_nth(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    let mut args = list_to_args(environment, args, true)?;
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "vec-remove-nth! takes two forms (index and list)",
        ));
    }
    let list = args.pop().unwrap();
    let idx = if let Expression::Atom(Atom::Int(i)) = args.pop().unwrap() {
        i
    } else {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "vec-remove-nth! first form must be an int",
        ));
    };
    match list {
        Expression::Vector(list) => {
            if idx < 0 || idx >= list.borrow().len() as i64 {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "vec-remove-nth! index out of range",
                ));
            }
            list.borrow_mut().remove(idx as usize);
            Ok(Expression::Vector(list))
        }
        _ => Err(io::Error::new(
            io::ErrorKind::Other,
            "vec-remove-nth! second form must be a list",
        )),
    }
}

// Destructive
fn builtin_vec_insert_nth(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    let mut args = list_to_args(environment, args, true)?;
    if args.len() != 3 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "vec-insert-nth! takes three forms (index, new element and list)",
        ));
    }
    let old_list = args.pop().unwrap();
    let new_element = args.pop().unwrap();
    let idx = if let Expression::Atom(Atom::Int(i)) = args.pop().unwrap() {
        i
    } else {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "vec-insert-nth! first form must be an int",
        ));
    };
    match old_list {
        Expression::Vector(list) => {
            if idx < 0 || idx > list.borrow().len() as i64 {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "vec-insert-nth! index out of range",
                ));
            }
            list.borrow_mut().insert(idx as usize, new_element);
            Ok(Expression::Vector(list))
        }
        _ => Err(io::Error::new(
            io::ErrorKind::Other,
            "vec-insert-nth! third form must be a list",
        )),
    }
}

pub fn add_vec_builtins<S: BuildHasher>(data: &mut HashMap<String, Rc<Reference>, S>) {
    data.insert(
        "vec".to_string(),
        Rc::new(Expression::make_function(
            builtin_vec,
            "Usage: (vec item1 item2 .. itemN)\n\nMake a new vector with items.",
        )),
    );
    data.insert(
        "make-vec".to_string(),
        Rc::new(Expression::make_function(
            builtin_make_vec,
            "Usage: (make-vec capacity default)\n\nMake a new vector with capacity and default item(s).",
        )),
    );
    data.insert(
        "vec-slice".to_string(),
        Rc::new(Expression::make_function(
            builtin_vec_slice,
            "Usage: (vec-slice vector start end?)\n\nReturns a slice of a vector (0 based indexes).",
        )),
    );
    data.insert(
        "vec-nth".to_string(),
        Rc::new(Expression::make_function(
            builtin_vec_nth,
            "Usage: (vec-nth index vector)\n\nGet the nth element (0 based) of a vector.",
        )),
    );
    data.insert(
        "vec-setnth!".to_string(),
        Rc::new(Reference {
            exp: Expression::Func(builtin_vec_setnth),
            meta: RefMetaData {
                namespace: Some("root".to_string()),
                doc_string: None,
            },
        }),
    );
    data.insert(
        "vec-push!".to_string(),
        Rc::new(Reference {
            exp: Expression::Func(builtin_vec_push),
            meta: RefMetaData {
                namespace: Some("root".to_string()),
                doc_string: None,
            },
        }),
    );
    data.insert(
        "vec-pop!".to_string(),
        Rc::new(Reference {
            exp: Expression::Func(builtin_vec_pop),
            meta: RefMetaData {
                namespace: Some("root".to_string()),
                doc_string: None,
            },
        }),
    );
    data.insert(
        "vec-empty?".to_string(),
        Rc::new(Reference {
            exp: Expression::Func(builtin_vec_is_empty),
            meta: RefMetaData {
                namespace: Some("root".to_string()),
                doc_string: None,
            },
        }),
    );
    data.insert(
        "vec-clear!".to_string(),
        Rc::new(Reference {
            exp: Expression::Func(builtin_vec_vclear),
            meta: RefMetaData {
                namespace: Some("root".to_string()),
                doc_string: None,
            },
        }),
    );
    data.insert(
        "vec-remove-nth!".to_string(),
        Rc::new(Reference {
            exp: Expression::Func(builtin_vec_remove_nth),
            meta: RefMetaData {
                namespace: Some("root".to_string()),
                doc_string: None,
            },
        }),
    );
    data.insert(
        "vec-insert-nth!".to_string(),
        Rc::new(Reference {
            exp: Expression::Func(builtin_vec_insert_nth),
            meta: RefMetaData {
                namespace: Some("root".to_string()),
                doc_string: None,
            },
        }),
    );
}
