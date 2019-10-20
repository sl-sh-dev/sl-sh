use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::BuildHasher;
use std::io;
use std::iter::FromIterator;
use std::rc::Rc;

use crate::builtins_util::*;
use crate::environment::*;
use crate::types::*;

fn builtin_vec(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.is_empty() {
        return Ok(Expression::List(Rc::new(RefCell::new(Vec::new()))));
    }
    let args = to_args(environment, args)?;
    Ok(Expression::with_list(args))
}

fn builtin_make_vec(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() > 2 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "make-vec takes at most two forms",
        ));
    }
    if args.is_empty() {
        return Ok(Expression::List(Rc::new(RefCell::new(Vec::new()))));
    }
    let cap = if let Expression::Atom(Atom::Int(c)) = args[0] {
        c
    } else {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "make-vec first arg must be an integer",
        ));
    };
    let mut list = Vec::with_capacity(cap as usize);
    if args.len() == 2 {
        let v = &args[1];
        for _ in 0..cap {
            list.push(v.clone());
        }
    }

    Ok(Expression::with_list(list))
}

fn builtin_vec_vslice(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() != 2 && args.len() != 3 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "vslice takes two or three forms",
        ));
    }
    let start = if let Expression::Atom(Atom::Int(i)) = args[1] {
        i as usize
    } else {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "vslice second arg must be an integer",
        ));
    };
    let end = if args.len() == 3 {
        if let Expression::Atom(Atom::Int(i)) = args[2] {
            i as usize
        } else {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "vslice second arg must be an integer",
            ));
        }
    } else {
        0
    };
    match &args[0] {
        Expression::List(list) => {
            let list = list.borrow();
            if !list.is_empty() {
                let len = list.len();
                if start == len {
                    return Ok(Expression::Atom(Atom::Nil));
                }
                if start > (len - 1) || end > len {
                    let msg = format!(
                        "vslice index out of range (start  {}, end {}, length {})",
                        start, end, len
                    );
                    return Err(io::Error::new(io::ErrorKind::Other, msg));
                }
                let slice = if args.len() == 3 {
                    Vec::from_iter(list[start..end].iter().cloned())
                } else {
                    Vec::from_iter(list[start..].iter().cloned())
                };
                Ok(Expression::with_list(slice))
            } else {
                Ok(Expression::Atom(Atom::Nil))
            }
        }
        _ => Err(io::Error::new(
            io::ErrorKind::Other,
            "vslice operates on a vector",
        )),
    }
}

fn builtin_vec_nth(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "nth takes two forms (int and list)",
        ));
    }
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
            let list = list.borrow();
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

// Destructive
fn builtin_vec_setnth(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    let mut args = list_to_args(environment, args, true)?;
    if args.len() != 3 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "setnth takes three forms (index, new element and list)",
        ));
    }
    let old_list = args.pop().unwrap();
    let new_element = args.pop().unwrap();
    let idx = if let Expression::Atom(Atom::Int(i)) = args.pop().unwrap() {
        i
    } else {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "setnth first form must be an int",
        ));
    };
    match old_list {
        Expression::List(list) => {
            if idx < 0 || idx >= list.borrow().len() as i64 {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "setnth index out of range",
                ));
            }
            list.borrow_mut()[idx as usize] = new_element;
            Ok(Expression::List(list))
        }
        _ => Err(io::Error::new(
            io::ErrorKind::Other,
            "setnth third form must be a list",
        )),
    }
}

fn builtin_str_append(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    let mut args = list_to_args(environment, args, true)?;
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "append takes two forms (both lists or Strings)",
        ));
    }
    let end = args.pop().unwrap();
    let start = args.pop().unwrap();
    if let Expression::Atom(Atom::String(end)) = end {
        if let Expression::Atom(Atom::String(start)) = start {
            let mut new_string = String::with_capacity(start.len() + end.len());
            new_string.push_str(&start);
            new_string.push_str(&end);
            Ok(Expression::Atom(Atom::String(new_string)))
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "append forms must both be lists or Strings",
            ))
        }
    } else {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "append forms must both be lists or Strings",
        ))
    }
}

fn builtin_vec_append(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    let mut new_args = list_to_args(environment, args, true)?;
    if new_args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "append takes two forms (both lists or Strings)",
        ));
    }
    let end_arg = new_args.pop().unwrap();
    let start_arg = new_args.pop().unwrap();
    if let Expression::List(end) = end_arg {
        if let Expression::List(start) = start_arg {
            let end = end.borrow();
            let start = start.borrow();
            let mut list: Vec<Expression> = Vec::with_capacity(start.len() + end.len());
            list.extend(start.iter().cloned());
            list.extend(end.iter().cloned());
            Ok(Expression::with_list(list))
        } else if let Expression::Atom(Atom::Nil) = start_arg {
            Ok(Expression::List(end))
        } else {
            let msg = format!(
                "append first form ({}) must be a list or String and match the second form (List)",
                start_arg.display_type()
            );
            Err(io::Error::new(io::ErrorKind::Other, msg))
        }
    } else if let Expression::Atom(Atom::Nil) = end_arg {
        if let Expression::List(start) = start_arg {
            Ok(Expression::List(start))
        } else if let Expression::Atom(Atom::Nil) = start_arg {
            Ok(Expression::Atom(Atom::Nil))
        } else {
            let msg = format!(
                "append first form ({}) must be a list or String and match the second form ({})",
                start_arg.display_type(),
                end_arg.display_type()
            );
            Err(io::Error::new(io::ErrorKind::Other, msg))
        }
    } else {
        new_args.push(start_arg);
        new_args.push(end_arg);
        builtin_str_append(environment, &new_args)
    }
}

// Destructive
fn builtin_vec_push(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let mut args = list_to_args(environment, args, true)?;
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "push takes two forms (list and form)",
        ));
    }
    let new_item = args.pop().unwrap();
    let old_list = args.pop().unwrap();
    match old_list {
        Expression::List(list) => {
            list.borrow_mut().push(new_item);
            Ok(Expression::List(list))
        }
        _ => Err(io::Error::new(
            io::ErrorKind::Other,
            "push's first form must be a list",
        )),
    }
}

// Destructive
fn builtin_vec_pop(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() != 1 {
        return Err(io::Error::new(io::ErrorKind::Other, "pop takes a list"));
    }
    let old_list = &args[0];
    match old_list {
        Expression::List(list) => {
            if let Some(item) = list.borrow_mut().pop() {
                Ok(item)
            } else {
                Ok(Expression::Atom(Atom::Nil))
            }
        }
        _ => Err(io::Error::new(
            io::ErrorKind::Other,
            "pop's first form must be a list",
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
            "is-empty takes a list",
        ));
    }
    let list = &args[0];
    match list {
        Expression::List(list) => {
            if list.borrow().is_empty() {
                Ok(Expression::Atom(Atom::True))
            } else {
                Ok(Expression::Atom(Atom::Nil))
            }
        }
        _ => Err(io::Error::new(
            io::ErrorKind::Other,
            "is-empty's first form must be a list",
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
        return Err(io::Error::new(io::ErrorKind::Other, "clear takes a list"));
    }
    let list = &args[0];
    match list {
        Expression::List(list) => {
            list.borrow_mut().clear();
            Ok(Expression::Atom(Atom::Nil))
        }
        _ => Err(io::Error::new(
            io::ErrorKind::Other,
            "clear's first form must be a list",
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
            "remove-nth takes two forms (index and list)",
        ));
    }
    let list = args.pop().unwrap();
    let idx = if let Expression::Atom(Atom::Int(i)) = args.pop().unwrap() {
        i
    } else {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "remove-nth first form must be an int",
        ));
    };
    match list {
        Expression::List(list) => {
            if idx < 0 || idx >= list.borrow().len() as i64 {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "remove-nth index out of range",
                ));
            }
            list.borrow_mut().remove(idx as usize);
            Ok(Expression::List(list))
        }
        _ => Err(io::Error::new(
            io::ErrorKind::Other,
            "remove-nth second form must be a list",
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
            "insert-nth takes three forms (index, new element and list)",
        ));
    }
    let old_list = args.pop().unwrap();
    let new_element = args.pop().unwrap();
    let idx = if let Expression::Atom(Atom::Int(i)) = args.pop().unwrap() {
        i
    } else {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "insert-nth first form must be an int",
        ));
    };
    match old_list {
        Expression::List(list) => {
            if idx < 0 || idx > list.borrow().len() as i64 {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "insert-nth index out of range",
                ));
            }
            list.borrow_mut().insert(idx as usize, new_element);
            Ok(Expression::List(list))
        }
        _ => Err(io::Error::new(
            io::ErrorKind::Other,
            "insert-nth third form must be a list",
        )),
    }
}

pub fn add_vec_builtins<S: BuildHasher>(data: &mut HashMap<String, Rc<Expression>, S>) {
    data.insert("vec".to_string(), Rc::new(Expression::Func(builtin_vec)));
    data.insert(
        "make-vec".to_string(),
        Rc::new(Expression::Func(builtin_make_vec)),
    );
    data.insert(
        "vslice".to_string(),
        Rc::new(Expression::Func(builtin_vec_vslice)),
    );
    data.insert(
        "vnth".to_string(),
        Rc::new(Expression::Func(builtin_vec_nth)),
    );
    data.insert(
        "vsetnth!".to_string(),
        Rc::new(Expression::Func(builtin_vec_setnth)),
    );
    data.insert(
        "append".to_string(),
        Rc::new(Expression::Func(builtin_vec_append)),
    );
    data.insert(
        "push!".to_string(),
        Rc::new(Expression::Func(builtin_vec_push)),
    );
    data.insert(
        "pop!".to_string(),
        Rc::new(Expression::Func(builtin_vec_pop)),
    );
    data.insert(
        "is-empty".to_string(),
        Rc::new(Expression::Func(builtin_vec_is_empty)),
    );
    data.insert(
        "vclear!".to_string(),
        Rc::new(Expression::Func(builtin_vec_vclear)),
    );
    data.insert(
        "vremove-nth!".to_string(),
        Rc::new(Expression::Func(builtin_vec_remove_nth)),
    );
    data.insert(
        "vinsert-nth!".to_string(),
        Rc::new(Expression::Func(builtin_vec_insert_nth)),
    );
}
