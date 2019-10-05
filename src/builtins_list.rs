use std::collections::HashMap;
use std::hash::BuildHasher;
use std::io;
use std::rc::Rc;

use crate::builtins_util::*;
use crate::environment::*;
use crate::shell::*;
use crate::types::*;

fn builtin_list(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.is_empty() {
        return Ok(Expression::Atom(Atom::Nil));
    }
    let args = to_args(environment, args)?;
    let mut list: Vec<Expression> = Vec::with_capacity(args.len());
    for arg in args {
        list.push(arg);
    }
    Ok(Expression::with_list(list))
}

fn builtin_list_first(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    if args.len() != 1 {
        return Err(io::Error::new(io::ErrorKind::Other, "first takes one form"));
    }
    let arg = eval(environment, &args[0])?;
    match arg {
        Expression::List(list) => {
            if !list.borrow().is_empty() {
                Ok(list.borrow().get(0).unwrap().clone())
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
    let arg = eval(environment, &args[0])?;
    match arg {
        Expression::List(list) => {
            if list.borrow().len() > 1 {
                let mut rest: Vec<Expression> = Vec::with_capacity(list.borrow().len() - 1);
                for a in &list.borrow()[1..] {
                    rest.push(a.clone());
                }
                Ok(Expression::with_list(rest))
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
    let arg = eval(environment, &args[0])?;
    match arg {
        Expression::Atom(Atom::Nil) => Ok(Expression::Atom(Atom::Int(0))),
        Expression::Atom(Atom::String(s)) => Ok(Expression::Atom(Atom::Int(s.len() as i64))),
        Expression::Atom(_) => Ok(Expression::Atom(Atom::Int(1))),
        Expression::List(list) => Ok(Expression::Atom(Atom::Int(list.borrow().len() as i64))),
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
    let arg = eval(environment, &args[0])?;
    match arg {
        Expression::List(list) => {
            if !list.borrow().is_empty() {
                Ok(list.borrow().last().unwrap().clone())
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

fn builtin_list_butlast(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    if args.len() != 1 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "butlast takes one form",
        ));
    }
    let arg = eval(environment, &args[0])?;
    match arg {
        Expression::List(list) => {
            let list = list.borrow().clone();
            if list.len() > 1 {
                let mut new_list = Vec::with_capacity(list.len() - 1);
                for a in &list[..(list.len() - 1)] {
                    new_list.push(a.clone());
                }
                Ok(Expression::with_list(new_list))
            } else {
                Ok(Expression::Atom(Atom::Nil))
            }
        }
        _ => Err(io::Error::new(
            io::ErrorKind::Other,
            "butlast operates on a list",
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
    let args = to_args(environment, &args)?;
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
    let mut args = to_args(environment, &args)?;
    let old_list = args.pop().unwrap();
    let new_car = args.pop().unwrap();
    match old_list {
        Expression::List(list) => {
            let mut nlist: Vec<Expression> = Vec::with_capacity(list.borrow().len() + 1);
            nlist.push(new_car);
            for a in list.borrow().iter() {
                nlist.push(a.clone());
            }
            Ok(Expression::with_list(nlist))
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
    let mut args = to_args(environment, &args)?;
    let new_cdr = args.pop().unwrap();
    let old_list = args.pop().unwrap();
    if let Expression::List(new_cdr) = new_cdr {
        if let Expression::List(old_list) = old_list {
            let mut list: Vec<Expression> = Vec::with_capacity(new_cdr.borrow().len() + 1);
            if !old_list.borrow().is_empty() {
                list.push(old_list.borrow().get(0).unwrap().clone());
            }
            for a in new_cdr.borrow().iter() {
                list.push(a.clone());
            }
            Ok(Expression::with_list(list))
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

fn builtin_list_setlast(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "setlast takes two forms (list and form)",
        ));
    }
    let mut args = to_args(environment, &args)?;
    let new_last = args.pop().unwrap();
    let old_list = args.pop().unwrap();
    match old_list {
        Expression::List(list) => {
            let mut new_list = Vec::with_capacity(list.borrow().len() + 1);
            for a in list.borrow().iter() {
                new_list.push(a.clone());
            }
            new_list.push(new_last);
            Ok(Expression::with_list(new_list))
        }
        _ => Err(io::Error::new(
            io::ErrorKind::Other,
            "setlast first form must be a list",
        )),
    }
}

fn builtin_list_setbutlast(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "setbutlast takes two forms (lists)",
        ));
    }
    let mut args = to_args(environment, &args)?;
    let old_list = args.pop().unwrap();
    let new_butlast = args.pop().unwrap();
    if let Expression::List(new_butlast) = new_butlast {
        if let Expression::List(old_list) = old_list {
            let mut list: Vec<Expression> = Vec::with_capacity(new_butlast.borrow().len() + 1);
            for a in new_butlast.borrow().iter() {
                list.push(a.clone());
            }
            if !old_list.borrow().is_empty() {
                list.push(old_list.borrow().last().unwrap().clone());
            }
            Ok(Expression::with_list(list))
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "setbutlast second form must be a list",
            ))
        }
    } else {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "setbutlast first form must be a list",
        ))
    }
}

// Destructive
fn builtin_list_setnth(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    if args.len() != 3 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "setnth takes three forms (index, new element and list)",
        ));
    }
    let mut args = to_args(environment, &args)?;
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
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "append takes two forms (both lists or Strings)",
        ));
    }
    let mut args = to_args(environment, &args)?;
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

fn builtin_list_append(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "append takes two forms (both lists or Strings)",
        ));
    }
    let mut new_args = to_args(environment, &args)?;
    let end_arg = new_args.pop().unwrap();
    let start_arg = new_args.pop().unwrap();
    if let Expression::List(end) = end_arg {
        if let Expression::List(start) = start_arg {
            let mut list: Vec<Expression> =
                Vec::with_capacity(start.borrow().len() + end.borrow().len());
            for a in start.borrow().iter() {
                list.push(a.clone());
            }
            for a in end.borrow().iter() {
                list.push(a.clone());
            }
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
fn builtin_list_push(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "push takes two forms (list and form)",
        ));
    }
    let mut args = to_args(environment, &args)?;
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
fn builtin_list_pop(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 1 {
        return Err(io::Error::new(io::ErrorKind::Other, "pop takes a list"));
    }
    let old_list = eval(environment, &args[0])?;
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

fn builtin_list_is_empty(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    if args.len() != 1 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "is-empty takes a list",
        ));
    }
    let list = eval(environment, &args[0])?;
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
fn builtin_list_clear(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    if args.len() != 1 {
        return Err(io::Error::new(io::ErrorKind::Other, "clear takes a list"));
    }
    let list = eval(environment, &args[0])?;
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
fn builtin_list_remove_nth(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "remove-nth takes two forms (index and list)",
        ));
    }
    let mut args = to_args(environment, &args)?;
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
fn builtin_list_insert_nth(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    if args.len() != 3 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "insert-nth takes three forms (index, new element and list)",
        ));
    }
    let mut args = to_args(environment, &args)?;
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

pub fn add_list_builtins<S: BuildHasher>(data: &mut HashMap<String, Rc<Expression>, S>) {
    data.insert("list".to_string(), Rc::new(Expression::Func(builtin_list)));
    data.insert(
        "first".to_string(),
        Rc::new(Expression::Func(builtin_list_first)),
    );
    data.insert(
        "rest".to_string(),
        Rc::new(Expression::Func(builtin_list_rest)),
    );
    data.insert(
        "length".to_string(),
        Rc::new(Expression::Func(builtin_list_length)),
    );
    data.insert(
        "last".to_string(),
        Rc::new(Expression::Func(builtin_list_last)),
    );
    data.insert(
        "butlast".to_string(),
        Rc::new(Expression::Func(builtin_list_butlast)),
    );
    data.insert(
        "nth".to_string(),
        Rc::new(Expression::Func(builtin_list_nth)),
    );
    data.insert(
        "setfirst".to_string(),
        Rc::new(Expression::Func(builtin_list_setfirst)),
    );
    data.insert(
        "setrest".to_string(),
        Rc::new(Expression::Func(builtin_list_setrest)),
    );
    data.insert(
        "setlast".to_string(),
        Rc::new(Expression::Func(builtin_list_setlast)),
    );
    data.insert(
        "setbutlast".to_string(),
        Rc::new(Expression::Func(builtin_list_setbutlast)),
    );
    data.insert(
        "setnth".to_string(),
        Rc::new(Expression::Func(builtin_list_setnth)),
    );
    data.insert(
        "append".to_string(),
        Rc::new(Expression::Func(builtin_list_append)),
    );
    data.insert(
        "push".to_string(),
        Rc::new(Expression::Func(builtin_list_push)),
    );
    data.insert(
        "pop".to_string(),
        Rc::new(Expression::Func(builtin_list_pop)),
    );
    data.insert(
        "is-empty".to_string(),
        Rc::new(Expression::Func(builtin_list_is_empty)),
    );
    data.insert(
        "clear".to_string(),
        Rc::new(Expression::Func(builtin_list_clear)),
    );
    data.insert(
        "remove-nth".to_string(),
        Rc::new(Expression::Func(builtin_list_remove_nth)),
    );
    data.insert(
        "insert-nth".to_string(),
        Rc::new(Expression::Func(builtin_list_insert_nth)),
    );
}
