use std::collections::HashMap;
use std::hash::BuildHasher;
use std::io;
use std::rc::Rc;

use crate::builtins_util::*;
use crate::environment::*;
use crate::shell::*;
use crate::types::*;

fn builtin_str_trim(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 1 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "str-trim takes one form",
        ));
    }
    let arg = eval(environment, &args[0])?.make_string(environment)?;
    Ok(Expression::Atom(Atom::String(arg.trim().to_string())))
}

fn builtin_str_ltrim(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 1 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "str-ltrim takes one form",
        ));
    }
    let arg = eval(environment, &args[0])?.make_string(environment)?;
    Ok(Expression::Atom(Atom::String(arg.trim_start().to_string())))
}

fn builtin_str_rtrim(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 1 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "str-rtrim takes one form",
        ));
    }
    let arg = eval(environment, &args[0])?.make_string(environment)?;
    Ok(Expression::Atom(Atom::String(arg.trim_end().to_string())))
}

fn builtin_str_replace(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    if args.len() != 3 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "str-replace takes three forms",
        ));
    }
    let args = to_args_str(environment, args)?;
    let new_str = args[0].replace(&args[1], &args[2]);
    Ok(Expression::Atom(Atom::String(new_str)))
}

fn builtin_str_split(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "str-split takes two forms",
        ));
    }
    let args = to_args_str(environment, args)?;
    let mut split_list: Vec<Expression> = Vec::new();
    for s in args[1].split(&args[0]) {
        split_list.push(Expression::Atom(Atom::String(s.to_string())));
    }
    Ok(Expression::with_list(split_list))
}

fn builtin_str_cat_list(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "str-cat-list takes two forms",
        ));
    }
    let args = to_args(environment, args)?;
    let join_str = args[0].make_string(environment)?;
    let mut new_str = String::new();
    if let Expression::List(list) = &args[1] {
        let mut first = true;
        for s in list.borrow().iter() {
            if !first {
                new_str.push_str(&join_str);
            }
            new_str.push_str(&s.make_string(environment)?);
            first = false;
        }
    } else {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "str-cat-list second form must be a list",
        ));
    }
    Ok(Expression::Atom(Atom::String(new_str)))
}

fn builtin_str_sub(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 3 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "str-sub takes three forms (int, int String)",
        ));
    }
    let start = if let Expression::Atom(Atom::Int(i)) = args[0] {
        i as usize
    } else {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "str-sub first form must be an int",
        ));
    };
    let len = if let Expression::Atom(Atom::Int(i)) = args[1] {
        i as usize
    } else {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "str-sub second form must be an int",
        ));
    };
    let arg3 = eval(environment, &args[2])?;
    if let Expression::Atom(Atom::String(s)) = &arg3 {
        if (start + len) <= s.len() {
            Ok(Expression::Atom(Atom::String(
                s.as_str()[start..(start + len)].to_string(),
            )))
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "str-sub index out of range",
            ))
        }
    } else {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "str-sub third form must be an String",
        ))
    }
}

pub fn add_str_builtins<S: BuildHasher>(data: &mut HashMap<String, Rc<Expression>, S>) {
    data.insert(
        "str-trim".to_string(),
        Rc::new(Expression::Func(builtin_str_trim)),
    );
    data.insert(
        "str-ltrim".to_string(),
        Rc::new(Expression::Func(builtin_str_ltrim)),
    );
    data.insert(
        "str-rtrim".to_string(),
        Rc::new(Expression::Func(builtin_str_rtrim)),
    );
    data.insert(
        "str-replace".to_string(),
        Rc::new(Expression::Func(builtin_str_replace)),
    );
    data.insert(
        "str-split".to_string(),
        Rc::new(Expression::Func(builtin_str_split)),
    );
    data.insert(
        "str-cat-list".to_string(),
        Rc::new(Expression::Func(builtin_str_cat_list)),
    );
    data.insert(
        "str-sub".to_string(),
        Rc::new(Expression::Func(builtin_str_sub)),
    );
}
