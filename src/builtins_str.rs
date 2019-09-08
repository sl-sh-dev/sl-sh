use std::collections::HashMap;
use std::hash::BuildHasher;
use std::io;

use crate::shell::*;
use crate::types::*;

fn builtin_str_trim(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 1 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "str-trim takes one form",
        ));
    }
    let arg = eval(environment, &args[0], Expression::Atom(Atom::Nil), false)?
        .make_string(environment)?;
    Ok(Expression::Atom(Atom::String(arg.trim().to_string())))
}

fn builtin_str_ltrim(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 1 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "str-ltrim takes one form",
        ));
    }
    let arg = eval(environment, &args[0], Expression::Atom(Atom::Nil), false)?
        .make_string(environment)?;
    Ok(Expression::Atom(Atom::String(arg.trim_start().to_string())))
}

fn builtin_str_rtrim(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 1 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "str-rtrim takes one form",
        ));
    }
    let arg = eval(environment, &args[0], Expression::Atom(Atom::Nil), false)?
        .make_string(environment)?;
    Ok(Expression::Atom(Atom::String(arg.trim_end().to_string())))
}

pub fn add_str_builtins<S: BuildHasher>(data: &mut HashMap<String, Expression, S>) {
    data.insert("str-trim".to_string(), Expression::Func(builtin_str_trim));
    data.insert("str-ltrim".to_string(), Expression::Func(builtin_str_ltrim));
    data.insert("str-rtrim".to_string(), Expression::Func(builtin_str_rtrim));
}
