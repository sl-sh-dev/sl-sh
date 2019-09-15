use std::collections::HashMap;
use std::fs::File;
use std::hash::BuildHasher;
use std::io;

use crate::builtins::*;
use crate::environment::*;
use crate::shell::*;
use crate::types::*;

fn builtin_err_null(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    environment.err_null = true;
    let res = builtin_progn(environment, args);
    environment.err_null = false;
    res
}

fn builtin_file_rdr(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() < 2 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "file_rdr must have at least two forms",
        ))
    } else {
        let arg0 = eval(environment, &args[0], Expression::Atom(Atom::Nil), false)?;
        let res = builtin_progn(environment, &args[1..])?;
        if let Expression::Atom(Atom::String(s)) = &arg0 {
            let mut writer = File::create(s)?;
            res.writef(environment, &mut writer)?;
        }
        Ok(res)
    }
}

fn builtin_stdout_to(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() < 2 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "stdout-to must have at least two forms",
        ))
    } else {
        let arg0 = eval(environment, &args[0], Expression::Atom(Atom::Nil), false)?;
        if let Expression::Atom(Atom::String(s)) = &arg0 {
            environment.state.borrow_mut().stdout_file = Some(s.clone());
        }
        let res = builtin_progn(environment, &args[1..])?;
        environment.state.borrow_mut().stdout_file = None;
        Ok(res)
    }
}

fn builtin_stderr_to(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() < 2 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "stderr-to must have at least two forms",
        ))
    } else {
        let arg0 = eval(environment, &args[0], Expression::Atom(Atom::Nil), false)?;
        if let Expression::Atom(Atom::String(s)) = &arg0 {
            environment.state.borrow_mut().stderr_file = Some(s.clone());
        }
        let res = builtin_progn(environment, &args[1..])?;
        environment.state.borrow_mut().stderr_file = None;
        Ok(res)
    }
}

pub fn add_file_builtins<S: BuildHasher>(data: &mut HashMap<String, Expression, S>) {
    data.insert("err-null".to_string(), Expression::Func(builtin_err_null));
    data.insert("file-rdr".to_string(), Expression::Func(builtin_file_rdr));
    data.insert("stdout-to".to_string(), Expression::Func(builtin_stdout_to));
    data.insert("stderr-to".to_string(), Expression::Func(builtin_stderr_to));
}
