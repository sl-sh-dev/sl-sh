use std::collections::HashMap;
use std::io;
use std::process::Child;

use crate::shell::*;
use crate::types::*;

pub fn to_args(
    env: &mut Environment,
    parts: &[Expression],
    use_stdout: bool,
) -> io::Result<Vec<EvalResult>> {
    let mut args: Vec<EvalResult> = Vec::with_capacity(parts.len());
    for a in parts {
        args.push(eval(env, a, EvalResult::Atom(Atom::Nil), use_stdout)?);
    }
    Ok(args)
}

pub fn to_args_str(
    environment: &mut Environment,
    parts: &[Expression],
    use_stdout: bool,
) -> io::Result<Vec<String>> {
    let mut args: Vec<String> = Vec::with_capacity(parts.len());
    for a in parts {
        args.push(
            eval(environment, a, EvalResult::Atom(Atom::Nil), use_stdout)?
                .make_string(environment)?,
        );
    }
    Ok(args)
}

pub fn print(
    environment: &mut Environment,
    args: Vec<EvalResult>,
    add_newline: bool,
) -> io::Result<EvalResult> {
    for a in args {
        a.write(environment)?;
    }
    if add_newline {
        println!();
    }
    Ok(EvalResult::Atom(Atom::Nil))
}

pub fn parse_list_of_ints(
    environment: &mut Environment,
    args: &mut [EvalResult],
) -> io::Result<Vec<i64>> {
    let mut list: Vec<i64> = Vec::with_capacity(args.len());
    for arg in args {
        list.push(arg.make_int(environment)?);
    }
    Ok(list)
}

pub fn parse_list_of_floats(
    environment: &mut Environment,
    args: &mut [EvalResult],
) -> io::Result<Vec<f64>> {
    let mut list: Vec<f64> = Vec::with_capacity(args.len());
    for arg in args {
        list.push(arg.make_float(environment)?);
    }
    Ok(list)
}

pub fn parse_list_of_strings(
    environment: &mut Environment,
    args: &mut [EvalResult],
) -> io::Result<Vec<String>> {
    let mut list: Vec<String> = Vec::with_capacity(args.len());
    for arg in args {
        list.push(arg.make_string(environment)?);
    }
    Ok(list)
}

pub fn get_expression(environment: &Environment, key: &str) -> Option<Expression> {
    match environment.data.get(key) {
        Some(exp) => Some(exp.clone()),
        None => match environment.outer {
            Some(outer) => get_expression(outer, key),
            None => None,
        },
    }
}

pub fn add_process(environment: &Environment, process: Child) -> u32 {
    let pid = process.id();
    environment.procs.borrow_mut().insert(pid, process);
    pid
}

pub fn build_new_scope<'a>(environment: &'a Environment<'a>) -> Environment<'a> {
    let data: HashMap<String, Expression> = HashMap::new();
    Environment {
        data,
        procs: environment.procs.clone(),
        outer: Some(environment),
    }
}
