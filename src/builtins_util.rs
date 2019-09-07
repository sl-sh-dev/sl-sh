use std::collections::HashMap;
use std::io;
use std::process::Child;

use crate::shell::*;
use crate::types::*;

pub fn to_args(
    env: &mut Environment,
    parts: &[Expression],
    use_stdout: bool,
) -> io::Result<Vec<Expression>> {
    let mut args: Vec<Expression> = Vec::with_capacity(parts.len());
    for a in parts {
        args.push(eval(env, a, Expression::Atom(Atom::Nil), use_stdout)?);
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
            eval(environment, a, Expression::Atom(Atom::Nil), use_stdout)?
                .make_string(environment)?,
        );
    }
    Ok(args)
}

pub fn print(
    environment: &mut Environment,
    args: Vec<Expression>,
    add_newline: bool,
) -> io::Result<Expression> {
    for a in args {
        a.write(environment)?;
    }
    if add_newline {
        println!();
    }
    Ok(Expression::Atom(Atom::Nil))
}

pub fn parse_list_of_ints(
    environment: &mut Environment,
    args: &mut [Expression],
) -> io::Result<Vec<i64>> {
    let mut list: Vec<i64> = Vec::with_capacity(args.len());
    for arg in args {
        list.push(arg.make_int(environment)?);
    }
    Ok(list)
}

pub fn parse_list_of_floats(
    environment: &mut Environment,
    args: &mut [Expression],
) -> io::Result<Vec<f64>> {
    let mut list: Vec<f64> = Vec::with_capacity(args.len());
    for arg in args {
        list.push(arg.make_float(environment)?);
    }
    Ok(list)
}

pub fn parse_list_of_strings(
    environment: &mut Environment,
    args: &mut [Expression],
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

pub fn wait_process(environment: &Environment, pid: u32) -> io::Result<()> {
    let mut procs = environment.procs.borrow_mut();
    let mut found = false;
    if let Some(child) = procs.get_mut(&pid) {
        child.wait()?;
        found = true;
    }
    if found {
        procs.remove(&pid);
    }
    Ok(())
}

pub fn reap_procs(environment: &Environment) -> io::Result<()> {
    let mut procs = environment.procs.borrow_mut();
    let keys: Vec<u32> = procs.keys().copied().collect();
    let mut dead_pids: Vec<u32> = Vec::with_capacity(keys.len());
    for key in keys {
        if let Some(child) = procs.get_mut(&key) {
            if let Some(status) = child.try_wait()? {
                println!("Child {} ended with status {}", key, status);
                dead_pids.push(key);
            }
        }
    }
    for pid in dead_pids {
        procs.remove(&pid);
    }
    // XXX remove them or better replace pid with exit status
    Ok(())
}

pub fn build_new_scope_with_data<'a, S: ::std::hash::BuildHasher>(
    environment: &'a Environment<'a>,
    mut data_in: HashMap<String, Expression, S>,
) -> Environment<'a> {
    let mut data: HashMap<String, Expression> = HashMap::new();
    for (k, v) in data_in.drain() {
        data.insert(k, v);
    }
    Environment {
        data,
        procs: environment.procs.clone(),
        outer: Some(environment),
    }
}

pub fn build_new_scope<'a>(environment: &'a Environment<'a>) -> Environment<'a> {
    let data: HashMap<String, Expression> = HashMap::new();
    Environment {
        data,
        procs: environment.procs.clone(),
        outer: Some(environment),
    }
}
