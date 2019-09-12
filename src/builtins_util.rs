use std::collections::HashMap;
use std::env;
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

pub fn is_expression(environment: &Environment, key: &str) -> bool {
    if key.starts_with('$') {
        env::var(&key[1..]).is_ok()
    } else {
        match environment.data.get(key) {
            Some(_) => true,
            None => match environment.outer {
                Some(outer) => is_expression(outer, key),
                None => false,
            },
        }
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
            if let Some(_status) = child.try_wait()? {
                // XXX turn off for now but should get waits in the proper places and bring back.
                //println!("Child {} ended with status {}", key, status);
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
        err_null: environment.err_null,
        in_pipe: environment.in_pipe,
        data,
        procs: environment.procs.clone(),
        outer: Some(environment),
    }
}

pub fn build_new_scope<'a>(environment: &'a Environment<'a>) -> Environment<'a> {
    let data: HashMap<String, Expression> = HashMap::new();
    Environment {
        err_null: environment.err_null,
        in_pipe: environment.in_pipe,
        data,
        procs: environment.procs.clone(),
        outer: Some(environment),
    }
}

pub fn clone_symbols<S: ::std::hash::BuildHasher>(
    environment: &Environment,
    data_in: &mut HashMap<String, Expression, S>,
) {
    for (k, v) in &environment.data {
        data_in.insert(k.clone(), v.clone());
    }
    if let Some(outer) = environment.outer {
        clone_symbols(outer, data_in);
    }
}

fn setup_args_final(
    environment: &mut Environment,
    mut var_names: Vec<String>,
    vars: &[Expression],
    min_params: usize,
    use_rest: bool,
) -> io::Result<()> {
    if vars.len() < min_params || (!use_rest && vars.len() > min_params) {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "wrong number of parameters",
        ));
    } else if use_rest {
        let rest_name = var_names.pop().unwrap();
        for (k, v) in var_names.iter().zip(vars.iter()) {
            environment.data.insert(k.clone(), v.clone());
        }
        if vars.len() > min_params {
            let mut rest_data: Vec<Expression> = Vec::with_capacity(vars.len() - min_params);
            for v in &vars[min_params..] {
                rest_data.push(v.clone());
            }
            environment
                .data
                .insert(rest_name.clone(), Expression::List(rest_data));
        } else {
            environment
                .data
                .insert(rest_name.clone(), Expression::Atom(Atom::Nil));
        }
    } else {
        for (k, v) in var_names.iter().zip(vars.iter()) {
            environment.data.insert(k.clone(), v.clone());
        }
    }
    Ok(())
}

pub fn setup_args(
    environment: &mut Environment,
    params: &Expression,
    args: &[Expression],
    eval_args: bool,
) -> io::Result<()> {
    if let Expression::List(l) = params {
        let mut var_names: Vec<String> = Vec::with_capacity(l.len());
        let mut use_rest = false;
        let mut post_rest_cnt = 0;
        let mut min_params = 0;
        for arg in l {
            if let Expression::Atom(Atom::Symbol(s)) = arg {
                match &s[..] {
                    "&rest" => {
                        if use_rest {
                            return Err(io::Error::new(
                                io::ErrorKind::Other,
                                "&rest can only appear once",
                            ));
                        }
                        use_rest = true;
                    }
                    _ => {
                        if post_rest_cnt > 1 {
                            return Err(io::Error::new(
                                io::ErrorKind::Other,
                                "&rest can only have one symbol after",
                            ));
                        }
                        if use_rest {
                            post_rest_cnt += 1;
                        } else {
                            min_params += 1;
                        }
                        var_names.push(s.clone());
                    }
                }
            } else {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "parameter name must be symbol",
                ));
            }
        }
        if use_rest && post_rest_cnt != 1 {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "&rest must have one symbol after",
            ));
        }
        let mut vars = args;
        let t_vars = if eval_args {
            Some(to_args(environment, args, false)?)
        } else {
            None
        };
        if let Some(t_vars) = t_vars {
            vars = &t_vars;
            setup_args_final(environment, var_names, vars, min_params, use_rest)?;
        } else {
            setup_args_final(environment, var_names, vars, min_params, use_rest)?;
        }
    }
    Ok(())
}
