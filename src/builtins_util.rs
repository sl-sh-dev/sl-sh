use std::env;
use std::io;

use crate::environment::*;
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
    args: &[Expression],
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

pub fn expand_tilde(path: &str) -> Option<String> {
    let mut home = match env::var("HOME") {
        Ok(val) => val,
        Err(_) => "/".to_string(),
    };
    if !home.ends_with('/') {
        home.push('/');
    }
    if path == "~" {
        Some(home)
    } else if path.contains("~/") {
        let new_path = path.replace("~/", &home);
        Some(new_path)
    } else {
        None
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
