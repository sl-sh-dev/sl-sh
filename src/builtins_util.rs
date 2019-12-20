use std::env;
use std::io;
use std::iter::FromIterator;
use std::rc::Rc;

use crate::environment::*;
use crate::eval::*;
use crate::types::*;

pub fn is_proper_list(exp: &Expression) -> bool {
    if let Expression::Pair(_e1, e2) = exp {
        if let Expression::Atom(Atom::Nil) = *e2.borrow() {
            true
        } else {
            is_proper_list(&e2.borrow())
        }
    } else {
        false
    }
}

pub fn list_to_args(
    environment: &mut Environment,
    parts: &[Expression],
    do_eval: bool,
) -> io::Result<Vec<Expression>> {
    if do_eval {
        let mut args: Vec<Expression> = Vec::with_capacity(parts.len());
        for a in parts {
            args.push(eval(environment, a)?);
        }
        Ok(args)
    } else {
        let args: Vec<Expression> = Vec::from_iter(parts.iter().cloned());
        Ok(args)
    }
}

pub fn exp_to_args(
    environment: &mut Environment,
    parts: &Expression,
    do_eval: bool,
) -> io::Result<Vec<Expression>> {
    if is_proper_list(parts) {
        let mut args: Vec<Expression> = Vec::new();
        let mut current = parts.clone();
        while let Expression::Pair(e1, e2) = current {
            if do_eval {
                args.push(eval(environment, &e1.borrow())?);
            } else {
                args.push(e1.borrow().clone());
            }
            current = e2.borrow().clone();
        }
        Ok(args)
    } else if do_eval {
        let mut args: Vec<Expression> = Vec::with_capacity(1);
        args.push(eval(environment, parts)?);
        Ok(args)
    } else {
        let mut args: Vec<Expression> = Vec::with_capacity(1);
        args.push(parts.clone());
        Ok(args)
    }
}

pub fn to_args(env: &mut Environment, parts: &[Expression]) -> io::Result<Vec<Expression>> {
    let mut args: Vec<Expression> = Vec::with_capacity(parts.len());
    for a in parts {
        args.push(eval(env, a)?);
    }
    Ok(args)
}

pub fn to_args_str(environment: &mut Environment, parts: &[Expression]) -> io::Result<Vec<String>> {
    let mut args: Vec<String> = Vec::with_capacity(parts.len());
    for a in parts {
        args.push(eval(environment, a)?.make_string(environment)?);
    }
    Ok(args)
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
    if path.contains('~') {
        let mut home = match env::var("HOME") {
            Ok(val) => val,
            Err(_) => "/".to_string(),
        };
        if home.ends_with('/') {
            home.pop();
        }
        let mut new_path = String::new();
        let mut last_ch = ' ';
        for ch in path.chars() {
            if ch == '~' {
                if last_ch == '\\' {
                    new_path.push('~');
                } else {
                    new_path.push_str(&home);
                }
            } else {
                if last_ch == '\\' {
                    new_path.push('\\');
                }
                if ch != '\\' {
                    new_path.push(ch);
                }
            }
            last_ch = ch;
        }
        Some(new_path)
    } else {
        None
    }
}

pub fn compress_tilde(path: &str) -> Option<String> {
    if let Ok(mut home) = env::var("HOME") {
        if home.ends_with('/') {
            home.pop();
        }
        if path.contains(&home) {
            Some(path.replace(&home, "~"))
        } else {
            None
        }
    } else {
        None
    }
}

fn setup_args_final<'a>(
    environment: &mut Environment,
    scope: &mut Option<&mut Scope>,
    var_names: &mut Vec<String>,
    mut vars: Box<dyn Iterator<Item = &Expression> + 'a>,
    min_params: usize,
    use_rest: bool,
    do_eval: bool,
) -> io::Result<()> {
    if use_rest {
        let rest_name = var_names.pop().unwrap();
        let mut names_iter = var_names.iter();
        let mut params = 0;
        loop {
            let k = names_iter.next();
            if k.is_none() {
                break;
            }
            let v = vars.next();
            if v.is_none() {
                let msg = format!(
                    "wrong number of parameters, expected {} got {}",
                    min_params, params
                );
                return Err(io::Error::new(io::ErrorKind::Other, msg));
            }
            let v2 = if do_eval {
                eval(environment, v.unwrap())?
            } else {
                v.unwrap().clone()
            };
            if let Some(scope) = scope {
                scope.data.insert(k.unwrap().clone(), Rc::new(v2));
            } else {
                set_expression_current(environment, k.unwrap().clone(), Rc::new(v2));
            }
            params += 1;
        }
        let mut rest_data: Vec<Expression> = Vec::new();
        for v in vars {
            let v2 = if do_eval {
                eval(environment, v)?
            } else {
                v.clone()
            };
            rest_data.push(v2);
        }
        if rest_data.is_empty() {
            if let Some(scope) = scope {
                scope
                    .data
                    .insert(rest_name, Rc::new(Expression::Atom(Atom::Nil)));
            } else {
                set_expression_current(
                    environment,
                    rest_name,
                    Rc::new(Expression::Atom(Atom::Nil)),
                );
            }
        } else if let Some(scope) = scope {
            scope
                .data
                .insert(rest_name, Rc::new(Expression::with_list(rest_data)));
        } else {
            set_expression_current(
                environment,
                rest_name,
                Rc::new(Expression::with_list(rest_data)),
            );
        }
    } else {
        let mut names_iter = var_names.iter();
        let mut params = 0;
        loop {
            let k = names_iter.next();
            let v = vars.next();
            if k.is_none() && v.is_none() {
                break;
            } else if k.is_none() || v.is_none() {
                if v.is_some() {
                    params += 1;
                }
                let msg = format!(
                    "wrong number of parameters, expected {} got {}",
                    min_params,
                    (params + vars.count())
                );
                return Err(io::Error::new(io::ErrorKind::Other, msg));
            }
            let v2 = if do_eval {
                eval(environment, v.unwrap())?
            } else {
                v.unwrap().clone()
            };
            if let Some(scope) = scope {
                scope.data.insert(k.unwrap().clone(), Rc::new(v2));
            } else {
                set_expression_current(environment, k.unwrap().clone(), Rc::new(v2));
            }
            params += 1;
        }
    }
    Ok(())
}

pub fn setup_args<'a>(
    environment: &mut Environment,
    mut new_scope: Option<&mut Scope>,
    params: &Expression,
    args: Box<dyn Iterator<Item = &Expression> + 'a>,
    eval_args: bool,
) -> io::Result<()> {
    let l;
    let p_iter = match params {
        Expression::Vector(li) => {
            l = li.borrow();
            Box::new(l.iter())
        }
        _ => params.iter(),
    };
    let mut var_names: Vec<String> = Vec::new(); //with_capacity(l.len());
    let mut use_rest = false;
    let mut post_rest_cnt = 0;
    let mut min_params = 0;
    for arg in p_iter {
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
            let msg = format!("parameter name must be symbol, got {:?}", arg);
            return Err(io::Error::new(io::ErrorKind::Other, msg));
        }
    }
    if use_rest && post_rest_cnt != 1 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "&rest must have one symbol after",
        ));
    }
    setup_args_final(
        environment,
        &mut new_scope,
        &mut var_names,
        args,
        min_params,
        use_rest,
        eval_args,
    )?;
    Ok(())
}
