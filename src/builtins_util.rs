use std::env;
use std::io;
use std::iter::FromIterator;

use crate::environment::*;
use crate::eval::*;
use crate::types::*;

pub fn is_proper_list(exp: &Expression) -> bool {
    // does not detect empty (nil) lists on purpose.
    if let ExpEnum::Pair(_e1, e2) = exp.get() {
        if e2.is_nil() {
            true
        } else {
            is_proper_list(e2)
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
    if path.ends_with('~') || path.contains("~/") {
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
            if ch == '~' && (last_ch == ' ' || last_ch == ':') {
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
        if last_ch == '\\' {
            new_path.push('\\');
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

fn set_arg(
    environment: &mut Environment,
    scope: &mut Option<&mut Scope>,
    key: &'static str,
    var: &Expression,
    do_eval: bool,
) -> io::Result<()> {
    let res_var;
    let var = if let ExpEnum::LazyFn(_, _) = var.get() {
        res_var = var.resolve(environment)?;
        &res_var
    } else {
        var
    };
    let v2 = if do_eval {
        if let ExpEnum::Atom(Atom::Symbol(s)) = var.get() {
            if let Some(reference) = get_expression(environment, s) {
                reference
            } else {
                let exp = eval(environment, var)?;
                Reference::new_rooted(
                    &mut environment.gc,
                    exp,
                    RefMetaData {
                        namespace: None,
                        doc_string: None,
                    },
                )
            }
        } else {
            let exp = eval(environment, var)?;
            Reference::new_rooted(
                &mut environment.gc,
                exp,
                RefMetaData {
                    namespace: None,
                    doc_string: None,
                },
            )
        }
    } else {
        Reference::new_rooted(
            &mut environment.gc,
            *var,
            RefMetaData {
                namespace: None,
                doc_string: None,
            },
        )
    };
    if let Some(scope) = scope {
        scope.data.insert(key, v2);
    } else {
        set_expression_current_ref(environment, key, v2);
    }
    Ok(())
}

fn setup_args_final<'a>(
    environment: &mut Environment,
    scope: &mut Option<&mut Scope>,
    var_names: &mut Vec<&'static str>,
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
            set_arg(environment, scope, k.unwrap(), v.unwrap(), do_eval)?;
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
                scope.insert_exp_data(&mut environment.gc, rest_name, ExpEnum::Nil);
            } else {
                set_expression_current_data(environment, rest_name, None, ExpEnum::Nil);
            }
        } else if let Some(scope) = scope {
            let data = Expression::with_list(&mut environment.gc, rest_data);
            scope.insert_exp(&mut environment.gc, rest_name, data);
        } else {
            let data = Expression::with_list(&mut environment.gc, rest_data);
            set_expression_current(environment, rest_name, None, data);
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
            set_arg(environment, scope, k.unwrap(), v.unwrap(), do_eval)?;
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
    let p_iter = match params.get() {
        ExpEnum::Vector(li) => Box::new(li.iter()),
        _ => params.iter(),
    };
    let mut var_names: Vec<&'static str> = Vec::new(); //with_capacity(l.len());
    let mut use_rest = false;
    let mut post_rest_cnt = 0;
    let mut min_params = 0;
    for arg in p_iter {
        if let ExpEnum::Atom(Atom::Symbol(s)) = arg.get() {
            match *s {
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
                    var_names.push(s);
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
