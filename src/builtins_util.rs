use std::env;
use std::iter::FromIterator;

use crate::environment::*;
use crate::eval::*;
use crate::gc::Handle;
use crate::types::*;

pub fn param_eval(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
    form: &str,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        eval(environment, arg)
    } else {
        let msg = format!(
            "{}: Missing required argument, see (doc '{}) for usage.",
            form, form
        );
        Err(LispError::new(msg))
    }
}

pub fn params_done(
    args: &mut dyn Iterator<Item = Expression>,
    form: &str,
) -> Result<(), LispError> {
    if args.next().is_none() {
        Ok(())
    } else {
        let msg = format!(
            "{}: To many arguments, see (doc '{}) for usage.",
            form, form
        );
        Err(LispError::new(msg))
    }
}

pub fn is_proper_list(exp: &Expression) -> bool {
    // does not detect empty (nil) lists on purpose.
    if let ExpEnum::Pair(_e1, e2) = &exp.get().data {
        let e2: Expression = e2.into();
        if e2.is_nil() {
            true
        } else {
            is_proper_list(&e2)
        }
    } else {
        false
    }
}

pub fn list_to_args(
    environment: &mut Environment,
    parts: &mut [Expression],
    do_eval: bool,
) -> Result<Vec<Expression>, LispError> {
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
) -> Result<Vec<i64>, LispError> {
    let mut list: Vec<i64> = Vec::with_capacity(args.len());
    for arg in args {
        list.push(arg.make_int(environment)?);
    }
    Ok(list)
}

pub fn parse_list_of_floats(
    environment: &mut Environment,
    args: &mut [Expression],
) -> Result<Vec<f64>, LispError> {
    let mut list: Vec<f64> = Vec::with_capacity(args.len());
    for arg in args {
        list.push(arg.make_float(environment)?);
    }
    Ok(list)
}

pub fn parse_list_of_strings(
    environment: &mut Environment,
    args: &mut [Expression],
) -> Result<Vec<String>, LispError> {
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
    var: Expression,
    do_eval: bool,
) -> Result<(), LispError> {
    let var = var.resolve(environment)?;
    let v2 = if do_eval {
        let var_d = var.get();
        if let ExpEnum::Symbol(_, _) = &var_d.data {
            if let Some(reference) = get_expression(environment, var.clone()) {
                reference
            } else {
                drop(var_d); // Release the read lock on var.
                let exp = eval(environment, var)?;
                Reference::new_rooted(
                    exp,
                    RefMetaData {
                        namespace: None,
                        doc_string: None,
                    },
                )
            }
        } else {
            drop(var_d); // Release the read lock on var.
            let exp = eval(environment, var)?;
            Reference::new_rooted(
                exp,
                RefMetaData {
                    namespace: None,
                    doc_string: None,
                },
            )
        }
    } else {
        Reference::new_rooted(
            var,
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

pub fn setup_args(
    environment: &mut Environment,
    mut scope: Option<&mut Scope>,
    var_names: &[&'static str],
    vars: &mut dyn Iterator<Item = Expression>,
    do_eval: bool,
) -> Result<(), LispError> {
    let mut names_iter = var_names.iter();
    let mut params = 0;
    loop {
        let k = names_iter.next();
        let v = vars.next();
        if k.is_none() && v.is_none() {
            break;
        } else if k.is_some() && *k.unwrap() == "&rest" {
            let rest_name = if let Some(k) = names_iter.next() {
                k
            } else {
                return Err(LispError::new("&rest requires a parameter to follow"));
            };
            if *rest_name == "&rest" {
                return Err(LispError::new("&rest can only appear once"));
            }
            if names_iter.next().is_some() {
                return Err(LispError::new("&rest must be before the last parameter"));
            }
            let mut rest_data: Vec<Handle> = Vec::new();
            if let Some(v) = v {
                let v2 = if do_eval { eval(environment, v)? } else { v };
                rest_data.push(v2.into());
            }
            for v in vars {
                let v2 = if do_eval { eval(environment, v)? } else { v };
                rest_data.push(v2.into());
            }
            if rest_data.is_empty() {
                if let Some(scope) = scope {
                    scope.insert_exp_data(rest_name, ExpEnum::Nil);
                } else {
                    set_expression_current_data(environment, rest_name, None, ExpEnum::Nil);
                }
            } else if let Some(scope) = scope {
                let data = Expression::with_list(rest_data);
                scope.insert_exp(rest_name, data);
            } else {
                let data = Expression::with_list(rest_data);
                set_expression_current(environment, rest_name, None, data);
            }
            return Ok(());
        } else if k.is_none() || v.is_none() {
            let mut min_params = params;
            if v.is_some() {
                params += 1;
            }
            let mut has_rest = false;
            for k in names_iter {
                if *k == "&rest" {
                    has_rest = true;
                } else {
                    min_params += 1;
                }
            }
            let msg = if has_rest {
                format!(
                    "wrong number of parameters, expected at least {} got {}",
                    min_params,
                    (params + vars.count())
                )
            } else {
                format!(
                    "wrong number of parameters, expected {} got {}",
                    min_params,
                    (params + vars.count())
                )
            };
            return Err(LispError::new(msg));
        }
        set_arg(environment, &mut scope, k.unwrap(), v.unwrap(), do_eval)?;
        params += 1;
    }
    Ok(())
}
