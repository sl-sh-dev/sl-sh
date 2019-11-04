use std::cell::RefCell;
use std::env;
use std::io;
use std::rc::Rc;
use std::sync::atomic::Ordering;

use crate::builtins_util::*;
use crate::environment::*;
use crate::process::*;
use crate::types::*;

fn box_slice_it<'a>(v: &'a [Expression]) -> Box<dyn Iterator<Item = &Expression> + 'a> {
    Box::new(v.iter())
}

fn call_lambda<'a>(
    environment: &mut Environment,
    lambda: &Lambda,
    args: Box<dyn Iterator<Item = &Expression> + 'a>,
) -> io::Result<Expression> {
    // DO NOT use ? in here, need to make sure the new_scope is popped off the
    // current_scope list before ending.
    let mut looping = true;
    let mut last_eval = Expression::Atom(Atom::Nil);
    let new_scope = build_new_scope(Some(lambda.capture.clone()));
    if let Err(err) = setup_args(
        environment,
        Some(&mut new_scope.borrow_mut()),
        &lambda.params,
        args,
        true,
    ) {
        return Err(err);
    }
    environment.current_scope.push(new_scope);
    let old_loose = environment.loose_symbols;
    environment.loose_symbols = false;
    while looping {
        last_eval = eval(environment, &lambda.body)?;
        looping = environment.state.recur_num_args.is_some() && environment.exit_code.is_none();
        if looping {
            let recur_args = environment.state.recur_num_args.unwrap();
            environment.state.recur_num_args = None;
            if let Expression::Vector(new_args) = &last_eval {
                if recur_args != new_args.borrow().len() {
                    environment.current_scope.pop();
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "Called recur in a non-tail position.",
                    ));
                }
                let new_args1 = new_args.borrow();
                let ib = box_slice_it(&new_args1);
                if let Err(err) = setup_args(environment, None, &lambda.params, ib, false) {
                    environment.current_scope.pop();
                    return Err(err);
                }
            }
        }
    }
    environment.loose_symbols = old_loose;
    environment.current_scope.pop();
    Ok(last_eval)
}

fn expand_macro<'a>(
    environment: &mut Environment,
    sh_macro: &Macro,
    args: Box<dyn Iterator<Item = &Expression> + 'a>,
) -> io::Result<Expression> {
    // DO NOT use ? in here, need to make sure the new_scope is popped off the
    // current_scope list before ending.
    let mut new_scope = Scope::default();
    match setup_args(
        environment,
        Some(&mut new_scope),
        &sh_macro.params,
        args,
        false,
    ) {
        Ok(_) => {}
        Err(err) => {
            return Err(err);
        }
    };
    new_scope.outer = Some(environment.current_scope.last().unwrap().clone());
    environment
        .current_scope
        .push(Rc::new(RefCell::new(new_scope)));
    match eval(environment, &sh_macro.body) {
        Ok(expansion) => {
            environment.current_scope.pop();
            eval(environment, &expansion)
        }
        Err(err) => {
            environment.current_scope.pop();
            Err(err)
        }
    }
}

pub fn fn_call<'a>(
    environment: &mut Environment,
    command: &Expression,
    mut args: Box<dyn Iterator<Item = &Expression> + 'a>,
) -> io::Result<Expression> {
    match command {
        Expression::Atom(Atom::Symbol(command)) => {
            if let Some(exp) = get_expression(environment, &command) {
                match &*exp {
                    Expression::Func(f) => {
                        let parts: Vec<Expression> = args.cloned().collect();
                        f(environment, &parts)
                    }
                    Expression::Function(c) if !c.is_special_form => {
                        (c.func)(environment, &mut *args)
                    }
                    Expression::Atom(Atom::Lambda(f)) => call_lambda(environment, &f, args),
                    _ => {
                        let msg = format!(
                            "Symbol {} is not callable (or is macro or special form).",
                            command
                        );
                        Err(io::Error::new(io::ErrorKind::Other, msg))
                    }
                }
            } else {
                let msg = format!(
                    "Symbol {} is not callable (or is macro or special form).",
                    command
                );
                Err(io::Error::new(io::ErrorKind::Other, msg))
            }
        }
        Expression::Atom(Atom::Lambda(l)) => call_lambda(environment, &l, args),
        Expression::Atom(Atom::Macro(m)) => expand_macro(environment, &m, args),
        Expression::Func(f) => {
            let parts: Vec<Expression> = args.cloned().collect();
            f(environment, &parts)
        }
        Expression::Function(c) if !c.is_special_form => (c.func)(environment, &mut *args),
        _ => {
            let msg = format!(
                "Called an invalid command {}, type {}.",
                command.make_string(environment)?,
                command.display_type()
            );
            Err(io::Error::new(io::ErrorKind::Other, msg))
        }
    }
}

fn fn_eval<'a>(
    environment: &mut Environment,
    command: &Expression,
    mut parts: Box<dyn Iterator<Item = &Expression> + 'a>,
) -> io::Result<Expression> {
    match command {
        Expression::Atom(Atom::Symbol(command)) => {
            if command.is_empty() {
                return Ok(Expression::Atom(Atom::Nil));
            }
            let form = if environment.form_type == FormType::Any
                || environment.form_type == FormType::FormOnly
            {
                get_expression(environment, &command)
            } else {
                None
            };
            if let Some(exp) = form {
                match &*exp {
                    Expression::Func(f) => {
                        let parts: Vec<Expression> = parts.cloned().collect();
                        f(environment, &parts)
                    }
                    Expression::Function(c) => (c.func)(environment, &mut *parts),
                    Expression::Atom(Atom::Lambda(f)) => call_lambda(environment, &f, parts),
                    Expression::Atom(Atom::Macro(m)) => expand_macro(environment, &m, parts),
                    _ => {
                        let exp = exp.clone();
                        eval(environment, &exp)
                    }
                }
            } else if environment.form_type == FormType::ExternalOnly
                || environment.form_type == FormType::Any
            {
                do_command(environment, command, parts)
            } else {
                let msg = format!("Not a valid form {}, not found.", command.to_string());
                Err(io::Error::new(io::ErrorKind::Other, msg))
            }
        }
        Expression::Vector(list) => match eval(environment, &Expression::Vector(list.clone()))? {
            Expression::Atom(Atom::Lambda(l)) => call_lambda(environment, &l, parts),
            Expression::Atom(Atom::Macro(m)) => expand_macro(environment, &m, parts),
            Expression::Func(f) => {
                let parts: Vec<Expression> = parts.cloned().collect();
                f(environment, &parts)
            }
            Expression::Function(c) => (c.func)(environment, &mut *parts),
            _ => {
                let msg = format!("Not a valid command {:?}", list);
                Err(io::Error::new(io::ErrorKind::Other, msg))
            }
        },
        Expression::Pair(e1, e2) => {
            match eval(environment, &Expression::Pair(e1.clone(), e2.clone()))? {
                Expression::Atom(Atom::Lambda(l)) => call_lambda(environment, &l, parts),
                Expression::Atom(Atom::Macro(m)) => expand_macro(environment, &m, parts),
                Expression::Func(f) => {
                    let parts: Vec<Expression> = parts.cloned().collect();
                    f(environment, &parts)
                }
                Expression::Function(c) => (c.func)(environment, &mut *parts),
                _ => {
                    let msg = format!("Not a valid command {:?}", command);
                    Err(io::Error::new(io::ErrorKind::Other, msg))
                }
            }
        }
        Expression::Atom(Atom::Lambda(l)) => call_lambda(environment, &l, parts),
        Expression::Atom(Atom::Macro(m)) => expand_macro(environment, &m, parts),
        Expression::Func(f) => {
            let parts: Vec<Expression> = parts.cloned().collect();
            f(environment, &parts)
        }
        Expression::Function(c) => (c.func)(environment, &mut *parts),
        _ => {
            let msg = format!(
                "Not a valid command {}, type {}.",
                command.make_string(environment)?,
                command.display_type()
            );
            Err(io::Error::new(io::ErrorKind::Other, msg))
        }
    }
}

fn internal_eval<'a>(
    environment: &mut Environment,
    expression: &'a Expression,
) -> io::Result<Expression> {
    if environment.sig_int.load(Ordering::Relaxed) {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "Script interupted by SIGINT.",
        ));
    }
    // exit was called so just return nil to unwind.
    if environment.exit_code.is_some() {
        return Ok(Expression::Atom(Atom::Nil));
    }
    let in_recur = environment.state.recur_num_args.is_some();
    if in_recur {
        environment.state.recur_num_args = None;
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "Called recur in a non-tail position.",
        ));
    }
    match expression {
        Expression::Vector(parts) => {
            let parts = parts.borrow();
            let (command, parts) = match parts.split_first() {
                Some((c, p)) => (c, p),
                None => {
                    eprintln!("No valid command.");
                    return Err(io::Error::new(io::ErrorKind::Other, "No valid command."));
                }
            };
            let ib = box_slice_it(&parts);
            fn_eval(environment, command, ib)
        }
        Expression::Pair(command, rest) => {
            fn_eval(environment, &command.borrow(), rest.borrow().iter())
        }
        Expression::Atom(Atom::Symbol(s)) => {
            if s.starts_with('$') {
                match env::var(&s[1..]) {
                    Ok(val) => Ok(Expression::Atom(Atom::String(val))),
                    Err(_) => Ok(Expression::Atom(Atom::String("".to_string()))),
                }
            } else if let Some(exp) = get_expression(environment, &s[..]) {
                match &*exp {
                    Expression::Func(_) => Ok(Expression::Atom(Atom::String(s.clone()))),
                    Expression::Vector(l) => Ok(Expression::Vector(l.clone())),
                    _ => {
                        let exp = &*exp;
                        Ok(exp.clone())
                    }
                }
            } else if environment.loose_symbols {
                Ok(Expression::Atom(Atom::String(s.clone())))
            } else {
                let msg = format!("Symbol {} not found.", s);
                Err(io::Error::new(io::ErrorKind::Other, msg))
            }
        }
        Expression::Atom(atom) => Ok(Expression::Atom(atom.clone())),
        Expression::Func(_) => Ok(Expression::Atom(Atom::Nil)),
        Expression::Function(_) => Ok(Expression::Atom(Atom::Nil)),
        Expression::Process(state) => Ok(Expression::Process(*state)),
        Expression::File(_) => Ok(Expression::Atom(Atom::Nil)),
    }
}

pub fn eval<'a>(
    environment: &mut Environment,
    expression: &'a Expression,
) -> io::Result<Expression> {
    environment.state.eval_level += 1;
    let result = internal_eval(environment, expression);
    if let Err(_err) = &result {
        eprintln!("{}: Error evaluting:", environment.state.eval_level,);
        let stderr = io::stderr();
        let mut handle = stderr.lock();
        if let Err(err) = expression.pretty_printf(environment, &mut handle) {
            eprintln!("\nGOT SECONDARY ERROR PRINTING EXPRESSION: {}", err);
        }
        eprintln!("\n=============================================================");
    }
    environment.state.eval_level -= 1;
    result
}
