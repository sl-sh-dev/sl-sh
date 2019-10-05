use std::cell::RefCell;
use std::env;
use std::io;
use std::rc::Rc;
use std::sync::atomic::Ordering;

use crate::builtins_util::*;
use crate::environment::*;
use crate::process::*;
use crate::types::*;

fn call_lambda(
    environment: &mut Environment,
    lambda: &Lambda,
    args: &[Expression],
) -> io::Result<Expression> {
    // DO NOT use ? in here, need to make sure the new_scope is popped off the
    // current_scope list before ending.
    let mut looping = true;
    let mut last_eval = Expression::Atom(Atom::Nil);
    let mut new_scope = Scope::default();
    if let Err(err) = setup_args(
        environment,
        Some(&mut new_scope),
        &lambda.params,
        args,
        true,
    ) {
        return Err(err);
    }
    new_scope.outer = Some(lambda.capture.clone());
    environment
        .current_scope
        .push(Rc::new(RefCell::new(new_scope)));
    let old_loose = environment.loose_symbols;
    environment.loose_symbols = false;
    while looping {
        last_eval = eval(environment, &lambda.body)?;
        looping = environment.state.recur_num_args.is_some();
        if looping {
            let recur_args = environment.state.recur_num_args.unwrap();
            environment.state.recur_num_args = None;
            if let Expression::List(new_args) = &last_eval {
                if recur_args != new_args.borrow().len() {
                    environment.current_scope.pop();
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "Called recur in a non-tail position.",
                    ));
                }
                if let Err(err) =
                    setup_args(environment, None, &lambda.params, &new_args.borrow(), false)
                {
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

fn expand_macro(
    environment: &mut Environment,
    sh_macro: &Macro,
    args: &[Expression],
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
        Ok(scope) => scope,
        Err(err) => return Err(err),
    };
    new_scope.outer = Some(environment.current_scope.last().unwrap().clone());
    environment
        .current_scope
        .push(Rc::new(RefCell::new(new_scope)));
    match eval(environment, &sh_macro.body) {
        Ok(expansion) => {
            environment.current_scope.pop();
            // Mess with eval_level to remove the extra level the macro added- helpful for executables and stdout detection.
            environment.state.eval_level -= 1;
            let result = eval(environment, &expansion);
            environment.state.eval_level += 1;
            result
        }
        Err(err) => {
            environment.current_scope.pop();
            Err(err)
        }
    }
}

fn internal_eval(environment: &mut Environment, expression: &Expression) -> io::Result<Expression> {
    if environment.sig_int.load(Ordering::Relaxed) {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "Script interupted by SIGINT.",
        ));
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
        Expression::List(parts) => {
            let parts = parts.borrow();
            let (command, parts) = match parts.split_first() {
                Some((c, p)) => (c, p),
                None => {
                    eprintln!("No valid command.");
                    return Err(io::Error::new(io::ErrorKind::Other, "No valid command."));
                }
            };
            match command {
                Expression::Atom(Atom::Symbol(_s)) => {}
                Expression::List(_list) => {}
                _ => {
                    let msg = format!(
                        "Not a valid command {}, must be a symbol.",
                        command.make_string(environment)?
                    );
                    return Err(io::Error::new(io::ErrorKind::Other, msg));
                }
            };

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
                    if form.is_some() {
                        let exp = &*form.unwrap();
                        if let Expression::Func(f) = exp {
                            f(environment, &parts)
                        } else if let Expression::Atom(Atom::Lambda(f)) = exp {
                            call_lambda(environment, &f, parts)
                        } else if let Expression::Atom(Atom::Macro(m)) = exp {
                            expand_macro(environment, &m, parts)
                        } else {
                            let exp = exp.clone();
                            eval(environment, &exp)
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
                Expression::List(list) => {
                    match eval(environment, &Expression::with_list(list.borrow().to_vec()))? {
                        Expression::Atom(Atom::Lambda(l)) => call_lambda(environment, &l, parts),
                        Expression::Atom(Atom::Macro(m)) => expand_macro(environment, &m, parts),
                        Expression::Func(f) => f(environment, &parts),
                        _ => Err(io::Error::new(io::ErrorKind::Other, "Not a valid command")),
                    }
                }
                Expression::Atom(Atom::Lambda(l)) => call_lambda(environment, &l, parts),
                Expression::Atom(Atom::Macro(m)) => expand_macro(environment, &m, parts),
                Expression::Func(f) => f(environment, &parts),
                _ => Err(io::Error::new(io::ErrorKind::Other, "Not a valid command")),
            }
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
                    Expression::List(l) => Ok(Expression::List(l.clone())),
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
        Expression::Process(state) => Ok(Expression::Process(*state)),
    }
}

pub fn eval(environment: &mut Environment, expression: &Expression) -> io::Result<Expression> {
    environment.state.eval_level += 1;
    let result = internal_eval(environment, expression);
    if let Err(_err) = &result {
        eprintln!(
            "{}: Error evaluting {}",
            environment.state.eval_level,
            expression.to_string()
        );
    }
    environment.state.eval_level -= 1;
    result
}
