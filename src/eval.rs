use std::cell::RefCell;
use std::env;
use std::io;
use std::rc::Rc;
use std::sync::atomic::Ordering;

use crate::builtins::expand_macro;
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
    let mut last_eval = Expression::nil();
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
        last_eval = match eval(environment, &lambda.body) {
            Ok(e) => e,
            Err(err) => {
                environment.current_scope.pop();
                return Err(err);
            }
        };
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

fn exec_macro<'a>(
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
                match &exp.exp {
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
        Expression::Atom(Atom::Macro(m)) => exec_macro(environment, &m, args),
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
                return Ok(Expression::nil());
            }
            let form = if environment.form_type == FormType::Any
                || environment.form_type == FormType::FormOnly
            {
                get_expression(environment, &command)
            } else {
                None
            };
            if let Some(exp) = form {
                match &exp.exp {
                    Expression::Function(c) => (c.func)(environment, &mut *parts),
                    Expression::Atom(Atom::Lambda(f)) => call_lambda(environment, &f, parts),
                    Expression::Atom(Atom::Macro(m)) => exec_macro(environment, &m, parts),
                    _ => {
                        let exp = exp.exp.clone();
                        eval(environment, &exp)
                    }
                }
            } else if environment.form_type == FormType::ExternalOnly
                || environment.form_type == FormType::Any
            {
                if command.starts_with('$') {
                    if let Ok(Expression::Atom(Atom::String(command))) =
                        str_process(environment, command)
                    {
                        do_command(environment, &command, parts)
                    } else {
                        let msg = format!("Not a valid form {}, not found.", command.to_string());
                        Err(io::Error::new(io::ErrorKind::Other, msg))
                    }
                } else {
                    do_command(environment, command, parts)
                }
            } else {
                let msg = format!("Not a valid form {}, not found.", command.to_string());
                Err(io::Error::new(io::ErrorKind::Other, msg))
            }
        }
        Expression::Vector(list) => match eval(environment, &Expression::Vector(list.clone()))? {
            Expression::Atom(Atom::Lambda(l)) => call_lambda(environment, &l, parts),
            Expression::Atom(Atom::Macro(m)) => exec_macro(environment, &m, parts),
            Expression::Function(c) => (c.func)(environment, &mut *parts),
            _ => {
                let msg = format!("Not a valid command {:?}", list);
                Err(io::Error::new(io::ErrorKind::Other, msg))
            }
        },
        Expression::Pair(p) => {
            if let Some((_e1, _e2)) = &*p.borrow() {
                match eval(environment, &Expression::Pair(p.clone()))? {
                    Expression::Atom(Atom::Lambda(l)) => call_lambda(environment, &l, parts),
                    Expression::Atom(Atom::Macro(m)) => exec_macro(environment, &m, parts),
                    Expression::Function(c) => (c.func)(environment, &mut *parts),
                    _ => {
                        let msg = format!("Not a valid command {:?}", command);
                        Err(io::Error::new(io::ErrorKind::Other, msg))
                    }
                }
            } else {
                Err(io::Error::new(
                    io::ErrorKind::Other,
                    "Not a valid command: nil.",
                ))
            }
        }
        Expression::Atom(Atom::Lambda(l)) => call_lambda(environment, &l, parts),
        Expression::Atom(Atom::Macro(m)) => exec_macro(environment, &m, parts),
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

fn str_process(environment: &mut Environment, string: &str) -> io::Result<Expression> {
    if !environment.str_ignore_expand && string.contains('$') {
        let mut new_string = String::new();
        let mut last_ch = '\0';
        let mut in_var = false;
        let mut var_start = 0;
        for (i, ch) in string.chars().enumerate() {
            if in_var {
                if ch == ' ' || ch == '"' || (ch == '$' && last_ch != '\\') {
                    in_var = false;
                    match env::var(&string[var_start + 1..i]) {
                        Ok(val) => new_string.push_str(&val),
                        Err(_) => new_string.push_str(""),
                    }
                }
                if ch == ' ' || ch == '"' {
                    new_string.push(ch);
                }
            } else if ch == '$' && last_ch != '\\' {
                in_var = true;
                var_start = i;
            } else if ch != '\\' {
                if last_ch == '\\' && ch != '$' {
                    new_string.push('\\');
                }
                new_string.push(ch);
            }
            last_ch = ch;
        }
        if in_var {
            match env::var(&string[var_start + 1..]) {
                Ok(val) => new_string.push_str(&val),
                Err(_) => new_string.push_str(""),
            }
        }
        Ok(Expression::Atom(Atom::String(new_string)))
    } else {
        Ok(Expression::Atom(Atom::String(string.to_string())))
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
        return Ok(Expression::nil());
    }
    let in_recur = environment.state.recur_num_args.is_some();
    if in_recur {
        environment.state.recur_num_args = None;
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "Called recur in a non-tail position.",
        ));
    }
    // If we have a macro expand it and replace the expression with the expansion.
    if let Some(exp) = expand_macro(environment, expression, false)? {
        let mut nv = Vec::new();
        if let Expression::Vector(list) = &exp {
            for item in &*list.borrow() {
                nv.push(item.clone());
            }
        } else if let Expression::Pair(_) = &exp {
            for item in exp.iter() {
                nv.push(item.clone());
            }
        }
        match expression {
            Expression::Vector(list) => {
                list.replace(nv);
            }
            Expression::Pair(p) => {
                if let Expression::Pair(np) = Expression::cons_from_vec(&mut nv) {
                    p.replace(np.borrow().clone());
                }
            }
            _ => {}
        }
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
        Expression::Pair(p) => {
            if let Some((command, rest)) = &*p.borrow() {
                fn_eval(environment, &command, rest.iter())
            } else {
                Ok(Expression::nil())
            }
        }
        Expression::Atom(Atom::Symbol(s)) => {
            if s.starts_with('$') {
                match env::var(&s[1..]) {
                    Ok(val) => Ok(Expression::Atom(Atom::String(val))),
                    Err(_) => Ok(Expression::nil()),
                }
            } else if s.starts_with(':') {
                // Got a keyword, so just be you...
                Ok(Expression::Atom(Atom::Symbol(s.clone())))
            } else if let Some(exp) = get_expression(environment, &s[..]) {
                let exp = &exp.exp;
                Ok(exp.clone())
            } else if environment.loose_symbols {
                str_process(environment, s)
            } else {
                let msg = format!("Symbol {} not found.", s);
                Err(io::Error::new(io::ErrorKind::Other, msg))
            }
        }
        Expression::HashMap(map) => Ok(Expression::HashMap(map.clone())),
        Expression::Atom(Atom::String(string)) => str_process(environment, &string),
        Expression::Atom(atom) => Ok(Expression::Atom(atom.clone())),
        Expression::Function(_) => Ok(Expression::nil()),
        Expression::Process(state) => Ok(Expression::Process(*state)),
        Expression::File(_) => Ok(Expression::nil()),
    }
}

pub fn eval<'a>(
    environment: &mut Environment,
    expression: &'a Expression,
) -> io::Result<Expression> {
    environment.state.eval_level += 1;
    let result = internal_eval(environment, expression);
    if let Err(_err) = &result {
        if environment.error_expression.is_none() {
            environment.error_expression = Some(expression.clone());
        }
        if environment.stack_on_error {
            eprintln!("{}: Error evaluting:", environment.state.eval_level);
            let stderr = io::stderr();
            let mut handle = stderr.lock();
            if let Err(err) = expression.pretty_printf(environment, &mut handle) {
                eprintln!("\nGOT SECONDARY ERROR PRINTING EXPRESSION: {}", err);
            }
            eprintln!("\n=============================================================");
        }
    }
    environment.state.eval_level -= 1;
    result
}
