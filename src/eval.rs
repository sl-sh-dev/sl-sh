use std::cell::RefCell;
use std::collections::HashMap;
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

pub fn call_lambda<'a>(
    environment: &mut Environment,
    lambda: &Lambda,
    args: Box<dyn Iterator<Item = &Expression> + 'a>,
    eval_args: bool,
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
        eval_args,
    ) {
        return Err(err);
    }
    environment.current_scope.push(new_scope);
    let old_loose = environment.loose_symbols;
    environment.loose_symbols = false;
    let mut lambda = lambda;
    let mut lambda_int;
    while looping {
        if environment.sig_int.load(Ordering::Relaxed) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "Lambda interupted by SIGINT.",
            ));
        }
        last_eval = match eval_nr(environment, &lambda.body) {
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
            if let Expression::Vector(new_args, _) = &last_eval {
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
        } else if environment.exit_code.is_none() {
            if let Expression::LazyFn(lam, parts) = &last_eval {
                lambda_int = lam.clone();
                lambda = &lambda_int;
                looping = true;
                environment.current_scope.pop();
                // scope is popped so can use ? in this if now.
                let new_scope = build_new_scope(Some(lambda.capture.clone()));
                let ib = Box::new(parts.iter());
                setup_args(
                    environment,
                    Some(&mut new_scope.borrow_mut()),
                    &lambda.params,
                    ib,
                    false,
                )?;
                environment.current_scope.push(new_scope);
            }
        }
    }
    environment.loose_symbols = old_loose;
    environment.current_scope.pop();
    Ok(last_eval.resolve(environment)?)
}

fn exec_macro<'a>(
    environment: &mut Environment,
    sh_macro: &Macro,
    args: Box<dyn Iterator<Item = &Expression> + 'a>,
) -> io::Result<Expression> {
    // DO NOT use ? in here, need to make sure the new_scope is popped off the
    // current_scope list before ending.
    let mut new_scope = Scope {
        data: HashMap::new(),
        outer: None,
        name: None,
    };
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
    let lazy = environment.allow_lazy_fn;
    environment.allow_lazy_fn = false;
    match eval(environment, &sh_macro.body) {
        Ok(expansion) => {
            let expansion = expansion.resolve(environment)?;
            environment.current_scope.pop();
            let res = eval(environment, &expansion);
            environment.allow_lazy_fn = lazy;
            res
        }
        Err(err) => {
            environment.allow_lazy_fn = lazy;
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
                    Expression::Atom(Atom::Lambda(f)) => call_lambda(environment, &f, args, true),
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
        Expression::Atom(Atom::Lambda(l)) => call_lambda(environment, &l, args, true),
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

fn make_lazy<'a>(
    environment: &mut Environment,
    lambda: &Lambda,
    args: Box<dyn Iterator<Item = &Expression> + 'a>,
) -> io::Result<Expression> {
    let mut parms = Vec::new();
    for p in args {
        parms.push(eval(environment, p)?);
    }
    Ok(Expression::LazyFn(lambda.clone(), parms))
}

fn fn_eval_lazy(environment: &mut Environment, expression: &Expression) -> io::Result<Expression> {
    let parts_int;
    let p_int;
    let p_bor;
    let (command, mut parts) = match expression {
        Expression::Vector(parts, _) => {
            parts_int = parts.borrow();
            let (command, parts) = match parts_int.split_first() {
                Some((c, p)) => (c, p),
                None => {
                    return Err(io::Error::new(io::ErrorKind::Other, "No valid command."));
                }
            };
            let ib = box_slice_it(&parts);
            (command, ib)
        }
        Expression::Pair(p, _) => {
            if p.borrow().is_none() {
                return Ok(Expression::nil());
            }
            p_bor = p.borrow();
            p_int = p_bor.as_ref().unwrap();
            (&p_int.0, p_int.1.iter())
        }
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "Not a callable expression.",
            ))
        }
    };
    let com_int;
    let command = if let Expression::LazyFn(_, _) = command {
        com_int = command.clone().resolve(environment)?;
        &com_int
    } else {
        command
    };
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
                    Expression::Atom(Atom::Lambda(f)) => {
                        if environment.allow_lazy_fn {
                            make_lazy(environment, f, parts)
                        } else {
                            call_lambda(environment, &f, parts, true)
                        }
                    }
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
                        let msg = format!("Not a valid form {}, not found.", command);
                        Err(io::Error::new(io::ErrorKind::Other, msg))
                    }
                } else {
                    do_command(environment, command, parts)
                }
            } else {
                let msg = format!("Not a valid form {}, not found.", command);
                Err(io::Error::new(io::ErrorKind::Other, msg))
            }
        }
        Expression::Vector(list, _) => {
            match eval(environment, &Expression::Vector(list.clone(), None))? {
                Expression::Atom(Atom::Lambda(l)) => {
                    if environment.allow_lazy_fn {
                        make_lazy(environment, &l, parts)
                    } else {
                        call_lambda(environment, &l, parts, true)
                    }
                }
                Expression::Atom(Atom::Macro(m)) => exec_macro(environment, &m, parts),
                Expression::Function(c) => (c.func)(environment, &mut *parts),
                _ => {
                    let msg = format!("Not a valid command {:?}", list);
                    Err(io::Error::new(io::ErrorKind::Other, msg))
                }
            }
        }
        Expression::Pair(p, _) => {
            if p.borrow().is_some() {
                match eval(environment, &Expression::Pair(p.clone(), None))? {
                    Expression::Atom(Atom::Lambda(l)) => {
                        if environment.allow_lazy_fn {
                            make_lazy(environment, &l, parts)
                        } else {
                            call_lambda(environment, &l, parts, true)
                        }
                    }
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
        Expression::Atom(Atom::Lambda(l)) => {
            if environment.allow_lazy_fn {
                make_lazy(environment, &l, parts)
            } else {
                call_lambda(environment, &l, parts, true)
            }
        }
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
        if environment.interner.contains(&new_string) {
            Ok(Expression::Atom(Atom::StringRef(
                environment.interner.intern(&new_string),
            )))
        } else {
            Ok(Expression::Atom(Atom::String(new_string)))
        }
    } else if environment.interner.contains(string) {
        Ok(Expression::Atom(Atom::StringRef(
            environment.interner.intern(string),
        )))
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
        if let Expression::Vector(list, _) = &exp {
            for item in &*list.borrow() {
                let item = if let Expression::LazyFn(_, _) = item {
                    item.clone().resolve(environment)?
                } else {
                    item.clone()
                };
                nv.push(item);
            }
        } else if let Expression::Pair(_, _) = &exp {
            for item in exp.iter() {
                let item = if let Expression::LazyFn(_, _) = item {
                    item.clone().resolve(environment)?
                } else {
                    item.clone()
                };
                nv.push(item);
            }
        }
        match expression {
            Expression::Vector(list, _) => {
                list.replace(nv);
            }
            Expression::Pair(p, _) => {
                if let Expression::Pair(np, _) = Expression::cons_from_vec(&mut nv, None) {
                    p.replace(np.borrow().clone());
                }
            }
            _ => {}
        }
    }
    match expression {
        Expression::Vector(_, _) => fn_eval_lazy(environment, expression),
        Expression::Pair(p, _) => {
            if p.borrow().is_some() {
                fn_eval_lazy(environment, expression)
            } else {
                Ok(Expression::nil())
            }
        }
        Expression::Atom(Atom::Symbol(s)) => {
            if s.starts_with('$') {
                match env::var(&s[1..]) {
                    Ok(val) => Ok(Expression::Atom(Atom::StringRef(
                        environment.interner.intern(&val),
                    ))),
                    Err(_) => Ok(Expression::nil()),
                }
            } else if s.starts_with(':') {
                // Got a keyword, so just be you...
                Ok(Expression::Atom(Atom::Symbol(s)))
            } else if let Some(exp) = get_expression(environment, s) {
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
        Expression::Atom(Atom::StringRef(string)) => str_process(environment, string),
        Expression::Atom(atom) => Ok(Expression::Atom(atom.clone())),
        Expression::Function(_) => Ok(Expression::nil()),
        Expression::Process(state) => Ok(Expression::Process(*state)),
        Expression::File(_) => Ok(Expression::nil()),
        Expression::LazyFn(_, _) => {
            let int_exp = expression.clone().resolve(environment)?;
            eval(environment, &int_exp)
        }
    }
}

pub fn eval_nr<'a>(
    environment: &mut Environment,
    expression: &'a Expression,
) -> io::Result<Expression> {
    if environment.return_val.is_some() {
        return Ok(Expression::nil());
    }
    if environment.state.eval_level > 500 {
        return Err(io::Error::new(io::ErrorKind::Other, "Eval calls to deep."));
    }
    environment.state.eval_level += 1;
    let tres = internal_eval(environment, expression);
    let result = if environment.state.eval_level == 1 && environment.return_val.is_some() {
        environment.return_val = None;
        Err(io::Error::new(
            io::ErrorKind::Other,
            "Return without matching block.",
        ))
    } else {
        tres
    };
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
            match expression {
                Expression::Vector(_, Some(meta)) => eprint!(
                    "\n[[[ {}, line: {}, column: {} ]]]",
                    meta.file, meta.line, meta.col
                ),
                Expression::Pair(_, Some(meta)) => eprint!(
                    "\n[[[ {}, line: {}, column: {} ]]]",
                    meta.file, meta.line, meta.col
                ),
                _ => {}
            }
            eprintln!("\n=============================================================");
        }
        if environment.error_meta.is_none() {
            match expression {
                Expression::Vector(_, Some(meta)) => {
                    environment.error_meta = Some(meta.clone());
                }
                Expression::Pair(_, Some(meta)) => {
                    environment.error_meta = Some(meta.clone());
                }
                _ => {}
            }
        }
    }
    environment.state.eval_level -= 1;
    result
}

pub fn eval<'a>(
    environment: &mut Environment,
    expression: &'a Expression,
) -> io::Result<Expression> {
    let result = eval_nr(environment, expression);
    if let Ok(res) = result {
        res.resolve(environment)
    } else {
        result
    }
}
