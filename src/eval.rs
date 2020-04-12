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

pub fn call_lambda<'a>(
    environment: &mut Environment,
    lambda: &Lambda,
    args: Box<dyn Iterator<Item = &Expression> + 'a>,
    eval_args: bool,
) -> io::Result<Expression> {
    // DO NOT use ? in here, need to make sure the new_scope is popped off the
    // current_scope list before ending.
    let mut looping = true;
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
    let mut llast_eval: Option<Expression> = None;
    while looping {
        if environment.sig_int.load(Ordering::Relaxed) {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "Lambda interupted by SIGINT.",
            ));
        }
        let last_eval = match eval_nr(environment, &lambda.body) {
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
            if let ExpEnum::Vector(new_args) = last_eval.get() {
                if recur_args != new_args.len() {
                    environment.current_scope.pop();
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "Called recur in a non-tail position.",
                    ));
                }
                let ib = Box::new(new_args.iter());
                if let Err(err) = setup_args(environment, None, &lambda.params, ib, false) {
                    environment.current_scope.pop();
                    return Err(err);
                }
            }
        } else if environment.exit_code.is_none() {
            if let ExpEnum::LazyFn(lam, parts) = last_eval.get() {
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
        llast_eval = Some(last_eval);
    }
    environment.loose_symbols = old_loose;
    environment.current_scope.pop();
    Ok(llast_eval.unwrap_or_else(|| Expression::make_nil(&mut environment.gc)).resolve(environment)?)
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
    match command.get() {
        ExpEnum::Atom(Atom::Symbol(command)) => {
            if let Some(exp) = get_expression(environment, &command) {
                match &exp.exp.get() {
                    ExpEnum::Function(c) if !c.is_special_form => (c.func)(environment, &mut *args),
                    ExpEnum::Atom(Atom::Lambda(f)) => call_lambda(environment, &f, args, true),
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
        ExpEnum::Atom(Atom::Lambda(l)) => call_lambda(environment, &l, args, true),
        ExpEnum::Atom(Atom::Macro(m)) => exec_macro(environment, &m, args),
        ExpEnum::Function(c) if !c.is_special_form => (c.func)(environment, &mut *args),
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
    Ok(Expression::alloc(
        &mut environment.gc,
        ExpObj {
            data: ExpEnum::LazyFn(lambda.clone(), parms),
            meta: None,
        },
    ))
}

fn box_slice_it<'a>(v: &'a [Expression]) -> Box<dyn Iterator<Item = &Expression> + 'a> {
    Box::new(v.iter())
}

fn fn_eval_lazy(environment: &mut Environment, expression: &Expression) -> io::Result<Expression> {
    let (command, mut parts) = match expression.get() {
        ExpEnum::Vector(parts) => {
            let (command, parts) = match parts.split_first() {
                Some((c, p)) => (c, p),
                None => {
                    return Err(io::Error::new(io::ErrorKind::Other, "No valid command."));
                }
            };
            let ib = box_slice_it(parts);
            (command, ib)
        }
        ExpEnum::Pair(e1, e2) => (e1, e2.iter()),
        ExpEnum::Nil => return Ok(Expression::alloc_data(&mut environment.gc, ExpEnum::Nil)),
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "Not a callable expression.",
            ))
        }
    };
    let com_int;
    let command = if let ExpEnum::LazyFn(_, _) = command.get() {
        com_int = command.resolve(environment)?;
        &com_int
    } else {
        command
    };
    match command.get() {
        ExpEnum::Atom(Atom::Symbol(command)) => {
            if command.is_empty() {
                return Ok(Expression::alloc_data(&mut environment.gc, ExpEnum::Nil));
            }
            let form = if environment.form_type == FormType::Any
                || environment.form_type == FormType::FormOnly
            {
                get_expression(environment, &command)
            } else {
                None
            };
            if let Some(exp) = form {
                match &exp.exp.get() {
                    ExpEnum::Function(c) => (c.func)(environment, &mut *parts),
                    ExpEnum::Atom(Atom::Lambda(f)) => {
                        if environment.allow_lazy_fn {
                            make_lazy(environment, f, parts)
                        } else {
                            call_lambda(environment, &f, parts, true)
                        }
                    }
                    ExpEnum::Atom(Atom::Macro(m)) => exec_macro(environment, &m, parts),
                    _ => {
                        let exp = exp.exp.clone();
                        eval(environment, &exp)
                    }
                }
            } else if environment.form_type == FormType::ExternalOnly
                || environment.form_type == FormType::Any
            {
                if command.starts_with('$') {
                    if let ExpEnum::Atom(Atom::String(command)) =
                        str_process(environment, command).get()
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
        ExpEnum::Vector(list) => {
            //match eval(environment, &Expression::alloc_data(&mut environment, ExpEnum::Vector(list.clone())))?.get() {
            match eval(environment, command)?.get() {
                ExpEnum::Atom(Atom::Lambda(l)) => {
                    if environment.allow_lazy_fn {
                        make_lazy(environment, &l, parts)
                    } else {
                        call_lambda(environment, &l, parts, true)
                    }
                }
                ExpEnum::Atom(Atom::Macro(m)) => exec_macro(environment, &m, parts),
                ExpEnum::Function(c) => (c.func)(environment, &mut *parts),
                _ => {
                    let msg = format!("Not a valid command {:?}", list);
                    Err(io::Error::new(io::ErrorKind::Other, msg))
                }
            }
        }
        ExpEnum::Pair(_, _) => match eval(environment, command)?.get() {
            ExpEnum::Atom(Atom::Lambda(l)) => {
                if environment.allow_lazy_fn {
                    make_lazy(environment, &l, parts)
                } else {
                    call_lambda(environment, &l, parts, true)
                }
            }
            ExpEnum::Atom(Atom::Macro(m)) => exec_macro(environment, &m, parts),
            ExpEnum::Function(c) => (c.func)(environment, &mut *parts),
            _ => {
                let msg = format!("Not a valid command {:?}", command);
                Err(io::Error::new(io::ErrorKind::Other, msg))
            }
        },
        ExpEnum::Atom(Atom::Lambda(l)) => {
            if environment.allow_lazy_fn {
                make_lazy(environment, &l, parts)
            } else {
                call_lambda(environment, &l, parts, true)
            }
        }
        ExpEnum::Atom(Atom::Macro(m)) => exec_macro(environment, &m, parts),
        ExpEnum::Function(c) => (c.func)(environment, &mut *parts),
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

fn str_process(environment: &mut Environment, string: &str) -> Expression {
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
            Expression::alloc_data(
                &mut environment.gc,
                ExpEnum::Atom(Atom::StringRef(environment.interner.intern(&new_string))),
            )
        } else {
            Expression::alloc_data(&mut environment.gc, ExpEnum::Atom(Atom::String(new_string)))
        }
    } else if environment.interner.contains(string) {
        Expression::alloc_data(
            &mut environment.gc,
            ExpEnum::Atom(Atom::StringRef(environment.interner.intern(string))),
        )
    } else {
        Expression::alloc_data(
            &mut environment.gc,
            ExpEnum::Atom(Atom::String(string.to_string())),
        )
    }
}

fn internal_eval(environment: &mut Environment, expression: &Expression) -> io::Result<Expression> {
    let mut expression = expression;
    if environment.sig_int.load(Ordering::Relaxed) {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "Script interupted by SIGINT.",
        ));
    }
    // exit was called so just return nil to unwind.
    if environment.exit_code.is_some() {
        return Ok(Expression::alloc_data(&mut environment.gc, ExpEnum::Nil));
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
    let texpression;
    let mut macro_replace = true;
    if let Some(exp) = expand_macro(environment, expression, false)? {
        let mut nv = Vec::new();
        if let ExpEnum::Vector(list) = exp.get() {
            for item in list {
                let item = if let ExpEnum::LazyFn(_, _) = item.get() {
                    item.resolve(environment)?
                } else {
                    item.clone()
                };
                nv.push(item);
            }
        } else if let ExpEnum::Pair(_, _) = exp.get() {
            for item in exp.iter() {
                let item = if let ExpEnum::LazyFn(_, _) = item.get() {
                    item.resolve(environment)?
                } else {
                    item.clone()
                };
                nv.push(item);
            }
        } else {
            texpression = exp.clone();
            expression = &texpression;
            macro_replace = false;
        }
        if macro_replace {
            match expression.get() {
                ExpEnum::Vector(_) => {
                    expression.get_mut().replace(ExpEnum::Vector(nv));
                }
                ExpEnum::Pair(_, _) => {
                    expression
                        .get_mut()
                        .replace(ExpEnum::cons_from_vec(&mut environment.gc, &mut nv));
                }
                _ => {}
            }
        }
    }
    match expression.get() {
        ExpEnum::Vector(_) => {
            environment.last_meta = expression.meta().clone();
            fn_eval_lazy(environment, expression)
        }
        ExpEnum::Pair(_, _) => {
            environment.last_meta = expression.meta().clone();
            fn_eval_lazy(environment, expression)
        }
        ExpEnum::Nil => Ok(*expression),
        ExpEnum::Atom(Atom::Symbol(s)) => {
            if s.starts_with('$') {
                match env::var(&s[1..]) {
                    Ok(val) => Ok(Expression::alloc_data(
                        &mut environment.gc,
                        ExpEnum::Atom(Atom::StringRef(environment.interner.intern(&val))),
                    )),
                    Err(_) => Ok(Expression::alloc_data(&mut environment.gc, ExpEnum::Nil)),
                }
            } else if s.starts_with(':') {
                // Got a keyword, so just be you...
                Ok(Expression::alloc_data(
                    &mut environment.gc,
                    ExpEnum::Atom(Atom::Symbol(s)),
                ))
            } else if let Some(exp) = get_expression(environment, s) {
                let exp = &exp.exp;
                Ok(exp.clone())
            } else if environment.loose_symbols {
                Ok(str_process(environment, s))
            } else {
                let msg = format!("Symbol {} not found.", s);
                Err(io::Error::new(io::ErrorKind::Other, msg))
            }
        }
        ExpEnum::HashMap(_) => Ok(*expression),
        ExpEnum::Atom(Atom::String(string)) => Ok(str_process(environment, &string)),
        ExpEnum::Atom(Atom::StringRef(string)) => Ok(str_process(environment, string)),
        ExpEnum::Atom(_) => Ok(*expression),
        ExpEnum::Function(_) => Ok(Expression::alloc_data(&mut environment.gc, ExpEnum::Nil)),
        ExpEnum::Process(_) => Ok(*expression),
        ExpEnum::File(_) => Ok(Expression::alloc_data(&mut environment.gc, ExpEnum::Nil)),
        ExpEnum::LazyFn(_, _) => {
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
        return Ok(Expression::alloc_data(&mut environment.gc, ExpEnum::Nil));
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
            if let Some(meta) = expression.meta() {
                eprint!(
                    "\n[[[ {}, line: {}, column: {} ]]]",
                    meta.file, meta.line, meta.col
                );
                if environment.error_meta.is_none() {
                    environment.error_meta = Some(meta.clone());
                }
            }
            eprintln!("\n=============================================================");
        }
    }
    environment.state.eval_level -= 1;
    environment.last_meta = None;
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

pub fn eval_data(environment: &mut Environment, data: ExpEnum) -> io::Result<Expression> {
    let data = Expression::alloc_data(&mut environment.gc, data);
    eval(environment, &data)
}
