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

pub fn call_lambda(
    environment: &mut Environment,
    lambda: &mut Lambda,
    args: &mut dyn Iterator<Item = Expression>,
    eval_args: bool,
) -> io::Result<Expression> {
    // DO NOT use ? in here, need to make sure the new_scope is popped off the
    // current_scope list before ending.
    let mut looping = true;
    let new_scope = build_new_scope(Some(lambda.capture.clone()));
    if let Err(err) = setup_args(
        environment,
        Some(&mut new_scope.borrow_mut()),
        lambda.params,
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
        let last_eval = match eval_nr(environment, lambda.body) {
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
            if let ExpEnum::Vector(new_args) = &last_eval.get().data {
                if recur_args != new_args.len() {
                    environment.current_scope.pop();
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "Called recur in a non-tail position.",
                    ));
                }
                let mut ib = Box::new(ListIter::new_list(&new_args));
                if let Err(err) = setup_args(environment, None, lambda.params, &mut ib, false) {
                    environment.current_scope.pop();
                    return Err(err);
                }
            }
        } else if environment.exit_code.is_none() {
            if let ExpEnum::LazyFn(lam, parts) = &last_eval.get().data {
                lambda_int = lam.clone();
                lambda = &mut lambda_int;
                looping = true;
                environment.current_scope.pop();
                // scope is popped so can use ? in this if now.
                let new_scope = build_new_scope(Some(lambda.capture.clone()));
                let mut ib = Box::new(ListIter::new_list(&parts));
                setup_args(
                    environment,
                    Some(&mut new_scope.borrow_mut()),
                    lambda.params,
                    &mut ib,
                    false,
                )?;
                environment.current_scope.push(new_scope);
            }
        }
        llast_eval = Some(last_eval);
    }
    environment.loose_symbols = old_loose;
    environment.current_scope.pop();
    Ok(llast_eval
        .unwrap_or_else(Expression::make_nil)
        .resolve(environment)?)
}

fn exec_macro(
    environment: &mut Environment,
    sh_macro: &mut Macro,
    args: &mut dyn Iterator<Item = Expression>,
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
        sh_macro.params,
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
    match eval(environment, sh_macro.body) {
        Ok(expansion) => {
            let expansion = expansion.resolve(environment)?;
            environment.current_scope.pop();
            let res = eval(environment, expansion);
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

pub fn fn_call(
    environment: &mut Environment,
    command: Expression,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    match command.get().data.clone() {
        ExpEnum::Atom(Atom::Symbol(command)) => {
            if let Some(exp) = get_expression(environment, command) {
                match exp.exp.get().data.clone() {
                    ExpEnum::Function(c) if !c.is_special_form => (c.func)(environment, &mut *args),
                    ExpEnum::Atom(Atom::Lambda(mut f)) => {
                        call_lambda(environment, &mut f, args, true)
                    }
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
        ExpEnum::Atom(Atom::Lambda(mut l)) => call_lambda(environment, &mut l, args, true),
        ExpEnum::Atom(Atom::Macro(mut m)) => exec_macro(environment, &mut m, args),
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

fn make_lazy(
    environment: &mut Environment,
    lambda: &Lambda,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    let mut parms = Vec::new();
    for p in args {
        parms.push(eval(environment, p)?);
    }
    Ok(Expression::alloc(ExpObj {
        data: ExpEnum::LazyFn(lambda.clone(), parms),
        meta: None,
    }))
}

pub fn box_slice_it<'a>(v: &'a mut [Expression]) -> Box<dyn Iterator<Item = Expression> + 'a> {
    Box::new(ListIter::new_slice(v))
}

fn fn_eval_lazy(environment: &mut Environment, expression: Expression) -> io::Result<Expression> {
    // XXX look into all these data clones...
    let exp_d = &mut expression.get_mut().data;
    let e2_d;
    let (command, mut parts) = match exp_d {
        ExpEnum::Vector(parts) => {
            let (command, parts) = match parts.split_first_mut() {
                Some((c, p)) => (c, p),
                None => {
                    return Err(io::Error::new(io::ErrorKind::Other, "No valid command."));
                }
            };
            let ib = box_slice_it(parts);
            (*command, ib)
        }
        ExpEnum::Pair(e1, e2) => {
            e2_d = e2.get();
            let e2_iter = if let ExpEnum::Vector(list) = &e2_d.data {
                Box::new(ListIter::new_list(&list))
            } else {
                e2.iter()
            };
            (*e1, e2_iter)
        }
        ExpEnum::Nil => return Ok(Expression::alloc_data(ExpEnum::Nil)),
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "Not a callable expression.",
            ))
        }
    };
    let command = if let ExpEnum::LazyFn(_, _) = &command.get().data {
        command.resolve(environment)?
    } else {
        command
    };
    let command_d = command.get().data.clone();
    match command_d {
        ExpEnum::Atom(Atom::Symbol(command)) => {
            if command.is_empty() {
                return Ok(Expression::alloc_data(ExpEnum::Nil));
            }
            let form = if environment.form_type == FormType::Any
                || environment.form_type == FormType::FormOnly
            {
                get_expression(environment, &command)
            } else {
                None
            };
            if let Some(exp) = form {
                match exp.exp.get().data.clone() {
                    ExpEnum::Function(c) => (c.func)(environment, &mut *parts),
                    ExpEnum::Atom(Atom::Lambda(mut f)) => {
                        if environment.allow_lazy_fn {
                            make_lazy(environment, &f, &mut parts)
                        } else {
                            call_lambda(environment, &mut f, &mut parts, true)
                        }
                    }
                    ExpEnum::Atom(Atom::Macro(mut m)) => {
                        exec_macro(environment, &mut m, &mut parts)
                    }
                    _ => {
                        let exp = exp.exp;
                        eval(environment, exp)
                    }
                }
            } else if environment.form_type == FormType::ExternalOnly
                || environment.form_type == FormType::Any
            {
                if command.starts_with('$') {
                    if let ExpEnum::Atom(Atom::String(command)) =
                        &str_process(environment, command).get().data
                    {
                        do_command(environment, &command, &mut parts)
                    } else {
                        let msg = format!("Not a valid form {}, not found.", command);
                        Err(io::Error::new(io::ErrorKind::Other, msg))
                    }
                } else {
                    do_command(environment, command, &mut parts)
                }
            } else {
                let msg = format!("Not a valid form {}, not found.", command);
                Err(io::Error::new(io::ErrorKind::Other, msg))
            }
        }
        ExpEnum::Vector(list) => match eval(environment, command)?.get().data.clone() {
            ExpEnum::Atom(Atom::Lambda(mut l)) => {
                if environment.allow_lazy_fn {
                    make_lazy(environment, &l, &mut parts)
                } else {
                    call_lambda(environment, &mut l, &mut parts, true)
                }
            }
            ExpEnum::Atom(Atom::Macro(mut m)) => exec_macro(environment, &mut m, &mut parts),
            ExpEnum::Function(c) => (c.func)(environment, &mut *parts),
            _ => {
                let msg = format!("Not a valid command {:?}", list);
                Err(io::Error::new(io::ErrorKind::Other, msg))
            }
        },
        ExpEnum::Pair(_, _) => match eval(environment, command)?.get().data.clone() {
            ExpEnum::Atom(Atom::Lambda(mut l)) => {
                if environment.allow_lazy_fn {
                    make_lazy(environment, &l, &mut parts)
                } else {
                    call_lambda(environment, &mut l, &mut parts, true)
                }
            }
            ExpEnum::Atom(Atom::Macro(mut m)) => exec_macro(environment, &mut m, &mut parts),
            ExpEnum::Function(c) => (c.func)(environment, &mut *parts),
            _ => {
                let msg = format!("Not a valid command {:?}", command);
                Err(io::Error::new(io::ErrorKind::Other, msg))
            }
        },
        ExpEnum::Atom(Atom::Lambda(mut l)) => {
            if environment.allow_lazy_fn {
                make_lazy(environment, &l, &mut parts)
            } else {
                call_lambda(environment, &mut l, &mut parts, true)
            }
        }
        ExpEnum::Atom(Atom::Macro(mut m)) => exec_macro(environment, &mut m, &mut parts),
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
            Expression::alloc_data(ExpEnum::Atom(Atom::StringRef(
                environment.interner.intern(&new_string),
            )))
        } else {
            Expression::alloc_data(ExpEnum::Atom(Atom::String(new_string)))
        }
    } else if environment.interner.contains(string) {
        Expression::alloc_data(ExpEnum::Atom(Atom::StringRef(
            environment.interner.intern(string),
        )))
    } else {
        Expression::alloc_data(ExpEnum::Atom(Atom::String(string.to_string())))
    }
}

fn internal_eval(environment: &mut Environment, expression: Expression) -> io::Result<Expression> {
    let mut expression = expression;
    if environment.sig_int.load(Ordering::Relaxed) {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "Script interupted by SIGINT.",
        ));
    }
    // exit was called so just return nil to unwind.
    if environment.exit_code.is_some() {
        return Ok(Expression::alloc_data(ExpEnum::Nil));
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
    let mut macro_replace = true;
    if let Some(exp) = expand_macro(environment, expression, false)? {
        let mut nv = Vec::new();
        if let ExpEnum::Vector(list) = &exp.get().data {
            for item in list {
                let item = if let ExpEnum::LazyFn(_, _) = &item.get().data {
                    item.resolve(environment)?
                } else {
                    *item
                };
                nv.push(item);
            }
        } else if let ExpEnum::Pair(_, _) = &exp.get().data {
            for item in exp.iter() {
                let item = if let ExpEnum::LazyFn(_, _) = &item.get().data {
                    item.resolve(environment)?
                } else {
                    item
                };
                nv.push(item);
            }
        } else {
            expression = exp;
            macro_replace = false;
        }
        if macro_replace {
            let exp_mut = &mut expression.get_mut().data;
            match exp_mut {
                ExpEnum::Vector(_) => {
                    exp_mut.replace(ExpEnum::Vector(nv));
                }
                ExpEnum::Pair(_, _) => {
                    exp_mut.replace(ExpEnum::cons_from_vec(&mut nv));
                }
                _ => {}
            }
        }
    }
    let exp_a = expression.get();
    let exp_d = &exp_a.data;
    match exp_d {
        ExpEnum::Vector(_) => {
            environment.last_meta = expression.meta();
            drop(exp_a);
            fn_eval_lazy(environment, expression)
        }
        ExpEnum::Pair(_, _) => {
            environment.last_meta = expression.meta();
            drop(exp_a);
            fn_eval_lazy(environment, expression)
        }
        ExpEnum::Nil => Ok(expression),
        ExpEnum::Atom(Atom::Symbol(s)) => {
            if s.starts_with('$') {
                match env::var(&s[1..]) {
                    Ok(val) => Ok(Expression::alloc_data(ExpEnum::Atom(Atom::StringRef(
                        environment.interner.intern(&val),
                    )))),
                    Err(_) => Ok(Expression::alloc_data(ExpEnum::Nil)),
                }
            } else if s.starts_with(':') {
                // Got a keyword, so just be you...
                Ok(Expression::alloc_data(ExpEnum::Atom(Atom::Symbol(s))))
            } else if let Some(exp) = get_expression(environment, s) {
                let exp = &exp.exp;
                Ok(*exp)
            } else if environment.loose_symbols {
                Ok(str_process(environment, s))
            } else {
                let msg = format!("Symbol {} not found.", s);
                Err(io::Error::new(io::ErrorKind::Other, msg))
            }
        }
        ExpEnum::HashMap(_) => Ok(expression),
        ExpEnum::Atom(Atom::String(string)) => Ok(str_process(environment, &string)),
        ExpEnum::Atom(Atom::StringRef(string)) => Ok(str_process(environment, string)),
        ExpEnum::Atom(_) => Ok(expression),
        ExpEnum::Function(_) => Ok(Expression::alloc_data(ExpEnum::Nil)),
        ExpEnum::Process(_) => Ok(expression),
        ExpEnum::File(_) => Ok(Expression::alloc_data(ExpEnum::Nil)),
        ExpEnum::LazyFn(_, _) => {
            let int_exp = expression.clone().resolve(environment)?;
            eval(environment, int_exp)
        }
    }
}

pub fn eval_nr(environment: &mut Environment, expression: Expression) -> io::Result<Expression> {
    if environment.return_val.is_some() {
        return Ok(Expression::alloc_data(ExpEnum::Nil));
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
            environment.error_expression = Some(expression);
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
                    environment.error_meta = Some(meta);
                }
            }
            eprintln!("\n=============================================================");
        }
    }
    environment.state.eval_level -= 1;
    environment.last_meta = None;
    result
}

pub fn eval(environment: &mut Environment, expression: Expression) -> io::Result<Expression> {
    let result = eval_nr(environment, expression);
    if let Ok(res) = result {
        res.resolve(environment)
    } else {
        result
    }
}

pub fn eval_data(environment: &mut Environment, data: ExpEnum) -> io::Result<Expression> {
    let data = Expression::alloc_data(data);
    eval(environment, data)
}
