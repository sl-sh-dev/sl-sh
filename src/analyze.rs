use crate::builtins::expand_macro;
use crate::environment::*;
use crate::symbols::*;
use crate::types::*;

pub fn make_fn(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
    outer_syms: &Option<Symbols>,
) -> Result<Lambda, LispError> {
    if let Some(params_exp) = args.next() {
        let params_d = params_exp.get();
        let (params_exp, no_recur) = if let ExpEnum::Symbol(":no-recur", _) = &params_d.data {
            if let Some(params_exp) = args.next() {
                (params_exp, true)
            } else {
                return Err(LispError::new(
                    "fn: needs at least one form after :no-recur",
                ));
            }
        } else {
            drop(params_d);
            (params_exp, false)
        };
        let (first, second) = (args.next(), args.next());
        let body = if let Some(first) = first {
            if let Some(second) = second {
                // The copies below are important, otherwise you will get shared
                // structure errors when analyzing and calling the lambda.
                let mut body = vec![first.copy(), second.copy()];
                for arg in args {
                    body.push(arg.copy());
                }
                MultiExpression::Multiple(body)
            } else {
                MultiExpression::Single(first.copy())
            }
        } else {
            MultiExpression::None
        };
        let mut params = Vec::new();
        let mut syms = Symbols::with_frame(environment, outer_syms);
        let mut has_rest = false;
        let mut num_params = 0;
        let mut num_post_rest = 0;
        for p in params_exp.iter() {
            if let ExpEnum::Symbol(s, _) = p.get().data {
                params.push(s);
                if s == "&rest" {
                    if has_rest {
                        return Err(LispError::new("fn: &rest can only appear once"));
                    }
                    has_rest = true;
                } else {
                    syms.insert(s);
                    num_params += 1;
                    if has_rest {
                        num_post_rest += 1;
                    }
                }
            } else {
                return Err(LispError::new(format!(
                    "fn: parameters must be symbols, got {}",
                    p
                )));
            }
        }
        if has_rest && num_post_rest != 1 {
            return Err(LispError::new(
                "fn: &rest must be before the last parameter",
            ));
        }
        if !no_recur {
            syms.insert("this-fn");
        }
        match &body {
            MultiExpression::None => {}
            MultiExpression::Single(arg) => analyze(environment, arg, &mut Some(syms.clone()))?,
            MultiExpression::Multiple(body) => {
                for arg in body {
                    analyze(environment, arg, &mut Some(syms.clone()))?;
                }
            }
        }
        return Ok(Lambda {
            params,
            num_params,
            has_rest,
            body,
            syms,
            namespace: environment.namespace.clone(),
            no_recur,
        });
    }
    Err(LispError::new("fn: needs at least one form"))
}

fn declare_var(
    args: &mut dyn Iterator<Item = Expression>,
    syms: &mut Option<Symbols>,
) -> Result<(), LispError> {
    if let Some(syms) = syms {
        if let Some(key) = args.next() {
            let mut key_d = key.get_mut();
            match &mut key_d.data {
                ExpEnum::Symbol(s, location) => {
                    if syms.contains_symbol(s) {
                        Err(LispError::new(format!(
                            "var: Symbol {} already defined in scope.",
                            s
                        )))
                    } else {
                        let idx = syms.insert(*s);
                        location.replace(SymLoc::Stack(idx));
                        Ok(())
                    }
                }
                _ => {
                    drop(key_d);
                    Err(LispError::new(format!(
                        "var: First form (binding key) must be a symbol, got {}- {}",
                        key.display_type(),
                        key
                    )))
                }
            }
        } else {
            Err(LispError::new("var: Requires a symbol."))
        }
    } else {
        Err(LispError::new(
            "var: Using var outside a lambda not allowed.",
        ))
    }
}

fn patch_symbol(
    environment: &mut Environment,
    syms: &mut Option<Symbols>,
    name: &'static str,
    location: &mut SymLoc,
) {
    if let SymLoc::None = location {
        if let Some(syms) = syms {
            if let Some(idx) = syms.get(name) {
                location.replace(SymLoc::Stack(idx));
            } else if syms.can_capture(name) {
                let idx = syms.insert_capture(name, environment);
                location.replace(SymLoc::Stack(idx));
            } else if let Some(binding) = syms.namespace().borrow().get_with_outer(name) {
                location.replace(SymLoc::Ref(binding));
            }
        } else if let Some(binding) = environment.namespace.borrow().get_with_outer(name) {
            location.replace(SymLoc::Ref(binding));
        }
    }
}

fn backquote_syms(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
    syms: &mut Option<Symbols>,
) {
    for exp in args {
        let mut arg_d = exp.get_mut();
        match &mut arg_d.data {
            ExpEnum::Symbol(s, loc) => {
                patch_symbol(environment, syms, s, loc);
            }
            ExpEnum::Vector(_) => {
                drop(arg_d);
                backquote_syms(environment, &mut exp.iter(), syms);
            }
            ExpEnum::Pair(_, _) => {
                drop(arg_d);
                backquote_syms(environment, &mut exp.iter(), syms);
            }
            _ => {}
        }
    }
}

fn is_quoted(environment: &mut Environment, args: &mut dyn Iterator<Item = Expression>) -> bool {
    if let Some(car) = args.next() {
        let car_d = car.get();
        if let ExpEnum::Symbol(_s, _location) = &car_d.data {
            drop(car_d);
            let form = get_expression_look(environment, car.clone(), true);
            if let Some(form_exp) = form {
                let exp_d = form_exp.get();
                return matches!(
                    &exp_d.data,
                    ExpEnum::Quote
                        | ExpEnum::BackQuote
                        | ExpEnum::DeclareFn
                        | ExpEnum::DeclareMacro
                );
            }
        }
    }
    false
}

fn analyze_seq(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
    syms: &mut Option<Symbols>,
) -> Result<(Option<ExpEnum>, bool), LispError> {
    if let Some(car) = args.next() {
        let car_d = car.get();
        if let ExpEnum::Symbol(_sym, _location) = &car_d.data {
            drop(car_d);
            let form = get_expression_look(environment, car.clone(), true);
            if let Some(form_exp) = form {
                let exp_d = form_exp.get();
                match &exp_d.data {
                    ExpEnum::DeclareFn => {
                        drop(exp_d);
                        let lambda = make_fn(environment, args, syms)?;
                        let lambda: Expression = ExpEnum::Lambda(lambda).into();
                        return Ok((Some(ExpEnum::Wrapper(lambda)), false));
                    }
                    ExpEnum::DeclareMacro => {
                        drop(exp_d);
                        let lambda = make_fn(environment, args, syms)?;
                        let lambda: Expression = ExpEnum::Macro(lambda).into();
                        return Ok((Some(ExpEnum::Wrapper(lambda)), false));
                    }
                    ExpEnum::DeclareDef => {}
                    ExpEnum::DeclareVar => {
                        drop(exp_d);
                        declare_var(args, syms)?;
                    }
                    ExpEnum::Quote => {
                        drop(exp_d);
                        if let ExpEnum::Symbol(name, loc) = &mut car.get_mut().data {
                            // Patch the 'quote' symbol so eval will work.
                            patch_symbol(environment, syms, name, loc);
                        }
                        if args.next().is_some() && args.next().is_none() {
                            return Ok((None, false));
                        }
                        return Err(LispError::new("quote: Takes one form."));
                    }
                    ExpEnum::BackQuote => {
                        drop(exp_d);
                        if let ExpEnum::Symbol(name, loc) = &mut car.get_mut().data {
                            patch_symbol(environment, syms, name, loc);
                        }
                        if let Some(arg) = args.next() {
                            let mut arg_d = arg.get_mut();
                            match &mut arg_d.data {
                                ExpEnum::Symbol(s, loc) => {
                                    patch_symbol(environment, syms, s, loc);
                                }
                                ExpEnum::Vector(_) => {
                                    drop(arg_d);
                                    backquote_syms(environment, &mut arg.iter(), syms);
                                }
                                ExpEnum::Pair(_, _) => {
                                    drop(arg_d);
                                    backquote_syms(environment, &mut arg.iter(), syms);
                                }
                                _ => {}
                            }
                        } else {
                            return Err(LispError::new("back-quote: takes one form"));
                        };
                        if args.next().is_some() {
                            return Err(LispError::new("back-quote: takes one form"));
                        }
                        return Ok((None, false));
                    }
                    _ => {
                        //eprintln!("WARNING: Non callable symbol {} of type {} in function call.", sym, form_exp.display_type());
                    }
                }
            } else {
                //eprintln!("WARNING: Unbound symbol {} in function call.", sym);
            }
        }
    }
    Ok((None, true))
}

fn analyze_expand(environment: &mut Environment, expression: Expression) -> Result<(), LispError> {
    let quoted = {
        let exp_d = expression.get();
        match &exp_d.data {
            ExpEnum::Vector(_) | ExpEnum::Pair(_, _) => {
                drop(exp_d);
                is_quoted(environment, &mut expression.iter())
            }
            _ => true,
        }
    };
    if !quoted {
        if let Some(exp) = expand_macro(environment, &expression, false, 0)? {
            expression.get_mut().data.replace(exp.into());
        }
        {
            let exp_d = expression.get();
            if let ExpEnum::Vector(v) = &exp_d.data {
                let v = v.clone();
                drop(exp_d);
                for exp in v {
                    analyze_expand(environment, exp)?;
                }
            } else if let ExpEnum::Pair(_car, _cdr) = &exp_d.data {
                drop(exp_d);
                for exp in expression.iter() {
                    analyze_expand(environment, exp)?;
                }
            }
        }
    }

    Ok(())
}

fn analyze_prep(
    environment: &mut Environment,
    expression: Expression,
    syms: &mut Option<Symbols>,
) -> Result<(), LispError> {
    {
        let exp_d = expression.get();
        match &exp_d.data {
            ExpEnum::Vector(_) | ExpEnum::Pair(_, _) => {
                drop(exp_d);
                let (exp_enum, do_list) = analyze_seq(environment, &mut expression.iter(), syms)?;
                if let Some(exp_enum) = exp_enum {
                    expression.get_mut().data.replace(exp_enum);
                } else if do_list {
                    for exp in expression.iter() {
                        analyze_prep(environment, exp, syms)?;
                    }
                }
            }
            ExpEnum::Symbol(_, _) => {
                drop(exp_d);
                if let ExpEnum::Symbol(s, location) = &mut expression.get_mut().data {
                    patch_symbol(environment, syms, s, location);
                }
            }
            _ => {}
        }
    }

    *expression.get().analyzed.borrow_mut() = true;
    Ok(())
}

pub fn analyze(
    environment: &mut Environment,
    expression_in: &Expression,
    syms: &mut Option<Symbols>,
) -> Result<(), LispError> {
    if *expression_in.get().analyzed.borrow() {
        return Ok(());
    }
    let expression = expression_in.clone();
    analyze_expand(environment, expression.clone())?;
    analyze_prep(environment, expression, syms)
}
