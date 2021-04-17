use std::cell::RefCell;

use crate::analyze::*;
use crate::builtins::{builtin_bquote, builtin_quote};
use crate::builtins_bind::{builtin_def, builtin_var};
use crate::environment::*;
use crate::signals::test_clear_sigint;
use crate::symbols::*;
use crate::types::*;

fn setup_args(
    environment: &mut Environment,
    num_params: usize,
    has_rest: bool,
    vars: &mut dyn Iterator<Item = Expression>,
) -> Result<(), LispError> {
    let mut params = 0;
    let mut rest_data: Option<Vec<Expression>> = if has_rest { Some(Vec::new()) } else { None };
    for v in vars {
        let var = v.resolve(environment)?;
        params += 1;
        if params < num_params || (!has_rest && params == num_params) {
            environment.stack.push(Binding::with_expression(var));
        } else if let Some(rest_data) = &mut rest_data {
            rest_data.push(var);
        }
    }
    if has_rest && params < (num_params - 1) {
        return Err(LispError::new(format!(
            "wrong number of parameters, expected at least {} got {}",
            (num_params - 1),
            params,
        )));
    } else if !has_rest && params != num_params {
        return Err(LispError::new(format!(
            "wrong number of parameters, expected {} got {}",
            num_params, params,
        )));
    }
    if let Some(rest_data) = rest_data {
        if rest_data.is_empty() {
            environment
                .stack
                .push(Binding::with_expression(Expression::make_nil()));
        } else {
            environment
                .stack
                .push(Binding::with_expression(Expression::with_list(rest_data)));
        }
    }
    Ok(())
}

fn prep_stack(
    environment: &mut Environment,
    vars: &mut dyn Iterator<Item = Expression>,
    lambda: &Lambda,
    lambda_exp: Expression,
) -> Result<(), LispError> {
    let index = environment.stack.len();
    setup_args(environment, lambda.num_params, lambda.has_rest, vars)?;
    let symbols = lambda.syms.clone();
    // Push the 'this-fn' value.
    environment.stack.push(Binding::with_expression(lambda_exp));
    let mut i = 0;
    let extras = symbols.len() - (environment.stack.len() - index);
    while i < extras {
        environment.stack.push(Binding::new());
        i += 1;
    }
    symbols.stack_captures(environment, index);
    environment.stack_frames.push(StackFrame { index, symbols });
    environment.stack_frame_base = index;
    Ok(())
}

fn call_lambda_int(
    environment: &mut Environment,
    lambda_exp: Expression,
    lambda: Lambda,
    args: &mut dyn Iterator<Item = Expression>,
    eval_args: bool,
) -> Result<Expression, LispError> {
    let mut lambda_int = lambda;
    let mut lambda: &mut Lambda = &mut lambda_int;
    let mut body: &MultiExpression = &lambda.body;
    let stack_len = environment.stack.len();
    let stack_frames_len = environment.stack_frames.len();
    let stack_base = environment.stack_frame_base;
    let mut lambda_current = lambda_exp;
    if eval_args {
        let mut tvars: Vec<Expression> = Vec::new();
        for v in args {
            tvars.push(eval(environment, &v)?);
        }
        let ib = &mut tvars.iter().cloned();
        prep_stack(environment, ib, &lambda, lambda_current.clone())?;
    } else {
        prep_stack(environment, args, &lambda, lambda_current.clone())?;
    }

    let mut llast_eval: Option<Expression> = None;
    let mut looping = true;
    while looping {
        if test_clear_sigint() {
            return Err(LispError::new("Lambda interupted by SIGINT."));
        }
        let mut tmp_eval: Option<Expression> = None;
        let last_eval = match body {
            MultiExpression::None => Expression::make_nil(),
            MultiExpression::Single(body) => eval_nr(environment, &body)?,
            MultiExpression::Multiple(body) => {
                for arg in body {
                    if let Some(ret) = tmp_eval {
                        ret.resolve(environment)?;
                    }
                    tmp_eval = Some(eval_nr(environment, arg.clone())?);
                }
                if let Some(exp) = tmp_eval {
                    exp
                } else {
                    Expression::make_nil()
                }
            }
        };
        looping = environment.recur_num_args.is_some() && environment.exit_code.is_none();
        if looping {
            // This is a recur call, must be a tail call.
            let recur_args = environment.recur_num_args.unwrap();
            environment.recur_num_args = None;
            if let ExpEnum::Vector(new_args) = &last_eval.get().data {
                if recur_args != new_args.len() {
                    return Err(LispError::new("Called recur in a non-tail position."));
                }
                environment.stack.truncate(stack_len);
                environment.stack_frames.truncate(stack_frames_len);
                environment.stack_frame_base = stack_base;
                prep_stack(
                    environment,
                    &mut last_eval.iter(),
                    &lambda,
                    lambda_current.clone(),
                )?;
            }
        } else if environment.exit_code.is_none() {
            // This will detect a normal tail call and optimize it.
            if let ExpEnum::LazyFn(lam, parts) = &last_eval.get().data {
                lambda_current = lam.clone();
                let lam_d = lambda_current.get();
                if let ExpEnum::Lambda(lam) = &lam_d.data {
                    lambda_int = lam.clone();
                    drop(lam_d);
                    lambda = &mut lambda_int;
                    body = &lambda.body;
                    looping = true;
                    environment.namespace = lambda.syms.namespace().clone();
                    environment.stack.truncate(stack_len);
                    environment.stack_frames.truncate(stack_frames_len);
                    environment.stack_frame_base = stack_base;
                    let ib = &mut parts.iter().cloned();
                    prep_stack(environment, ib, &lambda, lambda_current.clone())?;
                }
            }
        }
        llast_eval = Some(last_eval);
    }
    llast_eval
        .unwrap_or_else(Expression::make_nil)
        .resolve(environment)
}

pub fn call_lambda(
    environment: &mut Environment,
    lambda_exp: Expression,
    args: &mut dyn Iterator<Item = Expression>,
    eval_args: bool,
) -> Result<Expression, LispError> {
    let lambda = if let ExpEnum::Lambda(l) = &lambda_exp.get().data {
        l.clone()
    } else if let ExpEnum::Macro(l) = &lambda_exp.get().data {
        l.clone()
    } else {
        return Err(LispError::new(format!(
            "Lambda required got {} {}.",
            lambda_exp.display_type(),
            lambda_exp
        )));
    };
    let old_ns = environment.namespace.clone();
    environment.namespace = lambda.syms.namespace().clone();
    let stack_len = environment.stack.len();
    let stack_frames_len = environment.stack_frames.len();
    let old_base = environment.stack_frame_base;
    let ret = call_lambda_int(environment, lambda_exp, lambda, args, eval_args);
    environment.stack.truncate(stack_len);
    environment.stack_frames.truncate(stack_frames_len);
    environment.stack_frame_base = old_base;
    environment.namespace = old_ns;
    ret
}

fn make_lazy(
    environment: &mut Environment,
    lambda: Expression,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let mut parms: Vec<Expression> = Vec::new();
    for p in args {
        parms.push(eval(environment, p)?);
    }
    Ok(Expression::alloc(ExpObj {
        data: ExpEnum::LazyFn(lambda, parms),
        meta: None,
        meta_tags: None,
        analyzed: RefCell::new(true),
    }))
}

fn eval_command(
    environment: &mut Environment,
    com_exp: &Expression,
    parts: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let com_exp_d = com_exp.get();
    match &com_exp_d.data {
        ExpEnum::Lambda(_) => {
            drop(com_exp_d);
            if environment.allow_lazy_fn {
                make_lazy(environment, com_exp.clone(), parts)
            } else {
                call_lambda(environment, com_exp.clone(), parts, true)
            }
        }
        ExpEnum::Function(c) => (c.func)(environment, &mut *parts),
        ExpEnum::DeclareDef => builtin_def(environment, &mut *parts),
        ExpEnum::DeclareVar => builtin_var(environment, &mut *parts),
        ExpEnum::Quote => builtin_quote(environment, &mut *parts),
        ExpEnum::BackQuote => builtin_bquote(environment, &mut *parts),
        _ => {
            let msg = format!(
                "Not a valid command {}, type {}.",
                com_exp,
                com_exp.display_type()
            );
            Err(LispError::new(msg))
        }
    }
}

fn fn_eval_lazy(
    environment: &mut Environment,
    expression: &Expression,
) -> Result<Expression, LispError> {
    let exp_d = expression.get();
    let (command, mut parts) = {
        match &exp_d.data {
            ExpEnum::Vector(_) => {}
            ExpEnum::Pair(_, _) => {}
            ExpEnum::Nil => return Ok(Expression::alloc_data(ExpEnum::Nil)),
            _ => return Err(LispError::new("Not a callable expression.")),
        }
        let mut ib = expression.iter();
        let command = if let Some(c) = ib.next() {
            c
        } else {
            return Err(LispError::new("No valid command."));
        };
        (command, ib)
    };
    let command = command.resolve(environment)?;
    let command_d = command.get();
    match &command_d.data {
        ExpEnum::Symbol(command_sym, _) => {
            if command_sym.is_empty() {
                return Ok(Expression::alloc_data(ExpEnum::Nil));
            }
            let command_sym: &'static str = command_sym; // This makes the drop happy.
            drop(command_d);
            let form = get_expression(environment, command.clone());
            if let Some(exp) = form {
                match &exp.get().data {
                    ExpEnum::Function(c) => (c.func)(environment, &mut parts),
                    ExpEnum::DeclareDef => builtin_def(environment, &mut parts),
                    ExpEnum::DeclareVar => builtin_var(environment, &mut parts),
                    ExpEnum::Quote => builtin_quote(environment, &mut *parts),
                    ExpEnum::BackQuote => builtin_bquote(environment, &mut *parts),
                    ExpEnum::Lambda(_) => {
                        if environment.allow_lazy_fn {
                            make_lazy(environment, exp.clone(), &mut parts)
                        } else {
                            call_lambda(environment, exp.clone(), &mut parts, true)
                        }
                    }
                    _ => {
                        let msg = format!("Not a valid form {}, not found.", command_sym);
                        Err(LispError::new(msg))
                    }
                }
            } else {
                let msg = format!("Not a valid form {}, not found.", command);
                Err(LispError::new(msg))
            }
        }
        ExpEnum::Vector(_) => {
            drop(command_d); // Drop the lock on command.
            let com_exp = eval(environment, &command)?;
            eval_command(environment, &com_exp, &mut parts)
        }
        ExpEnum::Pair(_, _) => {
            drop(command_d); // Drop the lock on command.
            let com_exp = eval(environment, &command)?;
            eval_command(environment, &com_exp, &mut parts)
        }
        ExpEnum::Lambda(_) => {
            if environment.allow_lazy_fn {
                make_lazy(environment, command.clone(), &mut parts)
            } else {
                call_lambda(environment, command.clone(), &mut parts, true)
            }
        }
        ExpEnum::Function(c) => (c.func)(environment, &mut *parts),
        ExpEnum::DeclareDef => builtin_def(environment, &mut *parts),
        ExpEnum::DeclareVar => builtin_var(environment, &mut *parts),
        ExpEnum::Quote => builtin_quote(environment, &mut *parts),
        ExpEnum::BackQuote => builtin_bquote(environment, &mut *parts),
        ExpEnum::Wrapper(_) => {
            drop(command_d); // Drop the lock on command.
            let com_exp = eval(environment, &command)?;
            eval_command(environment, &com_exp, &mut parts)
        }
        _ => {
            let msg = format!(
                "Not a valid command {}, type {}.",
                command.make_string(environment)?,
                command.display_type()
            );
            Err(LispError::new(msg))
        }
    }
}

fn internal_eval(
    environment: &mut Environment,
    expression_in: &Expression,
) -> Result<Expression, LispError> {
    let expression = expression_in.clone();
    if test_clear_sigint() {
        return Err(LispError::new("Script interupted by SIGINT."));
    }
    // exit was called so just return nil to unwind.
    if environment.exit_code.is_some() {
        return Ok(Expression::alloc_data(ExpEnum::Nil));
    }
    let in_recur = environment.recur_num_args.is_some();
    if in_recur {
        environment.recur_num_args = None;
        return Err(LispError::new("Called recur in a non-tail position."));
    }
    let exp_a = expression.get();
    let exp_d = &exp_a.data;
    let ret = match exp_d {
        ExpEnum::Vector(_) => {
            drop(exp_a);
            environment.last_meta = expression.meta();
            let ret = fn_eval_lazy(environment, &expression)?;
            Ok(ret)
        }
        ExpEnum::Values(v) => {
            if v.is_empty() {
                Ok(Expression::make_nil())
            } else {
                internal_eval(environment, &v[0])
            }
        }
        ExpEnum::Pair(_, _) => {
            drop(exp_a);
            environment.last_meta = expression.meta();
            let ret = fn_eval_lazy(environment, &expression)?;
            Ok(ret)
        }
        ExpEnum::Nil => Ok(expression.clone()),
        ExpEnum::Symbol(_sym, SymLoc::Ref(binding)) => Ok(binding.get()),
        ExpEnum::Symbol(sym, SymLoc::Namespace(scope, idx)) => {
            if let Some(exp) = scope.borrow().get_idx(*idx) {
                Ok(exp)
            } else {
                Err(LispError::new(format!(
                    "Symbol {} not found in namespace {}.",
                    sym,
                    scope.borrow().name()
                )))
            }
        }
        ExpEnum::Symbol(_, SymLoc::Stack(idx)) => {
            if let Some(exp) = get_expression_stack(environment, *idx) {
                Ok(exp)
            } else {
                panic!("Invalid stack reference!");
            }
        }
        ExpEnum::Symbol(s, SymLoc::None) => {
            if s.starts_with(':') {
                // Got a keyword, so just be you...
                Ok(Expression::alloc_data(ExpEnum::Symbol(s, SymLoc::None)))
            } else if let Some(exp) = get_expression(environment, expression.clone()) {
                let exp_d = exp.get();
                if let ExpEnum::Symbol(sym, _) = &exp_d.data {
                    Ok(ExpEnum::Symbol(sym, SymLoc::None).into()) // XXX TODO- better copy.
                } else {
                    drop(exp_d);
                    Ok(exp)
                }
            } else {
                let msg = format!("Symbol {} not found.", s);
                Err(LispError::new(msg))
            }
        }
        ExpEnum::HashMap(_) => Ok(expression.clone()),
        ExpEnum::String(_, _) => Ok(expression.clone()),
        ExpEnum::True => Ok(expression.clone()),
        ExpEnum::Float(_) => Ok(expression.clone()),
        ExpEnum::Int(_) => Ok(expression.clone()),
        ExpEnum::Char(_) => Ok(expression.clone()),
        ExpEnum::CodePoint(_) => Ok(expression.clone()),
        ExpEnum::Lambda(_) => Ok(expression.clone()),
        ExpEnum::Macro(_) => Ok(expression.clone()),
        ExpEnum::Function(_) => Ok(Expression::alloc_data(ExpEnum::Nil)),
        ExpEnum::DeclareDef => Ok(Expression::alloc_data(ExpEnum::Nil)),
        ExpEnum::DeclareVar => Ok(Expression::alloc_data(ExpEnum::Nil)),
        ExpEnum::Quote => Ok(Expression::alloc_data(ExpEnum::Nil)),
        ExpEnum::BackQuote => Ok(Expression::alloc_data(ExpEnum::Nil)),
        ExpEnum::Process(_) => Ok(expression.clone()),
        ExpEnum::File(_) => Ok(Expression::alloc_data(ExpEnum::Nil)),
        ExpEnum::LazyFn(_, _) => {
            let int_exp = expression.clone().resolve(environment)?;
            eval(environment, int_exp)
        }
        ExpEnum::Wrapper(exp) => {
            let exp_d = exp.get();
            match &exp_d.data {
                ExpEnum::Lambda(l) => {
                    let p = l.params.clone();
                    let mut syms = l.syms.dup();
                    syms.refresh_captures(environment)?;
                    Ok(Expression::alloc_data(ExpEnum::Lambda(Lambda {
                        params: p,
                        num_params: l.num_params,
                        has_rest: l.has_rest,
                        body: l.body.clone(),
                        syms,
                        namespace: environment.namespace.clone(),
                    })))
                }
                ExpEnum::Macro(l) => {
                    let p = l.params.clone();
                    let mut syms = l.syms.dup();
                    syms.refresh_captures(environment)?;
                    Ok(Expression::alloc_data(ExpEnum::Macro(Lambda {
                        params: p,
                        num_params: l.num_params,
                        has_rest: l.has_rest,
                        body: l.body.clone(),
                        syms,
                        namespace: environment.namespace.clone(),
                    })))
                }
                _ => {
                    drop(exp_d);
                    Ok(exp.clone())
                }
            }
        }
        ExpEnum::DeclareFn => panic!("Illegal fn state in eval, was analyze skipped?"),
        ExpEnum::DeclareMacro => panic!("Illegal fn state in eval, was analyze skipped?"),
        ExpEnum::Undefined => {
            panic!("Illegal fn state in eval, tried to eval an undefined symbol!")
        }
    };
    ret
}

pub fn eval_nr(
    environment: &mut Environment,
    expression: impl AsRef<Expression>,
) -> Result<Expression, LispError> {
    let expression = expression.as_ref();
    if environment.supress_eval {
        return Ok(expression.clone());
    }
    if environment.return_val.is_some() {
        return Ok(Expression::alloc_data(ExpEnum::Nil));
    }
    if environment.eval_level > 500 {
        return Err(LispError::new("Eval calls to deep."));
    }
    environment.eval_level += 1;
    if let Err(mut err) = analyze(environment, expression, &mut None) {
        if err.backtrace.is_none() {
            err.backtrace = Some(Vec::new());
        }
        if let Some(backtrace) = &mut err.backtrace {
            backtrace.push(expression.clone());
        }
        return Err(err);
    }
    let tres = internal_eval(environment, &expression);
    let mut result = if environment.eval_level == 1 && environment.return_val.is_some() {
        environment.return_val = None;
        Err(LispError::new("Return without matching block."))
    } else {
        tres
    };
    if let Err(err) = &mut result {
        if err.backtrace.is_none() {
            err.backtrace = Some(Vec::new());
        }
        if let Some(backtrace) = &mut err.backtrace {
            backtrace.push(expression.clone());
        }
    }
    environment.eval_level -= 1;
    environment.last_meta = None;
    result
}

pub fn eval(
    environment: &mut Environment,
    expression: impl AsRef<Expression>,
) -> Result<Expression, LispError> {
    let expression = expression.as_ref();
    eval_nr(environment, expression)?.resolve(environment)
}

pub fn eval_data(environment: &mut Environment, data: ExpEnum) -> Result<Expression, LispError> {
    let data = Expression::alloc_data(data);
    eval(environment, data)
}

pub fn eval_no_values(
    environment: &mut Environment,
    expression: impl AsRef<Expression>,
) -> Result<Expression, LispError> {
    let expression = expression.as_ref();
    let exp = eval(environment, expression)?;
    let exp_d = exp.get();
    if let ExpEnum::Values(v) = &exp_d.data {
        if v.is_empty() {
            Ok(Expression::make_nil())
        } else {
            Ok(v[0].clone())
        }
    } else {
        drop(exp_d);
        Ok(exp)
    }
}
