use crate::environment::*;
use crate::eval::*;
use crate::types::*;
use std::cell::RefCell;

macro_rules! get_form_exp {
    ($exp:expr, $form:expr) => {{
        let exp_d = $exp.get();
        match &exp_d.data {
            ExpEnum::Pair(car, cdr) => {
                if let ExpEnum::Symbol($form, _) = &car.get().data {
                    if let ExpEnum::Pair(ncar, ncdr) = &cdr.get().data {
                        if ncdr.is_nil() {
                            return Ok(Some(ncar.clone()));
                        } else {
                            return Err(LispError::new(format!(
                                "Invalid {}, takes one expression.",
                                $form
                            )));
                        }
                    }
                }
            }
            ExpEnum::Vector(v) => {
                if let Some(car) = v.get(0) {
                    if let ExpEnum::Symbol($form, _) = &car.get().data {
                        if v.len() != 2 {
                            return Err(LispError::new(format!(
                                "Invalid {}, takes one expression.",
                                $form
                            )));
                        }
                        return Ok(Some(v[1].clone()));
                    }
                }
            }
            _ => {}
        }
        Ok(None)
    }};
}

fn get_unquote(exp: &Expression) -> Result<Option<Expression>, LispError> {
    get_form_exp!(exp, "unquote")
}

fn get_unquote_tmp(exp: &Expression) -> Result<Option<Expression>, LispError> {
    get_form_exp!(exp, "#<unquote>")
}

fn get_unquote_splice(exp: &Expression) -> Result<Option<Expression>, LispError> {
    get_form_exp!(exp, "unquote-splice")
}

fn get_unquote_splice_tmp(exp: &Expression) -> Result<Option<Expression>, LispError> {
    get_form_exp!(exp, "#<unquote-splice>")
}

fn get_unquote_splice_bang(exp: &Expression) -> Result<Option<Expression>, LispError> {
    get_form_exp!(exp, "unquote-splice!")
}

fn get_unquote_splice_either(exp: &Expression) -> Result<Option<Expression>, LispError> {
    let oe = get_form_exp!(exp, "unquote-splice");
    match oe {
        Err(err) => Err(err),
        Ok(oe) => {
            if oe.is_some() {
                Ok(oe)
            } else {
                get_form_exp!(exp, "unquote-splice!")
            }
        }
    }
}

fn get_unquote_splice_bang_tmp(exp: &Expression) -> Result<Option<Expression>, LispError> {
    get_form_exp!(exp, "#<unquote-splice!>")
}

fn get_backquote(exp: &Expression) -> Result<Option<Expression>, LispError> {
    get_form_exp!(exp, "back-quote")
}

fn get_backquote_tmp(exp: &Expression) -> Result<Option<Expression>, LispError> {
    get_form_exp!(exp, "#<back-quote>")
}

fn change_fn(exp: Expression, new_fn: &'static str) -> Result<Expression, LispError> {
    let exp_d = exp.get();
    match &exp_d.data {
        ExpEnum::Vector(v) => {
            if let Some(v0) = v.first() {
                v0.get_mut().data = ExpEnum::Symbol(new_fn, SymLoc::None);
                drop(exp_d);
                Ok(exp)
            } else {
                Err(LispError::new("Invalid form, not a list or vector."))
            }
        }
        ExpEnum::Pair(car, _) => {
            car.get_mut().data = ExpEnum::Symbol(new_fn, SymLoc::None);
            drop(exp_d);
            Ok(exp)
        }
        _ => Err(LispError::new("Invalid form, not a list or vector.")),
    }
}

struct ConsIter {
    current: Option<Expression>,
}

impl ConsIter {
    fn new(exp: Expression) -> ConsIter {
        ConsIter { current: Some(exp) }
    }
}

impl Iterator for ConsIter {
    type Item = Expression;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(current) = self.current.clone() {
            let current_d = current.get();
            match &current_d.data {
                ExpEnum::Pair(_, cdr) => {
                    let last_current = current.clone();
                    match &cdr.get().data {
                        ExpEnum::Pair(_, _) => self.current = Some(cdr.clone()),
                        ExpEnum::Nil => self.current = Some(cdr.clone()),
                        _ => {
                            self.current =
                                Some(ExpEnum::Pair(cdr.clone(), Expression::make_nil()).into())
                        }
                    }
                    Some(last_current)
                }
                ExpEnum::Nil => None,
                _ => {
                    // Should not happen since dotted pairs will be made proper.
                    None
                }
            }
        } else {
            None
        }
    }
}

fn do_backquote_list(
    environment: &mut Environment,
    args: Expression,
    meta: Option<ExpMeta>,
    bq_level: usize,
) -> Result<Expression, LispError> {
    fn add_item(head: &mut ExpEnum, last_pair: &mut ExpEnum, arg: Expression) {
        let new_last_pair = ExpEnum::Pair(arg, Expression::make_nil());
        if let ExpEnum::Pair(_, cdr) = &last_pair {
            cdr.get_mut().data = new_last_pair.clone();
        }
        if let ExpEnum::Nil = head {
            *head = new_last_pair.clone();
        }
        *last_pair = new_last_pair;
    }

    let mut last_pair = ExpEnum::Nil;
    let mut head = ExpEnum::Nil;
    let mut args = PairIter::new(args);
    let mut argo = args.next();
    while let Some(arg) = argo {
        if let Some(exp) = get_unquote_splice(&arg)? {
            if args.is_dotted() {
                return Err(LispError::new("unquote-splice (,@) not valid after '.'."));
            }
            if bq_level == 0 {
                let exp = eval(environment, &exp)?;
                let exp_d = exp.get();
                match &exp_d.data {
                    ExpEnum::Vector(v) => {
                        for arg in v {
                            add_item(&mut head, &mut last_pair, arg.clone());
                        }
                    }
                    ExpEnum::Pair(_, _) => {
                        for item in exp.iter() {
                            add_item(&mut head, &mut last_pair, item);
                        }
                    }
                    ExpEnum::Nil => {}
                    _ => {
                        return Err(LispError::new(
                            "unquote-splice (,@) only applies to a list or vector.",
                        ));
                    }
                }
            } else {
                add_item(
                    &mut head,
                    &mut last_pair,
                    backquote(environment, arg.clone(), bq_level)?,
                );
            }
        } else if let Some(exp) = get_unquote_splice_bang(&arg)? {
            if args.is_dotted() {
                return Err(LispError::new("unquote-splice! (,.) not valid after '.'."));
            }
            if bq_level == 0 {
                let exp = eval(environment, &exp)?;
                let exp_d = exp.get();
                match &exp_d.data {
                    ExpEnum::Vector(v) => {
                        for arg in v {
                            add_item(&mut head, &mut last_pair, arg.clone());
                        }
                    }
                    ExpEnum::Pair(_, _) => {
                        drop(exp_d);
                        if let ExpEnum::Pair(_, cdr) = &last_pair {
                            cdr.get_mut().data = exp.get().data.clone();
                        }
                        if let ExpEnum::Nil = head {
                            head = last_pair.clone();
                        }
                        // unwrap() should be safe since exp is a pair and the iterator will
                        // return at least one thing.
                        last_pair = ConsIter::new(exp).last().unwrap().into();
                    }
                    ExpEnum::Nil => {}
                    _ => {
                        return Err(LispError::new(
                            "unquote-splice! (,.) only applies to a list or vector.",
                        ));
                    }
                }
            } else {
                add_item(
                    &mut head,
                    &mut last_pair,
                    backquote(environment, arg.clone(), bq_level)?,
                );
            }
        } else {
            let arg = backquote(environment, arg.clone(), bq_level)?;
            if args.is_dotted() {
                if let ExpEnum::Pair(_, cdr) = &last_pair {
                    let mut cdr = cdr.get_mut();
                    cdr.data = arg.get().data.clone();
                    cdr.meta = arg.get().meta;
                }
            } else {
                add_item(&mut head, &mut last_pair, arg);
            }
        }
        argo = args.next();
    }
    let head = Expression::alloc(ExpObj {
        data: head,
        meta,
        meta_tags: None,
        analyzed: RefCell::new(false),
    });
    Ok(head)
}

fn do_backquote_vector(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
    meta: Option<ExpMeta>,
    bq_level: usize,
) -> Result<Expression, LispError> {
    let mut output: Vec<Expression> = Vec::new();
    for arg in args {
        if let Some(exp) = get_unquote_splice_either(&arg)? {
            if bq_level == 0 {
                let exp = eval(environment, &exp)?;
                let exp_d = exp.get();
                match &exp_d.data {
                    ExpEnum::Vector(v) => {
                        for arg in v {
                            output.push(arg.clone());
                        }
                    }
                    ExpEnum::Pair(_, _) => {
                        for item in exp.iter() {
                            output.push(item);
                        }
                    }
                    ExpEnum::Nil => {}
                    _ => {
                        return Err(LispError::new(
                            "unquote-splice (,@) only applies to a list or vector.",
                        ));
                    }
                }
            } else {
                output.push(backquote(environment, arg.clone(), bq_level)?);
            }
        } else {
            output.push(backquote(environment, arg.clone(), bq_level)?);
        }
    }
    Ok(Expression::with_list_meta(output, meta))
}

fn backquote(
    environment: &mut Environment,
    exp: Expression,
    bq_level: usize,
) -> Result<Expression, LispError> {
    let meta = exp.meta();
    if let Some(inexp) = get_unquote(&exp)? {
        if bq_level == 0 {
            eval(environment, inexp)
        } else {
            change_fn(exp, environment.interner.intern("#<unquote>"))
        }
    } else if get_unquote_splice(&exp)?.is_some() {
        if bq_level == 0 {
            Err(LispError::new(
                "unquote-splice (,@) invalid outside a list.",
            ))
        } else {
            change_fn(exp, environment.interner.intern("#<unquote-splice>"))
        }
    } else if get_unquote_splice_bang(&exp)?.is_some() {
        if bq_level == 0 {
            Err(LispError::new(
                "unquote-splice! (,.) invalid outside a list.",
            ))
        } else {
            change_fn(exp, environment.interner.intern("#<unquote-splice!>"))
        }
    } else if let Some(inexp) = get_backquote(&exp)? {
        let exp2 = backquote(environment, inexp, bq_level + 1)?;
        let output2 = vec![
            Expression::alloc_data(ExpEnum::Symbol(
                environment.interner.intern("#<back-quote>"),
                SymLoc::None,
            )),
            backquote(environment, exp2, bq_level)?,
        ];
        if let ExpEnum::Pair(_, _) = exp.get().data {
            Ok(Expression::cons_from_vec(&output2, meta))
        } else {
            Ok(Expression::with_list_meta(output2, meta))
        }
    } else {
        let exp_d = exp.get();
        match &exp_d.data {
            ExpEnum::Vector(_) => do_backquote_vector(environment, &mut exp.iter(), meta, bq_level),
            ExpEnum::Pair(_, _) => do_backquote_list(environment, exp.clone(), meta, bq_level),
            _ => {
                drop(exp_d);
                Ok(exp)
            }
        }
    }
}

fn scrub_backquote_list(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<(), LispError> {
    for arg in args {
        scrub_backquote(environment, arg)?;
    }
    Ok(())
}

fn scrub_backquote(environment: &mut Environment, exp: Expression) -> Result<(), LispError> {
    if let Some(inexp) = get_unquote_tmp(&exp)? {
        change_fn(exp.clone(), "unquote")?;
        scrub_backquote(environment, inexp)?;
    } else if let Some(inexp) = get_unquote_splice_tmp(&exp)? {
        change_fn(exp.clone(), "unquote-splice")?;
        scrub_backquote(environment, inexp)?;
    } else if let Some(inexp) = get_unquote_splice_bang_tmp(&exp)? {
        change_fn(exp.clone(), "unquote-splice!")?;
        scrub_backquote(environment, inexp)?;
    } else if let Some(inexp) = get_backquote_tmp(&exp)? {
        change_fn(exp.clone(), "back-quote")?;
        scrub_backquote(environment, inexp)?;
    } else {
        match &exp.get().data {
            ExpEnum::Vector(_) => scrub_backquote_list(environment, &mut exp.iter())?,
            ExpEnum::Pair(_, _) => scrub_backquote_list(environment, &mut exp.iter())?,
            _ => {}
        }
    }
    Ok(())
}

pub fn bquote(environment: &mut Environment, exp: Expression) -> Result<Expression, LispError> {
    let exp = backquote(environment, exp, 0)?;
    scrub_backquote(environment, exp.clone())?;
    Ok(exp)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::reader::read;

    fn build_def_env() -> Environment {
        let environment = build_default_environment();
        environment
    }

    #[test]
    fn test_get_unquote() -> Result<(), LispError> {
        let mut environment = build_def_env();
        let exp = read(&mut environment, "(unquote 1)", None, false).unwrap();
        if let Some(exp) = get_unquote(&exp)? {
            if let ExpEnum::Int(i) = exp.get().data {
                assert!(i == 1);
            } else {
                assert!(false);
            }
        } else {
            assert!(false);
        }
        let exp = read(&mut environment, "(unquote 1 2)", None, false).unwrap();
        assert!(get_unquote(&exp).is_err());
        let exp = read(&mut environment, "(1 unquote 1 2)", None, false).unwrap();
        assert!(get_unquote(&exp)?.is_none());
        let exp = read(&mut environment, "(symbol unquote)", None, false).unwrap();
        assert!(get_unquote(&exp)?.is_none());
        let exp = read(&mut environment, "(unquote (1 2))", None, false).unwrap();
        if let Some(exp) = get_unquote(&exp)? {
            if let ExpEnum::Pair(car, _cdr) = &exp.get().data {
                if let ExpEnum::Int(i) = car.get().data {
                    assert!(i == 1);
                } else {
                    assert!(false);
                }
            } else {
                assert!(false);
            }
        } else {
            assert!(false);
        }
        Ok(())
    }

    #[test]
    fn test_get_unquote_splice() -> Result<(), LispError> {
        let mut environment = build_def_env();
        let exp = read(&mut environment, "(unquote-splice 1)", None, false).unwrap();
        if let Some(exp) = get_unquote_splice(&exp)? {
            if let ExpEnum::Int(i) = exp.get().data {
                assert!(i == 1);
            } else {
                assert!(false);
            }
        } else {
            assert!(false);
        }
        let exp = read(&mut environment, "(unquote-splice 1 2)", None, false).unwrap();
        assert!(get_unquote_splice(&exp).is_err());
        let exp = read(&mut environment, "(1 unquote-splice 1 2)", None, false).unwrap();
        assert!(get_unquote_splice(&exp)?.is_none());
        let exp = read(&mut environment, "(unquote-splice (1 2))", None, false).unwrap();
        if let Some(exp) = get_unquote_splice(&exp)? {
            if let ExpEnum::Pair(car, _cdr) = &exp.get().data {
                if let ExpEnum::Int(i) = car.get().data {
                    assert!(i == 1);
                } else {
                    assert!(false);
                }
            } else {
                assert!(false);
            }
        } else {
            assert!(false);
        }
        Ok(())
    }

    /*#[test]
    fn test_backquote() -> Result<(), LispError> {
        let mut environment = build_def_env();
        let exp = read(
            &mut environment,
            //"``(1 2 ,',(+ 3 5) ,@'#(4 5 6))",
            "`(1 ```,,@,,@(list (+ 1 2)) 4)",
            //"`#(1 ,@(list 1 2) 4)",
            //"`(1 `,(+ 1 ,(+ 2 3)) 4)",
            None,
            false,
        )
        .unwrap();
        //let exp = read(&mut environment, "`(1 2 ,(+ 3 5) ,@'#(4 5 6))", None, false).unwrap();
        //let exp = read(&mut environment, "`(1 `,(+ 1 ,(+ 2 3)) 4)", None, false).unwrap();
        if let Some(exp) = get_backquote(&exp)? {
            println!("XXXX {}", bquote(&mut environment, exp)?);
            assert!(false);
        }
        Ok(())
    }*/
}
