use crate::environment::*;
use crate::eval::*;
use crate::types::*;

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
            if let Some(v0) = v.get(0) {
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

fn do_backquote_list(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
    is_list: bool,
    meta: Option<ExpMeta>,
    bq_level: usize,
) -> Result<Expression, LispError> {
    let mut output: Vec<Expression> = Vec::new();
    for arg in args {
        if let Some(exp) = get_unquote_splice(&arg)? {
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
                        //drop(exp_d);
                        //output.push(exp);
                        return Err(LispError::new(",@ only applies to a list or vector."));
                    }
                }
            } else {
                output.push(backquote(environment, arg.clone(), bq_level)?);
            }
        } else {
            output.push(backquote(environment, arg.clone(), bq_level)?);
        }
    }
    if is_list {
        Ok(Expression::cons_from_vec(&output, meta))
    } else {
        Ok(Expression::with_list_meta(output, meta))
    }
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
            Err(LispError::new(",@ invalid outside a list."))
        } else {
            change_fn(exp, environment.interner.intern("#<unquote-splice>"))
        }
    } else if let Some(inexp) = get_backquote(&exp)? {
        let exp2 = backquote(environment, inexp, bq_level + 1)?;
        let mut output2: Vec<Expression> = Vec::new();
        output2.push(Expression::alloc_data(ExpEnum::Symbol(
            environment.interner.intern("#<back-quote>"),
            SymLoc::None,
        )));
        output2.push(backquote(environment, exp2, bq_level)?);
        if let ExpEnum::Pair(_, _) = exp.get().data {
            Ok(Expression::cons_from_vec(&output2, meta))
        } else {
            Ok(Expression::with_list_meta(output2, meta))
        }
    } else {
        let exp_d = exp.get();
        match &exp_d.data {
            ExpEnum::Vector(_) => {
                do_backquote_list(environment, &mut exp.iter(), false, meta, bq_level)
            }
            ExpEnum::Pair(_, _) => {
                do_backquote_list(environment, &mut exp.iter(), true, meta, bq_level)
            }
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

    use std::sync::atomic::AtomicBool;
    use std::sync::Arc;

    fn build_def_env() -> Environment {
        let mut environment = build_default_environment(Arc::new(AtomicBool::new(false)));
        environment.reader_state = Some(ReaderState {
            line: 0,
            column: 0,
            file_name: None,
            end_ch: None,
        });
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