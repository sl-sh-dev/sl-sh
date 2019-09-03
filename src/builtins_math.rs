use std::collections::HashMap;
use std::hash::BuildHasher;
use std::io;

use crate::builtins_util::*;
use crate::types::*;

pub fn add_math_builtins<S: BuildHasher>(global: &mut HashMap<String, Expression, S>) {
    global.insert(
        "+".to_string(),
        Expression::Func(
            |env: &mut Environment, args: &[Expression]| -> io::Result<EvalResult> {
                let mut args: Vec<EvalResult> = to_args(env, args, false)?;
                if let Ok(ints) = parse_list_of_ints(&mut args) {
                    let sum: i64 = ints.iter().sum();
                    //fold(0, |sum, a| sum + a);
                    Ok(EvalResult::Atom(Atom::Int(sum)))
                } else {
                    let sum: f64 = parse_list_of_floats(&mut args)?.iter().sum();
                    Ok(EvalResult::Atom(Atom::Float(sum)))
                }
            },
        ),
    );

    global.insert(
        "*".to_string(),
        Expression::Func(
            |env: &mut Environment, args: &[Expression]| -> io::Result<EvalResult> {
                let mut args: Vec<EvalResult> = to_args(env, args, false)?;
                if let Ok(ints) = parse_list_of_ints(&mut args) {
                    let prod: i64 = ints.iter().product();
                    Ok(EvalResult::Atom(Atom::Int(prod)))
                } else {
                    let prod: f64 = parse_list_of_floats(&mut args)?.iter().product();
                    Ok(EvalResult::Atom(Atom::Float(prod)))
                }
            },
        ),
    );

    global.insert(
        "-".to_string(),
        Expression::Func(
            |env: &mut Environment, args: &[Expression]| -> io::Result<EvalResult> {
                let mut args: Vec<EvalResult> = to_args(env, args, false)?;
                if let Ok(ints) = parse_list_of_ints(&mut args) {
                    if let Some(first) = ints.first() {
                        let sum_of_rest: i64 = ints[1..].iter().sum();
                        Ok(EvalResult::Atom(Atom::Int(first - sum_of_rest)))
                    } else {
                        Err(io::Error::new(
                            io::ErrorKind::Other,
                            "expected at least one number",
                        ))
                    }
                } else {
                    let floats = parse_list_of_floats(&mut args)?;
                    if let Some(first) = floats.first() {
                        let sum_of_rest: f64 = floats[1..].iter().sum();
                        Ok(EvalResult::Atom(Atom::Float(first - sum_of_rest)))
                    } else {
                        Err(io::Error::new(
                            io::ErrorKind::Other,
                            "expected at least one number",
                        ))
                    }
                }
            },
        ),
    );

    global.insert(
        "/".to_string(),
        Expression::Func(
            |env: &mut Environment, args: &[Expression]| -> io::Result<EvalResult> {
                let mut args: Vec<EvalResult> = to_args(env, args, false)?;
                if let Ok(ints) = parse_list_of_ints(&mut args) {
                    if ints[1..].iter().any(|&x| x == 0) {
                        Err(io::Error::new(io::ErrorKind::Other, "can not divide by 0"))
                    } else if ints.len() > 1 {
                        let div: i64 = ints[1..]
                            .iter()
                            .fold(*ints.first().unwrap(), |div, a| div / a);
                        Ok(EvalResult::Atom(Atom::Int(div)))
                    } else {
                        Err(io::Error::new(
                            io::ErrorKind::Other,
                            "expected at least two numbers",
                        ))
                    }
                } else {
                    let floats = parse_list_of_floats(&mut args)?;
                    if floats[1..].iter().any(|&x| x == 0.0) {
                        Err(io::Error::new(io::ErrorKind::Other, "can not divide by 0"))
                    } else if floats.len() > 1 {
                        let div: f64 = floats[1..]
                            .iter()
                            .fold(*floats.first().unwrap(), |div, a| div / a);
                        Ok(EvalResult::Atom(Atom::Float(div)))
                    } else {
                        Err(io::Error::new(
                            io::ErrorKind::Other,
                            "expected at least two numbers",
                        ))
                    }
                }
            },
        ),
    );
}
