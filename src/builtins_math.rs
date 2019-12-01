use std::collections::HashMap;
use std::hash::BuildHasher;
use std::io;
use std::rc::Rc;

use crate::builtins_util::*;
use crate::environment::*;
use crate::types::*;

pub fn add_math_builtins<S: BuildHasher>(data: &mut HashMap<String, Rc<Expression>, S>) {
    data.insert(
        "+".to_string(),
        Rc::new(Expression::Func(
            |environment: &mut Environment, args: &[Expression]| -> io::Result<Expression> {
                let mut args = list_to_args(environment, args, true)?;
                if let Ok(ints) = parse_list_of_ints(environment, &mut args) {
                    let sum: i64 = ints.iter().sum();
                    //fold(0, |sum, a| sum + a);
                    Ok(Expression::Atom(Atom::Int(sum)))
                } else {
                    let sum: f64 = parse_list_of_floats(environment, &mut args)?.iter().sum();
                    Ok(Expression::Atom(Atom::Float(sum)))
                }
            },
        )),
    );

    data.insert(
        "*".to_string(),
        Rc::new(Expression::Func(
            |environment: &mut Environment, args: &[Expression]| -> io::Result<Expression> {
                let mut args = list_to_args(environment, args, true)?;
                if let Ok(ints) = parse_list_of_ints(environment, &mut args) {
                    let prod: i64 = ints.iter().product();
                    Ok(Expression::Atom(Atom::Int(prod)))
                } else {
                    let prod: f64 = parse_list_of_floats(environment, &mut args)?
                        .iter()
                        .product();
                    Ok(Expression::Atom(Atom::Float(prod)))
                }
            },
        )),
    );

    data.insert(
        "-".to_string(),
        Rc::new(Expression::Func(
            |environment: &mut Environment, args: &[Expression]| -> io::Result<Expression> {
                let mut args = list_to_args(environment, args, true)?;
                if let Ok(ints) = parse_list_of_ints(environment, &mut args) {
                    if let Some(first) = ints.first() {
                        let sum_of_rest: i64 = ints[1..].iter().sum();
                        Ok(Expression::Atom(Atom::Int(first - sum_of_rest)))
                    } else {
                        Err(io::Error::new(
                            io::ErrorKind::Other,
                            "expected at least one number",
                        ))
                    }
                } else {
                    let floats = parse_list_of_floats(environment, &mut args)?;
                    if let Some(first) = floats.first() {
                        let sum_of_rest: f64 = floats[1..].iter().sum();
                        Ok(Expression::Atom(Atom::Float(first - sum_of_rest)))
                    } else {
                        Err(io::Error::new(
                            io::ErrorKind::Other,
                            "expected at least one number",
                        ))
                    }
                }
            },
        )),
    );

    data.insert(
        "/".to_string(),
        Rc::new(Expression::Func(
            |environment: &mut Environment, args: &[Expression]| -> io::Result<Expression> {
                let mut args = list_to_args(environment, args, true)?;
                if let Ok(ints) = parse_list_of_ints(environment, &mut args) {
                    if ints[1..].iter().any(|&x| x == 0) {
                        Err(io::Error::new(io::ErrorKind::Other, "can not divide by 0"))
                    } else if ints.len() > 1 {
                        let div: i64 = ints[1..]
                            .iter()
                            .fold(*ints.first().unwrap(), |div, a| div / a);
                        Ok(Expression::Atom(Atom::Int(div)))
                    } else {
                        Err(io::Error::new(
                            io::ErrorKind::Other,
                            "expected at least two numbers",
                        ))
                    }
                } else {
                    let floats = parse_list_of_floats(environment, &mut args)?;
                    if floats[1..].iter().any(|&x| x == 0.0) {
                        Err(io::Error::new(io::ErrorKind::Other, "can not divide by 0"))
                    } else if floats.len() > 1 {
                        let div: f64 = floats[1..]
                            .iter()
                            .fold(*floats.first().unwrap(), |div, a| div / a);
                        Ok(Expression::Atom(Atom::Float(div)))
                    } else {
                        Err(io::Error::new(
                            io::ErrorKind::Other,
                            "expected at least two numbers",
                        ))
                    }
                }
            },
        )),
    );

    data.insert(
        "%".to_string(),
        Rc::new(Expression::Func(
            |environment: &mut Environment, args: &[Expression]| -> io::Result<Expression> {
                let mut args = list_to_args(environment, args, true)?;
                let ints = parse_list_of_ints(environment, &mut args)?;
                if ints.len() != 2 {
                    Err(io::Error::new(io::ErrorKind::Other, "expected two ints"))
                } else {
                    let arg1 = ints.get(0).unwrap();
                    let arg2 = ints.get(1).unwrap();
                    if *arg2 == 0 {
                        Err(io::Error::new(
                            io::ErrorKind::Other,
                            "expected two ints, second can not be 0",
                        ))
                    } else {
                        Ok(Expression::Atom(Atom::Int(arg1 % arg2)))
                    }
                }
            },
        )),
    );
}
