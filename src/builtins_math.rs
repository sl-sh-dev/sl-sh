use std::collections::HashMap;
use std::hash::BuildHasher;

use crate::builtins_util::*;
use crate::environment::*;
use crate::eval::eval;
use crate::interner::*;
use crate::types::*;

fn make_args(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Vec<Expression>, LispError> {
    let mut list: Vec<Expression> = Vec::new();
    for arg in args {
        list.push(eval(environment, arg)?);
    }
    Ok(list)
}

pub fn add_math_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, Reference, S>,
) {
    let root = interner.intern("root");
    data.insert(
        interner.intern("+"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                if let Ok(ints) = parse_list_of_ints(environment, &mut args) {
                    let sum: i64 = ints.iter().sum();
                    Ok(Expression::alloc_data(ExpEnum::Atom(Atom::Int(sum))))
                } else {
                    let sum: f64 = parse_list_of_floats(environment, &mut args)?.iter().sum();
                    Ok(Expression::alloc_data(ExpEnum::Atom(Atom::Float(sum))))
                }
            },
            "Usage: (+ number+)

Add a sequence of numbers.

Section: math

Example:
(test::assert-equal 5 (+ 5))
(test::assert-equal 5 (+ 5.0))
(test::assert-equal 6 (+ 1 5))
(test::assert-equal 6.5 (+ 1 5.5))
(test::assert-equal 7 (+ 1 2 4))
",
            root,
        ),
    );

    data.insert(
        interner.intern("*"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                if let Ok(ints) = parse_list_of_ints(environment, &mut args) {
                    let prod: i64 = ints.iter().product();
                    Ok(Expression::alloc_data(ExpEnum::Atom(Atom::Int(prod))))
                } else {
                    let prod: f64 = parse_list_of_floats(environment, &mut args)?
                        .iter()
                        .product();
                    Ok(Expression::alloc_data(ExpEnum::Atom(Atom::Float(prod))))
                }
            },
            "Usage: (* number+)

Multiply a sequence of numbers.

Section: math

Example:
(test::assert-equal 5 (* 5))
(test::assert-equal 5 (* 1 5))
(test::assert-equal 5.0 (* 1.0 5))
(test::assert-equal 7.5 (* 1.5 5))
(test::assert-equal 7.5 (* 1.5 5.0))
(test::assert-equal 15 (* 3 5))
(test::assert-equal 8 (* 1 2 4))
(test::assert-equal 16 (* 2 2 4))
(test::assert-equal 16.0 (* 2 2.0 4))
(test::assert-equal 16.0 (* 2.0 2.0 4.0))
",
            root,
        ),
    );

    data.insert(
        interner.intern("-"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                if let Ok(ints) = parse_list_of_ints(environment, &mut args) {
                    if let Some(first) = ints.first() {
                        let sum_of_rest: i64 = ints[1..].iter().sum();
                        Ok(Expression::alloc_data(ExpEnum::Atom(Atom::Int(
                            first - sum_of_rest,
                        ))))
                    } else {
                        Err(LispError::new("expected at least one number"))
                    }
                } else {
                    let floats = parse_list_of_floats(environment, &mut args)?;
                    if let Some(first) = floats.first() {
                        let sum_of_rest: f64 = floats[1..].iter().sum();
                        Ok(Expression::alloc_data(ExpEnum::Atom(Atom::Float(
                            first - sum_of_rest,
                        ))))
                    } else {
                        Err(LispError::new("expected at least one number"))
                    }
                }
            },
            "Usage: (- number+)

Subtract a sequence of numbers.

Section: math

Example:
(test::assert-equal 5 (- 5))
(test::assert-equal 5 (- 5.0))
(test::assert-equal -4 (- 1 5))
(test::assert-equal -4.5 (- 1 5.5))
(test::assert-equal 4 (- 10 2 4))
(test::assert-equal 4.9 (- 10.9 2 4))
",
            root,
        ),
    );

    data.insert(
        interner.intern("/"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                if let Ok(ints) = parse_list_of_ints(environment, &mut args) {
                    if ints[1..].iter().any(|&x| x == 0) {
                        Err(LispError::new("can not divide by 0"))
                    } else if ints.len() > 1 {
                        let div: i64 = ints[1..]
                            .iter()
                            .fold(*ints.first().unwrap(), |div, a| div / a);
                        Ok(Expression::alloc_data(ExpEnum::Atom(Atom::Int(div))))
                    } else {
                        Err(LispError::new("expected at least two numbers"))
                    }
                } else {
                    let floats = parse_list_of_floats(environment, &mut args)?;
                    if floats[1..].iter().any(|&x| x == 0.0) {
                        Err(LispError::new("can not divide by 0"))
                    } else if floats.len() > 1 {
                        let div: f64 = floats[1..]
                            .iter()
                            .fold(*floats.first().unwrap(), |div, a| div / a);
                        Ok(Expression::alloc_data(ExpEnum::Atom(Atom::Float(div))))
                    } else {
                        Err(LispError::new("expected at least two numbers"))
                    }
                }
            },
            "Usage: (/ number+)

Divide a sequence of numbers.  Requires at least two numbers.

Section: math

Example:
(test::assert-equal 5 (/ 50 10))
(test::assert-equal 5 (/ 50.0 10.0))
(test::assert-equal 0 (/ 1 5))
(test::assert-equal .2 (/ 1.0 5))
(test::assert-equal .2 (/ 1.0 5.0))
(test::assert-equal 5.5 (/ 5.5 1))
(test::assert-equal 2 (/ 16 2 4))
(test::assert-equal 5 (/ 100 2 5 2))
",
            root,
        ),
    );

    data.insert(
        interner.intern("%"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let ints = parse_list_of_ints(environment, &mut args)?;
                if ints.len() != 2 {
                    Err(LispError::new("expected two ints"))
                } else {
                    let arg1 = ints.get(0).unwrap();
                    let arg2 = ints.get(1).unwrap();
                    if *arg2 == 0 {
                        Err(LispError::new("expected two ints, second can not be 0"))
                    } else {
                        Ok(Expression::alloc_data(ExpEnum::Atom(Atom::Int(
                            arg1 % arg2,
                        ))))
                    }
                }
            },
            "Usage: (% int int)

Remainder from dividing first int by the second.

Section: math

Example:
(test::assert-equal 0 (% 50 10))
(test::assert-equal 5 (% 55 10))
(test::assert-equal 1 (% 1 2))
",
            root,
        ),
    );
}
