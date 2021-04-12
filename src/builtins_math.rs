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
    data: &mut HashMap<&'static str, (Expression, String), S>,
) {
    data.insert(
        interner.intern("+"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                if let Ok(ints) = parse_list_of_ints(environment, &mut args) {
                    let sum: i64 = ints.iter().sum();
                    Ok(Expression::alloc_data(ExpEnum::Int(sum)))
                } else {
                    let sum: f64 = parse_list_of_floats(environment, &mut args)?.iter().sum();
                    Ok(Expression::alloc_data(ExpEnum::Float(sum)))
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
                    Ok(Expression::alloc_data(ExpEnum::Int(prod)))
                } else {
                    let prod: f64 = parse_list_of_floats(environment, &mut args)?
                        .iter()
                        .product();
                    Ok(Expression::alloc_data(ExpEnum::Float(prod)))
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
                        Ok(Expression::alloc_data(ExpEnum::Int(first - sum_of_rest)))
                    } else {
                        Err(LispError::new("expected at least one number"))
                    }
                } else {
                    let floats = parse_list_of_floats(environment, &mut args)?;
                    if let Some(first) = floats.first() {
                        let sum_of_rest: f64 = floats[1..].iter().sum();
                        Ok(Expression::alloc_data(ExpEnum::Float(first - sum_of_rest)))
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
                        Ok(Expression::alloc_data(ExpEnum::Int(div)))
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
                        Ok(Expression::alloc_data(ExpEnum::Float(div)))
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
        ),
    );

    data.insert(
        interner.intern("avg"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let floats = parse_list_of_floats(environment, &mut args)?;
                let sum = floats.iter().sum::<f64>();
                let count = floats.len() as f64;
                let avg = match count {
                    positive if positive > 0.0 => ExpEnum::Float(sum / count),
                    _ => ExpEnum::Nil,
                };
                Ok(Expression::alloc_data(avg))
            },
            "Usage: (avg number+)

Average a sequence of numbers.

Section: math

Example:
(test::assert-equal nil (avg))
(test::assert-equal 5 (avg 5))
(test::assert-equal 7.5 (avg 5 10))
(test::assert-equal 5.5 (avg 1 2 3 4 5 6 7 8 9 10))
",
        ),
    );

    data.insert(
        interner.intern("std-dev"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let floats = parse_list_of_floats(environment, &mut args)?;
                let sum = floats.iter().sum::<f64>();
                let count = floats.len() as f64;
                let avg = sum / count;
                let std_dev = match count {
                    positive if positive > 0.0 => ExpEnum::Float(
                        (floats.iter().fold(0.0, |accum: f64, elem: &f64| -> f64 {
                            accum + (avg - elem).powf(2.0)
                        }) / count)
                            .sqrt(),
                    ),
                    _ => ExpEnum::Nil,
                };
                Ok(Expression::alloc_data(std_dev))
            },
            "Usage: (std-dev number+)

Returns standard deviation of a sequence of numbers.

Section: math

Example:
(test::assert-equal nil (std-dev))
(test::assert-equal 2.872281323269 (std-dev 1 2 3 4 5 6 7 8 9 10))
",
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
                        Ok(Expression::alloc_data(ExpEnum::Int(arg1 % arg2)))
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
        ),
    );

    data.insert(
        interner.intern("sqrt"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let floats = parse_list_of_floats(environment, &mut args)?;
                if floats.len() != 1 {
                    Err(LispError::new("expected one float"))
                } else {
                    let arg1 = floats.get(0).unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(arg1.sqrt())))
                }
            },
            "Usage: (sqrt num)

Take square root of argument.

Section: math

Example:
(test::assert-equal 2.0 (sqrt 4))
(test::assert-equal 2.04939015319192 (sqrt 4.2))
(test::assert-equal 12 (sqrt 144))
",
        ),
    );

    data.insert(
        interner.intern("2pow"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let floats = parse_list_of_floats(environment, &mut args)?;
                if floats.len() != 1 {
                    Err(LispError::new("expected one number"))
                } else {
                    let base = floats.get(0).unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(base.exp2())))
                }
            },
            "Usage: (2pow base)

Raise 2 to power of argument.

Section: math

Example:
(test::assert-equal 1024 (2pow 10))
(test::assert-equal (2pow (* 10 2)) (pow (2pow 10) 2))
",
        ),
    );

    data.insert(
        interner.intern("pow"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let floats = parse_list_of_floats(environment, &mut args)?;
                if floats.len() != 2 {
                    Err(LispError::new("expected two numbers"))
                } else {
                    let base = floats.get(0).unwrap();
                    let power = floats.get(1).unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(base.powf(*power))))
                }
            },
            "Usage: (pow base power)

Raise first argument to power of second argument.

Section: math

Example:
(test::assert-equal 16 (pow 4 2))
(test::assert-equal 10 (log (pow 2 10) 2))
(test::assert-equal (pow 8 15) (* (pow 8 10) (pow 8 5)))
(test::assert-equal (pow 100 3) (/ (pow 100 5) (pow 100 2)))
(test::assert-equal 1 (pow 85 0))
",
        ),
    );

    data.insert(
        interner.intern("*euler*"),
        (
            Expression::from(ExpEnum::Float(std::f64::consts::E)),
            "Usage: (print *e*)

Float representing euler's number.

Section: math

Example:
(test::assert-equal 2.718281828459045 *e*)
"
            .to_string(),
        ),
    );

    data.insert(
        interner.intern("*pi*"),
        (
            Expression::from(ExpEnum::Float(std::f64::consts::PI)),
            "Usage: (print *pi*)

Float representing pi.

Section: math

Example:
(test::assert-equal 3.141592653589793 *pi*)
"
            .to_string(),
        ),
    );

    data.insert(
        interner.intern("abs"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let floats = parse_list_of_floats(environment, &mut args)?;
                if floats.len() != 1 {
                    Err(LispError::new("expected one number"))
                } else {
                    let arg1 = floats.get(0).unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(arg1.abs())))
                }
            },
            "Usage: (abs arg)

Returns absolute value of arg.

Section: math

Example:
(test::assert-equal 2.0 (abs 2))
(test::assert-equal 144 (abs -144))
(test::assert-equal 4.53 (abs -4.53))
",
        ),
    );

    data.insert(
        interner.intern("floor"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let floats = parse_list_of_floats(environment, &mut args)?;
                if floats.len() != 1 {
                    Err(LispError::new("expected one number"))
                } else {
                    let arg1 = floats.get(0).unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Int(arg1.floor() as i64)))
                }
            },
            "Usage: (floor value)

Returns largest integer less than or equal to value.

Section: math

Example:
(test::assert-equal 2.0 (floor 2))
(test::assert-equal 144 (floor 144.444444))
(test::assert-equal 4 (floor 4.53))
",
        ),
    );

    data.insert(
        interner.intern("ceil"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let floats = parse_list_of_floats(environment, &mut args)?;
                if floats.len() != 1 {
                    Err(LispError::new("expected one number"))
                } else {
                    let arg1 = floats.get(0).unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Int(arg1.ceil() as i64)))
                }
            },
            "Usage: (ceil value)

Returns smallest integer greater than or equal to value.

Section: math

Example:
(test::assert-equal 2.0 (ceil 2))
(test::assert-equal 145 (ceil 144.444444))
(test::assert-equal 5 (ceil 4.53))
",
        ),
    );

    data.insert(
        interner.intern("round"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let floats = parse_list_of_floats(environment, &mut args)?;
                if floats.len() != 1 {
                    Err(LispError::new("expected one number"))
                } else {
                    let arg1 = floats.get(0).unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Int(arg1.round() as i64)))
                }
            },
            "Usage: (round arg)

Round arg to nearest int value.

Section: math

Example:
(test::assert-equal 2.0 (round 2))
(test::assert-equal 144 (round 144.444444))
(test::assert-equal 5 (round 4.53))
",
        ),
    );

    data.insert(
        interner.intern("log2"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let floats = parse_list_of_floats(environment, &mut args)?;
                if floats.len() != 1 {
                    Err(LispError::new("expected one number"))
                } else {
                    let arg1 = floats.get(0).unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(arg1.log2())))
                }
            },
            "Usage: (log2 num)

Returns log base 2 of input.

Section: math

Example:
(test::assert-equal 7 (log2 128))
(test::assert-equal (log 7 2) (/ 1.0 (log 2 7)))
",
        ),
    );

    data.insert(
        interner.intern("log"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let floats = parse_list_of_floats(environment, &mut args)?;
                if floats.len() != 2 {
                    Err(LispError::new("expected two numbers"))
                } else {
                    let num = floats.get(0).unwrap();
                    let base = floats.get(1).unwrap();
                    if *base == 2.0 {
                        Ok(Expression::alloc_data(ExpEnum::Float(num.log2())))
                    } else if *base == 10.0 {
                        Ok(Expression::alloc_data(ExpEnum::Float(num.log10())))
                    } else {
                        Ok(Expression::alloc_data(ExpEnum::Float(num.log(*base))))
                    }
                }
            },
            "Usage: (log num base)

Returns log of number given base.

Section: math

Example:
(test::assert-equal 8 (log 256 2))
(test::assert-equal 3 (log 27 3))
(test::assert-equal (log (pow 8 2) 10) (* 2 (log 8 10)))
(test::assert-equal 1 (log 11 11))
(test::assert-equal '-inf (log 0 11))
(test::assert-equal (log 11 5) (/ (log 11 3) (log 5 3)))
",
        ),
    );

    data.insert(
        interner.intern("exp"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let floats = parse_list_of_floats(environment, &mut args)?;
                if floats.len() != 1 {
                    Err(LispError::new("expected one number"))
                } else {
                    let arg1 = floats.get(0).unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(arg1.exp())))
                }
            },
            "Usage: (exp num)

Returns e ^ num, the exponential function.

Section: math

Example:
(test::assert-equal *euler* (exp 1))
(test::assert-equal 1 (exp 0))
(test::assert-equal 42 (exp (lne 42)))
",
        ),
    );

    data.insert(
        interner.intern("lne"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let floats = parse_list_of_floats(environment, &mut args)?;
                if floats.len() != 1 {
                    Err(LispError::new("expected one number"))
                } else {
                    let arg1 = floats.get(0).unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(arg1.ln())))
                }
            },
            "Usage: (lne num)

Returns natural logarithm of number

Section: math

Example:
(def x 7.0)
(def y 11.0)
(test::assert-equal 1 (lne *euler*))
(test::assert-equal 0 (lne 1))
(test::assert-equal (lne (* x y)) (+ (lne x) (lne y)))
(test::assert-equal (lne (/ x y)) (- (lne x) (lne y)))
(test::assert-equal (lne (pow x y)) (* y (lne x)))
",
        ),
    );

    data.insert(
        interner.intern("fract"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let floats = parse_list_of_floats(environment, &mut args)?;
                if floats.len() != 1 {
                    Err(LispError::new("expected one number"))
                } else {
                    let arg1 = floats.get(0).unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(arg1.fract())))
                }
            },
            "Usage: (fract num)

Returns fractional part of a number

Section: math

Example:
(test::assert-equal 0.9893582466233818 (fract 1911.9893582466233818))
(test::assert-equal 0.0 (fract 1911))
",
        ),
    );

    data.insert(
        interner.intern("sin"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let floats = parse_list_of_floats(environment, &mut args)?;
                if floats.len() != 1 {
                    Err(LispError::new("expected one number"))
                } else {
                    let arg1 = floats.get(0).unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(arg1.sin())))
                }
            },
            "Usage: (sin num)

Take sin of argument

Section: math

Example:
(test::assert-equal 0.9893582466233818 (sin 8))
(test::assert-equal (sin 6) (* (tan 6) (cos 6)))
",
        ),
    );

    data.insert(
        interner.intern("cos"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let floats = parse_list_of_floats(environment, &mut args)?;
                if floats.len() != 1 {
                    Err(LispError::new("expected one number"))
                } else {
                    let arg1 = floats.get(0).unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(arg1.cos())))
                }
            },
            "Usage: (cos num)

Take cos of argument

Section: math

Example:
(test::assert-equal -0.14550003380861354 (cos 8))
(test::assert-equal (cos 6) (/ (sin 6) (tan 6)))
",
        ),
    );

    data.insert(
        interner.intern("tan"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let floats = parse_list_of_floats(environment, &mut args)?;
                if floats.len() != 1 {
                    Err(LispError::new("expected one number"))
                } else {
                    let arg1 = floats.get(0).unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(arg1.tan())))
                }
            },
            "Usage: (tan num)

Take tan of argument

Section: math

Example:
(test::assert-equal -6.799711455220379 (tan 8))
(test::assert-equal (tan 6) (/ (sin 6) (cos 6)))
",
        ),
    );

    data.insert(
        interner.intern("to-degrees"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let floats = parse_list_of_floats(environment, &mut args)?;
                if floats.len() != 1 {
                    Err(LispError::new("expected one number"))
                } else {
                    let arg1 = floats.get(0).unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(arg1.to_degrees())))
                }
            },
            "Usage: (to-degrees num)

Convert degrees to radians.

Section: math

Example:
(test::assert-equal 0 (- (to-degrees *pi*) 180))
",
        ),
    );

    data.insert(
        interner.intern("to-radians"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let floats = parse_list_of_floats(environment, &mut args)?;
                if floats.len() != 1 {
                    Err(LispError::new("expected one number"))
                } else {
                    let arg1 = floats.get(0).unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(arg1.to_radians())))
                }
            },
            "Usage: (to-radians num)

Convert degrees to radians.

Section: math

Example:
(test::assert-equal 0 (- *pi* (to-radians 180)))
",
        ),
    );
}

// TODO need some primitive rand support
// TODO max and min values.
//  https://rust-lang-nursery.github.io/rust-cookbook/algorithms/randomness.html#generate-random-numbers
//  radians
