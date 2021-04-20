use rand::distributions::{Alphanumeric, Distribution};
use rand::Rng;
use std::collections::HashMap;
use std::hash::BuildHasher;
use std::iter;

use crate::builtins_util::*;
use crate::environment::*;
use crate::eval::eval;
use crate::interner::*;
use crate::types::*;
use std::mem;
use unicode_segmentation::UnicodeSegmentation;

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
(test::assert-equal 55.0000000001 (* 100 0.55))
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
        interner.intern("median"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let mut floats = parse_list_of_floats(environment, &mut args)?;
                let len = floats.len();
                let median;
                if len > 1 {
                    floats.sort_by(|a, b| a.partial_cmp(b).unwrap());
                    median = match len % 2 {
                        1 => {
                            let mid = (len - 1) / 2;
                            ExpEnum::Float(floats[mid])
                        }
                        _ => {
                            let mid = (len - 1) / 2;
                            ExpEnum::Float((floats[mid] + floats[mid + 1]) / 2.0)
                        }
                    };
                } else {
                    median = match floats.pop() {
                        Some(val) => ExpEnum::Float(val),
                        None => ExpEnum::Nil,
                    }
                }
                Ok(Expression::alloc_data(median))
            },
            "Usage: (median number+)

Returns median of sequence of numbers.

Section: math

Example:
(test::assert-equal nil (median))
(test::assert-equal 5 (median 5))
(test::assert-equal 7.5 (median 10 5))
(test::assert-equal 5.5 (median 10 9 8 7 6 5 4 3 2 1))
(test::assert-equal 6 (median 10 4 8 7 6 5 9 3 2 1 11))
",
        ),
    );

    // TODO should return values object instead
    data.insert(
        interner.intern("mode"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let floats = parse_list_of_floats(environment, &mut args)?;
                let mut freqs: HashMap<u64, i32> = HashMap::new();
                for float in floats {
                    *freqs.entry(unsafe { mem::transmute(float) }).or_insert(0) += 1;
                }

                let mut counts: HashMap<i32, Vec<f64>> = HashMap::new();
                let mut max_count = 0;
                for freq in freqs.iter() {
                    let (float_as_int, count) = freq;
                    let float: f64 = unsafe { mem::transmute(*float_as_int) };
                    let count: i32 = *count;
                    counts.entry(count).or_insert(Vec::new()).push(float);
                    if count > max_count {
                        max_count = count;
                    }
                }

                match counts.get(&max_count) {
                    Some(modes) => {
                        let mut float_expr = Vec::with_capacity(modes.len());
                        let mut modes = modes.to_vec();
                        modes.sort_by(|a, b| a.partial_cmp(b).unwrap());
                        for m in modes {
                            float_expr.push(Expression::alloc_data(ExpEnum::Float(m)));
                        }
                        Ok(Expression::alloc_data(ExpEnum::Vector(float_expr)))
                    }
                    None => Ok(Expression::alloc_data(ExpEnum::Nil)),
                }
            },
            "Usage: (mode number+)

Returns mode of a sequence of numbers. Since distributions can be multimodal, mode returns a list.

Section: math

Example:
(test::assert-equal nil (mode))
(test::assert-equal (list 5) (mode 5))
(test::assert-equal (list 1 3 4 5 6 7 8 9 10) (mode 1 3 4 5 6 7 8 9 10))
(test::assert-equal (list 7.0) (mode 1 7.0 3 4 5 6 7 8 9 10))
",
        ),
    );

    data.insert(
        interner.intern("mean"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let floats = parse_list_of_floats(environment, &mut args)?;
                let sum = floats.iter().sum::<f64>();
                let count = floats.len() as f64;
                let mean = match count {
                    positive if positive > 0.0 => ExpEnum::Float(sum / count),
                    _ => ExpEnum::Nil,
                };
                Ok(Expression::alloc_data(mean))
            },
            "Usage: (mean number+)

Average a sequence of numbers.

Section: math

Example:
(test::assert-equal nil (mean))
(test::assert-equal 5 (mean 5))
(test::assert-equal 7.5 (mean 5 10))
(test::assert-equal 5.5 (mean 1 2 3 4 5 6 7 8 9 10))
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
                let mean = sum / count;
                let std_dev = match count {
                    positive if positive > 0.0 => ExpEnum::Float(
                        (floats.iter().fold(0.0, |accum: f64, elem: &f64| -> f64 {
                            accum + (mean - elem).powf(2.0)
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
                    if (*base - 2.0).abs() < f64::EPSILON {
                        Ok(Expression::alloc_data(ExpEnum::Float(num.log2())))
                    } else if (*base - 10.0).abs() < f64::EPSILON {
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

    data.insert(
        interner.intern("random"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let mut rng = rand::thread_rng();
                let ints = parse_list_of_ints(environment, &mut args)?;
                let count = ints.len();
                match count {
                    0 => Ok(Expression::alloc_data(ExpEnum::Float(rng.gen()))),
                    1 => {
                        let i = ints.get(0).unwrap();
                        match i {
                            positive if positive > &0 => {
                                Ok(Expression::alloc_data(ExpEnum::Int(rng.gen_range(0..*i))))
                            }
                            _ => Err(LispError::new("Expected positive integer")),
                        }
                    }
                    _ => Err(LispError::new("Expected zero or one integers")),
                }
            },
            "Usage: (random), (random limit)

If no arguments are given, generates a float between 0 and 1, otherwise takes a positive integer,
limit, and returns positive integer between 0 and limit exclusive.

Section: math

Example:
(def rand-int (random 100))
(test::assert-true (and (> rand-int 0) (< rand-int 100))
(def rand-float (random))
(test::assert-true (and (> rand-float 0) (< rand-float 1)))
(test::assert-error-msg (random -1) \"Expected positive integer\")
(test::assert-error-msg (random 1 2) \"Expected zero or one integers\")
",
        ),
    );

    data.insert(
        interner.intern("probool"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let ints = parse_list_of_ints(environment, &mut args)?;
                let count = ints.len();
                let tup: Option<(u32, u32)> = match count {
                    0 => Some((1, 2)),
                    2 => {
                        let i = *ints.get(0).unwrap() as u32;
                        let j = *ints.get(1).unwrap() as u32;
                        Some((i, j))
                    }
                    _ => None,
                };
                match tup {
                    None => Err(LispError::new("Expected zero or two numbers")),
                    Some((_, 0)) => Err(LispError::new("Denominator can not be zero")),
                    Some((i, j)) => match i / j {
                        improper if improper > 1 => Ok(Expression::alloc_data(ExpEnum::True)),
                        _ => match rand::thread_rng().gen_ratio(i, j) {
                            true => Ok(Expression::alloc_data(ExpEnum::True)),
                            false => Ok(Expression::alloc_data(ExpEnum::Nil)),
                        },
                    },
                }
            },
            "Usage: (probool), (probool numerator denominator)

PRObability of a BOOLean.

If no arguments are given, returns #t 1/2 of the time, otherwise takes two
integers, numerator and denominator, and returns #t numerator/denominator of the
time. Throws an error if denominator is 0. If (>= (/ numerator denominator) 1)
probool always returns true. If numerator is 0 probool always returns false.

Section: math

Example:
(def val0 (probool))
(test::assert-true (or (= #t val0) (= nil val0)))
(def val1 (probool 17 42))
(test::assert-true (or (= #t val1) (= nil val1)))
(test::assert-true (probool 1 1))
(test::assert-false (probool 0 42))
(test::assert-error-msg (probool 0 0) \"Denominator can not be zero\")
(test::assert-error-msg (probool 0 0 0) \"Expected zero or two numbers\")
",
        ),
    );

    data.insert(
        interner.intern("random-str"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                if let Some(next_arg) = eval_next(environment, args)? {
                    if let ExpEnum::Int(i) = next_arg.get().data {
                        match i {
                            positive if positive > 0 => {
                                if let Some(opt_arg) = eval_next(environment, args)? {
                                    let opt_arg_d = opt_arg.get();
//                                    let distr: Distribution<T> = match &opt_arg_d.data {
//                                        ExpEnum::Symbol(sym, _) => {
//                                            match sym {
//                                                &":hex" => HexGraphemes,
//                                                &":ascii" => AsciiGraphemes,
//                                                _ => Err(LispError::new(format!("Unknown symbol {}", sym))),
//                                            }
//                                        }
//                                        ExpEnum::String(string, _) => UserProvidedGraphemes::new(string),
//                                        _ => Err(LispError::new("Optional second argument must be keyword or string")),
//                                    };
//                                    Ok(Expression::alloc_data(ExpEnum::String(
//                                        iter::repeat(())
//                                            .map(|()| {
//                                                rand::thread_rng().sample()
//                                            })
//                                            .take(*len as usize)
//                                            .collect(),
//                                        None,
//                                    )))
                                    Ok(Expression::alloc_data(ExpEnum::String(
                                        iter::repeat(())
                                            .map(|()| rand::thread_rng().sample(Alphanumeric))
                                            .map(char::from)
                                            .take(positive as usize)
                                            .collect(),
                                        None,
                                    )))
                                } else {
                                    Ok(Expression::alloc_data(ExpEnum::String(
                                        iter::repeat(())
                                            .map(|()| rand::thread_rng().sample(Alphanumeric))
                                            .map(char::from)
                                            .take(positive as usize)
                                            .collect(),
                                        None,
                                    )))
                                }
                            },
                            _ => Err(LispError::new("Expected positive number")),
                        }
                    }
                    else {
                        Err(LispError::new("Expected at least one number1"))
                    }
                } else {
                    Err(LispError::new("Expected at least one number2"))
                }
//                match ExpEnum::Int(next_arg) = &next_arg.data {
//                    Some(next_arg) => {
//
//                    },
//                    _ => Err(LispError::new("Expected at least one number")),
//                }
//                if ints.len() != 1 {
//                    Err(LispError::new("Expected positive number"))
//                } else {
//                    let len = ints.get(0).unwrap();
//                    match len {
//                        positive if positive > &0 =>
// Ok(Expression::alloc_data(ExpEnum::String(
//                            iter::repeat(())
//                                .map(|()| rand::thread_rng().sample(Alphanumeric))
//                                .map(char::from)
//                                .take(*len as usize)
//                                .collect(),
//                            None,
//                        ))),
//                        _ => Err(LispError::new("Expected positive number")),
//                    }
//                }
            },
            "Usage: (random-str str-length [char-set])

Takes a positive integer and returns a string of random alphanumeric characters
of provided length.

Section: math

Example:
(test::assert-equal 10 (length (random-str 10)))
(test::assert-error-msg (random-str -1) \"Expected positive number\")
",
        ),
    );

    #[derive(Debug)]
    struct AsciiGraphemes;

    impl Distribution<u8> for AsciiGraphemes {
        fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> u8 {
            const RANGE: u32 = 26 + 26 + 10 + 32;
            const ASCII_PRINTABLE_CHARSET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZ\
                abcdefghijklmnopqrstuvwxyz\
                0123456789\
                !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~";
            // We can pick from 94 characters. This is so close to a power of 2, 128,
            // that we can do better than `Uniform`. Use a simple bitshift and
            // rejection sampling. We do not use a bitmask, because for small RNGs
            // the most significant bits are usually of higher quality.
            loop {
                let var = rng.next_u32() >> (32 - 7);
                if var < RANGE {
                    return ASCII_PRINTABLE_CHARSET[var as usize];
                }
            }
        }
    }

    #[derive(Debug)]
    struct HexGraphemes;

    impl Distribution<u8> for HexGraphemes {
        fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> u8 {
            const RANGE: u32 = 16;
            const HEX_CHARSET: &[u8] = b"abcdef0123456789";
            // We can pick from 16 characters. This is a power of 2. Use a
            // simple bitshift and rejection sampling. We do not use a bitmask,
            // because for small RNGs/ the most significant bits are usually
            // of higher quality.
            loop {
                let var = rng.next_u32() >> (32 - 4);
                if var < RANGE {
                    return HEX_CHARSET[var as usize];
                }
            }
        }
    }

    #[derive(Debug)]
    struct UserProvidedGraphemes {
        sample_space: Vec<String>,
        len: usize,
    }

    impl UserProvidedGraphemes {
        pub fn new(s: &str) -> UserProvidedGraphemes {
            let mut sample_space: Vec<String> = Vec::new();
            for cluster in UnicodeSegmentation::graphemes(s, true) {
                sample_space.push(cluster.to_string());
            }
            let len = sample_space.len();
            UserProvidedGraphemes { sample_space, len }
        }
    }

    impl Distribution<String> for UserProvidedGraphemes {
        fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> String {
            self.sample_space
                .get(rng.gen_range(0..self.len))
                .unwrap()
                .to_owned()
        }
    }

    data.insert(
        interner.intern("random-str-sample"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let len = param_eval(environment, args, "random-str-sample")?;
                let to_sample = param_eval(environment, args, "random-str-sample")?;
                params_done(args, "random-str-sample")?;
                let len_d = len.get();
                if let ExpEnum::Int(len) = &len_d.data {
                    match len {
                        positive if positive > &0 => {
                            let to_sample_d = to_sample.get();
                            if let ExpEnum::String(string, _) = &to_sample_d.data {
                                Ok(Expression::alloc_data(ExpEnum::String(
                                    iter::repeat(())
                                        .map(|()| {
                                            rand::thread_rng().sample(UserProvidedGraphemes::new(string))
                                        })
                                        .take(*len as usize)
                                        .collect(),
                                    None,
                                )))
                            } else {
                                Err(LispError::new("Second argument should be string."))
                            }
                        }
                        _ => Err(LispError::new("First argument should be positive integer.")),
                    }
                } else {
                    Err(LispError::new("First argument should be positive integer."))
                }
            },
            "Usage: (random-str-sample str-length)

Takes a positive integer and a string to sample from and returns a string of random characters
of provided length from provided string.

Section: math

Example:
(def hex-chars \"0123456789abcdef\")
(random-str-sample)
",
        ),
    );
}
// TODO need sample-stats hash map with
//  - mean, std-deviation, mode, min, max, 25%, median 75%
//  - z-score?
//  - t-test?
//  - vec-sort!
//  - vec-sort
//  - move rand functions to builtins_rand use more standard pattern that does not load all builtins
// in one function.
