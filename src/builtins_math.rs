use std::collections::HashMap;
use std::hash::BuildHasher;

use crate::builtins_util::*;
use crate::environment::*;
use crate::eval::eval;
use crate::interner::*;
use crate::types::*;

fn norm_value(arg: Expression) -> Expression {
    let arg_d = arg.get();
    if let ExpEnum::Values(v) = &arg_d.data {
        if v.is_empty() {
            Expression::make_nil()
        } else {
            v[0].clone()
        }
    } else {
        drop(arg_d);
        arg
    }
}

pub fn add_root_math_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, (Expression, String), S>,
) {
    data.insert(
        interner.intern("+"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut sum = 0;
                let mut sum_float = 0.0;
                let mut is_float = false;
                for arg in args {
                    let a = norm_value(eval(environment, arg)?);
                    let a_d = a.get();
                    match &a_d.data {
                        ExpEnum::Int(i) if is_float => sum_float += *i as f64,
                        ExpEnum::Int(i) => sum += i,
                        ExpEnum::Float(f) if is_float => sum_float += f,
                        ExpEnum::Float(f) => {
                            is_float = true;
                            sum_float = (sum as f64) + f;
                        }
                        _ => {
                            return Err(LispError::new(format!(
                                "Can only add numbers, got {}/{}.",
                                a.display_type(),
                                a
                            )))
                        }
                    }
                }
                if is_float {
                    Ok(Expression::alloc_data(ExpEnum::Float(sum_float)))
                } else {
                    Ok(Expression::alloc_data(ExpEnum::Int(sum)))
                }
            },
            "Usage: (+ number*)

Add a sequence of numbers.  (+) will return 0.

Section: math

Example:
(ns-import 'math)
(test::assert-equal 0 (+))
(test::assert-equal 5 (+ 5))
(test::assert-equal 5 (+ (values 5)))
(test::assert-equal 5 (+ (values 5 6)))
(test::assert-equal 10 (+ 5 (values 5 6)))
(test::assert-equal 5 (+ 5.0))
(test::assert-equal 6 (+ 1 5))
(test::assert-equal 6.5 (+ 1 5.5))
(test::assert-equal 7 (+ 1 2 4))
(test::assert-error (+ 1 2 4 \"5\"))
",
        ),
    );

    data.insert(
        interner.intern("*"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut res = 0;
                let mut res_float = 0.0;
                let mut is_float = false;
                if let Ok(a) = param_eval(environment, args, "multiply") {
                    match norm_value(a).get().data {
                        ExpEnum::Int(i) => res = i,
                        ExpEnum::Float(f) => {
                            is_float = true;
                            res_float = f;
                        }
                        _ => return Err(LispError::new("Can only multiply numbers.")),
                    }
                } else {
                    // Missing args so return 1.
                    return Ok(Expression::alloc_data(ExpEnum::Int(1)));
                }
                for a in args {
                    let a = norm_value(eval(environment, a)?);
                    let a_d = a.get();
                    match &a_d.data {
                        ExpEnum::Int(i) if is_float => res_float *= *i as f64,
                        ExpEnum::Int(i) => res *= i,
                        ExpEnum::Float(f) if is_float => res_float *= f,
                        ExpEnum::Float(f) => {
                            is_float = true;
                            res_float = (res as f64) * f;
                        }
                        _ => return Err(LispError::new("Can only multiply numbers.")),
                    }
                }
                if is_float {
                    Ok(Expression::alloc_data(ExpEnum::Float(res_float)))
                } else {
                    Ok(Expression::alloc_data(ExpEnum::Int(res)))
                }
            },
            "Usage: (* number*)

Multiply a sequence of numbers.  (*) will return 1.

Section: math

Example:
(ns-import 'math)
(test::assert-equal 1 (*))
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
(test::assert-error (* 1 2 4 \"5\"))
",
        ),
    );

    data.insert(
        interner.intern("-"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut res = 0;
                let mut res_float = 0.0;
                let mut is_float = false;
                let mut has_two = false;
                match norm_value(param_eval(environment, args, "subtract")?)
                    .get()
                    .data
                {
                    ExpEnum::Int(i) => res = i,
                    ExpEnum::Float(f) => {
                        is_float = true;
                        res_float = f;
                    }
                    _ => return Err(LispError::new("Can only subtract numbers.")),
                }
                for a in args {
                    has_two = true;
                    let a = norm_value(eval(environment, a)?);
                    let a_d = a.get();
                    match &a_d.data {
                        ExpEnum::Int(i) if is_float => res_float -= *i as f64,
                        ExpEnum::Int(i) => res -= i,
                        ExpEnum::Float(f) if is_float => res_float -= f,
                        ExpEnum::Float(f) => {
                            is_float = true;
                            res_float = (res as f64) - f;
                        }
                        _ => return Err(LispError::new("Can only subtract numbers.")),
                    }
                }
                if is_float {
                    if has_two {
                        Ok(Expression::alloc_data(ExpEnum::Float(res_float)))
                    } else {
                        Ok(Expression::alloc_data(ExpEnum::Float(-res_float)))
                    }
                } else if has_two {
                    Ok(Expression::alloc_data(ExpEnum::Int(res)))
                } else {
                    Ok(Expression::alloc_data(ExpEnum::Int(-res)))
                }
            },
            "Usage: (- number+)

Subtract a sequence of numbers.  Requires at least one number (negate if only one number).

Section: math

Example:
(ns-import 'math)
(test::assert-error (-))
(test::assert-error (- 5 \"2\"))
(test::assert-equal -5 (- 5))
(test::assert-equal -5.0 (- 5.0))
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
                let mut res = 0;
                let mut res_float = 0.0;
                let mut is_float = false;
                let mut has_two = false;
                match norm_value(param_eval(environment, args, "divide")?)
                    .get()
                    .data
                {
                    ExpEnum::Int(i) => res = i,
                    ExpEnum::Float(f) => {
                        is_float = true;
                        res_float = f;
                    }
                    _ => return Err(LispError::new("Can only divide numbers.")),
                }
                for a in args {
                    has_two = true;
                    let a = norm_value(eval(environment, a)?);
                    let a_d = a.get();
                    match &a_d.data {
                        ExpEnum::Int(i) if *i == 0 => {
                            return Err(LispError::new("Can not divide by 0."))
                        }
                        ExpEnum::Int(i) if is_float => res_float /= *i as f64,
                        ExpEnum::Int(i) => res /= i,
                        ExpEnum::Float(f) if *f == 0.0 => {
                            return Err(LispError::new("Can not divide by 0.0."))
                        }
                        ExpEnum::Float(f) if is_float => res_float /= f,
                        ExpEnum::Float(f) => {
                            is_float = true;
                            res_float = (res as f64) / f;
                        }
                        _ => return Err(LispError::new("Can only divide numbers.")),
                    }
                }
                if !has_two {
                    Err(LispError::new("divide requires at least two numbers."))
                } else if is_float {
                    Ok(Expression::alloc_data(ExpEnum::Float(res_float)))
                } else {
                    Ok(Expression::alloc_data(ExpEnum::Int(res)))
                }
            },
            "Usage: (/ number+)

Divide a sequence of numbers.  Requires at least two numbers.

Section: math
Example:
(ns-import 'math)
(test::assert-equal 5 (/ 50 10))
(test::assert-equal 5 (/ 50.0 10.0))
(test::assert-equal 0 (/ 1 5))
(test::assert-equal .2 (/ 1.0 5))
(test::assert-equal .2 (/ 1.0 5.0))
(test::assert-equal 5.5 (/ 5.5 1))
(test::assert-equal 2 (/ 16 2 4))
(test::assert-equal 5 (/ 100 2 5 2))
(test::assert-error (/))
(test::assert-error (/ 1))
(test::assert-error (/ 1 0))
(test::assert-error (/ 10 5 0))
(test::assert-error (/ 10 \"5\" 2))
",
        ),
    );

    data.insert(
        interner.intern("%"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let arg1 =
                    norm_value(param_eval(environment, args, "modulo")?).make_int(environment)?;
                let arg2 =
                    norm_value(param_eval(environment, args, "modulo")?).make_int(environment)?;
                params_done(args, "modulo")?;
                if arg2 == 0 {
                    Err(LispError::new(
                        "modulo: expected two ints, second can not be 0",
                    ))
                } else {
                    Ok(Expression::alloc_data(ExpEnum::Int(arg1 % arg2)))
                }
            },
            "Usage: (% int int)

Remainder from dividing first int by the second.

Section: math

Example:
(ns-import 'math)
(test::assert-equal 0 (% 50 10))
(test::assert-equal 5 (% 55 10))
(test::assert-equal 1 (% 1 2))
(test::assert-error (%))
(test::assert-error (% 1))
(test::assert-error (% 1 2 3))
(test::assert-error (% 1 2.0))
",
        ),
    );
}

pub fn add_math_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, (Expression, String), S>,
) {
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
                    let arg1 = floats.first().unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(arg1.sqrt())))
                }
            },
            "Usage: (sqrt num)

Take square root of argument.

Section: math

Example:
(ns-import 'math)
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
                    let base = floats.first().unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(base.exp2())))
                }
            },
            "Usage: (2pow base)

Raise 2 to power of argument.

Section: math

Example:
(ns-import 'math)
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
                    let base = floats.first().unwrap();
                    let power = floats.get(1).unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(base.powf(*power))))
                }
            },
            "Usage: (pow base power)

Raise first argument to power of second argument.

Section: math

Example:
(ns-import 'math)
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
(ns-import 'math)
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
(ns-import 'math)
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
                    let arg1 = floats.first().unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(arg1.abs())))
                }
            },
            "Usage: (abs arg)

Returns absolute value of arg.

Section: math

Example:
(ns-import 'math)
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
                    let arg1 = floats.first().unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Int(arg1.floor() as i64)))
                }
            },
            "Usage: (floor value)

Returns largest integer less than or equal to value.

Section: math

Example:
(ns-import 'math)
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
                    let arg1 = floats.first().unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Int(arg1.ceil() as i64)))
                }
            },
            "Usage: (ceil value)

Returns smallest integer greater than or equal to value.

Section: math

Example:
(ns-import 'math)
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
                    let arg1 = floats.first().unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Int(arg1.round() as i64)))
                }
            },
            "Usage: (round arg)

Round arg to nearest int value.

Section: math

Example:
(ns-import 'math)
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
                    let arg1 = floats.first().unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(arg1.log2())))
                }
            },
            "Usage: (log2 num)

Returns log base 2 of input.

Section: math

Example:
(ns-import 'math)
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
                    let num = floats.first().unwrap();
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
(ns-import 'math)
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
                    let arg1 = floats.first().unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(arg1.exp())))
                }
            },
            "Usage: (exp num)

Returns e ^ num, the exponential function.

Section: math

Example:
(ns-import 'math)
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
                    let arg1 = floats.first().unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(arg1.ln())))
                }
            },
            "Usage: (lne num)

Returns natural logarithm of number

Section: math

Example:
(ns-import 'math)
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
                    let arg1 = floats.first().unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(arg1.fract())))
                }
            },
            "Usage: (fract num)

Returns fractional part of a number

Section: math

Example:
(ns-import 'math)
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
                    let arg1 = floats.first().unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(arg1.sin())))
                }
            },
            "Usage: (sin num)

Take sin of argument

Section: math

Example:
(ns-import 'math)
(test::assert-equal 0.9893582466233818 (sin 8))
(test::assert-equal (sin 6) (* (tan 6) (cos 6)))
",
        ),
    );

    data.insert(
        interner.intern("arcsin"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let floats = parse_list_of_floats(environment, &mut args)?;
                if floats.len() != 1 {
                    Err(LispError::new("expected one number"))
                } else {
                    let arg1 = floats.first().unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(arg1.sin())))
                }
            },
            "Usage: (arcsin num)

Take arcsin of argument

Section: math

Example:
(ns-import 'math)
(test::assert-equal 0.01 (sin (arcsin 0.01)))
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
                    let arg1 = floats.first().unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(arg1.cos())))
                }
            },
            "Usage: (cos num)

Take cos of argument

Section: math

Example:
(ns-import 'math)
(test::assert-equal -0.14550003380861354 (cos 8))
(test::assert-equal (cos 6) (/ (sin 6) (tan 6)))
",
        ),
    );

    data.insert(
        interner.intern("arccos"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let floats = parse_list_of_floats(environment, &mut args)?;
                if floats.len() != 1 {
                    Err(LispError::new("expected one number"))
                } else {
                    let arg1 = floats.first().unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(arg1.acos())))
                }
            },
            "Usage: (arccos num)

Take arccos of argument

Section: math

Example:
(ns-import 'math)
(test::assert-equal 0.01 (cos (arccos 0.01)))
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
                    let arg1 = floats.first().unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(arg1.tan())))
                }
            },
            "Usage: (tan num)

Take tan of argument

Section: math

Example:
(ns-import 'math)
(test::assert-equal -6.799711455220379 (tan 8))
(test::assert-equal (tan 6) (/ (sin 6) (cos 6)))
",
        ),
    );

    data.insert(
        interner.intern("arctan"),
        Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = Expression>|
             -> Result<Expression, LispError> {
                let mut args = make_args(environment, args)?;
                let floats = parse_list_of_floats(environment, &mut args)?;
                if floats.len() != 1 {
                    Err(LispError::new("expected one number"))
                } else {
                    let arg1 = floats.first().unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(arg1.tan())))
                }
            },
            "Usage: (arctan num)

Take arctan of argument

Section: math

Example:
(ns-import 'math)
(test::assert-equal 0.01 (tan (arctan 0.01)))
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
                    let arg1 = floats.first().unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(arg1.to_degrees())))
                }
            },
            "Usage: (to-degrees num)

Convert degrees to radians.

Section: math

Example:
(ns-import 'math)
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
                    let arg1 = floats.first().unwrap();
                    Ok(Expression::alloc_data(ExpEnum::Float(arg1.to_radians())))
                }
            },
            "Usage: (to-radians num)

Convert degrees to radians.

Section: math

Example:
(ns-import 'math)
(test::assert-equal 0 (- *pi* (to-radians 180)))
",
        ),
    );
}
