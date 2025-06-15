use crate::SloshVm;
use bridge_macros::sl_sh_fn;
use bridge_types::{LooseFloat, VarArgs};
use slvm::{VMError, VMResult, Value};
use slvm::float::F56;

/// Usage: (abs arg)
///
/// Returns absolute value of arg.
///
/// Section: math
///
/// Example:
/// (test::assert-equal 2 (abs 2))
/// (test::assert-equal 144 (abs -144))
/// (test::assert-equal 4.53 (abs -4.53))
/// (test::assert-equal 36028797018963967 (abs -36028797018963967))
#[sl_sh_fn(fn_name = "abs")]
pub fn abs(v: Value) -> VMResult<Value> {
    match v {
        Value::Float(f56) => {
            // Convert to f64, abs, convert back to f56
            Ok(Value::Float(f64::from(f56).abs().into()))
        }
        Value::Int(i56_bytes) => {
            // Convert to i64, abs, convert back to i56
            let i64 = slvm::from_i56(&i56_bytes);
            Ok(slvm::to_i56(i64.abs()))
        }
        Value::Byte(_b) => {
            // Byte is unsigned so just return the input
            Ok(v)
        }
        _ => Err(VMError::new_vm("abs: not a number".to_string())),
    }
}

/// Usage: (% int int)
///
/// Remainder from dividing (arg 1) by (arg 2).
/// Note: Remainder and Modulo are two similar mathematical operations,
/// called `rem` and `rem_euclid` in Rust.
/// This function uses `rem` which is the same as the `%` operator in C.
/// With `rem`, the sign of the result is the same as the dividend (arg 1).
/// With `rem_euclid`, the sign of the result is always positive.
///
/// Section: math
///
/// Example:
/// (test::assert-equal 0 (% 50 10))
/// (test::assert-equal 5 (% 55 10))
/// (test::assert-equal 1 (% 1 2))
/// (test::assert-equal -1 (% -10 3))
/// (test::assert-equal  1 (% 10 -3))
/// (test::assert-equal -1 (% -10 -3))
///
/// (test::assert-error (%))
/// (test::assert-error (% 1))
/// (test::assert-error (% 1 2 3))
/// (test::assert-error (% 1 2.0))
#[sl_sh_fn(fn_name = "%")]
pub fn rem_as_percent(dividend: i64, divisor: i64) -> VMResult<i64> {
    dividend.checked_rem(divisor).ok_or_else(|| {
        VMError::new_vm(format!(
            "rem: division by zero or overflow: {} % {}",
            dividend, divisor
        ))
    })
}

/// Usage: (rem int int)
///
/// Remainder from dividing (arg 1) by (arg 2).
/// Note: Remainder and Modulo are two similar mathematical operations,
/// called `rem` and `rem_euclid` in Rust.
/// This function uses `rem` which is the same as the `%` operator in C.
/// With `rem`, the sign of the result is the same as the dividend (arg 1).
/// With `rem_euclid`, the sign of the result is always positive.
///
/// Section: math
///
/// Example:
/// (test::assert-equal 0 (rem 50 10))
/// (test::assert-equal 5 (rem 55 10))
/// (test::assert-equal 1 (rem 1 2))
/// (test::assert-equal -1 (rem -10 3))
/// (test::assert-equal  1 (rem 10 -3))
/// (test::assert-equal -1 (rem -10 -3))
///
/// (test::assert-error (rem))
/// (test::assert-error (rem 1))
/// (test::assert-error (rem 1 2 3))
/// (test::assert-error (rem 1 2.0))
#[sl_sh_fn(fn_name = "rem")]
pub fn rem_as_rem(dividend: i64, divisor: i64) -> VMResult<i64> {
    rem_as_percent(dividend, divisor)
}

/// Usage: (rem_euclid int int)
///
/// Least Non-negative number that can be added to a multiple of the divisor (arg 2) to get the dividend (arg 1).
/// The result should always be 0 <= result < divisor (arg 2).
/// Note: Remainder and Modulo are two similar mathematical operations,
/// called `rem` and `rem_euclid` in Rust.
/// With `rem`, the sign of the result is the same as the dividend (arg 1).
/// With `rem_euclid`, the sign of the result is always positive.
///
/// Section: math
///
/// Example:
/// (test::assert-equal 0 (rem_euclid 50 10))
/// (test::assert-equal 5 (rem_euclid 55 10))
/// (test::assert-equal 1 (rem_euclid 1 2))
/// (test::assert-equal 2 (rem_euclid -10 3))
/// (test::assert-equal 1 (rem_euclid 10 -3))
/// (test::assert-equal 2 (rem_euclid -10 -3))
///
/// (test::assert-error (rem_euclid))
/// (test::assert-error (rem_euclid 1))
/// (test::assert-error (rem_euclid 1 2 3))
/// (test::assert-error (rem_euclid 1 2.0))
#[sl_sh_fn(fn_name = "rem_euclid")]
pub fn rem_euclid(dividend: i64, divisor: i64) -> VMResult<i64> {
    dividend.checked_rem_euclid(divisor).ok_or_else(|| {
        VMError::new_vm(format!(
            "rem_euclid: division by zero or overflow: {} % {}",
            dividend, divisor
        ))
    })
}

/// Usage: (2pow base)
///
/// Raise 2 to power of argument.
///
/// Section: math
///
/// Example:
/// (import 'math)
/// (test::assert-equal 1024 (2pow 10))
/// (test::assert-equal (2pow (* 10 2)) (pow (2pow 10) 2))
#[sl_sh_fn(fn_name = "2pow")]
pub fn two_pow(float: LooseFloat) -> VMResult<f64> {
    let float = f64::from(F56(float.0));
    Ok(float.exp2())
}

/// Usage: (min number+)
///
/// Return the minimum value from one or more numbers.
///
/// Section: math
///
/// Example:
/// (test::assert-equal 1 (min 1 2 3))
/// (test::assert-equal -5 (min 10 -5 3))
/// (test::assert-equal 42 (min 42))
/// (test::assert-equal 1.5 (min 3.0 1.5 2.0))
/// (test::assert-equal 1 (min 1 2.0 3))
/// (test::assert-error (min))
/// (test::assert-error (min 1 "two"))
#[sl_sh_fn(fn_name = "min")]
pub fn min(vals: VarArgs<Value>) -> VMResult<Value> {
    if vals.is_empty() {
        return Err(VMError::new_vm("min: requires at least one argument".to_string()));
    }

    let mut min_val = vals[0];
    let mut is_float = matches!(min_val, Value::Float(_));

    for val in &vals[1..] {
        match (min_val, val) {
            (Value::Int(a_bytes), Value::Int(b_bytes)) => {
                let a = slvm::from_i56(&a_bytes);
                let b = slvm::from_i56(b_bytes);
                if b < a {
                    min_val = *val;
                }
            }
            (Value::Float(a), Value::Float(b)) => {
                if f64::from(*b) < f64::from(a) {
                    min_val = *val;
                }
            }
            (Value::Int(a_bytes), Value::Float(b)) => {
                let a = slvm::from_i56(&a_bytes) as f64;
                let b = f64::from(*b);
                if b < a {
                    min_val = *val;
                    is_float = true;
                } else if is_float {
                    min_val = Value::Float(a.into());
                }
            }
            (Value::Float(a), Value::Int(b_bytes)) => {
                let a = f64::from(a);
                let b = slvm::from_i56(b_bytes) as f64;
                if b < a {
                    min_val = Value::Float(b.into());
                }
            }
            _ => return Err(VMError::new_vm("min: all arguments must be numbers".to_string())),
        }
    }

    Ok(min_val)
}

/// Usage: (max number+)
///
/// Return the maximum value from one or more numbers.
///
/// Section: math
///
/// Example:
/// (test::assert-equal 3 (max 1 2 3))
/// (test::assert-equal 10 (max 10 -5 3))
/// (test::assert-equal 42 (max 42))
/// (test::assert-equal 3.0 (max 3.0 1.5 2.0))
/// (test::assert-equal 3.0 (max 1 2.0 3))
/// (test::assert-error (max))
/// (test::assert-error (max 1 "two"))
#[sl_sh_fn(fn_name = "max")]
pub fn max(vals: VarArgs<Value>) -> VMResult<Value> {
    if vals.is_empty() {
        return Err(VMError::new_vm("max: requires at least one argument".to_string()));
    }

    let mut max_val = vals[0];
    let mut is_float = matches!(max_val, Value::Float(_));

    for val in &vals[1..] {
        match (max_val, val) {
            (Value::Int(a_bytes), Value::Int(b_bytes)) => {
                let a = slvm::from_i56(&a_bytes);
                let b = slvm::from_i56(b_bytes);
                if b > a {
                    max_val = *val;
                }
            }
            (Value::Float(a), Value::Float(b)) => {
                if f64::from(*b) > f64::from(a) {
                    max_val = *val;
                }
            }
            (Value::Int(a_bytes), Value::Float(b)) => {
                let a = slvm::from_i56(&a_bytes) as f64;
                let b = f64::from(*b);
                if b > a {
                    max_val = *val;
                    is_float = true;
                } else if is_float {
                    max_val = Value::Float(a.into());
                }
            }
            (Value::Float(a), Value::Int(b_bytes)) => {
                let a = f64::from(a);
                let b = slvm::from_i56(b_bytes) as f64;
                if b > a {
                    max_val = Value::Float(b.into());
                }
            }
            _ => return Err(VMError::new_vm("max: all arguments must be numbers".to_string())),
        }
    }

    Ok(max_val)
}

/// Usage: (pow base power)
///
/// Raise first argument to power of second argument.
///
/// Section: math
///
/// Example:
/// (test::assert-equal 16.0 (pow 4 2))
/// (test::assert-equal 1024.0 (pow 2 10))
/// (test::assert-equal 1.0 (pow 85 0))
/// (test::assert-equal 0.25 (pow 2 -2))
/// (test::assert-error (pow))
/// (test::assert-error (pow 2))
/// (test::assert-error (pow 2 3 4))
#[sl_sh_fn(fn_name = "pow")]
pub fn pow(base: LooseFloat, power: LooseFloat) -> VMResult<f64> {
    let base = f64::from(F56(base.0));
    let power = f64::from(F56(power.0));
    Ok(base.powf(power))
}

/// Usage: (sqrt num)
///
/// Take square root of argument.
///
/// Section: math
///
/// Example:
/// (test::assert-equal 2.0 (sqrt 4))
/// (test::assert-equal 12.0 (sqrt 144))
/// (test::assert-true (< 2.049 (sqrt 4.2) 2.050))
/// (test::assert-error (sqrt))
/// (test::assert-error (sqrt 4 2))
/// (test::assert-error (sqrt -4))
#[sl_sh_fn(fn_name = "sqrt")]
pub fn sqrt(num: LooseFloat) -> VMResult<f64> {
    let num = f64::from(F56(num.0));
    if num < 0.0 {
        Err(VMError::new_vm("sqrt: cannot take square root of negative number".to_string()))
    } else {
        Ok(num.sqrt())
    }
}

/// Usage: (exp power)
///
/// Calculate e raised to the given power.
///
/// Section: math
///
/// Example:
/// (test::assert-equal 1.0 (exp 0))
/// (test::assert-true (< 2.718 (exp 1) 2.719))
/// (test::assert-true (< 7.389 (exp 2) 7.390))
/// (test::assert-error (exp))
/// (test::assert-error (exp 1 2))
#[sl_sh_fn(fn_name = "exp")]
pub fn exp(power: LooseFloat) -> VMResult<f64> {
    let power = f64::from(F56(power.0));
    Ok(power.exp())
}

/// Usage: (log num [base])
///
/// Calculate logarithm of num. If base is not provided, uses natural logarithm (base e).
///
/// Section: math
///
/// Example:
/// (test::assert-equal 0.0 (log 1))
/// (test::assert-true (< 2.302 (log 10) 2.303))
/// (test::assert-equal 3.0 (log 8 2))
/// (test::assert-equal 2.0 (log 100 10))
/// (test::assert-error (log))
/// (test::assert-error (log -1))
/// (test::assert-error (log 10 -2))
#[sl_sh_fn(fn_name = "log")]
pub fn log(num: LooseFloat, base: Option<LooseFloat>) -> VMResult<f64> {
    let num = f64::from(F56(num.0));
    if num <= 0.0 {
        return Err(VMError::new_vm("log: argument must be positive".to_string()));
    }

    match base {
        Some(b) => {
            let base = f64::from(F56(b.0));
            if base <= 0.0 || base == 1.0 {
                Err(VMError::new_vm("log: base must be positive and not equal to 1".to_string()))
            } else {
                Ok(num.log(base))
            }
        }
        None => Ok(num.ln()),
    }
}

/// Usage: (log2 num)
///
/// Calculate base-2 logarithm of num.
///
/// Section: math
///
/// Example:
/// (test::assert-equal 0.0 (log2 1))
/// (test::assert-equal 3.0 (log2 8))
/// (test::assert-equal 10.0 (log2 1024))
/// (test::assert-error (log2))
/// (test::assert-error (log2 -1))
/// (test::assert-error (log2 0))
#[sl_sh_fn(fn_name = "log2")]
pub fn log2(num: LooseFloat) -> VMResult<f64> {
    let num = f64::from(F56(num.0));
    if num <= 0.0 {
        Err(VMError::new_vm("log2: argument must be positive".to_string()))
    } else {
        Ok(num.log2())
    }
}

pub fn add_math_builtins(env: &mut SloshVm) {
    intern_abs(env);
    intern_rem_as_rem(env);
    intern_rem_as_percent(env);
    intern_rem_euclid(env);
    intern_two_pow(env);
    intern_min(env);
    intern_max(env);
    intern_pow(env);
    intern_sqrt(env);
    intern_exp(env);
    intern_log(env);
    intern_log2(env);
}
