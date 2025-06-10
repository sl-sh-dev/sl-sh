use crate::SloshVm;
use bridge_macros::sl_sh_fn;
use bridge_types::LooseFloat;
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
/// (load "test.slosh")
/// (import test)
///
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

pub fn add_math_builtins(env: &mut SloshVm) {
    intern_abs(env);
    intern_rem_as_rem(env);
    intern_rem_as_percent(env);
    intern_rem_euclid(env);
}
