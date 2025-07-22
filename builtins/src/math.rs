use crate::SloshVm;
use bridge_macros::sl_sh_fn;
use slvm::{VMError, VMResult, Value};

/// Usage: (abs number) => absolute-value
///
/// Returns the absolute value of number.
///
/// Arguments:
/// - number: A real number (integer, float, or byte).
/// - absolute-value: A non-negative number of the same type as number.
///
/// The absolute value of a real number is its magnitude without regard to sign.
/// For non-negative numbers, abs returns the number unchanged.
/// For negative numbers, abs returns the negation of the number.
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

/// Usage: (% dividend divisor) => remainder
///
/// Returns the remainder of dividing dividend by divisor.
///
/// Arguments:
/// - dividend: An integer.
/// - divisor: A non-zero integer.
/// - remainder: An integer with the same sign as dividend.
///
/// This function computes the remainder using truncated division (same as C's % operator).
/// The sign of the result matches the sign of the dividend.
/// For Euclidean remainder with always non-negative results, use rem-euclid.
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

/// Usage: (rem dividend divisor) => remainder
///
/// Returns the remainder of dividing dividend by divisor.
///
/// Arguments:
/// - dividend: An integer. The number being divided.
/// - divisor: An integer. The non-zero number to divide by.
/// - remainder: An integer. The remainder with the same sign as dividend.
///
/// Note: Remainder and Modulo are two similar mathematical operations,
/// called `rem` and `rem-euclid` in Rust.
/// This function uses `rem` which is the same as the `%` operator in C.
/// With `rem`, the sign of the result is the same as the dividend (arg 1).
/// With `rem-euclid`, the sign of the result is always positive.
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

/// Usage: (rem-euclid dividend divisor) => remainder
///
/// Returns the Euclidean remainder of dividing dividend by divisor.
///
/// Arguments:
/// - dividend: An integer. The number being divided.
/// - divisor: An integer. The non-zero number to divide by.
/// - remainder: An integer. Always non-negative (0 <= result < |divisor|).
///
/// Least non-negative number that can be added to a multiple of the divisor to get the dividend.
/// Note: Remainder and Modulo are two similar mathematical operations,
/// called `rem` and `rem-euclid` in Rust.
/// With `rem`, the sign of the result is the same as the dividend.
/// With `rem-euclid`, the sign of the result is always positive.
///
/// Section: math
///
/// Example:
/// (test::assert-equal 0 (rem-euclid 50 10))
/// (test::assert-equal 5 (rem-euclid 55 10))
/// (test::assert-equal 1 (rem-euclid 1 2))
/// (test::assert-equal 2 (rem-euclid -10 3))
/// (test::assert-equal 1 (rem-euclid 10 -3))
/// (test::assert-equal 2 (rem-euclid -10 -3))
///
/// (test::assert-error (rem-euclid))
/// (test::assert-error (rem-euclid 1))
/// (test::assert-error (rem-euclid 1 2 3))
/// (test::assert-error (rem-euclid 1 2.0))
#[sl_sh_fn(fn_name = "rem-euclid")]
pub fn rem_euclid(dividend: i64, divisor: i64) -> VMResult<i64> {
    dividend.checked_rem_euclid(divisor).ok_or_else(|| {
        VMError::new_vm(format!(
            "rem-euclid: division by zero or overflow: {} % {}",
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
