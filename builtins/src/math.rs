use crate::SloshVm;
use bridge_macros::sl_sh_fn;
use slvm::{VMError, VMResult, Value};

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

pub fn add_math_builtins(env: &mut SloshVm) {
    intern_abs(env);
}
