use bridge_macros::sl_sh_fn;
use compile_state::state::new_slosh_vm;
use slvm::{VMResult, Value, F56};

pub fn main() {
    let mut vm = new_slosh_vm();
    let args = vec![];

    assert_eq!(
        Value::Float(F56::from(42_f64)),
        parse_return_option_float(&mut vm, args.as_slice()).unwrap()
    );
    assert_eq!(42f64, return_option_float().unwrap());
    assert_eq!(
        Value::Float(F56::from(42_f64)),
        parse_return_result_float(&mut vm, args.as_slice()).unwrap()
    );
    assert_eq!(42f64, return_result_float().unwrap());
    assert_eq!(
        Value::Float(F56::from(42_f64)),
        parse_return_result_float(&mut vm, args.as_slice()).unwrap()
    );
    assert_eq!(42f64, return_float());
}

/// obligatory doc
#[sl_sh_fn(fn_name = "return_option_float")]
pub fn return_option_float() -> Option<f64> {
    Some(42_f64)
}

/// obligatory doc
#[sl_sh_fn(fn_name = "return_option_float")]
pub fn return_result_float() -> VMResult<f64> {
    Ok(42_f64)
}

/// obligatory doc
#[sl_sh_fn(fn_name = "return_float")]
pub fn return_float() -> f64 {
    42_f64
}
