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

    assert_eq!(
        Value::True,
        parse_return_true(&mut vm, args.as_slice()).unwrap()
    );
    assert_eq!(true, return_true());

    assert_eq!(
        Value::False,
        parse_return_false(&mut vm, args.as_slice()).unwrap()
    );
    assert_eq!(false, return_false());

    let point = 'a';
    assert_eq!(
        Value::CodePoint(point),
        parse_return_char(&mut vm, args.as_slice()).unwrap()
    );

    let args = vec![Value::Float(F56::from(42_f64))];
    assert_eq!(
        Value::Nil,
        parse_accept_float(&mut vm, args.as_slice()).unwrap()
    );
    assert_eq!((), accept_float(42_f64));

    //TODO PC fix error messages, at least 1 arguments is wrong here and weird.
    let args = vec![];
    let err = parse_accept_float(&mut vm, args.as_slice()).unwrap_err();
    assert_eq!(
        "[rt]: accept_float not given enough arguments, expected at least 1 arguments, got 0.",
        err.to_string()
    );

    let args = vec![
        Value::Float(F56::from(42_f64)),
        Value::Float(F56::from(42_f64)),
    ];
    let err = parse_accept_float(&mut vm, args.as_slice()).unwrap_err();
    assert_eq!(
        "[rt]: accept_float given too many arguments, expected at least 1 arguments, got 2.",
        err.to_string()
    );

    let args = vec![Value::Float(F56::from(42_f64))];
    assert_eq!(
        Value::Float(F56::from(42_f64)),
        parse_accept_and_return_float(&mut vm, args.as_slice()).unwrap()
    );
    assert_eq!(42_f64, accept_and_return_float(42_f64));
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

/// obligatory doc
#[sl_sh_fn(fn_name = "return_true")]
pub fn return_true() -> bool {
    true
}

/// obligatory doc
#[sl_sh_fn(fn_name = "return_false")]
pub fn return_false() -> bool {
    false
}

/// obligatory doc
#[sl_sh_fn(fn_name = "return_char")]
pub fn return_char() -> char {
    'a'
}

/// obligatory doc
#[sl_sh_fn(fn_name = "accept_float")]
pub fn accept_float(_f: f64) {}

/// obligatory doc
#[sl_sh_fn(fn_name = "accept_return_float")]
pub fn accept_and_return_float(f: f64) -> f64 {
    f
}
