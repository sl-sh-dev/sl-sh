use bridge_macros::sl_sh_fn;
use compile_state::state::new_slosh_vm;
use slvm::{VMResult, Value};

pub fn main() {
    let mut vm = new_slosh_vm();

    let args = vec![Value::CodePoint('a')];
    assert!(parse_accept_option(&mut vm, args.as_slice()).is_ok());
    let args = vec![];
    assert!(parse_accept_option(&mut vm, args.as_slice()).is_ok());

    let no_args = vec![];
    assert!(parse_accept_mult_option(&mut vm, no_args.as_slice()).is_ok());
    let one_arg = vec![Value::CodePoint('a')];
    assert!(parse_accept_mult_option(&mut vm, one_arg.as_slice()).is_ok());
    let two_args = vec![Value::CodePoint('a'), Value::CodePoint('a')];
    assert!(parse_accept_mult_option(&mut vm, two_args.as_slice()).is_ok());
    let three_args = vec![
        Value::CodePoint('a'),
        Value::CodePoint('a'),
        Value::CodePoint('a'),
    ];
    assert!(parse_accept_mult_option(&mut vm, three_args.as_slice()).is_err());
}

/// obligatory doc
#[sl_sh_fn(fn_name = "accept_option")]
pub fn accept_option(_opt: Option<char>) -> VMResult<()> {
    Ok(())
}

/// obligatory doc
#[sl_sh_fn(fn_name = "accept_mult_option")]
pub fn accept_mult_option(_opt1: Option<char>, _opt2: Option<char>) -> VMResult<()> {
    Ok(())
}
