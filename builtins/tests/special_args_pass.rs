use bridge_macros::sl_sh_fn;
use bridge_types::VarArgs;
use compile_state::state::new_slosh_vm;
use slvm::{VMResult, Value};

pub fn main() {
    let mut vm = new_slosh_vm();
    let args = vec![Value::CodePoint('a')];
    assert!(parse_accept_option(&mut vm, args.as_slice()).is_ok());
    let args = vec![];
    assert!(parse_accept_option(&mut vm, args.as_slice()).is_ok());

    let args = vec![];
    assert!(parse_accept_mult_option(&mut vm, args.as_slice()).is_ok());
    let args = vec![Value::CodePoint('a')];
    assert!(parse_accept_mult_option(&mut vm, args.as_slice()).is_ok());
    let args = vec![Value::CodePoint('a'), Value::CodePoint('a')];
    assert!(parse_accept_mult_option(&mut vm, args.as_slice()).is_ok());
    let args = vec![Value::CodePoint('a'), Value::CodePoint('a')];
    assert!(parse_accept_mult_option(&mut vm, args.as_slice()).is_ok());
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

/// obligatory doc
#[sl_sh_fn(fn_name = "accept_varargs")]
pub fn accept_varargs(_args: VarArgs<char>) -> VMResult<()> {
    Ok(())
}

/// obligatory doc
#[sl_sh_fn(fn_name = "accept_vec")]
pub fn accept_vec(_args: Vec<char>) -> VMResult<()> {
    Ok(())
}
