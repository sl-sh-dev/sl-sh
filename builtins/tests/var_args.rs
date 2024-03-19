use bridge_macros::sl_sh_fn;
use bridge_types::VarArgs;
use compile_state::state::new_slosh_vm;
use slvm::{VMResult, Value};

pub fn main() {
    let mut vm = new_slosh_vm();

    let no_args = vec![];
    assert!(parse_accept_varargs(&mut vm, no_args.as_slice()).is_ok());
    let one_arg = vec![Value::CodePoint('a')];
    assert!(parse_accept_varargs(&mut vm, one_arg.as_slice()).is_ok());
    let two_args = vec![Value::CodePoint('a'), Value::CodePoint('a')];
    assert!(parse_accept_varargs(&mut vm, two_args.as_slice()).is_ok());
    let three_args = vec![
        Value::CodePoint('a'),
        Value::CodePoint('a'),
        Value::CodePoint('a'),
    ];
    assert!(parse_accept_varargs(&mut vm, three_args.as_slice()).is_ok());

    // TODO PC
    // business about accepting empty vec vs empty list AS VarArgs OR Vec!
}

/// obligatory doc
#[sl_sh_fn(fn_name = "accept_varargs")]
pub fn accept_varargs(_args: VarArgs<char>) -> VMResult<()> {
    Ok(())
}
