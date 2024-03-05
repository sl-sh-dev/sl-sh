use bridge_macros::sl_sh_fn;
use compile_state::state::new_slosh_vm;
use slvm::{VMResult, Value};

pub fn main() {
    let mut vm = new_slosh_vm();
    let args = vec![];
    assert_eq!(
        Value::Nil,
        parse_return_nil(&mut vm, args.as_slice()).unwrap()
    );
    assert_eq!((), return_nil().unwrap());
    assert_eq!(
        Value::Nil,
        parse_return_nil2(&mut vm, args.as_slice()).unwrap()
    );
    assert_eq!((), return_nil2());
    assert_eq!(
        Value::Nil,
        parse_return_option(&mut vm, args.as_slice()).unwrap()
    );
    assert_eq!((), return_option().unwrap());

    assert_eq!(
        Value::Nil,
        parse_return_option_none(&mut vm, args.as_slice()).unwrap()
    );
    assert_eq!((), return_option().unwrap());
}

/// obligatory doc
#[sl_sh_fn(fn_name = "return_nil")]
pub fn return_nil() -> VMResult<()> {
    Ok(())
}

/// obligatory doc
#[sl_sh_fn(fn_name = "return_nil2")]
pub fn return_nil2() {}

/// obligatory doc
#[sl_sh_fn(fn_name = "return_option")]
pub fn return_option() -> Option<()> {
    Some(())
}

/// obligatory doc
#[sl_sh_fn(fn_name = "return_option_none")]
pub fn return_option_none() -> Option<()> {
    None
}
