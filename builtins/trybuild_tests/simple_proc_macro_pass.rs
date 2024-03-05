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
}

/// obligatory doc
#[sl_sh_fn(fn_name = "triy")]
pub fn return_nil() -> VMResult<()> {
    Ok(())
}
