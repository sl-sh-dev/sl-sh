use bridge_macros::sl_sh_fn;
use compile_state::state::new_slosh_vm;
use slvm::{VMResult, Value};

pub fn main() {
    let mut vm = new_slosh_vm();
    let args = vec![];
    let expected = "return_string".to_string();

    match parse_return_string(&mut vm, args.as_slice()).unwrap() {
        Value::String(h) => {
            let s = vm.get_string(h).to_string();
            assert_eq!(expected, s);
        }
        _ => {
            panic!("Expected Value::String")
        }
    }
    assert_eq!(expected, return_string().unwrap());
}

/// obligatory doc
#[sl_sh_fn(fn_name = "return_string")]
pub fn return_string() -> VMResult<String> {
    Ok("return_string".to_string())
}
