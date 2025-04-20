use bridge_macros::sl_sh_fn;
use bridge_types::{LooseString, SloshChar};
use compile_state::state::new_slosh_vm;
use slvm::{VMResult, Value};
use std::borrow::Cow;

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

    match parse_return_loose_string(&mut vm, args.as_slice()).unwrap() {
        Value::String(h) => {
            let s = vm.get_string(h).to_string();
            assert_eq!(expected, s);
        }
        _ => {
            panic!("Expected Value::String")
        }
    }
    assert_eq!(LooseString::Owned(expected), return_string().unwrap());

    let expected = "a";
    match parse_return_slosh_char(&mut vm, args.as_slice()).unwrap() {
        Value::String(h) => {
            let s = vm.get_string(h);
            assert_eq!(expected, s);
        }
        _ => {
            panic!("Expected Value::String")
        }
    }
    assert_eq!(
        SloshChar::String(Cow::Borrowed(expected)),
        return_slosh_char().unwrap()
    );
}

/// obligatory doc
#[sl_sh_fn(fn_name = "return_string")]
pub fn return_string() -> VMResult<String> {
    Ok("return_string".to_string())
}

//TODO PC document use of generics in return parameters and limitations,
/// obligatory doc
#[sl_sh_fn(fn_name = "return_loose_string")]
pub fn return_loose_string<'a>() -> VMResult<LooseString<'a>> {
    Ok(LooseString::Owned("return_string".to_string()))
}
/// obligatory doc
#[sl_sh_fn(fn_name = "return_slosh_char")]
pub fn return_slosh_char<'a>() -> VMResult<SloshChar<'a>> {
    Ok(SloshChar::String(Cow::Borrowed("a")))
}

/// obligatory doc
#[sl_sh_fn(fn_name = "accept_str")]
pub fn accept_str(_s: &str) {}

/// obligatory doc
#[sl_sh_fn(fn_name = "accept_str_mut")]
pub fn accept_str_mut(_s: &mut String) {}

//TODO #220 return &mut String?
// require attribute?

///// obligatory doc
//#[sl_sh_fn(fn_name = "accept_str_and_ret")]
//pub fn accept_str_return_str(s: &str) -> &str {
//    s
//}
