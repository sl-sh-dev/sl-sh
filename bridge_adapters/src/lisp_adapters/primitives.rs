//! As of now for casting slosh value types to a boolean with the sl_sh_fn macro,
//! Value::* and Value::True map to bool true rust values
//! and Value::False, Value::Nil, and Value::Undefined map to bool false.

use crate::lisp_adapters::{SlFrom, SlFromRef};
use compile_state::state::SloshVm;
use slvm::{VMResult, Value};

impl SlFromRef<'_, Value> for Value {
    fn sl_from_ref(value: Value, _vm: &SloshVm) -> VMResult<Self> {
        Ok(value)
    }
}

impl SlFromRef<'_, Value> for bool {
    fn sl_from_ref(value: Value, _vm: &SloshVm) -> VMResult<Self> {
        match value {
            Value::True => Ok(true),
            Value::False => Ok(false),
            Value::Nil => Ok(false),
            Value::Undefined => panic!("Undefined is the default initialized state of a value and its presence in actual code is a VM logical error."),
            _ => Ok(true),
        }
    }
}

impl SlFrom<bool> for Value {
    fn sl_from(value: bool, _vm: &mut SloshVm) -> VMResult<Self> {
        if value {
            Ok(Value::True)
        } else {
            Ok(Value::False)
        }
    }
}

impl<T> SlFrom<Value> for Option<T>
where
    T: SlFrom<Value>,
{
    fn sl_from(value: Value, vm: &mut SloshVm) -> VMResult<Self> {
        if value.is_nil() {
            Ok(None)
        } else {
            let t = T::sl_from(value, vm)?;
            Ok(Some(t))
        }
    }
}

impl<'a, T> SlFromRef<'a, Value> for Option<T>
where
    T: SlFromRef<'a, Value>,
{
    fn sl_from_ref(value: Value, vm: &'a SloshVm) -> VMResult<Self> {
        if value.is_nil() {
            Ok(None)
        } else {
            let t = T::sl_from_ref(value, vm)?;
            Ok(Some(t))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lisp_adapters::{SlInto, SlIntoRef};
    use compile_state::state::new_slosh_vm;
    use slvm::Value;

    #[test]
    fn test_bool_conversions_from_rust() {
        let mut vm = new_slosh_vm();
        let vm = &mut vm;
        let f = false;
        let val: Value = f.sl_into(vm).expect("false can be converted to Value");
        assert_eq!(val, Value::False);

        let t = true;
        let val: Value = t.sl_into(vm).expect("true can be converted to Value");
        assert_eq!(val, Value::True);
    }

    #[test]
    fn test_bool_conversions_to_rust() {
        let mut vm = new_slosh_vm();
        let vm = &mut vm;
        let n = Value::Nil;
        let n: bool = n
            .sl_into_ref(vm)
            .expect("Value::Nil can be converted to bool");
        assert_eq!(n, false);

        let f = Value::False;
        let f: bool = f
            .sl_into_ref(vm)
            .expect("Value::False can be converted to bool");
        assert_eq!(f, false);

        let t = Value::True;
        let t: bool = t
            .sl_into_ref(vm)
            .expect("Value::True can be converted to bool");
        assert_eq!(t, true);
    }
}
