use crate::lisp_adapters::SlFrom;
use compile_state::state::SloshVm;
use slvm::{VMResult, Value};

impl SlFrom<&Value> for bool {
    fn sl_from(value: &Value, _vm: &mut SloshVm) -> VMResult<Self> {
        match value {
            Value::True => Ok(true),
            Value::False => Ok(false),
            Value::Nil => Ok(false),
            //TODO XXX should undefined be included here?
            Value::Undefined => Ok(false),
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

#[cfg(test)]
mod tests {
    use crate::lisp_adapters::SlInto;
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
        let n: bool = (&n)
            .sl_into(vm)
            .expect("Value::Nil can be converted to bool");
        assert_eq!(n, false);

        let f = Value::False;
        let f: bool = (&f)
            .sl_into(vm)
            .expect("Value::False can be converted to bool");
        assert_eq!(f, false);

        let t = Value::True;
        let t: bool = (&t)
            .sl_into(vm)
            .expect("Value::True can be converted to bool");
        assert_eq!(t, true);
    }
}
