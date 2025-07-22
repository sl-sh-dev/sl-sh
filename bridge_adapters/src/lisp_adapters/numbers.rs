use crate::lisp_adapters::{SlFrom, SlFromRef};
use crate::{BridgeError, BridgeResult};
use bridge_types::{ErrorStrings, LooseFloat, LooseInt, LooseString};
use compile_state::state::SloshVm;
use slvm::float::F56;
use slvm::value::ValueType;
use slvm::{to_i56, Value, ValueTypes, I56};

impl SlFrom<()> for Value {
    fn sl_from(_value: (), _vm: &mut SloshVm) -> BridgeResult<Self> {
        Ok(Value::Nil)
    }
}

impl<'a> SlFromRef<'a, Value> for () {
    fn sl_from_ref(value: Value, _vm: &'a SloshVm) -> BridgeResult<()> {
        match value {
            Value::Nil => Ok(()),
            _ => Err(BridgeError::Error(ErrorStrings::mismatched_type(
                <&'static str>::from(ValueType::Nil),
                value.display_type(_vm),
            ))),
        }
    }
}

impl<'a> SlFromRef<'a, Value> for LooseFloat {
    fn sl_from_ref(value: Value, vm: &'a SloshVm) -> BridgeResult<Self> {
        let res = match value {
            Value::Byte(byte) => {
                let f = byte as f64;
                let f = F56::from(f);
                Ok(f.0)
            }
            Value::Int(i) => {
                let i = I56::from_inner(&i);
                let f = F56::from(i as f64);
                Ok(f.0)
            }
            Value::Float(f) => Ok(f.0),
            v @ (Value::CodePoint(_)
            | Value::CharCluster(_, _)
            | Value::CharClusterLong(_)
            | Value::String(_)
            | Value::Symbol(_)
            | Value::Keyword(_)
            | Value::StringConst(_)) => {
                let f = LooseString::sl_from_ref(v, vm)?
                    .parse::<f64>()
                    .map_err(|_e| BridgeError::Error("Not a valid float.".to_string()))?;
                Ok(F56::from(f).0)
            }
            _ => Err(BridgeError::Error(ErrorStrings::invalid_string(
                <&'static str>::from(ValueType::Int),
                value.display_type(vm),
            ))),
        };
        res.map(LooseFloat::from)
    }
}

impl SlFrom<LooseFloat> for Value {
    fn sl_from(value: LooseFloat, _vm: &mut SloshVm) -> BridgeResult<Self> {
        Ok(Value::Float(F56(value.0)))
    }
}

impl<'a> SlFromRef<'a, Value> for LooseInt {
    fn sl_from_ref(value: Value, vm: &'a SloshVm) -> BridgeResult<Self> {
        let res = match value {
            Value::Byte(byte) => Ok(I56::into_inner(byte as i64)),
            Value::Int(i) => Ok(i),
            Value::Float(f) => {
                let f = f64::from(f);
                let i56 =
                    I56::into_i56_fallible(f).map_err(|e| BridgeError::Error(e.to_string()))?;
                Ok(I56::into_inner(i56))
            }
            v @ (Value::CodePoint(_)
            | Value::Symbol(_)
            | Value::CharCluster(_, _)
            | Value::CharClusterLong(_)
            | Value::String(_)
            | Value::Keyword(_)
            | Value::StringConst(_)) => LooseString::sl_from_ref(v, vm)?
                .parse::<i64>()
                .or(LooseString::sl_from_ref(v, vm)?
                    .parse::<f64>()
                    .map_err(|_e| BridgeError::Error("Not a valid integer.".to_string()))
                    .and_then(|f| {
                        I56::into_i56_fallible(f).map_err(|e| BridgeError::Error(e.to_string()))
                    }))
                .map(I56::into_inner)
                .map_err(|_e| BridgeError::Error("Not a valid integer.".to_string())),
            _ => Err(BridgeError::Error(ErrorStrings::mismatched_type(
                <&'static str>::from(ValueType::Int),
                value.display_type(vm),
            ))),
        };
        res.map(LooseInt::from)
    }
}

impl SlFrom<LooseInt> for Value {
    fn sl_from(value: LooseInt, _vm: &mut SloshVm) -> BridgeResult<Self> {
        Ok(Value::Int(value.0))
    }
}

impl SlFrom<i32> for Value {
    fn sl_from(value: i32, _vm: &mut SloshVm) -> BridgeResult<Self> {
        Ok(to_i56(value as i64))
    }
}
impl SlFrom<u32> for Value {
    fn sl_from(value: u32, _vm: &mut SloshVm) -> BridgeResult<Self> {
        Ok(to_i56(value as i64))
    }
}

impl SlFrom<i64> for Value {
    fn sl_from(value: i64, _vm: &mut SloshVm) -> BridgeResult<Self> {
        Ok(to_i56(value))
    }
}
impl<'a> SlFromRef<'a, Value> for i32 {
    fn sl_from_ref(value: Value, vm: &'a SloshVm) -> BridgeResult<i32> {
        match value {
            Value::Int(num) => {
                let num = I56::from_inner(&num);
                num.try_into().map_err(|_| {
                    BridgeError::Error(
                        "Provided slosh value too small to fit desired type.".to_string(),
                    )
                })
            }
            _ => Err(BridgeError::Error(ErrorStrings::mismatched_type(
                <&'static str>::from(ValueType::Int),
                value.display_type(vm),
            ))),
        }
    }
}

impl SlFrom<f64> for Value {
    fn sl_from(value: f64, _vm: &mut SloshVm) -> BridgeResult<Self> {
        Ok(Value::Float(value.into()))
    }
}

impl<'a> SlFromRef<'a, Value> for f64 {
    fn sl_from_ref(value: Value, vm: &'a SloshVm) -> BridgeResult<Self> {
        match value {
            Value::Float(float) => Ok(f64::from(float)),
            _ => Err(BridgeError::Error(ErrorStrings::mismatched_type(
                <&'static str>::from(ValueType::Float),
                value.display_type(vm),
            ))),
        }
    }
}

impl SlFrom<u8> for Value {
    fn sl_from(value: u8, _vm: &mut SloshVm) -> BridgeResult<Self> {
        Ok(Value::Byte(value))
    }
}

impl<'a> SlFromRef<'a, Value> for u8 {
    fn sl_from_ref(value: Value, vm: &'a SloshVm) -> BridgeResult<Self> {
        match value {
            Value::Byte(i) => Ok(i),
            _ => Err(BridgeError::Error(ErrorStrings::mismatched_type(
                String::from(ValueTypes::from([ValueType::Byte])),
                value.display_type(vm),
            ))),
        }
    }
}

impl SlFrom<usize> for Value {
    fn sl_from(value: usize, _vm: &mut SloshVm) -> BridgeResult<Self> {
        Ok(to_i56(value as i64))
    }
}

impl SlFrom<u64> for Value {
    fn sl_from(value: u64, _vm: &mut SloshVm) -> BridgeResult<Self> {
        Ok(to_i56(value as i64))
    }
}

impl<'a> SlFromRef<'a, Value> for usize {
    fn sl_from_ref(value: Value, vm: &'a SloshVm) -> BridgeResult<Self> {
        match value {
            Value::Int(i) => usize::try_from(I56::from_inner(&i)).map_err(|_| {
                BridgeError::Error(ErrorStrings::mismatched_type(
                    <&'static str>::from(ValueType::Int),
                    value.display_type(vm),
                ))
            }),
            _ => Err(BridgeError::Error(ErrorStrings::mismatched_type(
                String::from(ValueTypes::from([ValueType::Int])),
                value.display_type(vm),
            ))),
        }
    }
}

impl<'a> SlFromRef<'a, Value> for i64 {
    fn sl_from_ref(value: Value, vm: &'a SloshVm) -> BridgeResult<Self> {
        match value {
            Value::Int(i) => Ok(I56::from_inner(&i)),
            _ => Err(BridgeError::Error(ErrorStrings::mismatched_type(
                <&'static str>::from(ValueType::Int),
                value.display_type(vm),
            ))),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lisp_adapters::SlFromRef;
    use crate::lisp_adapters::SlInto;
    use compile_state::state::new_slosh_vm;
    use slvm::{Value, to_i56};

    #[test]
    fn test_i32_conversions_rust_to_value() {
        let mut vm = new_slosh_vm();
        let vm = &mut vm;
        let test_vals = vec![0_i32, 1_i32, -1_i32, i32::MIN, i32::MAX];
        for val in test_vals {
            let val: Value = val.sl_into(vm).expect("i32 can be converted to Value");
            assert!(matches!(val, Value::Int(_)));
        }
    }

    #[test]
    fn test_i32_conversions_value_to_rust() {
        let mut vm = new_slosh_vm();
        let vm = &mut vm;
        let val = to_i56(7_i32 as i64);
        let _val: i32 = i32::sl_from_ref(val, vm).expect("Value can be converted to i32");
    }
}
