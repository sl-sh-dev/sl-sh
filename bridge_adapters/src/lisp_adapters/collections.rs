use crate::lisp_adapters::{SlFromRef, SlFromRefMut};
use bridge_types::ErrorStrings;
use compile_state::state::SloshVm;
use slvm::{VMError, VMResult, Value, ValueType};
use std::collections::HashMap;

impl<'a> SlFromRef<'a, Value> for &'a HashMap<Value, Value> {
    fn sl_from_ref(value: Value, vm: &'a SloshVm) -> VMResult<Self> {
        match value {
            Value::Map(h) => Ok(vm.get_map(h)),
            _ => Err(VMError::new_conversion(
                ErrorStrings::fix_me_mismatched_type(
                    <&'static str>::from(ValueType::Map),
                    value.display_type(vm),
                ),
            )),
        }
    }
}

impl<'a> SlFromRefMut<'a, Value> for &'a mut HashMap<Value, Value> {
    fn sl_from_ref_mut(value: Value, vm: &'a mut SloshVm) -> VMResult<Self> {
        match value {
            Value::Map(h) => Ok(vm.get_map_mut(h)?),
            _ => Err(VMError::new_conversion(
                ErrorStrings::fix_me_mismatched_type(
                    <&'static str>::from(ValueType::Map),
                    value.display_type(vm),
                ),
            )),
        }
    }
}
