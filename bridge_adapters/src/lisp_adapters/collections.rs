use crate::lisp_adapters::{SlFromRef, SlFromRefMut};
use crate::{BridgeResult, BridgeError};
use bridge_types::ErrorStrings;
use compile_state::state::SloshVm;
use slvm::vm_hashmap::VMHashMap;
use slvm::{Value, ValueType};


impl<'a> SlFromRef<'a, Value> for &'a VMHashMap {
    fn sl_from_ref(value: Value, vm: &'a SloshVm) -> BridgeResult<Self> {
        match value {
            Value::Map(h) => Ok(vm.get_map(h)),
            _ => Err(BridgeError::Error(
                ErrorStrings::mismatched_type(
                    <&'static str>::from(ValueType::Map),
                    value.display_type(vm),
                ),
            )),
        }
    }
}

impl<'a> SlFromRefMut<'a, Value> for &'a mut VMHashMap {
    fn sl_from_ref_mut(value: Value, vm: &'a mut SloshVm) -> BridgeResult<Self> {
        match value {
            Value::Map(h) => vm.get_map_mut(h).map_err(|e| BridgeError::Error(e.to_string())),
            _ => Err(BridgeError::Error(
                ErrorStrings::mismatched_type(
                    <&'static str>::from(ValueType::Map),
                    value.display_type(vm),
                ),
            )),
        }
    }
}
