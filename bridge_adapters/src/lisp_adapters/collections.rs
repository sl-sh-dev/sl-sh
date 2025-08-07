use crate::lisp_adapters::{SlAsMut, SlAsRef, SlFromRef, SlFromRefMut};
use bridge_types::ErrorStrings;
use compile_state::state::SloshVm;
use slvm::vm_hashmap::VMHashMap;
use slvm::{VMError, VMResult, Value, ValueType};

impl<'a> SlFromRef<'a, Value> for &'a VMHashMap {
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

impl<'a> SlFromRefMut<'a, Value> for &'a mut VMHashMap {
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

impl<'a> SlAsRef<'a, VMHashMap> for &'a Value {
    fn sl_as_ref(&self, vm: &'a SloshVm) -> VMResult<&'a VMHashMap> {
        match self {
            Value::Map(h) => Ok(vm.get_map(*h)),
            _ => Err(VMError::new_conversion(
                ErrorStrings::fix_me_mismatched_type(
                    <&'static str>::from(ValueType::Map),
                    self.display_type(vm),
                ),
            )),
        }
    }
}

impl<'a> SlAsMut<'a, VMHashMap> for &'a Value {
    fn sl_as_mut(&mut self, vm: &'a mut SloshVm) -> VMResult<&'a mut VMHashMap> {
        match self {
            Value::Map(h) => vm.get_map_mut(*h),
            _ => Err(VMError::new_conversion(
                ErrorStrings::fix_me_mismatched_type(
                    <&'static str>::from(ValueType::Map),
                    self.display_type(vm),
                ),
            )),
        }
    }
}
