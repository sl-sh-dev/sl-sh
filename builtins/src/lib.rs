use slvm::{VMError, VMResult, Value, Vm};

pub mod collections;
pub mod print;
pub mod string;
pub mod utils;

pub fn get_prop(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 2 {
        return Err(VMError::new_vm(
            "get-prop: Invalid arguments (object symbol)".to_string(),
        ));
    }
    let key = match registers[1] {
        Value::Keyword(key) => key,
        Value::Symbol(key) => key,
        _ => return Err(VMError::new_vm("get-prop: key must be a keywork or symbol")),
    };
    match registers[0] {
        Value::Global(idx) => Ok(vm.get_global_property(idx, key).unwrap_or(Value::Nil)),
        Value::Symbol(si) => {
            if let Some(idx) = vm.global_intern_slot(si) {
                Ok(vm.get_global_property(idx, key).unwrap_or(Value::Nil))
            } else {
                Ok(Value::Nil)
            }
        }
        _ => {
            let handle = registers[0].get_handle().ok_or_else(|| {
                VMError::new_vm("get-prop: Not a heap object or global symbol".to_string())
            })?;
            Ok(vm
                .get_heap_property_interned(handle, key)
                .unwrap_or(Value::Nil))
        }
    }
}

pub fn set_prop(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 3 {
        return Err(VMError::new_vm(
            "set-prop: Invalid arguments (object symbol value)".to_string(),
        ));
    }
    let key = match registers[1] {
        Value::Keyword(key) => key,
        Value::Symbol(key) => key,
        _ => return Err(VMError::new_vm("set-prop: key must be a keyword or symbol")),
    };
    if let Value::Global(idx) = registers[0] {
        vm.set_global_property(idx, key, registers[2]);
        Ok(registers[2])
    } else {
        let handle = registers[0].get_handle().ok_or_else(|| {
            VMError::new_vm("set-prop: Not a heap object or global symbol".to_string())
        })?;
        vm.set_heap_property_interned(handle, key, registers[2]);
        Ok(registers[2])
    }
}

pub fn sizeof_heap_object(_vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    if !registers.is_empty() {
        return Err(VMError::new_vm(
            "sizeof-heap-object: takes no arguments".to_string(),
        ));
    }
    Ok(Value::UInt(Vm::sizeof_heap_object() as u64))
}

pub fn sizeof_value(_vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    if !registers.is_empty() {
        return Err(VMError::new_vm(
            "sizeof-value: takes no arguments".to_string(),
        ));
    }
    Ok(Value::UInt(std::mem::size_of::<Value>() as u64))
}
