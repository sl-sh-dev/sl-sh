use crate::SloshVm;
use slvm::{VMError, VMResult, Value};

pub fn str_trim(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(string), None) = (i.next(), i.next()) {
        let string = string.get_string(vm)?.trim().to_string();
        Ok(vm.alloc_string(string))
    } else {
        Err(VMError::new_vm("str-trim: takes one argument".to_string()))
    }
}

pub fn str_ltrim(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(string), None) = (i.next(), i.next()) {
        let string = string.get_string(vm)?.trim_start().to_string();
        Ok(vm.alloc_string(string))
    } else {
        Err(VMError::new_vm("str-ltrim: takes one argument".to_string()))
    }
}

pub fn str_rtrim(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(string), None) = (i.next(), i.next()) {
        let string = string.get_string(vm)?.trim_end().to_string();
        Ok(vm.alloc_string(string))
    } else {
        Err(VMError::new_vm("str-rtrim: takes one argument".to_string()))
    }
}

pub fn str_replace(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(string), Some(from), Some(to), None) = (i.next(), i.next(), i.next(), i.next()) {
        let from = from.get_string(vm)?;
        let to = to.get_string(vm)?;
        let new_string = string.get_string(vm)?.replace(from, to);
        Ok(vm.alloc_string(new_string))
    } else {
        Err(VMError::new_vm(
            "str-replace: takes three arguments".to_string(),
        ))
    }
}

pub fn str_contains(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(string), Some(pat), None) = (i.next(), i.next(), i.next()) {
        let string = string.get_string(vm)?;
        let pat = pat.get_string(vm)?;
        if string.contains(&pat) {
            Ok(Value::True)
        } else {
            Ok(Value::False)
        }
    } else {
        Err(VMError::new_vm(
            "str-contains: Invalid arguments".to_string(),
        ))
    }
}
