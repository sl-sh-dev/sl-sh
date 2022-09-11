use slvm::{VMError, VMResult, Value, Vm};
use std::collections::HashMap;

pub fn vec_slice(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    let (vector, start, end) = match registers.len() {
        2 => {
            if let (Value::Vector(vector), Ok(start)) = (registers[0], registers[1].get_int()) {
                let v = vm.get_vector(vector);
                (v, start as usize, v.len())
            } else {
                return Err(VMError::new_vm("vec-slice: Invalid arguments".to_string()));
            }
        }
        3 => {
            if let (Value::Vector(vector), Ok(start), Ok(end)) =
                (registers[0], registers[1].get_int(), registers[2].get_int())
            {
                let v = vm.get_vector(vector);
                (v, start as usize, end as usize)
            } else {
                return Err(VMError::new_vm("vec-slice: Invalid arguments".to_string()));
            }
        }
        _ => {
            return Err(VMError::new_vm(
                "vec-slice: Invalid arguments (requires two or three)".to_string(),
            ))
        }
    };
    let len = vector.len();
    if start == len && end <= len {
        Ok(vm.alloc_vector(Vec::new()))
    } else if start >= len || end > len {
        Err(VMError::new_vm(
            "vec-slice: Invalid arguments- out of bounds".to_string(),
        ))
    } else {
        let new_vec = vector[start..end].to_vec();
        Ok(vm.alloc_vector(new_vec))
    }
}

pub fn vec_to_list(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 1 {
        return Err(VMError::new_vm(
            "vec->list: Invalid arguments (requires one vector)".to_string(),
        ));
    }
    if let Value::Vector(vhandle) = registers[0] {
        // Do this so we can use vector as a slice and not allocate a new useless vector instead.
        // This is safe since we only call alloc_pair and that will not mess with vector.
        // Note, this also needs register[0] to NOT be garbage collected during a call to alloc_pair,
        // this should be fine since it is from a register and therefore will be a root.
        let unsafe_vm: &mut Vm = unsafe { (vm as *mut Vm).as_mut().unwrap() };
        let vector = vm.get_vector(vhandle);

        let mut last = Value::Nil;
        for item in vector.iter().rev() {
            let old_last = last;
            last = unsafe_vm.alloc_pair(*item, old_last);
        }
        Ok(last)
    } else {
        Err(VMError::new_vm(
            "vec->list: Invalid arguments (requires one vector)".to_string(),
        ))
    }
}

pub fn make_hash(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() % 2 != 0 {
        return Err(VMError::new_vm(
            "make-hash: Invalid arguments (must be even, [key val]*)".to_string(),
        ));
    }
    let mut map = HashMap::new();
    let mut args = registers.iter();
    while let (Some(key), Some(val)) = (args.next(), args.next()) {
        map.insert(*key, *val);
    }
    Ok(vm.alloc_map(map))
}

pub fn hash_set(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(Value::Map(map_handle)), Some(key), Some(val), None) =
        (i.next(), i.next(), i.next(), i.next())
    {
        let map = vm.get_map_mut(*map_handle)?;
        map.insert(*key, *val);
        Ok(Value::Map(*map_handle))
    } else {
        Err(VMError::new_vm(
            "takes three arguments (hash-map key value)".to_string(),
        ))
    }
}

pub fn hash_remove(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(Value::Map(map_handle)), Some(key), None) = (i.next(), i.next(), i.next()) {
        let map = vm.get_map_mut(*map_handle)?;
        if let Some(old) = map.remove(key) {
            Ok(old)
        } else {
            Ok(Value::Nil)
        }
    } else {
        Err(VMError::new_vm(
            "takes three arguments (hash-map key value)".to_string(),
        ))
    }
}

pub fn hash_get(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(Value::Map(map_handle)), Some(key), default, None) =
        (i.next(), i.next(), i.next(), i.next())
    {
        let map = vm.get_map(*map_handle);
        if let Some(val) = map.get(key) {
            Ok(*val)
        } else if let Some(val) = default {
            Ok(*val)
        } else {
            Ok(Value::Nil)
        }
    } else {
        Err(VMError::new_vm(
            "takes two or three arguments (hash-map key default?)".to_string(),
        ))
    }
}

pub fn hash_hashkey(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(Value::Map(map_handle)), Some(key), None) = (i.next(), i.next(), i.next()) {
        let map = vm.get_map(*map_handle);
        if map.contains_key(key) {
            Ok(Value::True)
        } else {
            Ok(Value::False)
        }
    } else {
        Err(VMError::new_vm(
            "takes two arguments (hash-map key)".to_string(),
        ))
    }
}

pub fn hash_keys(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(Value::Map(map_handle)), None) = (i.next(), i.next()) {
        let map = vm.get_map(*map_handle);
        let mut keys = Vec::with_capacity(map.len());
        for key in map.keys() {
            keys.push(*key);
        }
        Ok(vm.alloc_vector(keys))
    } else {
        Err(VMError::new_vm("takes one argument (hash-map)".to_string()))
    }
}

pub fn hash_clear(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(Value::Map(map_handle)), None) = (i.next(), i.next()) {
        let map = vm.get_map_mut(*map_handle)?;
        map.clear();
        Ok(Value::Map(*map_handle))
    } else {
        Err(VMError::new_vm("takes one argument (hash-map)".to_string()))
    }
}
