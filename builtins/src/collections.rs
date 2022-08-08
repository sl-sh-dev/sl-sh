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
        let vector = vm.get_vector(vhandle).to_vec();

        let mut last = Value::Nil;
        for item in vector.iter().rev() {
            let old_last = last;
            last = vm.alloc_pair(*item, old_last);
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
