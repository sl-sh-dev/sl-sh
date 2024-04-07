use crate::SloshVm;
use bridge_adapters::add_builtin;
use slvm::{VMError, VMResult, Value};

pub fn vec_slice(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let (vector, start, end) = match registers.len() {
        2 => {
            if let (Value::Vector(vector), Ok(start)) = (registers[0], registers[1].get_int(vm)) {
                let v = vm.get_vector(vector);
                (v, start as usize, v.len())
            } else {
                return Err(VMError::new_vm("vec-slice: Invalid arguments".to_string()));
            }
        }
        3 => {
            if let (Value::Vector(vector), Ok(start), Ok(end)) = (
                registers[0],
                registers[1].get_int(vm),
                registers[2].get_int(vm),
            ) {
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

pub fn vec_to_list(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
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
        let unsafe_vm: &mut SloshVm = unsafe { (vm as *mut SloshVm).as_mut().unwrap() };
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

pub fn hash_remove(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
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

pub fn hash_hashkey(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
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

pub fn hash_keys(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
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

pub fn flatten(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut flat = Vec::with_capacity(registers.len());
    for i in registers.iter() {
        let mut val = None;
        if i.iter(vm).next().is_some() {
            // is iter
            let vs = i.iter(vm).collect::<Vec<Value>>();
            let v = flatten(vm, vs.as_slice())?;
            val = Some(v);
        } else {
            flat.push(*i)
        }
        if let Some(val) = val {
            let mut vs = val.iter_all(vm).collect::<Vec<Value>>();
            flat.append(&mut vs);
        }
    }
    Ok(vm.alloc_vector(flat))
}

pub fn setup_collection_builtins(env: &mut SloshVm) {
    add_builtin(
        env,
        "flatten",
        flatten,
        "Usage: (flatten & rest)

Takes a sequence composed of individual values or sequences of values and turns
it into one vector of values.

Section: core

Example:
(assert-equal [1 2 3 1 2 3] (flatten 1 2 3 (list 1 2 3)))
(assert-equal [1 2 3 1 2 3] (flatten 1 2 3 [1 2 3]))
(assert-equal [1 2 3 1 2] (flatten 1 2 3 (list 1 2)))
(assert-equal [1 2 3 1 2 3 1 2] (flatten 1 2 3 (list 1 2 3 (list 1 2))))
",
    );
    add_builtin(
        env,
        "vec-slice",
        vec_slice,
        "Usage: (vec-slice vector start end?)

Returns a slice of a vector (0 based indexes, end is exclusive).

Section: vector

Example:
(test::assert-equal [5 6] (vec-slice [1 2 3 4 5 6] 4 6))
(test::assert-equal [1 2 3] (vec-slice [1 2 3 4 5 6] 0 3))
(test::assert-equal [3 4 5] (vec-slice [1 2 3 4 5 6] 2 5))
(test::assert-equal [3 4 5 6] (vec-slice [1 2 3 4 5 6] 2))
",
    );
    add_builtin(
        env,
        "vec->list",
        vec_to_list,
        "Usage: (vec->list vector)

Convert a vector to a list.

Section: vector
",
    );

    /*  XXXX add these
        add_docstring(
            env,
            "vec-remove!",
            "Usage: (vec-remove! vector index) -> vector

    Remove the element at index from vector, shifting all elements after it to the left.
    This is destructive!

    Section: vector

    Example:
    (def test-remove-nth-vec (vec 1 2 3))
    (test::assert-equal '(1 2 3) test-remove-nth-vec)
    (vec-remove! test-remove-nth-vec 1)
    (test::assert-equal '(1 3) test-remove-nth-vec)
    (vec-remove! test-remove-nth-vec 1)
    (test::assert-equal '(1) test-remove-nth-vec)
    (vec-remove! test-remove-nth-vec 0)
    (test::assert-equal '() test-remove-nth-vec)
    ",
        );
        add_docstring(
            env,
            "vec-insert!",
            "Usage: (vec-insert! vector index new-element) -> vector

    Inserts new-element at index and moves following elements right in vector.  This is destructive!

    Section: vector

    Example:
    (def test-insert-nth-vec (vec 1 2 3))
    (test::assert-equal '(1 2 3) test-insert-nth-vec)
    (vec-insert! test-insert-nth-vec 1 5)
    (test::assert-equal '(1 5 2 3) test-insert-nth-vec)
    (vec-insert! test-insert-nth-vec 2 6)
    (test::assert-equal '(1 5 6 2 3) test-insert-nth-vec)
    (vec-insert! test-insert-nth-vec 0 4)
    (test::assert-equal '(4 1 5 6 2 3) test-insert-nth-vec)
    ",
        );
         */
}
