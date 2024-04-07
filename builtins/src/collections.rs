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

pub fn list_append(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let Some(ret @ Value::List(target, _)) = i.next() {
        match i.next() {
            Some(Value::Vector(to_add)) => {
                let mut with_vec = vm.get_vector(*to_add).to_vec();
                let mut append_to = vm.get_vector(*target).to_vec();
                append_to.append(&mut with_vec);
                Ok(*ret)
            }
            Some(Value::Pair(to_add)) => {
                let p = vm.get_pair(*to_add);
                let mut append_to = vm.get_vector(*target).to_vec();
                append_to.push(p.0);
                list_append(vm, &[*ret, p.1])
            }
            Some(Value::List(to_add, _)) => {
                let mut with_vec = vm.get_vector(*to_add).to_vec();
                let mut append_to = vm.get_vector(*target).to_vec();
                append_to.append(&mut with_vec);
                Ok(*ret)
            }
            Some(Value::Nil) => Ok(*ret),
            val => {
                let l = val.map(|x| x.display_type(vm));
                Err(VMError::new_vm(format!(
                    "list-append: Second argument must be a sequence: {:?}",
                    l
                )))
            }
        }
    } else {
        Err(VMError::new_vm(
            "list-append: First argument must be a list".to_string(),
        ))
    }
}

pub fn vec_append(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let Some(ret @ Value::Vector(target)) = i.next() {
        match i.next() {
            Some(Value::Vector(to_add)) => {
                let mut with_vec = vm.get_vector(*to_add).to_vec();
                let mut append_to = vm.get_vector(*target).to_vec();
                append_to.append(&mut with_vec);
                Ok(*ret)
            }
            Some(Value::Pair(to_add)) => {
                let p = vm.get_pair(*to_add);
                let mut append_to = vm.get_vector(*target).to_vec();
                append_to.push(p.0);
                vec_append(vm, &[*ret, p.1])
            }
            Some(Value::List(to_add, _)) => {
                let mut with_vec = vm.get_vector(*to_add).to_vec();
                let mut append_to = vm.get_vector(*target).to_vec();
                append_to.append(&mut with_vec);
                Ok(*ret)
            }
            Some(Value::Nil) => Ok(*ret),
            val => {
                let l = val.map(|x| x.display_type(vm));
                Err(VMError::new_vm(format!(
                    "vec-append: Second argument must be a sequence: {:?}",
                    l
                )))
            }
        }
    } else {
        Err(VMError::new_vm(
            "vec-append: First argument must be a vector".to_string(),
        ))
    }
}

pub fn setup_collection_builtins(env: &mut SloshVm) {
    add_builtin(
        env,
        "list-append",
        list_append,
        "Usage: (list-append target_list append_to_target)

Return a new vector with all items from append_to_target added to back of target_vector.

Section: pair

Example:
(test::assert-equal '(1 2 3 4 5 6 4 6) (list-append '(1 2 3 4 5 6) [4 6]))
(test::assert-equal '(1 2 3 4 5 6 0 3) (list-append '(1 2 3 4 5 6) (list 0 3)))
(test::assert-equal '(1 2 3 4 5 6 2 5) (list-append '(1 2 3 4 5 6) '(2 5)))
(test::assert-equal '(1 2 3 4 5 6 2) (list-append '(1 2 3 4 5 6) [2]))
",
    );
    add_builtin(
        env,
        "vec-append",
        vec_append,
        "Usage: (vec-append target_vector append_to_target)

Return a new vector with all items from append_to_target added to back of target_vector.

Section: vector

Example:
(test::assert-equal [1 2 3 4 5 6 4 6] (vec-append [1 2 3 4 5 6] [4 6]))
(test::assert-equal [1 2 3 4 5 6 0 3] (vec-append [1 2 3 4 5 6] (list 0 3)))
(test::assert-equal [1 2 3 4 5 6 2 5] (vec-append [1 2 3 4 5 6] (2 . 5)))
(test::assert-equal [1 2 3 4 5 6 2] (vec-append [1 2 3 4 5 6] [2]))
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
