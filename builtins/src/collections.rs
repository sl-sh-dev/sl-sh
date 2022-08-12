use crate::utils::{add_builtin, add_docstring};
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

pub fn setup_vecs(vm: &mut Vm) {
    add_docstring(
        vm,
        "vec",
        "Usage: (vec item1 item2 .. itemN)

Make a new vector with items.

Section: vector

Example:
(test::assert-equal '() (vec))
(test::assert-equal '(1 2 3) (vec 1 2 3))
",
    );
    add_docstring(
        vm,
        "make-vec",
        "Usage: (make-vec capacity default)

Make a new vector with capacity and default item(s).

Section: vector

Example:
(test::assert-equal '() (make-vec))
(test::assert-equal '(x x x) (make-vec 3 'x))
(test::assert-equal '(nil nil nil nil nil) (make-vec 5 nil))
(test::assert-equal '() (make-vec 5))
",
    );
    add_docstring(
        vm,
        "vec-nth",
        "Usage: (vec-nth vector index) -> object

Get the nth element (0 based) of a vector. If you need the equivalent operation
on a list use [nth](root::nth).

Section: vector

Example:
(test::assert-equal 5 (vec-nth '#(1 2 3 4 5 6) 4))
(test::assert-equal 1 (vec-nth '#(1 2 3 4 5 6) 0))
(test::assert-equal 3 (vec-nth '#(1 2 3 4 5 6) 2))
(test::assert-equal 6 (vec-nth '#(1 2 3 4 5 6) 5))
",
    );
    add_docstring(
        vm,
        "vec-set!",
        "Usage: (vec-set! vector index value) -> vector

Set the nth index (0 based) of a vector to value. This is destructive! If you
need the equivalent operation on a list use [setnth!](root::setnth!).

Section: vector

Example:
(def test-setnth-vec (vec 1 2 3))
(test::assert-equal '(1 5 3) (vec-set! test-setnth-vec 1 5))
(test::assert-equal '(7 5 3) (vec-set! test-setnth-vec 0 7))
(test::assert-equal '(7 5 9) (vec-set! test-setnth-vec 2 9))
",
    );
    add_docstring(
        vm,
        "vec-push!",
        "Usage: (vec-push! vector object) -> vector

Pushes the provided object onto the end of the vector.  This is destructive!

Section: vector

Example:
(def test-push-vec (vec))
(test::assert-equal '(1) (vec-push! test-push-vec 1))
(test::assert-equal '(1) test-push-vec)
(test::assert-equal '(1 2) (vec-push! test-push-vec 2))
(test::assert-equal '(1 2) test-push-vec)
(test::assert-equal '(1 2 3) (vec-push! test-push-vec 3))
(test::assert-equal '(1 2 3) test-push-vec)
",
    );
    add_docstring(
        vm,
        "vec-pop!",
        "Usage: (vec-pop! vector) -> object

Pops the last object off of the end of the vector.  This is destructive!

Section: vector

Example:
(def test-pop-vec (vec 1 2 3))
(test::assert-equal 3 (vec-pop! test-pop-vec))
(test::assert-equal '(1 2) test-pop-vec)
(test::assert-equal 2 (vec-pop! test-pop-vec))
(test::assert-equal '(1) test-pop-vec)
(test::assert-equal 1 (vec-pop! test-pop-vec))
(test::assert-equal '() test-pop-vec)
",
    );
    add_builtin(
        vm,
        "vec-slice",
        vec_slice,
        "Usage: (vec-slice vector start end?)

Returns a slice of a vector (0 based indexes, end is exclusive).

Section: vector

Example:
(test::assert-equal '(5 6) (vec-slice '#(1 2 3 4 5 6) 4 6))
(test::assert-equal '(1 2 3) (vec-slice '#(1 2 3 4 5 6) 0 3))
(test::assert-equal '(3 4 5) (vec-slice '#(1 2 3 4 5 6) 2 5))
(test::assert-equal '(3 4 5 6) (vec-slice '#(1 2 3 4 5 6) 2))
",
    );
    add_builtin(
        vm,
        "vec->list",
        vec_to_list,
        "Usage: (vec->list vector)

Convert a vector to a list.

Section: vector
",
    );

    add_docstring(
        vm,
        "vec-empty?",
        "Usage: (vec-empty? vector)

True if the vector is empty.

Section: vector

Example:
(test::assert-true (vec-empty? '#()))
(test::assert-false (vec-empty? '#(1 2 3)))
",
    );
    add_docstring(
        vm,
        "vec-clear!",
        "Usage: (vec-clear! vector)

Clears a vector.  This is destructive!

Section: vector

Example:
(def test-clear-vec (vec 1 2 3))
(test::assert-false (vec-empty? test-clear-vec))
(vec-clear! test-clear-vec)
(test::assert-true (vec-empty? test-clear-vec))
",
    );
    add_docstring(
        vm,
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
        vm,
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
}
