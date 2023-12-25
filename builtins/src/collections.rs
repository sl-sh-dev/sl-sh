use crate::{add_builtin, SloshVm};
use slvm::{VMError, VMResult, Value};
use std::collections::HashMap;
use unicode_segmentation::UnicodeSegmentation;

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

pub fn make_hash(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
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

pub fn hash_set(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
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

pub fn hash_get(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
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

fn hash_clear(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(Value::Map(map_handle)), None) = (i.next(), i.next()) {
        let map = vm.get_map_mut(*map_handle)?;
        map.clear();
        Ok(Value::Map(*map_handle))
    } else {
        Err(VMError::new_vm("takes one argument (hash-map)".to_string()))
    }
}

fn length(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(val), None) = (i.next(), i.next()) {
        match val {
            Value::String(h) => {
                let mut len: i32 = 0;
                for _ in UnicodeSegmentation::graphemes(vm.get_string(*h), true) {
                    len += 1;
                }
                Ok(Value::Int32(len))
            }
            Value::StringConst(i) => {
                let mut len: i32 = 0;
                for _ in UnicodeSegmentation::graphemes(vm.get_interned(*i), true) {
                    len += 1;
                }
                Ok(Value::Int32(len))
            }
            Value::Vector(h) => Ok(Value::Int32(vm.get_vector(*h).len() as i32)),
            Value::List(h, i) => Ok(Value::Int32(vm.get_vector(*h).len() as i32 - *i as i32)),
            Value::Pair(h) => {
                let mut len: i32 = 1;
                let (_, mut cdr) = vm.get_pair(*h);
                while let Value::Pair(h) = cdr {
                    let (_, c) = vm.get_pair(h);
                    cdr = c;
                    len += 1;
                }
                Ok(Value::Int32(len))
            }
            Value::Map(h) => Ok(Value::Int32(vm.get_map(*h).len() as i32)),
            Value::Nil => Ok(Value::Int32(0)),
            _ => Err(VMError::new_vm(format!(
                "len: net valid for value of type {}",
                val.display_type(vm)
            ))),
        }
    } else {
        Err(VMError::new_vm("len: takes one argument".to_string()))
    }
}

pub fn setup_collection_builtins(env: &mut SloshVm) {
    add_builtin(
        env,
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
    add_builtin(
        env,
        "make-hash",
        make_hash,
        "Usage: (make-hash associations?)

Make a new hash map.

If associations is provided (makes an empty map if not) then it is a list of
pairs (key . value) that populate the initial map.  Neither key nor value in the
associations will be evaluated.

Section: hashmap

",
    );
    add_builtin(
        env,
        "hash-clear!",
        hash_clear,
    "Usage: (hash-clear! hashmap)

Clears a hashmap.  This is a destructive form!

Section: hashmap

Example:
(def tst-hash (make-hash '((:key1 . \"val one\")(key2 . \"val two\")(\"key3\" . \"val three\")(#\\S . \"val S\"))))
(test::assert-equal 4 (length (hash-keys tst-hash)))
(test::assert-true (hash-haskey tst-hash :key1))
(test::assert-true (hash-haskey tst-hash 'key2))
(test::assert-true (hash-haskey tst-hash \"key3\"))
(test::assert-true (hash-haskey tst-hash #\\S))
(hash-clear! tst-hash)
(test::assert-equal 0 (length (hash-keys tst-hash)))
(test::assert-false (hash-haskey tst-hash :key1))
(test::assert-false (hash-haskey tst-hash 'key2))
(test::assert-false (hash-haskey tst-hash \"key3\"))
(test::assert-false (hash-haskey tst-hash #\\S))",
    );
    add_builtin(
        env,
        "len",
        length,
        r#"Usage: (length expression) -> int

Return length of supplied expression.

Section: core

Example:
(test::assert-equal 0 (length nil))
(test::assert-equal 5 (length \"12345\"))
; Note the unicode symbol is only one char even though it is more then one byte.
(test::assert-equal 6 (length \"12345Σ\"))
(test::assert-equal 3 (length '(1 2 3)))
(test::assert-equal 3 (length '#(1 2 3)))
(test::assert-equal 3 (length (list 1 2 3)))
(test::assert-equal 3 (length (vec 1 2 3)))
(test::assert-error (length 100))
(test::assert-error (length 100.0))
(test::assert-error (length #\\x))
"#,
    );
}
