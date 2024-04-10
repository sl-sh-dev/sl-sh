use crate::SloshVm;
use bridge_adapters::add_builtin;
use bridge_macros::sl_sh_fn;
use slvm::{VMError, VMResult, Value, ValueType};
use std::collections::HashMap;

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

/// Usage: (hash-remove! hashmap key)
///
/// Remove a key from a hashmap. This is a destructive form!
///
/// Section: hashmap
///
/// Example:
/// (def tst-hash {:key1  "val one" 'key2 "val two" "key3" "val three" \S "val S"})
/// (test::assert-equal 4 (len (hash-keys tst-hash)))
/// (test::assert-equal "val one" tst-hash.:key1)
/// (test::assert-equal "val two" (get tst-hash 'key2))
/// (test::assert-equal "val three" (get tst-hash "key3"))
/// (test::assert-equal "val S" (get tst-hash \S))
/// (hash-remove! tst-hash 'key2)
/// (test::assert-equal 3 (len (hash-keys tst-hash)))
/// (test::assert-equal "val one" tst-hash.:key1)
/// (test::assert-equal "val three" (get tst-hash "key3"))
/// (test::assert-equal "val S" (get tst-hash \S))
/// (hash-remove! tst-hash :key1)
/// (test::assert-equal 2 (len (hash-keys tst-hash)))
/// (test::assert-equal "val three" (get tst-hash "key3"))
/// (test::assert-equal "val S" (get tst-hash \S))
/// (hash-remove! tst-hash "key3")
/// (test::assert-equal 1 (len (hash-keys tst-hash)))
/// (test::assert-equal "val S" (get tst-hash \S))
/// (hash-remove! tst-hash \S)
/// (test::assert-equal 0 (len (hash-keys tst-hash)))
#[sl_sh_fn(fn_name = "hash-remove!")]
pub fn hash_remove(map: &mut HashMap<Value, Value>, key: Value) -> VMResult<Value> {
    if let Some(old) = map.remove(&key) {
        Ok(old)
    } else {
        Ok(Value::Nil)
    }
}

/// Usage: (hash-haskey hashmap key)
///
/// Checks if a key is in a hashmap.
///
/// Section: hashmap
///
/// Example:
/// (def tst-hash {:key1  "val one" 'key2 "val two" "key3" "val three" \S "val S"})
/// (test::assert-equal 4 (len (hash-keys tst-hash)))
/// (test::assert-true (hash-haskey tst-hash :key1))
/// (test::assert-true (hash-haskey tst-hash 'key2))
/// (test::assert-true (hash-haskey tst-hash "key3"))
/// (test::assert-true (hash-haskey tst-hash \S))
/// (test::assert-false (hash-haskey tst-hash 'key1))
/// (test::assert-false (hash-haskey tst-hash :key2))
/// (test::assert-false (hash-haskey tst-hash "keynone"))
/// (hash-remove! tst-hash :key1)
/// (test::assert-false (hash-haskey tst-hash :key1))
/// (set! tst-hash :key1 "val one b")
/// (test::assert-true (hash-haskey tst-hash :key1))
#[sl_sh_fn(fn_name = "hash-haskey?")]
pub fn hash_hashkey(map: &mut HashMap<Value, Value>, key: Value) -> VMResult<Value> {
    if map.contains_key(&key) {
        Ok(Value::True)
    } else {
        Ok(Value::False)
    }
}

/// Usage: (occurs (list 1 2 ...) 7)
///
/// Counts instances of item in sequence.
///
/// Section: core
///
/// Example:
/// (test::assert-equal 1 (occurs (list 1 3 5 2 4 8 2 4 88 2 1) 8))
/// (test::assert-equal 3 (occurs (list 1 3 5 2 4 10 2 4 88 2 1) 2))
/// (test::assert-equal 0 (occurs (list 1 3 5 2 4 10 2 4 88 2 1) 42))
#[sl_sh_fn(fn_name = "occurs", takes_env = true)]
pub fn occurs(environment: &mut SloshVm, haystack: Value, needle: Value) -> VMResult<u64> {
    let mut occurrences = 0;
    for hay in haystack.iter(environment) {
        if environment.is_equal_pair(hay, needle)? == Value::True {
            occurrences += 1;
        }
    }
    Ok(occurrences)
}

/// Usage: (in? needle haystack)
///
/// In provided sequence, haystack, find a specific value, needle.
///
/// Section: collection
///
/// Example:
/// (assert-true (in? [1 2 3 4 5] 3))
/// (assert-false (in? [1 2 3 4 5] 9))
/// (assert-true (in? (list 1 2 3 4 5) 3))
/// (assert-true (in? '(1 2 3 4 5) 5))
#[sl_sh_fn(fn_name = "in?", takes_env = true)]
pub fn is_in(environment: &mut SloshVm, haystack: Value, needle: Value) -> VMResult<Value> {
    let mut stack = vec![];
    for hay in haystack.iter_all(environment) {
        let is_list = matches!(
            hay.value_type(environment),
            ValueType::Vector | ValueType::List
        ) || hay.is_proper_list(environment);
        if is_list {
            stack.push(hay);
        }
        match environment.is_equal_pair(hay, needle)? {
            Value::True => {
                return Ok(Value::True);
            }
            _ => {}
        }
    }
    for hay in stack {
        let v = is_in(environment, hay, needle)?;
        if v == Value::True {
            return Ok(v);
        }
    }
    Ok(Value::False)
}

/// Usage: (to-list any)
///
/// Turns any one value into a vector. If that value or if it was a sequence
/// a new sequence with the same values.
///
/// Section: core
#[sl_sh_fn(fn_name = "to-vec", takes_env = true)]
pub fn to_vec(environment: &mut SloshVm, src: Value) -> VMResult<Value> {
    Ok(environment.alloc_vector(src.iter_all(environment).collect::<Vec<Value>>()))
}

/// Usage: (to-list any)
///
/// Turns any one value into a list. If that value or if it was a sequence
/// a new sequence with the same values.
///
/// Section: core
#[sl_sh_fn(fn_name = "to-list", takes_env = true)]
pub fn to_list(environment: &mut SloshVm, src: Value) -> VMResult<Value> {
    let v = environment.alloc_vector(src.iter_all(environment).collect::<Vec<Value>>());
    let h = v.get_handle().unwrap();
    Ok(Value::List(h, 0))
}

///  Usage: (hash-keys hashmap)
///
///  Returns a vector of all the hashmaps keys.  The keys will be unordered.
///
///  Section: hashmap
///
///  Example:
///  (def tst-hash {:key1  "val one" 'key2 "val two" "key3" "val three" \S "val S"})
///  (test::assert-equal 4 (len (hash-keys tst-hash)))
///  (test::assert-true (in? (hash-keys tst-hash) :key1) " Test :key1")
///  (test::assert-true (in? (hash-keys tst-hash) 'key2) " Test key2")
///  (test::assert-true (in? (hash-keys tst-hash) \S) " Test S")
///  (test::assert-true (in? (hash-keys tst-hash) "key3") " Test key3")
///  (test::assert-false (in? (hash-keys tst-hash) :key4))
#[sl_sh_fn(fn_name = "hash-keys")]
pub fn hash_keys(map: &HashMap<Value, Value>) -> VMResult<Vec<Value>> {
    let mut keys = Vec::with_capacity(map.len());
    for key in map.keys() {
        keys.push(*key);
    }
    Ok(keys)
}

fn flatten_helper(vec: &mut Vec<Value>, vm: &mut SloshVm, registers: &[Value]) -> VMResult<()> {
    for i in registers.iter() {
        if i.iter(vm).next().is_some() {
            // is iterble
            let vs = i.iter(vm).collect::<Vec<Value>>();
            flatten_helper(vec, vm, vs.as_slice())?;
        } else {
            if !i.is_nil() {
                vec.push(*i);
            }
        }
    }
    Ok(())
}

pub fn flatten(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut flat = Vec::with_capacity(registers.len());
    flatten_helper(&mut flat, vm, registers)?;
    Ok(vm.alloc_vector(flat))
}

/// Usage: (reverse items)
///
/// Produce a vector that is the reverse of items. Will call iter on items and requires a double ended iterator.
///
/// Section: collection
///
/// Example:
/// (def tmap [1 2 3 0])
/// (assert-false (empty? tmap))
/// (def tmap (reverse tmap))
/// (assert-equal 2 (get tmap 2))
/// (assert-equal 1 (get tmap 3))
/// (assert-equal 0 (get tmap 0))
/// (assert-error (reverse "string"))
#[sl_sh_fn(fn_name = "reverse", takes_env = true)]
pub fn reverse(environment: &mut SloshVm, seq: Value) -> VMResult<Value> {
    let seq = seq
        .iter(environment)
        .collect::<Vec<Value>>()
        .into_iter()
        .rev()
        .collect::<Vec<Value>>();
    Ok(environment.alloc_vector(seq))
}

pub fn setup_collection_builtins(env: &mut SloshVm) {
    intern_occurs(env);
    intern_reverse(env);
    intern_hash_keys(env);
    intern_hash_remove(env);
    intern_is_in(env);
    intern_to_vec(env);
    intern_to_list(env);
    add_builtin(
        env,
        "flatten",
        flatten,
        "Usage: (flatten & rest)

Takes a sequence composed of individual values or sequences of values and turns
it into one vector of values.

Section: collection

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
        "Usage: (vec-slice vector start end)

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
