use std::borrow::Cow;
use std::collections::HashMap;
use std::hash::BuildHasher;

use crate::environment::*;
use crate::eval::*;
use crate::gc::Handle;
use crate::interner::*;
use crate::types::*;

#[allow(clippy::ptr_arg)]
pub(crate) fn cow_to_ref(environment: &mut Environment, input: &Cow<'static, str>) -> &'static str {
    match input {
        Cow::Borrowed(s) => s,
        Cow::Owned(s) => environment.interner.intern(&s),
    }
}

fn build_map(
    environment: &mut Environment,
    mut map: HashMap<&'static str, Handle>,
    assocs: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    for key_val in assocs {
        if let ExpEnum::Pair(key, val) = &key_val.get().data {
            let key: Expression = key.into();
            match &key.get().data {
                ExpEnum::Symbol(sym, _) => map.insert(sym, val.clone()),
                ExpEnum::String(s, _) => map.insert(cow_to_ref(environment, &s), val.clone()),
                ExpEnum::Char(ch) => map.insert(cow_to_ref(environment, &ch), val.clone()),
                _ => {
                    return Err(LispError::new(
                        "make-hash key can only be a symbol or string",
                    ))
                }
            };
        } else {
            return Err(LispError::new(
                "make-hash each association must be a pair (key . val)",
            ));
        }
    }
    Ok(Expression::alloc_data(ExpEnum::HashMap(map)))
}

fn builtin_make_hash(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let map: HashMap<&'static str, Handle> = HashMap::new();
    if let Some(assocs) = args.next() {
        if args.next().is_none() {
            let assocs = eval(environment, assocs)?;
            let assocs_d = assocs.get();
            match &assocs_d.data {
                ExpEnum::Pair(_, _) => build_map(environment, map, &mut assocs.iter()),
                ExpEnum::Nil => Ok(Expression::alloc_data(ExpEnum::HashMap(map))),
                ExpEnum::Vector(_) => build_map(environment, map, &mut assocs.iter()),
                _ => Err(LispError::new("make-hash takes a sequence")),
            }
        } else {
            Err(LispError::new("make-hash takes one form, a sequence"))
        }
    } else {
        Ok(Expression::alloc_data(ExpEnum::HashMap(map)))
    }
}

fn builtin_hash_set(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(map) = args.next() {
        if let Some(key) = args.next() {
            if let Some(val) = args.next() {
                if args.next().is_none() {
                    let exp_map = eval(environment, map)?;
                    let key = eval(environment, key)?;
                    let val = eval(environment, val)?;
                    let mut exp_map_d = exp_map.get_mut();
                    if let ExpEnum::HashMap(map) = &mut exp_map_d.data {
                        let val: Handle = val.into();
                        match &key.get().data {
                            ExpEnum::Symbol(sym, _) => {
                                map.insert(*sym, val);
                                return Ok(exp_map.clone());
                            }
                            ExpEnum::String(s, _) => {
                                map.insert(cow_to_ref(environment, &s), val);
                                return Ok(exp_map.clone());
                            }
                            ExpEnum::Char(ch) => {
                                map.insert(cow_to_ref(environment, &ch), val);
                                return Ok(exp_map.clone());
                            }
                            _ => {
                                return Err(LispError::new(
                                    "hash-set! key can only be a symbol or string",
                                ));
                            }
                        }
                    }
                }
            }
        }
    }
    Err(LispError::new("hash-set! takes a hashmap, key and value"))
}

fn builtin_hash_remove(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    fn do_rem(map: &mut HashMap<&'static str, Handle>, sym: &str) -> Result<Expression, LispError> {
        let old = map.remove(sym);
        if let Some(old) = old {
            let old: Expression = old.into();
            Ok(old)
        } else {
            Ok(Expression::make_nil())
        }
    }
    if let Some(map) = args.next() {
        if let Some(key) = args.next() {
            if args.next().is_none() {
                let map = eval(environment, map)?;
                let key = eval(environment, key)?;
                let mut map_d = map.get_mut();
                if let ExpEnum::HashMap(map) = &mut map_d.data {
                    match &key.get().data {
                        ExpEnum::Symbol(sym, _) => {
                            return do_rem(map, sym);
                        }
                        ExpEnum::String(s, _) => {
                            return do_rem(map, &s);
                        }
                        ExpEnum::Char(ch) => {
                            return do_rem(map, &ch);
                        }
                        _ => {
                            return Err(LispError::new(
                                "hash-remove! key can only be a symbol or string",
                            ));
                        }
                    }
                }
            }
        }
    }
    Err(LispError::new(
        "hash-remove! takes a hashmap and key to remove",
    ))
}

fn builtin_hash_get(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    fn do_get(
        environment: &mut Environment,
        map: &HashMap<&'static str, Handle>,
        sym: &str,
        default: Option<Expression>,
    ) -> Result<Expression, LispError> {
        let old = map.get(sym);
        if let Some(old) = old {
            let old: Expression = old.into();
            Ok(old)
        } else if let Some(exp) = default {
            eval(environment, exp)
        } else {
            Ok(Expression::make_nil())
        }
    }
    if let Some(map) = args.next() {
        if let Some(key) = args.next() {
            let default = if let Some(default) = args.next() {
                Some(default)
            } else {
                None
            };
            if args.next().is_none() {
                let map = eval(environment, map)?;
                let key = eval(environment, key)?;
                let map_d = map.get();
                if let ExpEnum::HashMap(map) = &map_d.data {
                    match &key.get().data {
                        ExpEnum::Symbol(sym, _) => {
                            return do_get(environment, map, sym, default);
                        }
                        ExpEnum::String(s, _) => {
                            return do_get(environment, map, &s, default);
                        }
                        ExpEnum::Char(ch) => {
                            return do_get(environment, map, &ch, default);
                        }
                        _ => {
                            return Err(LispError::new(
                                "hash-get: key can only be a symbol or string",
                            ));
                        }
                    }
                }
            }
        }
    }
    Err(LispError::new(
        "hash-get: takes a hashmap and key to get and optional default value",
    ))
}

fn builtin_hash_haskey(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    fn do_has(map: &HashMap<&'static str, Handle>, sym: &str) -> Result<Expression, LispError> {
        if map.contains_key(sym) {
            Ok(Expression::make_true())
        } else {
            Ok(Expression::make_nil())
        }
    }
    if let Some(map) = args.next() {
        if let Some(key) = args.next() {
            if args.next().is_none() {
                let map = eval(environment, map)?;
                let key = eval(environment, key)?;
                let map_d = map.get();
                if let ExpEnum::HashMap(map) = &map_d.data {
                    match &key.get().data {
                        ExpEnum::Symbol(sym, _) => {
                            return do_has(map, sym);
                        }
                        ExpEnum::String(s, _) => {
                            return do_has(map, &s);
                        }
                        ExpEnum::Char(ch) => {
                            return do_has(map, &ch);
                        }
                        _ => {
                            let msg =
                                format!("hash-haskey key can only be a symbol or string {:?}", key);
                            return Err(LispError::new(
                                msg,
                                //"hash-haskey key can only be a symbol or string",
                            ));
                        }
                    }
                }
            }
        }
    }
    Err(LispError::new(
        "hash-haskey takes a hashmap and key to test for existence of",
    ))
}

fn builtin_hash_keys(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(map) = args.next() {
        if args.next().is_none() {
            let map = eval(environment, map)?;
            let map_d = map.get();
            if let ExpEnum::HashMap(map) = &map_d.data {
                let mut key_list = Vec::with_capacity(map.len());
                for key in map.keys() {
                    key_list.push(Expression::alloc_data_h(ExpEnum::Symbol(
                        environment.interner.intern(key),
                        SymLoc::None,
                    )));
                }
                return Ok(Expression::with_list(key_list));
            }
        }
    }
    Err(LispError::new(
        "hash-keys takes a hashmap and returns it's keys",
    ))
}

fn builtin_hash_clear(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(map) = args.next() {
        if args.next().is_none() {
            let map = eval(environment, map)?;
            let mut map_d = map.get_mut();
            if let ExpEnum::HashMap(inner_map) = &mut map_d.data {
                inner_map.clear();
                return Ok(map.clone());
            }
        }
    }
    Err(LispError::new("hash-clear! takes a hashmap and clears it"))
}

pub fn add_hash_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, (Expression, String), S>,
) {
    data.insert(
        interner.intern("make-hash"),
        Expression::make_function(
            builtin_make_hash,
            "Usage: (make-hash associations?)

Make a new hash map.

If associations is provided (makes an empty map if not) then it is a list of
pairs (key . value) that populate the intial map.  Neither key nor value in the
associations will be evaluated.

Section: hashmap

Example:
(def tst-hash (make-hash))
(test::assert-equal 0 (length (hash-keys tst-hash)))
(def tst-hash (make-hash ()))
(test::assert-equal 0 (length (hash-keys tst-hash)))
(def tst-hash (make-hash nil))
(test::assert-equal 0 (length (hash-keys tst-hash)))
(def tst-hash (make-hash '((:key1 . \"val one\")(key2 . \"val two\")(\"key3\" . \"val three\"))))
(test::assert-equal 3 (length (hash-keys tst-hash)))
(test::assert-equal \"val one\" (hash-get tst-hash :key1))
(test::assert-equal \"val two\" (hash-get tst-hash 'key2))
(test::assert-equal \"val three\" (hash-get tst-hash \"key3\"))
(def tst-hash (make-hash '#((:keyv1 . \"val one\")(keyv2 . \"val two\")(\"keyv3\" . \"val three\"))))
(test::assert-equal 3 (length (hash-keys tst-hash)))
(test::assert-equal \"val one\" (hash-get tst-hash :keyv1))
(test::assert-equal \"val two\" (hash-get tst-hash 'keyv2))
(test::assert-equal \"val three\" (hash-get tst-hash \"keyv3\"))
; Not in test below that tst-hash-val is NOT evaluated so the symbol is the value.
(def tst-hash-val \"some val\")
(def tst-hash (make-hash '#((:keyv1 . \"val one\")(:keyv2 . \"val two\")(:keyv3 . tst-hash-val))))
(test::assert-equal 3 (length (hash-keys tst-hash)))
(test::assert-equal \"val one\" (hash-get tst-hash :keyv1))
(test::assert-equal \"val two\" (hash-get tst-hash :keyv2))
(test::assert-equal 'tst-hash-val (hash-get tst-hash :keyv3))
"
        ),
    );
    data.insert(
        interner.intern("hash-set!"),
        Expression::make_function(
            builtin_hash_set,
            "Usage: (hash-set! hashmap key value)

Add or update a hashmap key's value.  This is a destructive form!

Section: hashmap

Example:
(def tst-hash (make-hash))
(test::assert-equal 0 (length (hash-keys tst-hash)))
(hash-set! tst-hash :new-key '(1 2 3))
(test::assert-equal 1 (length (hash-keys tst-hash)))
(test::assert-equal '(1 2 3) (hash-get tst-hash :new-key))
(def tst-hash (make-hash '((:key1 . \"val one\")(key2 . \"val two\")(\"key3\" . \"val three\"))))
(test::assert-equal 3 (length (hash-keys tst-hash)))
(test::assert-equal \"val one\" (hash-get tst-hash :key1))
(test::assert-equal \"val two\" (hash-get tst-hash 'key2))
(test::assert-equal \"val three\" (hash-get tst-hash \"key3\"))
(hash-set! tst-hash :new-key '(1 2 3))
(test::assert-equal 4 (length (hash-keys tst-hash)))
(test::assert-equal \"val one\" (hash-get tst-hash :key1))
(test::assert-equal \"val two\" (hash-get tst-hash 'key2))
(test::assert-equal \"val three\" (hash-get tst-hash \"key3\"))
(test::assert-equal '(1 2 3) (hash-get tst-hash :new-key))
(hash-set! tst-hash 'key2 \"val two b\")
(hash-set! tst-hash :key1 \"val one b\")
(hash-set! tst-hash \"key3\" \"val three b\")
(test::assert-equal 4 (length (hash-keys tst-hash)))
(test::assert-equal \"val one b\" (hash-get tst-hash :key1))
(test::assert-equal \"val two b\" (hash-get tst-hash 'key2))
(test::assert-equal \"val three b\" (hash-get tst-hash \"key3\"))
(test::assert-equal '(1 2 3) (hash-get tst-hash :new-key))
",
        ),
    );
    data.insert(
        interner.intern("hash-remove!"),
        Expression::make_function(
            builtin_hash_remove,
            "Usage: (hash-remove! hashmap key)

Remove a key from a hashmap.  This is a destructive form!

Section: hashmap

Example:
(def tst-hash (make-hash '((:key1 . \"val one\")(key2 . \"val two\")(\"key3\" . \"val three\")(#\\S . \"val S\"))))
(test::assert-equal 4 (length (hash-keys tst-hash)))
(test::assert-equal \"val one\" (hash-get tst-hash :key1))
(test::assert-equal \"val two\" (hash-get tst-hash 'key2))
(test::assert-equal \"val three\" (hash-get tst-hash \"key3\"))
(test::assert-equal \"val S\" (hash-get tst-hash #\\S))
(hash-remove! tst-hash 'key2)
(test::assert-equal 3 (length (hash-keys tst-hash)))
(test::assert-equal \"val one\" (hash-get tst-hash :key1))
(test::assert-equal \"val three\" (hash-get tst-hash \"key3\"))
(test::assert-equal \"val S\" (hash-get tst-hash #\\S))
(hash-remove! tst-hash :key1)
(test::assert-equal 2 (length (hash-keys tst-hash)))
(test::assert-equal \"val three\" (hash-get tst-hash \"key3\"))
(test::assert-equal \"val S\" (hash-get tst-hash #\\S))
(hash-remove! tst-hash \"key3\")
(test::assert-equal 1 (length (hash-keys tst-hash)))
(test::assert-equal \"val S\" (hash-get tst-hash #\\S))
(hash-remove! tst-hash #\\S)
(test::assert-equal 0 (length (hash-keys tst-hash)))
",
        ),
    );
    data.insert(
        interner.intern("hash-get"),
        Expression::make_special(
            builtin_hash_get,
            "Usage: (hash-get hashmap key default?) -> value

Get a value for a key from a hashmap.  If the optional default is provided and
the key is not in the hash then evaluate and return it.
NOTE: default will only be evaluted if it is used.

Section: hashmap

Example:
(def tst-hash (make-hash '((:key1 . \"val one\")(key2 . \"val two\")(\"key3\" . \"val three\")(#\\S . \"val S\"))))
(test::assert-equal 4 (length (hash-keys tst-hash)))
(test::assert-equal \"val one\" (hash-get tst-hash :key1))
(test::assert-equal \"val two\" (hash-get tst-hash 'key2))
(test::assert-equal \"val three\" (hash-get tst-hash \"key3\"))
(test::assert-equal \"val S\" (hash-get tst-hash #\\S))
(test::assert-equal \"default\" (hash-get tst-hash :not-here \"default\"))
(test::assert-equal \"string default\" (hash-get tst-hash :not-here (str \"string \" \"default\")))
",
        ),
    );
    data.insert(
        interner.intern("hash-haskey"),
        Expression::make_function(
            builtin_hash_haskey,
            "Usage: (hash-haskey hashmap key)

Checks if a key is in a hashmap.

Section: hashmap

Example:
(def tst-hash (make-hash '((:key1 . \"val one\")(key2 . \"val two\")(\"key3\" . \"val three\")(#\\S . \"val S\"))))
(test::assert-equal 4 (length (hash-keys tst-hash)))
(test::assert-true (hash-haskey tst-hash :key1))
(test::assert-true (hash-haskey tst-hash 'key2))
(test::assert-true (hash-haskey tst-hash \"key3\"))
(test::assert-true (hash-haskey tst-hash #\\S))
(test::assert-false (hash-haskey tst-hash 'key1))
(test::assert-false (hash-haskey tst-hash :key2))
(test::assert-false (hash-haskey tst-hash \"keynone\"))
(hash-remove! tst-hash :key1)
(test::assert-false (hash-haskey tst-hash :key1))
(hash-set! tst-hash :key1 \"val one b\")
(test::assert-true (hash-haskey tst-hash :key1))
",
        ),
    );
    data.insert(
        interner.intern("hash-keys"),
        Expression::make_function(
            builtin_hash_keys,
            "Usage: (hash-keys hashmap)

Returns a vector of all the hashmaps keys.  The keys will be unordered.

Section: hashmap

Example:
(def tst-hash (make-hash '((:key1 . \"val one\")(key2 . \"val two\")(\"key3\" . \"val three\")(#\\S . \"val S\"))))
(test::assert-equal 4 (length (hash-keys tst-hash)))
(test::assert-true (in? (hash-keys tst-hash) :key1) \" Test :key1\")
(test::assert-true (in? (hash-keys tst-hash) 'key2) \" Test key2\")
; Note string or char used as a key will be a symbol in the hash-keys list...
(test::assert-true (in? (hash-keys tst-hash) 'S) \" Test S\")
(test::assert-true (in? (hash-keys tst-hash) 'key3) \" Test key3\")
(test::assert-false (in? (hash-keys tst-hash) :key4))
",
        ),
    );
    data.insert(
        interner.intern("hash-clear!"),
        Expression::make_function(
            builtin_hash_clear,
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
(test::assert-false (hash-haskey tst-hash #\\S))
",
        ),
    );
}
