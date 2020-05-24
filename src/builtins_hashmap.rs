use std::collections::HashMap;
use std::hash::BuildHasher;
use std::io;

use crate::environment::*;
use crate::eval::*;
use crate::gc::Handle;
use crate::interner::*;
use crate::types::*;

fn build_map(
    mut map: HashMap<String, Handle>,
    assocs: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    for key_val in assocs {
        if let ExpEnum::Pair(key, val) = &key_val.get().data {
            let key: Expression = key.into();
            match &key.get().data {
                ExpEnum::Atom(Atom::Symbol(sym)) => map.insert((*sym).to_string(), val.clone()),
                ExpEnum::Atom(Atom::String(s)) => map.insert(s.to_string(), val.clone()),
                _ => {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "make-hash key can only be a symbol or string",
                    ))
                }
            };
        } else {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "make-hash each association must be a pair (key . val)",
            ));
        }
    }
    Ok(Expression::alloc_data(ExpEnum::HashMap(map)))
}

fn builtin_make_hash(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    let map: HashMap<String, Handle> = HashMap::new();
    if let Some(assocs) = args.next() {
        if args.next().is_none() {
            let assocs = eval(environment, assocs)?;
            let assocs_d = assocs.get();
            match &assocs_d.data {
                ExpEnum::Pair(_, _) => build_map(map, &mut assocs.iter()),
                ExpEnum::Nil => Ok(Expression::alloc_data(ExpEnum::HashMap(map))),
                ExpEnum::Vector(list) => build_map(map, &mut Box::new(ListIter::new_list(&list))),
                _ => Err(io::Error::new(
                    io::ErrorKind::Other,
                    "make-hash takes a sequence",
                )),
            }
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "make-hash takes one form, a sequence",
            ))
        }
    } else {
        Ok(Expression::alloc_data(ExpEnum::HashMap(map)))
    }
}

fn builtin_hash_set(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
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
                            ExpEnum::Atom(Atom::Symbol(sym)) => {
                                map.insert((*sym).to_string(), val);
                                return Ok(exp_map.clone());
                            }
                            ExpEnum::Atom(Atom::String(s)) => {
                                map.insert(s.to_string(), val);
                                return Ok(exp_map.clone());
                            }
                            _ => {
                                return Err(io::Error::new(
                                    io::ErrorKind::Other,
                                    "hash-set! key can only be a symbol or string",
                                ));
                            }
                        }
                    }
                }
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "hash-set! takes a hashmap, key and value",
    ))
}

fn builtin_hash_remove(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    fn do_rem(map: &mut HashMap<String, Handle>, sym: &str) -> io::Result<Expression> {
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
                        ExpEnum::Atom(Atom::Symbol(sym)) => {
                            return do_rem(map, sym);
                        }
                        ExpEnum::Atom(Atom::String(s)) => {
                            return do_rem(map, &s);
                        }
                        _ => {
                            return Err(io::Error::new(
                                io::ErrorKind::Other,
                                "hash-remove! key can only be a symbol or string",
                            ));
                        }
                    }
                }
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "hash-remove! takes a hashmap and key to remove",
    ))
}

fn builtin_hash_get(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    fn do_get(map: &HashMap<String, Handle>, sym: &str) -> io::Result<Expression> {
        let old = map.get(sym);
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
                let map_d = map.get();
                if let ExpEnum::HashMap(map) = &map_d.data {
                    match &key.get().data {
                        ExpEnum::Atom(Atom::Symbol(sym)) => {
                            return do_get(map, sym);
                        }
                        ExpEnum::Atom(Atom::String(s)) => {
                            return do_get(map, &s);
                        }
                        _ => {
                            return Err(io::Error::new(
                                io::ErrorKind::Other,
                                "hash-get key can only be a symbol or string",
                            ));
                        }
                    }
                }
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "hash-get takes a hashmap and key to get",
    ))
}

fn builtin_hash_haskey(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    fn do_has(map: &HashMap<String, Handle>, sym: &str) -> io::Result<Expression> {
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
                        ExpEnum::Atom(Atom::Symbol(sym)) => {
                            return do_has(map, sym);
                        }
                        ExpEnum::Atom(Atom::String(s)) => {
                            return do_has(map, &s);
                        }
                        _ => {
                            let msg =
                                format!("hash-haskey key can only be a symbol or string {:?}", key);
                            return Err(io::Error::new(
                                io::ErrorKind::Other,
                                msg,
                                //"hash-haskey key can only be a symbol or string",
                            ));
                        }
                    }
                }
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "hash-haskey takes a hashmap and key to test for existence of",
    ))
}

fn builtin_hash_keys(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(map) = args.next() {
        if args.next().is_none() {
            let map = eval(environment, map)?;
            let map_d = map.get();
            if let ExpEnum::HashMap(map) = &map_d.data {
                let mut key_list = Vec::with_capacity(map.len());
                for key in map.keys() {
                    key_list.push(Expression::alloc_data_h(ExpEnum::Atom(Atom::Symbol(
                        environment.interner.intern(key),
                    ))));
                }
                return Ok(Expression::with_list(key_list));
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "hash-keys takes a hashmap and returns it's keys",
    ))
}

fn builtin_hash_clear(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
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
    Err(io::Error::new(
        io::ErrorKind::Other,
        "hash-clear! takes a hashmap and clears it",
    ))
}

pub fn add_hash_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, Reference, S>,
) {
    let root = interner.intern("root");
    data.insert(
        interner.intern("make-hash"),
        Expression::make_function(
            builtin_make_hash,
            "Usage: (make-hash associations?)

Make a new hash map.

If associations is provided (makes an empty map if not) then it is a list of
pairs (key . value) that populate the intial map.

Section: hashmap

Example:
(def 'tst-hash (make-hash))
(test::assert-equal 0 (length (hash-keys tst-hash)))
(def 'tst-hash (make-hash ()))
(test::assert-equal 0 (length (hash-keys tst-hash)))
(def 'tst-hash (make-hash nil))
(test::assert-equal 0 (length (hash-keys tst-hash)))
(def 'tst-hash (make-hash '((:key1 . \"val one\")(key2 . \"val two\")(\"key3\" . \"val three\"))))
(test::assert-equal 3 (length (hash-keys tst-hash)))
(test::assert-equal \"val one\" (hash-get tst-hash :key1))
(test::assert-equal \"val two\" (hash-get tst-hash 'key2))
(test::assert-equal \"val three\" (hash-get tst-hash \"key3\"))
(def 'tst-hash (make-hash '#((:keyv1 . \"val one\")(keyv2 . \"val two\")(\"keyv3\" . \"val three\"))))
(test::assert-equal 3 (length (hash-keys tst-hash)))
(test::assert-equal \"val one\" (hash-get tst-hash :keyv1))
(test::assert-equal \"val two\" (hash-get tst-hash 'keyv2))
(test::assert-equal \"val three\" (hash-get tst-hash \"keyv3\"))
", root
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
(def 'tst-hash (make-hash))
(test::assert-equal 0 (length (hash-keys tst-hash)))
(hash-set! tst-hash :new-key '(1 2 3))
(test::assert-equal 1 (length (hash-keys tst-hash)))
(test::assert-equal '(1 2 3) (hash-get tst-hash :new-key))
(def 'tst-hash (make-hash '((:key1 . \"val one\")(key2 . \"val two\")(\"key3\" . \"val three\"))))
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
            root,
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
(def 'tst-hash (make-hash '((:key1 . \"val one\")(key2 . \"val two\")(\"key3\" . \"val three\"))))
(test::assert-equal 3 (length (hash-keys tst-hash)))
(test::assert-equal \"val one\" (hash-get tst-hash :key1))
(test::assert-equal \"val two\" (hash-get tst-hash 'key2))
(test::assert-equal \"val three\" (hash-get tst-hash \"key3\"))
(hash-remove! tst-hash 'key2)
(test::assert-equal 2 (length (hash-keys tst-hash)))
(test::assert-equal \"val one\" (hash-get tst-hash :key1))
(test::assert-equal \"val three\" (hash-get tst-hash \"key3\"))
(hash-remove! tst-hash :key1)
(test::assert-equal 1 (length (hash-keys tst-hash)))
(test::assert-equal \"val three\" (hash-get tst-hash \"key3\"))
(hash-remove! tst-hash \"key3\")
(test::assert-equal 0 (length (hash-keys tst-hash)))
",
            root,
        ),
    );
    data.insert(
        interner.intern("hash-get"),
        Expression::make_function(
            builtin_hash_get,
            "Usage: (hash-get hashmap key)

Get a value for a key from a hashmap.

Section: hashmap

Example:
(def 'tst-hash (make-hash '((:key1 . \"val one\")(key2 . \"val two\")(\"key3\" . \"val three\"))))
(test::assert-equal 3 (length (hash-keys tst-hash)))
(test::assert-equal \"val one\" (hash-get tst-hash :key1))
(test::assert-equal \"val two\" (hash-get tst-hash 'key2))
(test::assert-equal \"val three\" (hash-get tst-hash \"key3\"))
",
            root,
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
(def 'tst-hash (make-hash '((:key1 . \"val one\")(key2 . \"val two\")(\"key3\" . \"val three\"))))
(test::assert-equal 3 (length (hash-keys tst-hash)))
(test::assert-true (hash-haskey tst-hash :key1))
(test::assert-true (hash-haskey tst-hash 'key2))
(test::assert-true (hash-haskey tst-hash \"key3\"))
(test::assert-false (hash-haskey tst-hash 'key1))
(test::assert-false (hash-haskey tst-hash :key2))
(test::assert-false (hash-haskey tst-hash \"keynone\"))
(hash-remove! tst-hash :key1)
(test::assert-false (hash-haskey tst-hash :key1))
(hash-set! tst-hash :key1 \"val one b\")
(test::assert-true (hash-haskey tst-hash :key1))
",
            root,
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
(def 'tst-hash (make-hash '((:key1 . \"val one\")(key2 . \"val two\")(\"key3\" . \"val three\"))))
(test::assert-equal 3 (length (hash-keys tst-hash)))
(test::assert-true (in? (hash-keys tst-hash) :key1) \"Test :key1\")
(test::assert-true (in? (hash-keys tst-hash) 'key2) \"Test key2\")
; Note string used as a key will be a symbol in the hash-keys list...
(test::assert-true (in? (hash-keys tst-hash) 'key3) \"Test key3\")
(test::assert-false (in? (hash-keys tst-hash) :key4))
",
            root,
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
(def 'tst-hash (make-hash '((:key1 . \"val one\")(key2 . \"val two\")(\"key3\" . \"val three\"))))
(test::assert-equal 3 (length (hash-keys tst-hash)))
(test::assert-true (hash-haskey tst-hash :key1))
(test::assert-true (hash-haskey tst-hash 'key2))
(test::assert-true (hash-haskey tst-hash \"key3\"))
(hash-clear! tst-hash)
(test::assert-equal 0 (length (hash-keys tst-hash)))
(test::assert-false (hash-haskey tst-hash :key1))
(test::assert-false (hash-haskey tst-hash 'key2))
(test::assert-false (hash-haskey tst-hash \"key3\"))
",
            root,
        ),
    );
}
