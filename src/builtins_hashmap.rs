use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::BuildHasher;
use std::io;
use std::rc::Rc;

use crate::environment::*;
use crate::eval::*;
use crate::types::*;

fn build_map(
    mut map: HashMap<String, Rc<Expression>>,
    assocs: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    for key_val in assocs {
        if let Expression::Pair(p) = key_val {
            if let Some((key, val)) = &*p.borrow() {
                match key {
                    Expression::Atom(Atom::Symbol(sym)) => {
                        map.insert((*sym).to_string(), Rc::new(val.clone()))
                    }
                    Expression::Atom(Atom::String(s)) => {
                        map.insert(s.to_string(), Rc::new(val.clone()))
                    }
                    Expression::Atom(Atom::StringRef(s)) => {
                        map.insert((*s).to_string(), Rc::new(val.clone()))
                    }
                    Expression::Atom(Atom::StringBuf(s)) => {
                        map.insert(s.borrow().to_string(), Rc::new(val.clone()))
                    }
                    _ => {
                        return Err(io::Error::new(
                            io::ErrorKind::Other,
                            "make-hash key can only be a symbol or string",
                        ))
                    }
                };
            } else {
                // Nil
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "make-hash each association must be a pair (key . val)",
                ));
            }
        } else {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "make-hash each association must be a pair (key . val)",
            ));
        }
    }
    Ok(Expression::HashMap(Rc::new(RefCell::new(map))))
}

fn builtin_make_hash(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    let map: HashMap<String, Rc<Expression>> = HashMap::new();
    if let Some(assocs) = args.next() {
        if args.next().is_none() {
            let assocs = eval(environment, assocs)?;
            match &assocs {
                Expression::Pair(p) => {
                    if let Some((_, _)) = &*p.borrow() {
                        build_map(map, &mut *assocs.iter())
                    } else {
                        // Nil
                        Ok(Expression::HashMap(Rc::new(RefCell::new(map))))
                    }
                }
                Expression::Vector(list) => build_map(map, &mut *Box::new(list.borrow().iter())),
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
        Ok(Expression::HashMap(Rc::new(RefCell::new(map))))
    }
}

fn builtin_hash_set(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(map) = args.next() {
        if let Some(key) = args.next() {
            if let Some(val) = args.next() {
                if args.next().is_none() {
                    let map = eval(environment, map)?;
                    let key = eval(environment, key)?;
                    let val = eval(environment, val)?;
                    if let Expression::HashMap(map) = map {
                        match key {
                            Expression::Atom(Atom::Symbol(sym)) => {
                                map.borrow_mut().insert(sym.to_string(), Rc::new(val));
                                return Ok(Expression::HashMap(map));
                            }
                            Expression::Atom(Atom::String(s)) => {
                                map.borrow_mut().insert(s, Rc::new(val));
                                return Ok(Expression::HashMap(map));
                            }
                            Expression::Atom(Atom::StringRef(s)) => {
                                map.borrow_mut().insert(s.to_string(), Rc::new(val));
                                return Ok(Expression::HashMap(map));
                            }
                            Expression::Atom(Atom::StringBuf(s)) => {
                                map.borrow_mut()
                                    .insert(s.borrow().to_string(), Rc::new(val));
                                return Ok(Expression::HashMap(map));
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
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    fn do_rem(map: &mut HashMap<String, Rc<Expression>>, sym: &str) -> io::Result<Expression> {
        let old = map.remove(sym);
        if let Some(old) = old {
            let exp = &*old;
            return Ok(exp.clone());
        }
        Ok(Expression::nil())
    }
    if let Some(map) = args.next() {
        if let Some(key) = args.next() {
            if args.next().is_none() {
                let map = eval(environment, map)?;
                let key = eval(environment, key)?;
                if let Expression::HashMap(map) = map {
                    match key {
                        Expression::Atom(Atom::Symbol(sym)) => {
                            return do_rem(&mut map.borrow_mut(), sym);
                        }
                        Expression::Atom(Atom::String(s)) => {
                            return do_rem(&mut map.borrow_mut(), &s);
                        }
                        Expression::Atom(Atom::StringRef(s)) => {
                            return do_rem(&mut map.borrow_mut(), s);
                        }
                        Expression::Atom(Atom::StringBuf(s)) => {
                            return do_rem(&mut map.borrow_mut(), &s.borrow());
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
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    fn do_get(map: &HashMap<String, Rc<Expression>>, sym: &str) -> io::Result<Expression> {
        let old = map.get(sym);
        if let Some(old) = old {
            let exp = &*old.clone();
            return Ok(exp.clone());
        }
        Ok(Expression::nil())
    }
    if let Some(map) = args.next() {
        if let Some(key) = args.next() {
            if args.next().is_none() {
                let map = eval(environment, map)?;
                let key = eval(environment, key)?;
                if let Expression::HashMap(map) = map {
                    match key {
                        Expression::Atom(Atom::Symbol(sym)) => {
                            return do_get(&map.borrow(), sym);
                        }
                        Expression::Atom(Atom::String(s)) => {
                            return do_get(&map.borrow(), &s);
                        }
                        Expression::Atom(Atom::StringRef(s)) => {
                            return do_get(&map.borrow(), s);
                        }
                        Expression::Atom(Atom::StringBuf(s)) => {
                            return do_get(&map.borrow(), &s.borrow());
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
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    fn do_has(map: &HashMap<String, Rc<Expression>>, sym: &str) -> io::Result<Expression> {
        if map.contains_key(sym) {
            Ok(Expression::Atom(Atom::True))
        } else {
            Ok(Expression::nil())
        }
    }
    if let Some(map) = args.next() {
        if let Some(key) = args.next() {
            if args.next().is_none() {
                let map = eval(environment, map)?;
                let key = eval(environment, key)?;
                if let Expression::HashMap(map) = map {
                    match key {
                        Expression::Atom(Atom::Symbol(sym)) => {
                            return do_has(&map.borrow(), sym);
                        }
                        Expression::Atom(Atom::String(s)) => {
                            return do_has(&map.borrow(), &s);
                        }
                        Expression::Atom(Atom::StringRef(s)) => {
                            return do_has(&map.borrow(), s);
                        }
                        Expression::Atom(Atom::StringBuf(s)) => {
                            return do_has(&map.borrow(), &s.borrow());
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
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(map) = args.next() {
        if args.next().is_none() {
            let map = eval(environment, map)?;
            if let Expression::HashMap(map) = map {
                let mut key_list = Vec::with_capacity(map.borrow().len());
                for key in map.borrow().keys() {
                    key_list.push(Expression::Atom(Atom::Symbol(
                        environment.interner.intern(key),
                    )));
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
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(map) = args.next() {
        if args.next().is_none() {
            let map = eval(environment, map)?;
            if let Expression::HashMap(map) = map {
                map.borrow_mut().clear();
                return Ok(Expression::nil());
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
    data: &mut HashMap<&'static str, Rc<Reference>, S>,
) {
    data.insert(
        interner.intern("make-hash"),
        Rc::new(Expression::make_function(
            builtin_make_hash,
            "Usage (make-hash associations?)

Make a new hash map.

If associations is provided (makes an empty map if not) then it is a list of
pairs (key . value) that populate the intial map.

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
",
        )),
    );
    data.insert(
        interner.intern("hash-set!"),
        Rc::new(Expression::make_function(
            builtin_hash_set,
            "Usage (hash-set! hashmap key value)

Add or update a hashmap key's value.  This is a destructive form!

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
        )),
    );
    data.insert(
        interner.intern("hash-remove!"),
        Rc::new(Expression::make_function(
            builtin_hash_remove,
            "Usage (hash-remove! hashmap key)

Remove a key from a hashmap.  This is a destructive form!

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
        )),
    );
    data.insert(
        interner.intern("hash-get"),
        Rc::new(Expression::make_function(
            builtin_hash_get,
            "Usage (hash-get hashmap key)

Get a value for a key from a hashmap.

Example:
(def 'tst-hash (make-hash '((:key1 . \"val one\")(key2 . \"val two\")(\"key3\" . \"val three\"))))
(test::assert-equal 3 (length (hash-keys tst-hash)))
(test::assert-equal \"val one\" (hash-get tst-hash :key1))
(test::assert-equal \"val two\" (hash-get tst-hash 'key2))
(test::assert-equal \"val three\" (hash-get tst-hash \"key3\"))
",
        )),
    );
    data.insert(
        interner.intern("hash-haskey"),
        Rc::new(Expression::make_function(
            builtin_hash_haskey,
            "Usage (hash-haskey hashmap key)

Checks if a key is in a hashmap.

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
        )),
    );
    data.insert(
        interner.intern("hash-keys"),
        Rc::new(Expression::make_function(
            builtin_hash_keys,
            "Usage (hash-keys hashmap)

Returns a vector of all the hashmaps keys.  The keys will be unordered.

Example:
(def 'tst-hash (make-hash '((:key1 . \"val one\")(key2 . \"val two\")(\"key3\" . \"val three\"))))
(test::assert-equal 3 (length (hash-keys tst-hash)))
(test::assert-true (in? (hash-keys tst-hash) :key1) \"Test :key1\")
(test::assert-true (in? (hash-keys tst-hash) 'key2) \"Test key2\")
; Note string used as a key will be a symbol in the hash-keys list...
(test::assert-true (in? (hash-keys tst-hash) 'key3) \"Test key3\")
(test::assert-false (in? (hash-keys tst-hash) :key4))
",
        )),
    );
    data.insert(
        interner.intern("hash-clear!"),
        Rc::new(Expression::make_function(
            builtin_hash_clear,
            "Usage (hash-clear! hashmap)

Clears a hashmap.  This is a destructive form!

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
        )),
    );
}
