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
        if let Expression::Pair(key, val) = key_val {
            match &*key.borrow() {
                Expression::Atom(Atom::Symbol(sym)) => {
                    map.insert(sym.to_string(), Rc::new(val.borrow().clone()))
                }
                Expression::Atom(Atom::String(s)) => {
                    map.insert(s.to_string(), Rc::new(val.borrow().clone()))
                }
                Expression::Atom(Atom::StringBuf(s)) => {
                    map.insert(s.borrow().to_string(), Rc::new(val.borrow().clone()))
                }
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
            return match assocs {
                Expression::Pair(_, _) => build_map(map, &mut *assocs.iter()),
                Expression::Vector(list) => build_map(map, &mut *Box::new(list.borrow().iter())),
                Expression::Atom(Atom::Nil) => Ok(Expression::HashMap(Rc::new(RefCell::new(map)))),
                _ => Err(io::Error::new(
                    io::ErrorKind::Other,
                    "make-hash takes a sequence",
                )),
            };
        }
    }
    Ok(Expression::HashMap(Rc::new(RefCell::new(map))))
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
                                return Ok(Expression::HashMap(map.clone()));
                            }
                            Expression::Atom(Atom::String(s)) => {
                                map.borrow_mut().insert(s, Rc::new(val));
                                return Ok(Expression::HashMap(map.clone()));
                            }
                            Expression::Atom(Atom::StringBuf(s)) => {
                                map.borrow_mut()
                                    .insert(s.borrow().to_string(), Rc::new(val));
                                return Ok(Expression::HashMap(map.clone()));
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
        Ok(Expression::Atom(Atom::Nil))
    }
    if let Some(map) = args.next() {
        if let Some(key) = args.next() {
            if args.next().is_none() {
                let map = eval(environment, map)?;
                let key = eval(environment, key)?;
                if let Expression::HashMap(map) = map {
                    match key {
                        Expression::Atom(Atom::Symbol(sym)) => {
                            return do_rem(&mut map.borrow_mut(), &sym);
                        }
                        Expression::Atom(Atom::String(s)) => {
                            return do_rem(&mut map.borrow_mut(), &s);
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
        Ok(Expression::Atom(Atom::Nil))
    }
    if let Some(map) = args.next() {
        if let Some(key) = args.next() {
            if args.next().is_none() {
                let map = eval(environment, map)?;
                let key = eval(environment, key)?;
                if let Expression::HashMap(map) = map {
                    match key {
                        Expression::Atom(Atom::Symbol(sym)) => {
                            return do_get(&map.borrow(), &sym);
                        }
                        Expression::Atom(Atom::String(s)) => {
                            return do_get(&map.borrow(), &s);
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
            Ok(Expression::Atom(Atom::Nil))
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
                            return do_has(&map.borrow(), &sym);
                        }
                        Expression::Atom(Atom::String(s)) => {
                            return do_has(&map.borrow(), &s);
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
                    key_list.push(Expression::Atom(Atom::Symbol(key.to_string())));
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
                return Ok(Expression::Atom(Atom::Nil));
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "hash-clear! takes a hashmap and clears it",
    ))
}

pub fn add_hash_builtins<S: BuildHasher>(data: &mut HashMap<String, Rc<Expression>, S>) {
    data.insert(
        "make-hash".to_string(),
        Rc::new(Expression::make_function(
            builtin_make_hash,
            "Make a new hash map.",
        )),
    );
    data.insert(
        "hash-set!".to_string(),
        Rc::new(Expression::make_function(
            builtin_hash_set,
            "Add or update a hashmap key's value.",
        )),
    );
    data.insert(
        "hash-remove!".to_string(),
        Rc::new(Expression::make_function(
            builtin_hash_remove,
            "Remove a key from a hashmap.",
        )),
    );
    data.insert(
        "hash-get".to_string(),
        Rc::new(Expression::make_function(
            builtin_hash_get,
            "Gets a key from a hashmap.",
        )),
    );
    data.insert(
        "hash-haskey".to_string(),
        Rc::new(Expression::make_function(
            builtin_hash_haskey,
            "Checks if a key is in a hashmap.",
        )),
    );
    data.insert(
        "hash-keys".to_string(),
        Rc::new(Expression::make_function(
            builtin_hash_keys,
            "Returns a vector of all the hashmaps keys.",
        )),
    );
    data.insert(
        "hash-clear!".to_string(),
        Rc::new(Expression::make_function(
            builtin_hash_clear,
            "Clears a hashmap.",
        )),
    );
}
