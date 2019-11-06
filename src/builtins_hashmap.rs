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

/*fn builtin_hash_set(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
}

fn builtin_hash_remove(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
}

fn builtin_hash_clear(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
}

fn builtin_hash_copy(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
}*/

pub fn add_hash_builtins<S: BuildHasher>(data: &mut HashMap<String, Rc<Expression>, S>) {
    data.insert(
        "make-hash".to_string(),
        Rc::new(Expression::make_function(
            builtin_make_hash,
            "Make a new hash map.",
        )),
    );
}
