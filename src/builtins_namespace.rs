use std::collections::HashMap;
use std::hash::BuildHasher;
use std::io;

use crate::environment::*;
use crate::eval::*;
use crate::interner::*;
use crate::types::*;

fn builtin_ns_create(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if environment
        .current_scope
        .last()
        .unwrap()
        .borrow()
        .name
        .is_none()
    {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "ns-create can only create a namespace when not in a lexical scope",
        ));
    }
    if let Some(key) = args.next() {
        if args.next().is_none() {
            let key = match &eval(environment, key)?.get().data {
                ExpEnum::Atom(Atom::Symbol(sym)) => sym,
                ExpEnum::Atom(Atom::StringRef(s)) => s,
                ExpEnum::Atom(Atom::String(s)) => environment.interner.intern(&s),
                ExpEnum::Atom(Atom::StringBuf(s)) => environment.interner.intern(&s.borrow()),
                _ => {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "ns-create: namespace must be a symbol or string",
                    ))
                }
            };
            let scope = match build_new_namespace(environment, key) {
                Ok(scope) => scope,
                Err(msg) => return Err(io::Error::new(io::ErrorKind::Other, msg)),
            };
            environment.current_scope.push(scope);
            return Ok(Expression::make_nil());
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "ns-create takes one arg, the name of the new namespace",
    ))
}

fn builtin_ns_enter(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if environment
        .current_scope
        .last()
        .unwrap()
        .borrow()
        .name
        .is_none()
    {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "ns-enter can only enter a namespace when not in a lexical scope",
        ));
    }
    if let Some(key) = args.next() {
        if args.next().is_none() {
            let key = match &eval(environment, key)?.get().data {
                ExpEnum::Atom(Atom::Symbol(sym)) => sym,
                ExpEnum::Atom(Atom::StringRef(s)) => s,
                ExpEnum::Atom(Atom::String(s)) => environment.interner.intern(&s),
                ExpEnum::Atom(Atom::StringBuf(s)) => environment.interner.intern(&s.borrow()),
                _ => {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "ns-enter: namespace must be a symbol or string",
                    ))
                }
            };
            let scope = match get_namespace(environment, key) {
                Some(scope) => scope,
                None => {
                    let msg = format!("Error, namespace {} does not exist!", key);
                    return Err(io::Error::new(io::ErrorKind::Other, msg));
                }
            };
            environment.current_scope.push(scope);
            return Ok(Expression::make_nil());
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "ns-enter takes one arg, the name of the namespace to enter",
    ))
}

fn builtin_ns_exists(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(key) = args.next() {
        if args.next().is_none() {
            let key = match &eval(environment, key)?.get().data {
                ExpEnum::Atom(Atom::Symbol(sym)) => sym,
                ExpEnum::Atom(Atom::StringRef(s)) => s,
                ExpEnum::Atom(Atom::String(s)) => environment.interner.intern(&s),
                ExpEnum::Atom(Atom::StringBuf(s)) => environment.interner.intern(&s.borrow()),
                _ => {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "ns-exists?: namespace must be a symbol or string",
                    ))
                }
            };
            if environment.namespaces.contains_key(key) {
                return Ok(Expression::make_true());
            } else {
                return Ok(Expression::make_nil());
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "ns-exists? takes one arg, the name of the namespace to test existance of",
    ))
}

fn builtin_ns_list(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if args.next().is_none() {
        let mut ns_list = Vec::with_capacity(environment.namespaces.len());
        for ns in environment.namespaces.keys() {
            ns_list.push(Expression::alloc_data_h(ExpEnum::Atom(Atom::StringRef(ns))));
        }
        return Ok(Expression::with_list(ns_list));
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "ns-list takes no args",
    ))
}

fn builtin_ns_pop(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if args.next().is_some() {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "ns-pop: takes no parameters",
        ));
    }

    if environment.current_scope.len() < 2 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "ns-pop: no more namespaces",
        ));
    }
    if environment
        .current_scope
        .last()
        .unwrap()
        .borrow()
        .name
        .is_none()
    {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "ns-pop: can only be used when not in a lexical scope (current scope must be a namespace)",
        ));
    }
    if let Some(scope) = environment.current_scope.pop() {
        if environment
            .current_scope
            .last()
            .unwrap()
            .borrow()
            .name
            .is_none()
        {
            environment.current_scope.push(scope);
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "ns-pop: outer scope must be a namespace",
            ));
        }
    } else {
        return Err(io::Error::new(io::ErrorKind::Other, "ns-pop: NO SCOPES"));
    }
    Ok(Expression::make_nil())
}

fn builtin_ns_symbols(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(key) = args.next() {
        if args.next().is_none() {
            let key = match &eval(environment, key)?.get().data {
                ExpEnum::Atom(Atom::Symbol(sym)) => sym,
                ExpEnum::Atom(Atom::String(s)) => environment.interner.intern(&s),
                ExpEnum::Atom(Atom::StringRef(s)) => s,
                ExpEnum::Atom(Atom::StringBuf(s)) => environment.interner.intern(&*s.borrow()),
                _ => {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "ns-symbols: namespace must be a symbol or string",
                    ))
                }
            };
            if environment.namespaces.contains_key(key) {
                if let Some(symbols) = environment.namespaces.get(key) {
                    let mut ns_symbols = Vec::new();
                    for sym in symbols.borrow().data.keys() {
                        ns_symbols.push(Expression::alloc_data_h(ExpEnum::Atom(Atom::Symbol(sym))));
                    }
                    return Ok(Expression::with_list(ns_symbols));
                }
                return Ok(Expression::make_nil());
            } else {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "ns-symbols: namespace not found",
                ));
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "ns-symbols: requires one arg- a namespace",
    ))
}

pub fn add_namespace_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, Reference, S>,
) {
    let root = interner.intern("root");
    data.insert(
        interner.intern("ns-create"),
        Expression::make_function(
            builtin_ns_create,
            "Usage: (ns-create namespace)

Creates and enters a new a namespace (must evaluate to a string or symbol).

Section: namespace

Example:
(ns-create 'ns-create-test-namespace)
(def 'test-symbol \"testing\")
(test::assert-equal \"testing\" test-symbol)
(ns-pop)
(test::assert-false (def? 'test-symbol))
",
            root,
        ),
    );
    data.insert(
        interner.intern("ns-enter"),
        Expression::make_function(
            builtin_ns_enter,
            "Usage: (ns-enter namespace)

Enters an existing namespace (must evaluate to a string or symbol).

Section: namespace

Example:
(ns-create 'ns-enter-test-namespace)
(def 'test-symbol \"testing\")
(test::assert-equal \"testing\" test-symbol)
(ns-pop)
(test::assert-false (def? 'test-symbol))
(ns-enter 'ns-enter-test-namespace)
(test::assert-true (def? 'test-symbol))
(test::assert-equal \"testing\" test-symbol)
(ns-pop)
t
",
            root,
        ),
    );
    data.insert(
        interner.intern("ns-exists?"),
        Expression::make_function(
            builtin_ns_exists,
            "Usage: (ns-exists? namespace)

True if the supplied namespace exists (must evaluate to a string or symbol).

Section: namespace

Example:
(test::assert-false (ns-exists? 'ns-exists-test-namespace))
(ns-create 'ns-exists-test-namespace)
(ns-pop)
(test::assert-true (ns-exists? 'ns-exists-test-namespace))
",
            root,
        ),
    );
    data.insert(
        interner.intern("ns-list"),
        Expression::make_function(
            builtin_ns_list,
            "Usage: (ns-list)

Returns a vector of all namespaces.

Section: namespace

Example:
(test::assert-not-includes \"ns-list-test-namespace\" (ns-list))
(ns-create 'ns-list-test-namespace)
(ns-pop)
(test::assert-includes \"ns-list-test-namespace\" (ns-list))
t
",
            root,
        ),
    );
    data.insert(
        interner.intern("ns-pop"),
        Expression::make_function(
            builtin_ns_pop,
            "Usage: (ns-pop)

Returns to the previous namespace.

Section: namespace

Example:
(ns-create 'ns-pop-test-namespace)
(test::assert-equal \"ns-pop-test-namespace\" *ns*)
(ns-pop)
(test::assert-not-equal \"ns-pop-test-namespace\" *ns*)
",
            root,
        ),
    );
    data.insert(
        interner.intern("ns-symbols"),
        Expression::make_function(
            builtin_ns_symbols,
            "Usage: (ns-symbols namespace)

Returns the list of all symbols in namespace (must evaluate to a string or symbol).

Section: namespace

Example:
(test::assert-not-includes 'dumb-symbol-xxx (ns-symbols 'core))
(test::assert-includes 'loop (ns-symbols 'core))
(test::assert-not-includes 'dumb-symbol-xxx (ns-symbols 'root))
(test::assert-includes 'car (ns-symbols 'root))
t
",
            root,
        ),
    );
}
