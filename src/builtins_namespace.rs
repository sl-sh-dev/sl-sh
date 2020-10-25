use std::collections::HashMap;
use std::hash::BuildHasher;

use crate::environment::*;
use crate::eval::*;
use crate::interner::*;
use crate::types::*;

fn set_active_namespace(environment: &mut Environment, ns: &'static str) {
    environment.root_scope.borrow_mut().data.insert(
        environment.interner.intern("*active-ns*"),
        Reference::new(
            ExpEnum::String(ns.into(), None),
            RefMetaData {
                namespace: Some("root"),
                doc_string: None,
            },
        ),
    );
}

fn builtin_ns_create(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(key) = args.next() {
        if args.next().is_none() {
            let key = match &eval(environment, key)?.get().data {
                ExpEnum::Symbol(sym, _) => sym,
                ExpEnum::String(s, _) => environment.interner.intern(&s),
                _ => {
                    return Err(LispError::new(
                        "ns-create: namespace must be a symbol or string",
                    ))
                }
            };
            let scope = match build_new_namespace(environment, key) {
                Ok(scope) => scope,
                Err(msg) => return Err(LispError::new(msg)),
            };
            set_active_namespace(environment, key);
            environment.namespace = scope;
            return Ok(Expression::make_nil());
        }
    }
    Err(LispError::new(
        "ns-create takes one arg, the name of the new namespace",
    ))
}

fn builtin_ns_enter(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(key) = args.next() {
        if args.next().is_none() {
            let key = match &eval(environment, key)?.get().data {
                ExpEnum::Symbol(sym, _) => sym,
                ExpEnum::String(s, _) => environment.interner.intern(&s),
                _ => {
                    return Err(LispError::new(
                        "ns-enter: namespace must be a symbol or string",
                    ))
                }
            };
            let scope = match get_namespace(environment, key) {
                Some(scope) => scope,
                None => {
                    let msg = format!("Error, namespace {} does not exist!", key);
                    return Err(LispError::new(msg));
                }
            };
            set_active_namespace(environment, key);
            environment.namespace = scope;
            return Ok(Expression::make_nil());
        }
    }
    Err(LispError::new(
        "ns-enter takes one arg, the name of the namespace to enter",
    ))
}

fn builtin_ns_exists(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(key) = args.next() {
        if args.next().is_none() {
            let key = match &eval(environment, key)?.get().data {
                ExpEnum::Symbol(sym, _) => sym,
                ExpEnum::String(s, _) => environment.interner.intern(&s),
                _ => {
                    return Err(LispError::new(
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
    Err(LispError::new(
        "ns-exists? takes one arg, the name of the namespace to test existance of",
    ))
}

fn builtin_ns_list(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if args.next().is_none() {
        let mut ns_list = Vec::with_capacity(environment.namespaces.len());
        for ns in environment.namespaces.keys() {
            ns_list.push(Expression::alloc_data_h(ExpEnum::String(
                (*ns).into(),
                None,
            )));
        }
        return Ok(Expression::with_list(ns_list));
    }
    Err(LispError::new("ns-list takes no args"))
}

fn builtin_ns_symbols(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(key) = args.next() {
        if args.next().is_none() {
            let key = match &eval(environment, key)?.get().data {
                ExpEnum::Symbol(sym, _) => sym,
                ExpEnum::String(s, _) => environment.interner.intern(&s),
                _ => {
                    return Err(LispError::new(
                        "ns-symbols: namespace must be a symbol or string",
                    ))
                }
            };
            if environment.namespaces.contains_key(key) {
                if let Some(symbols) = environment.namespaces.get(key) {
                    let mut ns_symbols = Vec::new();
                    for sym in symbols.borrow().data.keys() {
                        ns_symbols
                            .push(Expression::alloc_data_h(ExpEnum::Symbol(sym, SymLoc::None)));
                    }
                    return Ok(Expression::with_list(ns_symbols));
                }
                return Ok(Expression::make_nil());
            } else {
                return Err(LispError::new("ns-symbols: namespace not found"));
            }
        }
    }
    Err(LispError::new("ns-symbols: requires one arg- a namespace"))
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
(ns-push 'test-ns-create)
(def test-ns-enter *ns*)
(ns-create 'ns-create-test-namespace)
(def test-symbol \"testing\")
(test::assert-equal \"testing\" test-symbol)
(ns-enter test-ns-create::test-ns-enter)
(test::assert-false (def? test-symbol))
(ns-pop)
t
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
(ns-push 'test-ns-enter)
(def test-ns-enter *ns*)
(ns-create 'ns-enter-test-namespace)
(def test-symbol \"testing\")
(test::assert-equal \"testing\" test-symbol)
(ns-enter test-ns-enter::test-ns-enter)
(test::assert-false (def? test-symbol))
(ns-enter 'ns-enter-test-namespace)
(test::assert-true (def? test-symbol))
(test::assert-equal \"testing\" test-symbol)
(ns-enter test-ns-enter::test-ns-enter)
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
(ns-push 'ns-exists-test-namespace)
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
(ns-push 'ns-list-test-namespace)
(ns-pop)
(test::assert-includes \"ns-list-test-namespace\" (ns-list))
t
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
(test::assert-includes 'loop (ns-symbols 'root))
(test::assert-not-includes 'dumb-symbol-xxx (ns-symbols 'root))
(test::assert-includes 'car (ns-symbols 'root))
t
",
            root,
        ),
    );
}
