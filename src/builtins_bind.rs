use std::collections::HashMap;
use std::hash::BuildHasher;

use crate::builtins_util::*;
use crate::environment::*;
use crate::eval::*;
use crate::interner::*;
use crate::types::*;

pub fn proc_set_vars<'a>(
    environment: &mut Environment,
    args: &'a mut dyn Iterator<Item = Expression>,
) -> Result<(&'static str, Option<String>, Expression), LispError> {
    if let Some(key) = args.next() {
        if let Some(arg1) = args.next() {
            let key_d = key.get();
            let key = match &key_d.data {
                ExpEnum::Symbol(s, _) => *s,
                _ => return Err(LispError::new("first form (binding key) must be a symbol")),
            };
            if let Some(arg2) = args.next() {
                if args.next().is_none() {
                    let doc_str = if let Ok(docs) = eval(environment, arg1)?.as_string(environment)
                    {
                        Some(docs)
                    } else {
                        None
                    };
                    return Ok((key, doc_str, arg2));
                }
            } else {
                return Ok((key, None, arg1));
            }
        }
    }
    Err(LispError::new(
        "bindings requires a key, optional docstring and value",
    ))
}

fn do_set(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
    name: &str,
    is_var: bool,
) -> Result<Expression, LispError> {
    if let Some(key) = args.next() {
        if let Some(val) = args.next() {
            if args.next().is_none() {
                let val = eval(environment, val)?;
                match &mut key.get_mut().data {
                    ExpEnum::Symbol(key_str, location) => match location {
                        SymLoc::None if !is_var => {
                            if let Some(binding) =
                                environment.namespace.borrow().get_with_outer(key_str)
                            {
                                binding.replace(val.clone());
                            } else {
                                return Err(LispError::new(format!(
                                    "{}: symbol {} not found",
                                    name, key_str
                                )));
                            }
                        }
                        SymLoc::Ref(binding) if !is_var => {
                            binding.replace(val.clone());
                        }
                        SymLoc::Namespace(_scope, _idx) if !is_var => {
                            // XXX TODO- code this or get rid of this case...
                        }
                        SymLoc::Stack(idx) => {
                            if let Some(binding) =
                                environment.stack.get(environment.stack_frame_base + *idx)
                            {
                                binding.replace(val.clone());
                            }
                        }
                        _ => {
                            return Err(LispError::new(format!(
                                "{}: symbol {} not found",
                                name, key_str
                            )));
                        }
                    },
                    _ => {
                        return Err(LispError::new(format!(
                            "{}: first form must be a symbol",
                            name
                        )))
                    }
                }
                return Ok(val);
            }
        }
    }

    Err(LispError::new(format!(
        "{}: requires a symbol and value",
        name
    )))
}

pub(crate) fn builtin_set(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    do_set(environment, args, "set!", false)
}

pub(crate) fn builtin_var(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    do_set(environment, args, "var", true)
}

pub(crate) fn builtin_def(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    fn current_namespace(environment: &mut Environment) -> &'static str {
        environment.namespace.borrow().name()
    }
    let (key, doc_string, val) = proc_set_vars(environment, args)?;
    if key.contains("::") {
        // namespace reference.
        let mut key_i = key.splitn(2, "::");
        if let Some(namespace) = key_i.next() {
            if let Some(key) = key_i.next() {
                let namespace = if namespace == "ns" {
                    current_namespace(environment)
                } else {
                    namespace
                };
                let mut scope = Some(environment.namespace.clone());
                while let Some(in_scope) = scope {
                    let name = in_scope.borrow().name();
                    if name == namespace {
                        let val = eval(environment, val)?;
                        in_scope
                            .borrow_mut()
                            .insert_with_doc(key, val.clone(), doc_string);
                        return Ok(val);
                    }
                    scope = in_scope.borrow().outer();
                }
            }
        }
        let msg = format!(
            "def: namespaced symbol {} not valid or namespace not a parent namespace",
            key
        );
        Err(LispError::new(msg))
    } else {
        let val = eval(environment, val)?;
        environment
            .namespace
            .borrow_mut()
            .insert_with_doc(key, val.clone(), doc_string);
        Ok(val)
    }
}

fn builtin_undef(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(key) = args.next() {
        if args.next().is_none() {
            let key_d = &key.get().data;
            if let ExpEnum::Symbol(k, _) = key_d {
                return if let Some(rexp) = environment.namespace.borrow_mut().remove(k) {
                    Ok(rexp)
                } else {
                    let msg = format!("undef: symbol {} not defined in current scope (can only undef symbols in current scope).", k);
                    Err(LispError::new(msg))
                };
            }
        }
    }
    Err(LispError::new(
        "undef: can only have one expression (symbol)",
    ))
}

fn builtin_is_def(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    fn get_ret(environment: &mut Environment, name: &str) -> Expression {
        if is_expression(environment, name) {
            Expression::alloc_data(ExpEnum::True)
        } else {
            Expression::alloc_data(ExpEnum::Nil)
        }
    }
    fn do_list(environment: &mut Environment, key: Expression) -> Result<Expression, LispError> {
        let new_key = eval(environment, key)?;
        let new_key_d = new_key.get();
        if let ExpEnum::Symbol(s, _) = &new_key_d.data {
            Ok(get_ret(environment, s))
        } else {
            Err(LispError::new(
                "def?: takes a symbol to lookup (list must eval to symbol)",
            ))
        }
    }
    if let Some(key) = args.next() {
        params_done(args, "def?")?;
        let key_d = key.get();
        match &key_d.data {
            ExpEnum::Symbol(s, _) => Ok(get_ret(environment, s)),
            ExpEnum::Pair(_, _) => {
                drop(key_d);
                do_list(environment, key.clone())
            }
            ExpEnum::Vector(_) => {
                drop(key_d);
                do_list(environment, key.clone())
            }
            _ => Err(LispError::new("def?: takes a symbol to lookup")),
        }
    } else {
        Err(LispError::new("def? takes one form (symbol)"))
    }
}

fn builtin_ref(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(key) = args.next() {
        params_done(args, "ref")?;
        let key_d = key.get();
        let key = match &key_d.data {
            ExpEnum::Symbol(_, _) => key.clone(),
            ExpEnum::Pair(_, _) => {
                drop(key_d);
                eval(environment, key.clone())?
            }
            ExpEnum::Vector(_) => {
                drop(key_d);
                eval(environment, key.clone())?
            }
            _ => return Err(LispError::new("ref: takes a symbol")),
        };
        let key_d = key.get();
        match &key_d.data {
            ExpEnum::Symbol(s, _) => {
                if let Some(form) = get_expression(environment, key.clone()) {
                    Ok(form)
                } else {
                    Err(LispError::new(format!("ref: symbol {} not bound", s)))
                }
            }
            _ => Err(LispError::new("ref: takes a bound symbol")),
        }
    } else {
        Err(LispError::new("ref: takes one form (symbol)"))
    }
}

pub fn add_bind_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, (Expression, String), S>,
) {
    data.insert(
        interner.intern("set!"),
        Expression::make_special(
            builtin_set,
            "Usage: (set! symbol expression) -> expression

Sets an existing expression in the current scope(s).  Return the expression that was set.
Symbol is not evaluted.

Set will set the first binding it finds starting in the current scope and then
trying enclosing scopes until exhausted.

Section: core

Example:
(def test-do-one nil)
(def test-do-two nil)
(def test-do-three (do (set! test-do-one \"One\")(set! test-do-two \"Two\")\"Three\"))
(test::assert-equal \"One\" test-do-one)
(test::assert-equal \"Two\" test-do-two)
(test::assert-equal \"Three\" test-do-three)
(let ((test-do-one nil))
    ; set the currently scoped value.
    (test::assert-equal \"1111\" (set! test-do-one \"1111\"))
    (test::assert-equal \"1111\" test-do-one))
; Original outer scope not changed.
(test::assert-equal \"One\" test-do-one)
;(set! (sym \"test-do-one\") \"do one\")
;(test::assert-equal \"do one\" test-do-one)
(test::assert-error (set! (sym->str test-do-one) \"do one 2\"))
",
        ),
    );
    data.insert(
        interner.intern("def"),
        Expression::make_special_def(
            "Usage: (def symbol doc_string? expression) -> expression

Adds an expression to the current namespace.  Return the expression that was defined.
Symbol is not evaluted.  Can take an option doc string (docstrings can only be
set on namespaced (global) symbols).

Section: core

Example:
(def test-do-one nil)
(def test-do-two nil)
(def test-do-three (do (set! test-do-one \"One\")(set! test-do-two \"Two\")\"Three\"))
(test::assert-equal \"One\" test-do-one)
(test::assert-equal \"Two\" test-do-two)
(test::assert-equal \"Three\" test-do-three)
(let ((test-do-one nil))
    ; Add this to tthe let's scope (shadow the outer test-do-two).
    (test::assert-equal \"Default\" (def ns::test-do-four \"Default\"))
    ; set the currently scoped value.
    (set! test-do-one \"1111\")
    (set! test-do-two \"2222\")
    (test::assert-equal \"1111\" test-do-one)
    (test::assert-equal \"2222\" test-do-two)
    (test::assert-equal \"Default\" test-do-four))
; Original outer scope not changed.
(test::assert-equal \"One\" test-do-one)
(test::assert-equal \"Default\" test-do-four)
;(def (sym \"test-do-one\") \"do one\")
(test::assert-error (def (sym \"test-do-one\") \"do one\"))
;(test::assert-equal \"do one\" test-do-one)
(test::assert-error (def (sym->str test-do-one) \"do one 2\"))
",
        ),
    );
    data.insert(
        interner.intern("var"),
        Expression::make_special_var(
            "Usage: (var symbol expression) -> expression

NOTE: var is deprecated, use let or let* to create local bindings.
Adds an expression to the current lexical scope.  Return the expression that was defined.
This will not add to a namespace (use def for that), use it within functions or
let forms to create local bindings.
Symbol is not evaluted.

Section: core

Example:
(let (())
(var test-do-one nil)
(var test-do-two nil)
(var test-do-three (do (set! test-do-one \"One\")(set! test-do-two \"Two\")\"Three\"))
(test::assert-equal \"One\" test-do-one)
(test::assert-equal \"Two\" test-do-two)
(test::assert-equal \"Three\" test-do-three)
(let ((test-do-one nil))
    ; Add this to the let's scope (shadow the outer test-do-two).
    (test::assert-equal \"Default\" (var test-do-two \"Default\"))
    ; set the currently scoped value.
    (set! test-do-one \"1111\")
    (set! test-do-two \"2222\")
    (test::assert-equal \"1111\" test-do-one)
    (test::assert-equal \"2222\" test-do-two))
; Original outer scope not changed.
(test::assert-equal \"One\" test-do-one)
(test::assert-equal \"Two\" test-do-two))
",
        ),
    );
    data.insert(
        interner.intern("undef"),
        Expression::make_special(
            builtin_undef,
            "Usage: (undef symbol) -> expression

Remove a symbol from the current namespace (if it exists).  Returns the expression
that was removed.  It is an error if symbol is not defined in the current namespace.
Symbol is not evaluted.

Section: core

Example:
(def test-undef nil)
(test::assert-true (def? test-undef))
(undef test-undef)
(test::assert-false (def? test-undef))
(def test-undef \"undef\")
(test::assert-equal \"undef\" (undef test-undef))
(test::assert-false (def? test-undef))
(test::assert-equal \"undef: symbol test-undef not defined in current scope (can only undef symbols in current scope).\" (cadr (get-error (undef test-undef))))
",
        ),
    );
    data.insert(
        interner.intern("def?"),
        Expression::make_special(
            builtin_is_def,
            "Usage: (def? expression) -> t|nil

Return true if is a defined symbol (bound within the current scope). If expression
is a symbol it is not evaluted and if a list it is evaluted to produce a symbol.

Section: core

Example:
(def test-is-def #t)
(def test-is-def2 'test-is-def)
(test::assert-true (def? test-is-def))
(test::assert-true (def? (sym \"test-is-def\")))
(test::assert-true (def? (ref test-is-def2)))
(test::assert-false (def? test-is-def-not-defined))
(test::assert-false (def? (sym \"test-is-def-not-defined\")))
(test::assert-error (def? (ref test-is-def)))
",
        ),
    );
    data.insert(
        interner.intern("ref"),
        Expression::make_special(
            builtin_ref,
            "Usage: (ref symbol) -> expression

Return the expression that is referenced by symbol.
Symbol is only evaluated if a list (that produces a symbol) and must be bound
in the current scope or an error is raised.

Section: core

Example:
(def test-is-def #t)
(def test-is-def2 'test-is-def)
(test::assert-true (ref test-is-def))
(set! test-is-def '(1 2 3))
(test::assert-equal '(1 2 3) (ref test-is-def))
(test::assert-error (ref test-is-def-no-exist))
(test::assert-error (ref (ref test-is-def)))
(test::assert-equal '(1 2 3) (ref (ref test-is-def2)))
",
        ),
    );
}
