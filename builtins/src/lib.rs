extern crate core;

use bridge_adapters::add_builtin;
use bridge_adapters::lisp_adapters::SlFromRef;
use bridge_macros::sl_sh_fn;
use bridge_types::LooseString;
use compile_state::state::{SloshVm, SloshVmTrait};
use slvm::{Interned, VMError, VMResult, Value};
use std::collections::HashSet;

pub mod bridge_macro_tests;
pub mod collections;
pub mod conversions;
pub mod fs_meta;
pub mod fs_temp;
pub mod io;
pub mod math;
pub mod print;
pub mod rand;
pub mod string;

pub const NOOP: &str = "noop";

fn noop(_vm: &mut SloshVm, _registers: &[Value]) -> VMResult<Value> {
    Ok(Value::Nil)
}

fn get_builtin_slot_and_value(vm: &mut SloshVm, s: impl AsRef<str>) -> (u32, Value) {
    let sym = vm.intern(s.as_ref());
    let sym_slot = vm.get_reserve_global(sym);
    let inplace_val = vm.get_global(sym_slot);
    (sym_slot, inplace_val)
}

pub fn noop_fn(environment: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() == 1 {
        let v = registers[0];
        let fcn = LooseString::sl_from_ref(v, environment)?.to_string();
        noop_swap_internal(environment, fcn, NoopSwap::MakeNoop)
    } else {
        Err(VMError::new_vm(
            "noop-fn: takes one argument, a builtin function to swap with noop.".to_string(),
        ))
    }
}

pub fn un_noop_fn(environment: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() == 1 {
        let v = registers[0];
        let fcn = LooseString::sl_from_ref(v, environment)?.to_string();
        noop_swap_internal(environment, fcn, NoopSwap::MakeNotNoop)
    } else {
        Err(VMError::new_vm(
            "un-noop-fn: takes one argument, a builtin function to verify is not set to noop."
                .to_string(),
        ))
    }
}

/// Helper enum to "force" a given function to be noop'ed or not.
#[derive(Debug, Copy, Clone)]
pub enum NoopSwap {
    MakeNoop,
    MakeNotNoop,
}

pub fn noop_swap_internal(
    environment: &mut SloshVm,
    fcn: String,
    force_noop: NoopSwap,
) -> VMResult<Value> {
    let (sym_slot, inplace_val) = get_builtin_slot_and_value(environment, &fcn);
    let (_noop_slot, noop_val) = get_builtin_slot_and_value(environment, NOOP);

    if inplace_val == noop_val && matches!(force_noop, NoopSwap::MakeNotNoop) {
        // the value for fcn name is the same as noop slot, this means the function
        // is currently set to noop
        if let Some(original_value) = environment.env_mut().remove_noop(fcn) {
            environment.set_global(sym_slot, original_value);
            return Ok(Value::True);
        }
    } else if matches!(force_noop, NoopSwap::MakeNoop) {
        // value is *not* noop, save off the original value and then replace the slot with the
        // noop value.
        let _ = environment.env_mut().save_noop(fcn, inplace_val);
        environment.set_global(sym_slot, noop_val);
        return Ok(Value::True);
    }

    Ok(Value::False)
}

fn is_noop(environment: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() == 1 {
        let v = registers[0];
        let fcn = LooseString::sl_from_ref(v, environment)?.to_string();
        let (_sym_slot, inplace_val) = get_builtin_slot_and_value(environment, fcn);
        let (_noop_slot, noop_val) = get_builtin_slot_and_value(environment, NOOP);

        if inplace_val == noop_val {
            // the value for fcn name is the same as noop slot, this means the function
            // is currently set to noop
            Ok(Value::True)
        } else {
            Ok(Value::False)
        }
    } else {
        Err(VMError::new_vm(
            "is-noop: takes one argument, a builtin function to check whether or not it is currently set to noop.".to_string(),
        ))
    }
}

fn get_globals(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if !registers.is_empty() {
        return Err(VMError::new_vm(
            "get-globals: takes no arguments".to_string(),
        ));
    }
    let mut result = vec![];
    for g in vm.globals().keys() {
        result.push(Value::Symbol(*g));
    }
    Ok(vm.alloc_vector(result))
}

pub fn get_namespaces_interned(vm: &mut SloshVm) -> HashSet<Interned> {
    let mut result = HashSet::new();
    let i = vm.intern_static("root");
    result.insert(i);
    let mut buffer = String::new();
    let gkeys: Vec<_> = vm.globals().keys().copied().collect();
    for g in gkeys.into_iter() {
        let name = vm.get_interned(g);
        buffer.clear();
        let mut spaces: Vec<_> = name.split("::").collect();
        spaces.pop();
        for (i, part) in spaces.iter().enumerate() {
            if i > 0 {
                buffer.push_str("::");
            }
            buffer.push_str(part);
            let i = vm.intern(&buffer);
            result.insert(i);
        }
    }
    result
}

/// Usage: (get-namespaces)
///
/// Return a vector containing all the namespaces currently defined globally.
///
/// Section: namespace
#[sl_sh_fn(fn_name = "get-namespaces", takes_env = true)]
fn get_namespaces(environment: &mut SloshVm) -> VMResult<Value> {
    let namespaces = get_namespaces_interned(environment);
    let result: Vec<Value> = namespaces.iter().map(|x| Value::Symbol(*x)).collect();
    Ok(environment.alloc_vector(result))
}

pub fn retrieve_in_namespace(vm: &mut SloshVm, interned: &Interned) -> Vec<Value> {
    let namespace = vm.get_interned(*interned);
    let mut result = vec![];
    if namespace.eq("root") {
        for g in vm.globals().keys() {
            let sym = vm.get_interned(*g);
            if !sym.contains("::") {
                result.push(Value::Symbol(*g));
            }
        }
    } else {
        let mut namespace = namespace.to_string();
        namespace.push_str("::");
        for g in vm.globals().keys() {
            let sym = vm.get_interned(*g);
            if sym.starts_with(&namespace) {
                result.push(Value::Symbol(*g));
            }
        }
    }
    result
}

/// Usage: (get-in-namespace 'SYMBOL)
///
/// Return a vector containing all the globals currently defined namespace SYMBOL.
///
/// Section: namespace
#[sl_sh_fn(fn_name = "get-in-namespace", takes_env = true)]
fn get_in_namespace(environment: &mut SloshVm, symbol: bridge_types::Symbol) -> VMResult<Value> {
    let i = Interned::from(symbol);
    let v = retrieve_in_namespace(environment, &i);
    Ok(environment.alloc_vector(v))
}

fn get_prop(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 2 {
        return Err(VMError::new_vm(
            "get-prop: Invalid arguments (object symbol)".to_string(),
        ));
    }
    let key = match registers[1] {
        Value::Keyword(key) => key,
        Value::Symbol(key) => key,
        _ => return Err(VMError::new_vm("get-prop: key must be a keyword or symbol")),
    };
    match registers[0] {
        Value::Symbol(si) => {
            if let Some(idx) = vm.global_intern_slot(si) {
                Ok(vm.get_global_property(idx, key).unwrap_or(Value::Nil))
            } else {
                Err(VMError::new_vm(
                    "get-prop: Not a heap object or global symbol".to_string(),
                ))
            }
        }
        _ => Ok(vm
            .get_heap_property_interned(registers[0], key)
            .unwrap_or(Value::Nil)),
    }
}

fn set_prop(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 3 {
        return Err(VMError::new_vm(
            "set-prop: Invalid arguments (object symbol value)".to_string(),
        ));
    }
    let key = match registers[1] {
        Value::Keyword(key) => key,
        Value::Symbol(key) => key,
        _ => return Err(VMError::new_vm("set-prop: key must be a keyword or symbol")),
    };
    if let Value::Symbol(si) = registers[0] {
        if let Some(idx) = vm.global_intern_slot(si) {
            vm.set_global_property(idx, key, registers[2]);
            Ok(registers[2])
        } else {
            Err(VMError::new_vm(
                "set-prop: Not a heap object or global symbol".to_string(),
            ))
        }
    } else {
        vm.set_heap_property_interned(registers[0], key, registers[2]);
        Ok(registers[2])
    }
}

fn sizeof_heap_object(_vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if !registers.is_empty() {
        return Err(VMError::new_vm(
            "sizeof-heap-object: takes no arguments".to_string(),
        ));
    }
    Ok((SloshVm::sizeof_heap_object() as i64).into())
}

fn sizeof_value(_vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if !registers.is_empty() {
        return Err(VMError::new_vm(
            "sizeof-value: takes no arguments".to_string(),
        ));
    }
    Ok((std::mem::size_of::<Value>() as i64).into())
}

fn gensym(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if !registers.is_empty() {
        return Err(VMError::new_vm("gensym: takes no arguments".to_string()));
    }
    let line = vm.env().line();
    let sym_idx = vm.env_mut().next_gensym();
    let sym = vm.intern(&format!("#<SYM:{line}:{sym_idx}>"));
    Ok(Value::Symbol(sym))
}

fn expand_macro(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 1 {
        return Err(VMError::new_vm(
            "expand-macro: takes one arguments".to_string(),
        ));
    }
    let arg = registers[0];
    match arg {
        Value::Pair(_) | Value::List(_, _) => {
            let (car, cdr) = arg.get_pair(vm).expect("Pair/List not a Pair or List?");
            if let Value::Symbol(i) = car {
                if let Some(idx) = vm.global_intern_slot(i) {
                    let car = vm.get_global(idx);
                    match car {
                        Value::Lambda(h) => {
                            let l = vm.get_lambda(h);
                            let args: Vec<Value> = cdr.iter(vm).collect();
                            match vm.do_call(l, &args[..], None) {
                                Ok(v) => Ok(v),
                                Err(e) => {
                                    Err(VMError::new_vm(format!("expand-macro: error on call {e}")))
                                }
                            }
                        }
                        Value::Closure(h) => {
                            let (l, tcaps) = vm.get_closure(h);
                            let caps = Vec::from(tcaps);
                            let args: Vec<Value> = cdr.iter(vm).collect();
                            match vm.do_call(l, &args[..], Some(&caps[..])) {
                                Ok(v) => Ok(v),
                                Err(e) => {
                                    Err(VMError::new_vm(format!("expand-macro: error on call {e}")))
                                }
                            }
                        }
                        _ => Err(VMError::new_vm("expand-macro: not a callable".to_string())),
                    }
                } else {
                    Err(VMError::new_vm("expand-macro: not a symbol".to_string()))
                }
            } else {
                Err(VMError::new_vm("expand-macro: not a symbol".to_string()))
            }
        }
        _ => Err(VMError::new_vm("expand-macro: requires a list".to_string())),
    }
}

pub fn add_global_value(env: &mut SloshVm, name: &str, val: Value, doc_string: &str) {
    let si = env.set_named_global(name, val);
    let key = env.intern("doc-string");
    let s = env.alloc_string(doc_string.to_string());
    env.set_global_property(si, key, s);
}

pub fn add_misc_builtins(env: &mut SloshVm) {
    add_builtin(
        env,
        "get-prop",
        get_prop,
        r#"Usage: (get-prop 'a-symbol :a-property))

Read property that maps to given keyword for provided-symbol. Most forms have
a :doc-string property that returns the docstring for the symbol.

Section: core

Example:
#t
"#,
    );
    add_builtin(
        env,
        "set-prop",
        set_prop,
        r#"Usage: (set-prop 'a-symbol :a-property a-value)

Write property with value for the given symbol.

Section: core

Example:
(def foo #t)
(set-prop 'foo :bar "baz")
(test::assert-equal "baz" (get-prop 'foo :bar))
"#,
    );
    add_builtin(
        env,
        "sizeof-heap-object",
        sizeof_heap_object,
        r#"Usage: (sizeof-heap-object)

Returns the default size of a heap object by the current runtime in bytes.

Section: core

Example:
(test::assert-equal 16 (sizeof-heap-object))
"#,
    );
    add_builtin(
        env,
        "sizeof-value",
        sizeof_value,
        r#"Usage: (sizeof-value)

Returns the default size of a value by the current runtime in bytes. Optimized to
be 8 bytes so that a given primitve or pointer to a head object fits in one word
on a 64 bit machine.

Section: core

Example:
(test::assert-equal 8 (sizeof-value))
"#,
    );
    add_builtin(
        env,
        "gensym",
        gensym,
        r#"Usage: (gensym)

Used to make macros hygenic by creating a random symbol name to be used
in code output by a macro to avoid conflicting variable names.

Section: core

Example:
#t
"#,
    );
    add_builtin(
        env,
        "expand-macro",
        expand_macro,
        r#"Usage: (expand-macro 'code)

Output code, any macro invocation will be replaced with the code it would generate.
This is particularly useful for introspection when debugging macros.

Section: core

Example:
#t
"#,
    );
    add_builtin(
        env,
        NOOP,
        noop,
        r#"Usage: (noop any*)

Takes any number of arguments and always returns nil.

Section: core

Example:
(test::assert-equal nil (noop 'noop))
(test::assert-equal nil (noop "foo" :bar 'baz))
(test::assert-equal nil (noop))
"#,
    );
    add_builtin(
        env,
        "noop-fn",
        noop_fn,
        r#"Usage: (noop-fn 'fn-to-noop)

Alter the runtime so that the provided function is a no-operation (no-op or noop) that
does nothing and returns nil for the provided function. Any future call to this function
will do nothing, regardless of the arguments it is given.

Section: core

Example:
#t
"#,
    );
    add_builtin(
        env,
        "un-noop-fn",
        un_noop_fn,
        r#"Usage: (un-noop-fn 'fn-to-un-noop)

If the runtime was previously altered for the provided function (to make it do nothing)
swap the old function back in so the previous behavior is restored and the function
behaves as normal.

Section: core

Example:
#t
"#,
    );
    add_builtin(
        env,
        "is-noop",
        is_noop,
        r#"Usage: (is-noop 'fn-to-test)

Report whether or not the provided function is currently set to do nothing.
When called with the 'noop function always returns true.

Section: core

Example:
(test::assert-true (is-noop 'noop))
(test::assert-false (is-noop 'fs-meta))
"#,
    );
    add_builtin(
        env,
        "get-globals",
        get_globals,
        r#"Usage: (get-globals)

Return a vector containing all the symbols currently defined globally.

Section: core
"#,
    );
    intern_get_in_namespace(env);
    intern_get_namespaces(env);
}
