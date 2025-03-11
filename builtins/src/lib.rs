extern crate core;

use std::collections::HashSet;

use compile_state::state::{SloshVm, SloshVmTrait};
use slvm::{Interned, VMError, VMResult, Value};

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

fn get_namespaces(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if !registers.is_empty() {
        return Err(VMError::new_vm(
            "get-namespaces: takes no arguments".to_string(),
        ));
    }
    let mut result = HashSet::new();
    let i = vm.intern_static("root");
    result.insert(Value::Symbol(i));
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
            result.insert(Value::Symbol(i));
        }
    }
    let v: Vec<Value> = result.into_iter().collect();
    Ok(vm.alloc_vector(v))
}

fn retrieve_in_namespace(vm: &mut SloshVm, interned: &Interned) -> Vec<Value> {
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

fn get_in_namespace(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut regs = registers.iter();
    let ns = if let (Some(Value::Symbol(i)), None) = (regs.next(), regs.next()) {
        retrieve_in_namespace(vm, i)
    } else {
        return Err(VMError::new_vm(
            "get-in-namespace: takes one argument, a symbol".to_string(),
        ));
    };
    Ok(vm.alloc_vector(ns))
}


/// Usage: (ns-list sym)
///
/// Returns symbols in provided namespace
///
/// Section: namespace
///
/// Example:
/// (test::assert-equal 1 (occurs (list 1 3 5 2 4 8 2 4 88 2 1) 8))
/// (test::assert-equal 3 (occurs (list 1 3 5 2 4 10 2 4 88 2 1) 2))
/// (test::assert-equal 0 (occurs (list 1 3 5 2 4 10 2 4 88 2 1) 42))
///
#[bridge_macros::sl_sh_fn(fn_name = "ns-list", takes_env = true)]
fn ns_list(environment: &mut SloshVm, symbol: bridge_types::Symbol) -> VMResult<Value> {
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
    env.set_global_builtin("get-prop", get_prop);
    env.set_global_builtin("set-prop", set_prop);
    env.set_global_builtin("sizeof-heap-object", sizeof_heap_object);
    env.set_global_builtin("sizeof-value", sizeof_value);
    env.set_global_builtin("gensym", gensym);
    env.set_global_builtin("expand-macro", expand_macro);
    bridge_adapters::add_builtin(
        env,
        "get-globals",
        get_globals,
        "Usage: (get-globals)

Return a vector containing all the symbols currently defined globally.

Section: core
",
    );
    bridge_adapters::add_builtin(
        env,
        "get-namespaces",
        get_namespaces,
        "Usage: (get-namespaces)

Return a vector containing all the namespaces currently defined globally.

Section: namespace
",
    );
    bridge_adapters::add_builtin(
        env,
        "get-in-namespace",
        get_in_namespace,
        "Usage: (get-in-namespace 'SYMBOL)

Return a vector containing all the globals currently defined namespace SYMBOL.

Section: namespace
",
    );
    intern_ns_list(env);
}
