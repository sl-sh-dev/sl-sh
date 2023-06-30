use compile_state::state::{CompileEnvironment, SloshVm, SloshVmTrait};
use slvm::{CallFuncSig, VMError, VMResult, Value};

pub mod collections;
pub mod print;
pub mod string;

pub fn get_prop(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 2 {
        return Err(VMError::new_vm(
            "get-prop: Invalid arguments (object symbol)".to_string(),
        ));
    }
    let key = match registers[1] {
        Value::Keyword(key) => key,
        Value::Symbol(key) => key,
        _ => return Err(VMError::new_vm("get-prop: key must be a keywork or symbol")),
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

pub fn set_prop(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
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

pub fn sizeof_heap_object(_vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if !registers.is_empty() {
        return Err(VMError::new_vm(
            "sizeof-heap-object: takes no arguments".to_string(),
        ));
    }
    Ok(Value::UInt32(SloshVm::sizeof_heap_object() as u32))
}

pub fn sizeof_value(_vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if !registers.is_empty() {
        return Err(VMError::new_vm(
            "sizeof-value: takes no arguments".to_string(),
        ));
    }
    Ok(Value::UInt32(std::mem::size_of::<Value>() as u32))
}

pub fn gensym(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if !registers.is_empty() {
        return Err(VMError::new_vm("gensym: takes no arguments".to_string()));
    }
    let line = vm.env().line();
    let sym_idx = vm.env_mut().next_gensym();
    let sym = vm.intern(&format!("#<SYM:{line}:{sym_idx}>"));
    Ok(Value::Symbol(sym))
}

pub fn expand_macro(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
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

pub fn add_builtin(
    env: &mut SloshVm,
    name: &str,
    func: CallFuncSig<CompileEnvironment>,
    doc_string: &str,
) {
    let si = env.set_global_builtin(name, func);
    let key = env.intern("doc-string");
    let s = env.alloc_string(doc_string.to_string());
    env.set_global_property(si, key, s);
}

pub fn add_docstring(env: &mut SloshVm, name: &str, doc_string: &str) {
    let si = env.set_named_global(name, Value::Undefined);
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
}
