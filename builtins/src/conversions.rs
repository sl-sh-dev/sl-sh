use bridge_adapters::add_builtin;
use compile_state::state::{SloshVm, SloshVmTrait};
use slvm::{VMError, VMResult, Value};

fn to_sym(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(string), None) = (i.next(), i.next()) {
        match string {
            Value::StringConst(i) => Ok(Value::Symbol(*i)),
            Value::Keyword(i) => Ok(Value::Symbol(*i)),
            Value::Symbol(i) => Ok(Value::Symbol(*i)),
            _ => {
                let string = string.pretty_value(vm);
                let i = vm.intern(&string);
                Ok(Value::Symbol(i))
            }
        }
    } else {
        Err(VMError::new_conversion("->sym: takes one arg".to_string()))
    }
}

fn to_key(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(string), None) = (i.next(), i.next()) {
        match string {
            Value::StringConst(i) => Ok(Value::Keyword(*i)),
            Value::Keyword(i) => Ok(Value::Keyword(*i)),
            Value::Symbol(i) => Ok(Value::Keyword(*i)),
            _ => {
                let string = string.pretty_value(vm);
                let i = vm.intern(&string);
                Ok(Value::Keyword(i))
            }
        }
    } else {
        Err(VMError::new_conversion("->key: takes one arg".to_string()))
    }
}

fn global_ref(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(symbol), None) = (i.next(), i.next()) {
        match symbol {
            Value::Symbol(i) => {
                if let Some(slot) = vm.global_intern_slot(*i) {
                    Ok(vm.get_global(slot))
                } else {
                    Err(VMError::new_conversion("ref: not a global var".to_string()))
                }
            }
            _ => Err(VMError::new_conversion(format!(
                "ref: expected a symbol, got a {}",
                symbol.display_type(vm)
            ))),
        }
    } else {
        Err(VMError::new_conversion("ref: takes one arg".to_string()))
    }
}

fn is_def(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(symbol), None) = (i.next(), i.next()) {
        match symbol {
            Value::Symbol(i) => {
                if let Some(_slot) = vm.global_intern_slot(*i) {
                    Ok(Value::True)
                } else {
                    Ok(Value::False)
                }
            }
            _ => Err(VMError::new_conversion(format!(
                "def?: expected a symbol, got a {}",
                symbol.display_type(vm)
            ))),
        }
    } else {
        Err(VMError::new_conversion("def?: takes one arg".to_string()))
    }
}

pub fn add_conv_builtins(env: &mut SloshVm) {
    add_builtin(
        env,
        "->sym",
        to_sym,
        //space is needed here for test to pass do not know why
        r#" Usage: (->sym value) => symbol

Convert a value to a symbol.

Arguments:
- value: Any type. The value to convert (string, keyword, symbol, or other).
- symbol: A symbol. The converted symbol.

If value is already a symbol or keyword, returns the corresponding symbol.
Other values are converted to their string representation first.

Section: conversion
"#,
    );
    add_builtin(
        env,
        "->key",
        to_key,
        r#"Usage: (->key value) => keyword

Convert a value to a keyword.

Arguments:
- value: Any type. The value to convert (string, symbol, keyword, or other).
- keyword: A keyword. The converted keyword.

If value is already a keyword or symbol, returns the corresponding keyword.
Other values are converted to their string representation first.

Section: conversion
"#,
    );
    add_builtin(
        env,
        "ref",
        global_ref,
        r#"Usage: (ref symbol) => value

Get the value of a global symbol.

Arguments:
- symbol: A symbol. The global variable name to dereference.
- value: Any type. The value stored in the global variable.

Returns an error if the symbol is not defined as a global variable.

Section: conversion
"#,
    );
    add_builtin(
        env,
        "def?",
        is_def,
        r#"Usage: (def? symbol) => boolean

Test whether a symbol is defined as a global variable.

Arguments:
- symbol: A symbol. The symbol to test.
- boolean: A boolean. True if defined, false otherwise.

Section: conversion
"#,
    );
}
