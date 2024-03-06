use compile_state::add_builtin;
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
        r#"Usage: (->sym exp) -> symbol

Converts exp to a symbol.

Section: conv
"#,
    );
    add_builtin(
        env,
        "->key",
        to_key,
        r#"Usage: (->key exp) -> keyword

Converts exp to a keyword.

Section: conv
"#,
    );
    add_builtin(
        env,
        "ref",
        global_ref,
        r#"Usage: (ref symol) -> Value

If symbol is defined then return the thing it references.

Section: conv
"#,
    );
    add_builtin(
        env,
        "def?",
        is_def,
        r#"Usage: (def? symol) -> #t/#f

If symbol is defined then return true else false.

Section: conv
"#,
    );
}
