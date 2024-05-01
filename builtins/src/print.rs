use crate::SloshVm;
use compile_state::state::{CompileState, SloshVmTrait};
use sl_compiler::compile;
use sl_compiler::pass1::pass1;
use slvm::{Interned, VMError, VMResult, Value};
use std::io::{stderr, stdout, Write};

fn is_sym(vm: &SloshVm, name: &str, intern: Interned) -> bool {
    if let Some(i) = vm.get_if_interned(name) {
        if intern == i {
            return true;
        }
    }
    false
}

fn quotey(vm: &SloshVm, car: Value, buf: &mut String) -> bool {
    if let Value::Symbol(i) = car {
        if is_sym(vm, "quote", i) {
            buf.push('\'');
            true
        } else if is_sym(vm, "back-quote", i) {
            buf.push('`');
            true
        } else if is_sym(vm, "unquote", i) {
            buf.push(',');
            true
        } else if is_sym(vm, "unquote-splice", i) {
            buf.push_str(",@");
            true
        } else if is_sym(vm, "unquote-splice!", i) {
            buf.push_str(",.");
            true
        } else {
            false
        }
    } else {
        false
    }
}

fn list_out(vm: &SloshVm, res: &mut String, lst: Value) {
    let mut first = true;
    let mut cdr = lst;
    loop {
        if let Value::Nil = cdr {
            break;
        }
        if !first {
            res.push(' ');
        } else {
            first = false;
        }
        match cdr {
            Value::Pair(_) | Value::List(_, _) => {
                let (car, ncdr) = cdr.get_pair(vm).expect("pair/list not a pair/list");
                res.push_str(&display_value(vm, car));
                cdr = ncdr;
            }
            _ => {
                res.push_str(". ");
                res.push_str(&display_value(vm, cdr));
                break;
            }
        }
    }
}

pub fn display_value(vm: &SloshVm, val: Value) -> String {
    match &val {
        Value::Pair(_) | Value::List(_, _) => {
            let (car, cdr) = val.get_pair(vm).expect("pair/list not a pair/list");
            let mut res = String::new();
            if quotey(vm, car, &mut res) {
                if let Some((cadr, Value::Nil)) = cdr.get_pair(vm) {
                    res.push_str(&display_value(vm, cadr));
                } else {
                    res.push_str(&display_value(vm, cdr));
                }
            } else {
                res.push('(');
                list_out(vm, &mut res, val);
                res.push(')');
            }
            res
        }
        _ => val.display_value(vm),
    }
}

pub fn pretty_value(vm: &SloshVm, val: Value) -> String {
    match &val {
        Value::StringConst(i) => vm.get_interned(*i).to_string(),
        Value::CodePoint(ch) => format!("{ch}"),
        Value::CharCluster(l, c) => {
            format!("{}", String::from_utf8_lossy(&c[0..*l as usize]))
        }
        Value::CharClusterLong(h) => vm.get_string(*h).to_string(),
        Value::String(h) => vm.get_string(*h).to_string(),
        _ => display_value(vm, val),
    }
}

pub fn pr(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    for v in registers {
        print!("{}", pretty_value(vm, *v));
    }
    stdout().flush()?;
    Ok(Value::Nil)
}

pub fn epr(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    for v in registers {
        eprint!("{}", pretty_value(vm, *v));
    }
    stderr().flush()?;
    Ok(Value::Nil)
}

pub fn prn(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    for v in registers {
        print!("{}", pretty_value(vm, *v));
    }
    println!();
    Ok(Value::Nil)
}

pub fn eprn(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    for v in registers {
        eprint!("{}", pretty_value(vm, *v));
    }
    eprintln!();
    Ok(Value::Nil)
}

pub fn dasm(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 1 {
        return Err(VMError::new_compile(
            "dasm: wrong number of args, expected one",
        ));
    }
    let exp = registers[0].unref(vm);
    match exp {
        Value::Lambda(handle) => {
            let l = vm.get_lambda(handle);
            l.disassemble_chunk(vm, 0)?;
            Ok(Value::Nil)
        }
        Value::Closure(handle) => {
            let (l, _) = vm.get_closure(handle);
            l.disassemble_chunk(vm, 0)?;
            Ok(Value::Nil)
        }
        Value::List(_handle, _start_idx) => {
            let mut state = CompileState::new_state("", 1, None);
            pass1(vm, &mut state, exp)?;
            compile(vm, &mut state, exp, 0)?;
            state.chunk.disassemble_chunk(vm, 0)?;
            Ok(Value::Nil)
        }
        Value::Pair(_handle) => {
            let mut state = CompileState::new_state("", 1, None);
            pass1(vm, &mut state, exp)?;
            compile(vm, &mut state, exp, 0)?;
            state.chunk.disassemble_chunk(vm, 0)?;
            Ok(Value::Nil)
        }
        _ => Err(VMError::new_vm("DASM: Not a callable.")),
    }
}

pub fn add_print_builtins(env: &mut SloshVm) {
    env.set_global_builtin("pr", pr);
    env.set_global_builtin("epr", epr);
    env.set_global_builtin("prn", prn);
    env.set_global_builtin("eprn", eprn);
    env.set_global_builtin("dasm", dasm);
}
