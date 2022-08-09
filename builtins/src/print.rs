use slvm::{Interned, VMError, VMResult, Value, Vm};

fn is_sym(vm: &Vm, name: &str, intern: Interned) -> bool {
    if let Some(i) = vm.get_if_interned(name) {
        if intern == i {
            return true;
        }
    }
    false
}

fn quotey(vm: &Vm, car: Value, buf: &mut String) -> bool {
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

fn list_out_iter(vm: &Vm, res: &mut String, itr: &mut dyn Iterator<Item = Value>) {
    let mut first = true;
    for p in itr {
        if !first {
            res.push(' ');
        } else {
            first = false;
        }
        res.push_str(&display_value(vm, p));
    }
}

fn list_out(vm: &Vm, res: &mut String, lst: Value) {
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
            Value::Pair(h) => {
                let (car, ncdr) = vm.get_pair(h);
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

pub fn display_value(vm: &Vm, val: Value) -> String {
    match &val {
        Value::True => "true".to_string(),
        Value::False => "false".to_string(),
        Value::Float(f) => format!("{}", f.0),
        Value::Int(i) => format!("{}", i),
        Value::UInt(i) => format!("{}", i),
        Value::Byte(b) => format!("{}", b),
        Value::Symbol(i) => vm.get_interned(*i).to_string(),
        Value::Keyword(i) => format!(":{}", vm.get_interned(*i)),
        Value::StringConst(i) => format!("\"{}\"", vm.get_interned(*i)),
        Value::CodePoint(ch) => format!("\\{}", ch),
        Value::CharCluster(l, c) => {
            format!("\\{}", String::from_utf8_lossy(&c[0..*l as usize]))
        }
        Value::CharClusterLong(_) => "Char".to_string(), // XXX TODO- move this to Object?
        Value::Builtin(_) => "#<Function>".to_string(),
        Value::Global(_) => display_value(vm, val.unref(vm)),
        Value::Nil => "nil".to_string(),
        Value::Undefined => "#<Undefined>".to_string(), //panic!("Tried to get type for undefined!"),
        Value::Lambda(_) => "#<Lambda>".to_string(),
        Value::Closure(_) => "#<Lambda>".to_string(),
        Value::Continuation(_) => "#<Continuation>".to_string(),
        Value::CallFrame(_) => "#<CallFrame>".to_string(),
        Value::Vector(h) => {
            let v = vm.get_vector(*h);
            let mut res = String::new();
            res.push_str("#(");
            list_out_iter(vm, &mut res, &mut v.iter().copied());
            res.push(')');
            res
        }
        Value::Map(handle) => {
            let mut res = String::new();
            res.push('{');
            for (key, val) in vm.get_map(*handle).iter() {
                res.push_str(&format!(
                    "{} {}, ",
                    key.display_value(vm),
                    val.display_value(vm)
                ));
            }
            res.push('}');
            res
        }
        Value::Pair(h) => {
            let (car, cdr) = vm.get_pair(*h);
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
        Value::List(h, start) => {
            let v = vm.get_vector(*h);
            let mut res = String::new();
            res.push('(');
            list_out_iter(vm, &mut res, &mut v[*start as usize..].iter().copied());
            res.push(')');
            res
        }
        Value::String(h) => format!("\"{}\"", vm.get_string(*h)),
        Value::Bytes(_) => "Bytes".to_string(), // XXX TODO
        Value::Value(h) => display_value(vm, vm.get_value(*h)),
    }
}

pub fn pretty_value(vm: &Vm, val: Value) -> String {
    match &val {
        Value::StringConst(i) => vm.get_interned(*i).to_string(),
        Value::CodePoint(ch) => format!("{}", ch),
        Value::CharCluster(l, c) => {
            format!("{}", String::from_utf8_lossy(&c[0..*l as usize]))
        }
        Value::CharClusterLong(_) => "Char".to_string(),
        Value::String(h) => vm.get_string(*h).to_string(),
        _ => display_value(vm, val),
    }
}

pub fn pr(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    for v in registers {
        print!("{}", pretty_value(vm, *v));
    }
    Ok(Value::Nil)
}

pub fn prn(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    for v in registers {
        print!("{}", pretty_value(vm, *v));
    }
    println!();
    Ok(Value::Nil)
}

pub fn dasm(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 1 {
        return Err(VMError::new_compile(
            "dasm: wrong number of args, expected one",
        ));
    }
    match registers[0].unref(vm) {
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
        _ => Err(VMError::new_vm("DASM: Not a callable.")),
    }
}
