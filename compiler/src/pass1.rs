use crate::{CompileEnvironment, CompileState};
use slvm::{VMResult, Value};

pub fn pass1(env: &mut CompileEnvironment, state: &mut CompileState, exp: Value) -> VMResult<()> {
    let fn_ = env.vm_mut().intern("fn");
    let mac_ = env.vm_mut().intern("macro");
    //let def_ = env.vm().intern("def");
    match exp {
        Value::Pair(_) | Value::List(_, _) => {
            let (car, _) = exp
                .get_pair(env.vm())
                .expect("Pair/List not a Pair or List?");
            // short circuit on an fn form, will be handled with it's own state.
            if let Value::Symbol(i) = car {
                if i == fn_ || i == mac_ {
                    return Ok(());
                }
            }
            // XXX boo on this collect.
            for r in exp.iter(env.vm()).collect::<Vec<Value>>() {
                pass1(env, state, r)?;
            }
        }
        Value::Symbol(i) => {
            if state.get_symbol(i).is_none() && state.symbols.borrow().can_capture(i) {
                state.symbols.borrow_mut().insert_capture(env.vm_mut(), i);
                if let Some(dbg_args) = state.chunk.dbg_args.as_mut() {
                    dbg_args.push(i);
                }
            }
        }
        Value::True => {}
        Value::False => {}
        Value::Nil => {}
        Value::Undefined => {}
        Value::Byte(_) => {}
        Value::Int(i) if i >= 0 && i <= u16::MAX as i64 => {}
        Value::UInt(i) if i <= u16::MAX as u64 => {}

        Value::String(_) => {}
        Value::Bytes(_) => {}
        Value::Lambda(_) => {}
        Value::Closure(_) => {}
        Value::Continuation(_) => {}
        Value::CallFrame(_) => {}
        Value::Value(_) => {}

        _ => {
            state.add_constant(exp);
        }
    }
    Ok(())
}
