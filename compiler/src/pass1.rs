use crate::compile_fn::mk_state;
use crate::{CompileState, SloshVm};
use slvm::{VMResult, Value, from_i56};

pub fn pass1(env: &mut SloshVm, state: &mut CompileState, exp: Value) -> VMResult<()> {
    let fn_ = env.intern("fn");
    let mac_ = env.intern("macro");
    match exp {
        Value::Pair(_) | Value::List(_, _) => {
            let (car, cdr) = exp.get_pair(env).expect("Pair/List not a Pair or List?");
            // Do an extra pass1 on lambda's so we can get all captures upfront.
            if let Value::Symbol(i) = car
                && (i == fn_ || i == mac_)
            {
                // XXX boo on this collect.
                let cdr = cdr.iter(env).collect::<Vec<Value>>();
                if !cdr.is_empty() {
                    let (mut new_state, _, _) = mk_state(env, state, cdr[0])?;
                    for r in cdr[1..].iter() {
                        pass1(env, &mut new_state, *r)?;
                    }
                }
                return Ok(());
            }
            // XXX boo on this collect.
            for r in exp.iter(env).collect::<Vec<Value>>() {
                pass1(env, state, r)?;
            }
        }
        Value::Symbol(i) => {
            if state.get_symbol(i).is_none() && state.symbols.borrow().can_capture(i) {
                state.symbols.borrow_mut().insert_capture(env, i);
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
        Value::Int(i) => {
            let i = from_i56(&i);
            if i < 0 || i > u16::MAX as i64 {
                env.heap_immutable(exp);
                state.add_constant(exp);
            }
        }
        Value::String(_) => {}
        Value::Bytes(_) => {}
        Value::Lambda(_) => {}
        Value::Closure(_) => {}
        Value::Continuation(_) => {}
        Value::CallFrame(_) => {}
        Value::Value(_) => {}

        _ => {
            env.heap_immutable(exp);
            state.add_constant(exp);
        }
    }
    Ok(())
}
