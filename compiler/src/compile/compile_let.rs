use std::cell::RefCell;
use std::rc::Rc;

use slvm::error::*;
use slvm::opcodes::*;
use slvm::value::*;

use crate::compile::util::get_args_iter;
use crate::state::*;
use crate::{compile, CompileEnvironment};

pub(crate) fn compile_let(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
) -> VMResult<()> {
    fn inner(
        env: &mut CompileEnvironment,
        state: &mut CompileState,
        cdr: &[Value],
        result: usize,
        old_tail: bool,
    ) -> VMResult<()> {
        let start_defers = state.defers;
        let symbols = Rc::new(RefCell::new(Symbols::with_let(
            state.symbols.clone(),
            result,
        )));
        state.symbols = symbols.clone();
        let mut cdr_iter = cdr.iter();
        let args = cdr_iter.next().unwrap(); // unwrap safe, length is at least 1
        let mut opt_comps: Vec<(usize, Value)> = Vec::new();
        let scratch = env.vm_mut().intern("[SCRATCH]");
        env.set_line_val(state, *args);
        let args_iter: Vec<Value> = get_args_iter(env, *args, "let")?.collect();
        // XXX fixme
        //new_state.chunk.dbg_args = Some(Vec::new());
        for a in args_iter {
            env.set_line_val(state, a);
            let mut args_iter = get_args_iter(env, a, "let")?;
            if let Some(Value::Symbol(i)) = args_iter.next() {
                let reg = symbols.borrow_mut().insert(i) + 1;
                if let Some(dbg_args) = state.chunk.dbg_args.as_mut() {
                    if dbg_args.len() < reg - 1 {
                        dbg_args.resize(reg - 1, scratch);
                    }
                    if reg < dbg_args.len() {
                        // This register will have multiple names, maybe concat them or something.
                        dbg_args[reg] = i;
                    } else {
                        dbg_args.push(i);
                    }
                }
                if let Some(r) = args_iter.next() {
                    opt_comps.push((reg, r));
                } else {
                    opt_comps.push((reg, Value::Nil));
                }
                // XXX Check to make sure only two elements...
            }
        }
        let mut free_reg = result;
        for (reg, val) in opt_comps {
            compile(env, state, val, reg)?;
            free_reg = reg + 1;
        }
        // TODO XXX check for underflow...
        let last_thing = cdr.len() - 2;
        for (i, r) in cdr_iter.enumerate() {
            if i == last_thing {
                state.tail = old_tail;
            }
            compile(env, state, *r, free_reg)?;
        }
        if free_reg != result {
            state
                .chunk
                .encode2(MOV, result as u16, free_reg as u16, env.own_line())?;
        }
        for _ in start_defers..state.defers {
            state.chunk.encode0(DFRPOP, env.own_line())?;
        }
        Ok(())
    }

    if cdr.is_empty() {
        return Err(VMError::new_compile(
            "Too few arguments, need at least 1 got 0.",
        ));
    }
    let old_symbols = state.symbols.clone();
    let old_tail = state.tail;
    state.tail = false;
    let old_defers = state.defers;
    let result = inner(env, state, cdr, result, old_tail);
    state.tail = old_tail;
    state.symbols = old_symbols;
    state.defers = old_defers;
    result
}
