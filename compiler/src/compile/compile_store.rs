use crate::{compile, CompileState, SloshVm};
use compile_state::state::SloshVmTrait;
use slvm::*;

pub(crate) fn compile_def(
    env: &mut SloshVm,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
) -> VMResult<()> {
    match (cdr.len(), cdr.get(0)) {
        (_, None) => return Err(VMError::new_compile("def: expected symbol")),
        (1, Some(Value::Symbol(si))) => {
            // 'def symbol' predeclares a symbol to be used later, no bytecode.
            let si_const = env.get_reserve_global(*si);
            if let Some(doc_string) = state.doc_string {
                let key = env.intern("doc-string");
                env.set_global_property(si_const, key, doc_string);
            }
        }
        (2, Some(Value::Symbol(si))) => {
            let si_const = env.get_reserve_global(*si);
            if let Some(doc_string) = state.doc_string {
                let key = env.intern("doc-string");
                env.set_global_property(si_const, key, doc_string);
            }
            compile(env, state, cdr[1], result + 1)?;
            state
                .chunk
                .encode_refi(result as u16, si_const, env.own_line())?;
            state
                .chunk
                .encode2(DEF, result as u16, (result + 1) as u16, env.own_line())?;
        }
        _ => return Err(VMError::new_compile("def: malformed")),
    }
    Ok(())
}

pub(crate) fn compile_set(
    env: &mut SloshVm,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
) -> VMResult<()> {
    if cdr.len() == 2 {
        if let Value::Symbol(si) = cdr[0] {
            if let Some(idx) = state.get_symbol(si) {
                compile(env, state, cdr[1], result)?;
                state
                    .chunk
                    .encode2(SET, idx as u16, result as u16, env.own_line())?;
            } else if let Some(si_const) = env.global_intern_slot(si) {
                compile(env, state, cdr[1], result + 1)?;
                state
                    .chunk
                    .encode_refi(result as u16, si_const, env.own_line())?;
                state
                    .chunk
                    .encode2(DEF, result as u16, (result + 1) as u16, env.own_line())?;
            } else {
                let sym = env.get_interned(si);
                return Err(VMError::new_compile(format!("Symbol {sym} not defined (maybe you need to use 'def {sym}' to pre-declare it).")));
            }
        } else {
            return Err(VMError::new_compile("set!: expected symbol"));
        }
    } else {
        return Err(VMError::new_compile("set!: malformed"));
    }
    Ok(())
}
