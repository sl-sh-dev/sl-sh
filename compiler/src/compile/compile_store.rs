use crate::{compile, CompileEnvironment, CompileState};
use slvm::*;

pub(crate) fn compile_def(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
) -> VMResult<()> {
    match (cdr.len(), cdr.get(0)) {
        (_, None) => return Err(VMError::new_compile("def: expected symbol")),
        (1, Some(Value::Symbol(si))) => {
            // 'def symbol' predeclares a symbol to be used later, no bytecode.
            let _ = env.vm_mut().reserve_index(*si);
        }
        (2, Some(Value::Symbol(si))) => {
            compile(env, state, cdr[1], result + 1)?;
            let si_const = env.vm_mut().reserve_index(*si);
            state
                .chunk
                .encode_refi(result as u16, si_const, env.own_line())?;
            state
                .chunk
                .encode2(DEF, result as u16, (result + 1) as u16, env.own_line())?;
        }
        (3, Some(Value::Symbol(si))) => {
            let si_const = env.vm_mut().reserve_index(*si);
            // Set docstring
            let set_prop = env.vm_mut().intern("set-prop");
            if let Some(set_prop) = env.vm().global_intern_slot(set_prop) {
                let doc_const = state
                    .chunk
                    .add_constant(Value::Keyword(env.vm_mut().intern("doc-string")));
                state
                    .chunk
                    .encode_refi((result + 1) as u16, si_const, env.own_line())?;
                state.chunk.encode2(
                    CONST,
                    (result + 2) as u16,
                    doc_const as u16,
                    env.own_line(),
                )?;
                compile(env, state, cdr[1], result + 3)?;
                state
                    .chunk
                    .encode_callg(set_prop as u32, 3, result as u16, env.own_line())?;
            }

            compile(env, state, cdr[2], result + 1)?;
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
    env: &mut CompileEnvironment,
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
                    .encode2(SET, (idx + 1) as u16, result as u16, env.own_line())?;
            } else if let Some(si_const) = env.vm().global_intern_slot(si) {
                compile(env, state, cdr[1], result + 1)?;
                state
                    .chunk
                    .encode_refi(result as u16, si_const, env.own_line())?;
                state
                    .chunk
                    .encode2(DEF, result as u16, (result + 1) as u16, env.own_line())?;
            } else {
                let sym = env.vm().get_interned(si);
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
