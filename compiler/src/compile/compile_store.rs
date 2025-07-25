use crate::{CompileState, SloshVm, compile};
use compile_state::state::SloshVmTrait;
use slvm::*;

fn get_global_with_ns(env: &mut SloshVm, si: Interned) -> u32 {
    if env.env().get_namespace().name().is_empty() {
        env.get_reserve_global(si)
    } else {
        let mut ns = env.env().get_namespace().name().to_string();
        ns.push_str("::");
        ns.push_str(env.get_interned(si));
        let i = env.intern(&ns);
        env.get_reserve_global(i)
    }
}

pub(crate) fn compile_def(
    env: &mut SloshVm,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
) -> VMResult<()> {
    match (cdr.len(), cdr.first()) {
        (_, None) => return Err(VMError::new_compile("def: expected symbol")),
        (1, Some(Value::Symbol(si))) => {
            // 'def symbol' predeclares a symbol to be used later, no bytecode.
            let si_const = get_global_with_ns(env, *si);
            if let Some(doc_string) = state.doc_string {
                let key = env.intern("doc-string");
                env.set_global_property(si_const, key, doc_string);
                state.doc_string = None;
            }
        }
        (2, Some(Value::Symbol(si))) => {
            let si_const = get_global_with_ns(env, *si);
            if let Some(doc_string) = state.doc_string {
                let key = env.intern("doc-string");
                env.set_global_property(si_const, key, doc_string);
                state.doc_string = None;
            }
            compile(env, state, cdr[1], result)?;
            state
                .chunk
                .encode_def(result as u16, si_const, env.own_line(), false)?;
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
                compile(env, state, cdr[1], result)?;
                state
                    .chunk
                    .encode_def(result as u16, si_const, env.own_line(), false)?;
            } else {
                let sym = env.get_interned(si);
                return Err(VMError::new_compile(format!(
                    "Symbol {sym} not defined (maybe you need to use 'def {sym}' to pre-declare it)."
                )));
            }
        } else {
            match cdr[0].get_pair(env) {
                Some((Value::Symbol(i), ncdr)) if i == env.specials().get => {
                    if cdr.len() != 2 {
                        return Err(VMError::new_compile(
                            "Wrong number of arguments, expected two.",
                        ));
                    }
                    compile(env, state, cdr[1], result)?;
                    let mut cdr_i = ncdr.iter(env);
                    let collection = cdr_i.next().expect("had two elements!");
                    let field = cdr_i.next().expect("had two elements!");
                    drop(cdr_i);
                    compile(env, state, collection, result + 1)?;
                    compile(env, state, field, result + 2)?;
                    state.chunk.encode3(
                        SETCOL,
                        result as u16,
                        (result + 1) as u16,
                        (result + 2) as u16,
                        env.own_line(),
                    )?;
                }
                _ => return Err(VMError::new_compile("set!: expected symbol")),
            }
        }
    } else {
        return Err(VMError::new_compile("set!: malformed"));
    }
    Ok(())
}
