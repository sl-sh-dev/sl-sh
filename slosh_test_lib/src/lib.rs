use compile_state::state::{new_slosh_vm, CompileState, SloshVm, SloshVmTrait};
use sl_compiler::pass1::pass1;
use sl_compiler::{compile, Reader};
use slvm::{Chunk, VMError, VMResult, Value, RET};
use std::sync::Arc;
use bridge_adapters::add_builtin;
use slosh_lib::{set_builtins, set_builtins_shell};

pub mod docs;

/// TODO sls - discussion - are run_reader and load_one_expression acceptable clones of their counterparts?
///     are they only used in test and if so does it make sense to use them here
///     or should we actually be using something else?
///     with namespaces and def, is it possible each test should be running in it's own VM?
pub fn run_reader(reader: &mut Reader) -> VMResult<Value> {
    let mut last = Value::False;
    while let Some(exp) = reader.next() {
        let reader_vm = reader.vm();
        let exp = exp.map_err(|e| VMError::new("read", e.to_string()))?;
        reader_vm.heap_sticky(exp);

        let result = load_one_expression(reader_vm, exp, "", None);

        reader_vm.heap_unsticky(exp);
        let (chunk, _new_doc_string) = result?;
        last = reader_vm.execute(chunk)?;
    }
    Ok(last)
}

pub fn new_slosh_vm_with_doc_builtins_and_core() -> SloshVm {
    let mut env = new_slosh_vm();
    docs::add_builtins(&mut env);

    // TODO PC is the pause necessary still?
    env.pause_gc();
    set_builtins_shell(&mut env);
    add_builtin(
        &mut env,
        "version",
        fake_version,
        r#"Return the software version string."#,
    );
    slosh_lib::load_core(&mut env);
    slosh_lib::load_color(&mut env);
    // TODO PC is this possible?
    //load_sloshrc(&mut env);
    env.unpause_gc();
    env
}

fn new_slosh_vm_with_builtins_and_core_slim(env: &mut SloshVm) {
    env.pause_gc();
    //slosh_lib::add_shell_builtins(env);
    set_builtins(env);
    //set_builtins_shell(env);
    bridge_adapters::add_builtin(
        env,
        "version",
        fake_version,
        r#"Return the software version string."#,
    );
    //load_core(env);
    //load_sloshrc(env);

    {
        let mut reader = Reader::from_string(
            r#"(do
                        (load "core.slosh")
                        ;;(load "sh-color.slosh")
                        )"#
                .to_string(),
            env,
            "",
            1,
            0,
        );
        _ = run_reader(&mut reader);
    }

    env.unpause_gc();
}


fn fake_version(vm: &mut SloshVm, registers: &[slvm::Value]) -> VMResult<slvm::Value> {
    if !registers.is_empty() {
        return Err(VMError::new_compile("version: requires no argument"));
    }
    Ok(vm.alloc_string("fake-book".to_string()))
}

pub fn load_one_expression(
    vm: &mut SloshVm,
    exp: Value,
    name: &'static str,
    doc_string: Option<Value>,
) -> VMResult<(Arc<Chunk>, Option<Value>)> {
    let line_num = vm.line_num();
    let mut state = CompileState::new_state(name, line_num, None);
    state.chunk.dbg_args = Some(Vec::new());
    state.doc_string = doc_string;
    if let Err(e) = pass1(vm, &mut state, exp) {
        println!(
            "Compile error (pass one), {}, line {}: {}",
            name,
            vm.line_num(),
            e
        );
        return Err(e);
    }
    if let Err(e) = compile(vm, &mut state, exp, 0) {
        println!(
            "Compile error, {} line {}: {} exp: {}",
            name,
            vm.line_num(),
            e,
            exp.display_value(vm)
        );
        return Err(e);
    }
    if let Err(e) = state.chunk.encode0(RET, vm.own_line()) {
        println!("Compile error, {} line {}: {}", name, vm.line_num(), e);
        return Err(e);
    }
    state.chunk.extra_regs = state.max_regs;
    Ok((Arc::new(state.chunk), state.doc_string))
}
