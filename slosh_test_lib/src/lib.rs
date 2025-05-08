use compile_state::state::{self, SloshVm};
use sl_compiler::load_eval;
use sl_compiler::Reader;
use slvm::{VMError, VMResult};

pub mod docs;

pub fn new_slosh_vm_with_builtins_and_core() -> SloshVm {
    let mut env = state::new_slosh_vm();

    vm_with_builtins_and_core(&mut env, false);

    env
}

pub fn vm_with_builtins_and_core(env: &mut SloshVm, print_noop: bool) {
    docs::add_builtins(env);
    slosh_lib::set_builtins_shell_print_noop(env, print_noop);
    bridge_adapters::add_builtin(
        env,
        "version",
        fake_version,
        r#"Return the software version string."#,
    );
    slosh_lib::load_core(env);
    slosh_lib::load_color(env);
}

pub fn add_user_builtins(env: &mut SloshVm, load_paths: &[String], files_to_load: &[String]) {
    let code = r#"(do (load "core.slosh") (load "sh-color.slosh"))"#.to_string();
    let mut reader = Reader::from_string(code, env, "", 1, 0);
    _ = load_eval::run_reader(&mut reader).expect("should be able to run this code.");

    let load_paths: Vec<&str> = load_paths.iter().map(AsRef::as_ref).collect();
    slosh_lib::set_initial_load_path(env, load_paths);
    for script in files_to_load {
        let script = env.intern(script);
        let script = env.get_interned(script);
        let _ = load_eval::load_internal(env, script);
    }
}

fn fake_version(vm: &mut SloshVm, registers: &[slvm::Value]) -> VMResult<slvm::Value> {
    if !registers.is_empty() {
        return Err(VMError::new_compile("version: requires no argument"));
    }
    Ok(vm.alloc_string("fake-book".to_string()))
}
