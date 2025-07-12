use builtins::{noop_swap_internal, NoopSwap};
use compile_state::state::SloshVm;
use sl_compiler::load_eval;
use slosh_lib::load_builtins_lisp_less_sloshrc;
use slvm::VMResult;

pub mod docs;

/// pr/prn/dasm/dump-globals which write directly to stdout are handed new behavior according
/// to variable: [`NoopSwap`].
pub fn do_to_vm_stdout_fcns(env: &mut SloshVm, noop_swap_type: NoopSwap) {
    let _ = noop_swap_internal(env, "pr".to_string(), noop_swap_type);
    let _ = noop_swap_internal(env, "prn".to_string(), noop_swap_type);
    let _ = noop_swap_internal(env, "dasm".to_string(), noop_swap_type);
    let _ = noop_swap_internal(env, "dump-globals".to_string(), noop_swap_type);
}

/// pr/prn/dasm/dump-globals which write directly to stdout are mapped to noop
pub fn vm_with_stdout_disabled(env: &mut SloshVm) {
    do_to_vm_stdout_fcns(env, NoopSwap::MakeNoop);
}

/// pr/prn/dasm/dump-globals which write directly to stdout are not mapped to noop
pub fn vm_with_stdout_enabled(env: &mut SloshVm) {
    do_to_vm_stdout_fcns(env, NoopSwap::MakeNotNoop);
}

/// If noop_stdout is set to true then all functions that write to stdout
/// (pr/prn/dasm/dump-globals) will be overwritten with the noop function.
pub fn vm_with_builtins_and_core(env: &mut SloshVm, noop_stdout: bool) -> VMResult<()> {
    docs::add_builtins(env);
    slosh_lib::set_builtins(env);
    if noop_stdout {
        // must be called at this point. before set_shell_builtins. because
        // set_shell_builtins calls set_environment which loads the sloshrc
        // which is allowed to write to stdout and is something that should *NOT*
        // happen while this VM is being used.
        vm_with_stdout_disabled(env);
    }
    slosh_lib::set_shell_builtins(env);
    slosh_lib::load_builtins_lisp_less_sloshrc(env)
}

pub fn add_user_builtins(env: &mut SloshVm, load_paths: &[String], files_to_load: &[String]) {
    let _ = load_builtins_lisp_less_sloshrc(env);

    let load_paths: Vec<&str> = load_paths.iter().map(AsRef::as_ref).collect();
    slosh_lib::set_initial_load_path(env, load_paths);
    for script in files_to_load {
        let script = env.intern(script);
        let script = env.get_interned(script);
        let _ = load_eval::load_internal(env, script);
    }
}
