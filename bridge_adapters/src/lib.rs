use compile_state::state::{CompileEnvironment, SloshVm, SloshVmTrait};
use slvm::CallFuncSig;

pub mod lisp_adapters;

pub fn add_builtin(
    env: &mut SloshVm,
    name: &str,
    func: CallFuncSig<CompileEnvironment>,
    doc_string: &str,
) {
    let si = env.set_global_builtin(name, func);
    let key = env.intern("doc-string");
    let s = env.alloc_string(doc_string.to_string());
    env.set_global_property(si, key, s);
}