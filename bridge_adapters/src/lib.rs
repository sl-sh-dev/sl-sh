use compile_state::state::{CompileEnvironment, SloshVm, SloshVmTrait};
use slvm::{CallFuncSig, Value, VMError, VMResult};
use std::error::Error;
use std::fmt::{Display, Formatter};

pub mod lisp_adapters;

pub type BridgeResult<T> = Result<T, BridgeError>;

#[derive(Debug, Clone)]
pub enum BridgeError {
    Error(String),
}

impl Error for BridgeError {}

impl BridgeError {
    pub fn with_fn<T>(br: BridgeResult<T>, fn_name: impl AsRef<str>) -> VMResult<T> {
        br.map_err(|e| VMError::new_conversion(format!("{}: {}", fn_name.as_ref(), e)))
    }
}

impl Display for BridgeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BridgeError::Error(s) => write!(f, "{s}"),
        }
    }
}

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

pub fn add_named_global_doc(env: &mut SloshVm, string: &str, f_val: Value, doc_string: &str) {
    let si = env.set_named_global(string, f_val);
    let key = env.intern("doc-string");
    let s = env.alloc_string(doc_string.to_string());
    env.set_global_property(si, key, s);
}
