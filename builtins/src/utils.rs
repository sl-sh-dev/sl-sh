use slvm::{CallFunc, CallFuncSig, Value, Vm};

pub fn add_builtin(vm: &mut Vm, name: &str, func: CallFuncSig, doc_string: &str) {
    if let Value::Global(si) = vm.set_global(name, Value::Builtin(CallFunc { func })) {
        let key = vm.intern("doc-string");
        let s = vm.alloc_string(doc_string.to_string());
        vm.set_global_property(si, key, s);
    }
}

pub fn add_docstring(vm: &mut Vm, name: &str, doc_string: &str) {
    if let Value::Global(si) = vm.set_global(name, Value::Undefined) {
        let key = vm.intern("doc-string");
        let s = vm.alloc_string(doc_string.to_string());
        vm.set_global_property(si, key, s);
    }
}
