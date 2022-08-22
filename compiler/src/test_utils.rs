use crate::{compile, CompileEnvironment, CompileState, ReadError, Reader};
use slvm::*;
use std::sync::Arc;

pub fn read_test(vm: &mut Vm, text: &'static str) -> Value {
    let reader = Reader::from_string(text.to_string(), vm, "", 1, 0);
    let exps: Vec<Value> = reader.collect::<Result<Vec<Value>, ReadError>>().unwrap();
    // Don't exit early without unpausing....
    vm.pause_gc();
    let res = if exps.len() == 1 {
        exps[0]
    } else {
        vm.alloc_vector_ro(exps)
    };
    vm.unpause_gc();
    res
}

pub fn exec(vm: &mut Vm, input: &'static str) -> Value {
    let exp = read_test(vm, input);
    let mut env = CompileEnvironment::new(vm);
    let mut state = CompileState::new();
    compile(&mut env, &mut state, exp, 0).unwrap();
    state.chunk.encode0(RET, Some(1)).unwrap();
    vm.execute(Arc::new(state.chunk)).unwrap();
    vm.stack()[0]
}

pub fn exec_compile_error(vm: &mut Vm, input: &'static str) {
    let exp = read_test(vm, input);
    let mut env = CompileEnvironment::new(vm);
    let mut state = CompileState::new();
    assert!(
        compile(&mut env, &mut state, exp, 0).is_err(),
        "expected compile error"
    );
}

pub fn exec_runtime_error(vm: &mut Vm, input: &'static str) {
    let exp = read_test(vm, input);
    let mut env = CompileEnvironment::new(vm);
    let mut state = CompileState::new();
    compile(&mut env, &mut state, exp, 0).unwrap();
    state.chunk.encode0(RET, Some(1)).unwrap();
    assert!(
        vm.execute(Arc::new(state.chunk)).is_err(),
        "expected runtime error"
    );
}

pub fn assert_vals(vm: &Vm, val1: Value, val2: Value) {
    let res = vm
        .is_equal_pair(val1, val2)
        .unwrap_or(Value::False)
        .is_true();
    if !res {
        println!(
            "Value {} != {}",
            val1.display_value(vm),
            val2.display_value(vm)
        );
        println!("Debug {:?} / {:?}", val1, val2);
    }
    assert!(res);
}
