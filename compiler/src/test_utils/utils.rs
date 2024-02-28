#![allow(unused)]

// This is a utility file and is used as a regular module but also as a module
// imported via the [`path`] attribute in other crates in this workspace, therefore,
// its imports may be different based on which crate it's being imported into.
//
// As a result the convention is to have this test utilities file declare
// `use super::*;` and it is up to the parent module to provide all necessary
// dependencies.
use super::*;

/// Read text for a test.  Will convert multiple forms into a vector of Values.
pub fn read_test<T>(vm: &mut SloshVm, text: T) -> Value
where
    T: AsRef<str> + 'static,
{
    let reader = Reader::from_string(text.as_ref().to_string(), vm, "", 1, 0);
    let exps: Vec<Value> = reader.collect::<Result<Vec<Value>, ReadError>>().unwrap();
    // Don't exit early without unpausing....
    vm.pause_gc();
    let res = if exps.len() == 1 {
        exps[0]
    } else {
        vm.alloc_vector_ro(exps)
    };
    vm.unpause_gc();
    // Make sure we don't GC this stuff.  Don't bother with unsticky since we are testing (not the GC).
    vm.heap_sticky(res);
    res
}

/// Read input, compile and execute the result and return the Value this produces.
pub fn exec<T>(env: &mut SloshVm, input: T) -> Value
where
    T: AsRef<str> + 'static,
{
    let exp = read_test(env, input);
    let mut state = CompileState::new();
    if let Value::Vector(_) = exp {
        env.pause_gc();
        for e in exp.iter(env).collect::<Vec<Value>>() {
            pass1(env, &mut state, e).unwrap();
            compile(env, &mut state, e, 0).unwrap();
        }
        state.chunk.encode0(RET, Some(1)).unwrap();
        let chunk = Arc::new(state.chunk);
        env.unpause_gc();
        let c_alloc = env.alloc_lambda(chunk.clone());
        // Keep chunk from getting GCed...
        env.set_named_global("#<remember-me>", c_alloc);
        env.execute(chunk).unwrap();
    } else {
        env.pause_gc();
        pass1(env, &mut state, exp).unwrap();
        compile(env, &mut state, exp, 0).unwrap();
        state.chunk.encode0(RET, Some(1)).unwrap();
        let chunk = Arc::new(state.chunk);
        env.unpause_gc();
        let c_alloc = env.alloc_lambda(chunk.clone());
        // Keep chunk from getting GCed...
        env.set_named_global("#<remember-me>", c_alloc);
        env.execute(chunk).unwrap();
    }
    env.stack(0).unref(env)
}

/// Same as exec() but dump the registers and disassembled bytecode after executing.
/// Only use this when debugging a test, otherwise use exec().
pub fn exec_with_dump<T>(env: &mut SloshVm, input: T) -> Value
where
    T: AsRef<str> + 'static,
{
    let exp = read_test(env, input);
    let mut state = CompileState::new();
    if let Value::Vector(_) = exp {
        for e in exp.iter(env).collect::<Vec<Value>>() {
            pass1(env, &mut state, e).unwrap();
            compile(env, &mut state, e, 0).unwrap();
        }
        state.chunk.encode0(RET, Some(1)).unwrap();
        env.execute(Arc::new(state.chunk.clone())).unwrap();
    } else {
        pass1(env, &mut state, exp).unwrap();
        compile(env, &mut state, exp, 0).unwrap();
        state.chunk.encode0(RET, Some(1)).unwrap();
        env.execute(Arc::new(state.chunk.clone())).unwrap();
    }

    let mut reg_names = state.chunk.dbg_args.as_ref().map(|iargs| iargs.iter());
    for (i, r) in env.stack_slice()[0..=state.chunk.extra_regs]
        .iter()
        .enumerate()
    {
        let aname = if i == 0 {
            "params/result"
        } else if let Some(reg_names) = reg_names.as_mut() {
            if let Some(n) = reg_names.next() {
                env.get_interned(*n)
            } else {
                "[SCRATCH]"
            }
        } else {
            "[SCRATCH]"
        };
        if let Value::Value(_) = r {
            println!(
                "{:#03} ^{:#20}: {:#12} {}",
                i,
                aname,
                r.display_type(env),
                r.pretty_value(env)
            );
        } else {
            println!(
                "{:#03}  {:#20}: {:#12} {}",
                i,
                aname,
                r.display_type(env),
                r.pretty_value(env)
            );
        }
    }
    let _ = state.chunk.disassemble_chunk(env, 0);

    env.stack(0)
}

/// Read and compile input and fail if compiling does not result in an error.
pub fn exec_compile_error<T>(env: &mut SloshVm, input: T)
where
    T: AsRef<str> + 'static,
{
    let exp = read_test(env, input);
    let mut state = CompileState::new();
    assert!(
        compile(env, &mut state, exp, 0).is_err(),
        "expected compile error"
    );
    env.reset();
}

/// Read, compile and execute input and fail if execution does not result in an error.
pub fn exec_runtime_error<T>(env: &mut SloshVm, input: T)
where
    T: AsRef<str> + 'static,
{
    let exp = read_test(env, input);
    let mut state = CompileState::new();
    compile(env, &mut state, exp, 0).unwrap();
    state.chunk.encode0(RET, Some(1)).unwrap();
    assert!(
        env.execute(Arc::new(state.chunk)).is_err(),
        "expected runtime error"
    );
    env.reset();
}

/// Assert that val1 and val2 are the same.
pub fn assert_vals(vm: &SloshVm, val1: Value, val2: Value) {
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
        println!("Debug {val1:?} / {val2:?}");
    }
    assert!(res);
}
