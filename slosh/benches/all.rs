use std::path::PathBuf;

use compile_state::state::{CompileState, SloshVm, SloshVmTrait};
use sl_compiler::pass1::pass1;
use sl_compiler::{compile, new_slosh_vm_with_builtins, Reader};
use slvm::{Chunk, VMError, VMResult, Value, RET};
use std::sync::Arc;

fn load_one_expression(
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

pub fn run_float_bench(n: usize, m: f32) -> String {
    format!("(eval-pol {} {})", n, m)
}

pub fn run_reader(reader: &mut Reader) -> VMResult<Value> {
    let mut last = Value::False;
    while let Some(exp) = reader.next() {
        let reader_vm = reader.vm();
        let exp = exp
            .map_err(|e| VMError::new("read", e.to_string()))
            .unwrap();
        reader_vm.heap_sticky(exp);

        let result = load_one_expression(reader_vm, exp, "", None);

        reader_vm.heap_unsticky(exp);
        let (chunk, _new_doc_string) = result.unwrap();
        last = reader_vm.execute(chunk)?;
    }
    Ok(last)
}

/// returns ./slosh/benches/ PathBuf
pub fn get_benches_directory() -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("benches");
    path
}

pub fn get_float_benchmark() -> PathBuf {
    get_benches_directory().join("float-bench.slosh")
}

fn load_file(fname: PathBuf, vm: &mut SloshVm) {
    match std::fs::File::open(&fname) {
        Ok(file) => {
            let mut reader = Reader::from_file(file, vm, "", 1, 0);
            let _ = run_reader(&mut reader);
        }
        Err(e) => {
            panic!("Could not open file: {:?}", e);
        }
    }
}

fn run_float_script(n: usize, m: f32, expected: f64) {
    let vm = &mut new_slosh_vm_with_builtins();
    let fname = get_float_benchmark();
    load_file(fname, vm);
    let mut reader = Reader::from_string(run_float_bench(n, m), vm, "", 1, 0);
    let last = run_reader(&mut reader);
    match last {
        Ok(Value::Float(f)) => {
            assert_eq!(f64::from(f), expected);
        }
        _ => {
            panic!("Not a float");
        }
    }
}

pub fn get_search_benchmark() -> PathBuf {
    get_benches_directory().join("vec-search-bench.slosh")
}

pub fn run_recursive_search_bench(n: usize) -> String {
    format!("(run-recursive {})", n)
}

pub fn run_continuation_search_bench(n: usize) -> String {
    format!("(run-continuation {})", n)
}

fn run_bench_assert_true(n: usize, f: fn(n: usize) -> String, vm: &mut SloshVm) {
    let mut reader = Reader::from_string(f(n), vm, "", 1, 0);
    let last = run_reader(&mut reader);
    match last {
        Ok(v) => {
            assert_eq!(Value::True, v);
        }
        _ => {
            panic!("Not true");
        }
    }
}

fn run_recursive_search_script(n: usize) {
    let vm = &mut new_slosh_vm_with_builtins();
    let fname = get_search_benchmark();
    load_file(fname, vm);

    run_bench_assert_true(n, run_recursive_search_bench, vm);
}

fn run_continuation_search_script(n: usize) {
    let vm = &mut new_slosh_vm_with_builtins();
    let fname = get_search_benchmark();
    load_file(fname, vm);

    run_bench_assert_true(n, run_continuation_search_bench, vm);
}

#[cfg(target_arch = "x86_64")]
mod instruction_count {
    use super::*;
    use criterion::Criterion;
    use iai::{black_box, main};

    fn float_one_hundred() {
        black_box(run_float_script(100, 0.5, 399.99999999958527));
    }

    fn float_one_thousand() {
        black_box(run_float_script(1000, 0.05, 2105.2631578924484));
    }

    fn float_ten_thousand() {
        black_box(run_float_script(10_000, 0.2, 24999.999999998603));
    }

    fn recursive_vec_search_one_hundred() {
        black_box(run_recursive_search_script(100));
    }

    fn recursive_vec_search_one_thousand() {
        black_box(run_recursive_search_script(0100));
    }

    fn recursive_vec_search_ten_thousand() {
        bench.iter(|| std::hint::black_box(run_recursive_search_script(10_000)));
    }

    fn continuation_vec_search_one_hundred() {
        black_box(run_continuation_search_script(100));
    }

    fn continuation_vec_search_one_thousand() {
        black_box(run_continuation_search_script(1000));
    }

    fn continuation_vec_search_ten_thousand() {
        black_box(run_continuation_search_script(10_000));
    }

    main!(
        float_one_hundred,
        float_one_thousand,
        float_ten_thousand,
        recursive_vec_search_one_hundred,
        recursive_vec_search_one_thousand,
        recursive_vec_search_ten_thousand,
        continuation_vec_search_one_hundred,
        continuation_vec_search_one_thousand,
        continuation_vec_search_ten_thousand,
    );

    pub fn run() {
        main();
    }
}

#[cfg(not(target_arch = "x86_64"))]
mod wall_clock {
    use super::*;
    use criterion::{criterion_group, criterion_main, Criterion};

    fn float_one_hundred(c: &mut Criterion) {
        c.bench_function("float_ten", |bench| {
            bench.iter(|| std::hint::black_box(run_float_script(100, 0.5, 399.99999999958527)));
        });
    }

    fn float_one_thousand(c: &mut Criterion) {
        c.bench_function("float_one_thousand", |bench| {
            bench.iter(|| std::hint::black_box(run_float_script(1000, 0.05, 2105.2631578924484)));
        });
    }

    fn float_ten_thousand(c: &mut Criterion) {
        c.bench_function("float_one_hundred", |bench| {
            bench.iter(|| std::hint::black_box(run_float_script(10_000, 0.2, 24999.999999998603)));
        });
    }

    fn recursive_vec_search_one_hundred(c: &mut Criterion) {
        c.bench_function("recursive_vec_search_one_hundred", |bench| {
            bench.iter(|| std::hint::black_box(run_recursive_search_script(100)));
        });
    }

    fn recursive_vec_search_one_thousand(c: &mut Criterion) {
        c.bench_function("recursive_vec_search_one_thousand", |bench| {
            bench.iter(|| std::hint::black_box(run_recursive_search_script(1000)));
        });
    }

    fn recursive_vec_search_ten_thousand(c: &mut Criterion) {
        c.bench_function("recursive_vec_search_ten_thousand", |bench| {
            bench.iter(|| std::hint::black_box(run_recursive_search_script(10_000)));
        });
    }

    fn continuation_vec_search_one_hundred(c: &mut Criterion) {
        c.bench_function("continuation_vec_search_one_hundred", |bench| {
            bench.iter(|| std::hint::black_box(run_continuation_search_script(100)));
        });
    }

    fn continuation_vec_search_one_thousand(c: &mut Criterion) {
        c.bench_function("continuation_vec_search_one_thousand", |bench| {
            bench.iter(|| std::hint::black_box(run_continuation_search_script(1000)));
        });
    }

    fn continuation_vec_search_ten_thousand(c: &mut Criterion) {
        c.bench_function("continuation_vec_search_ten_thousand", |bench| {
            bench.iter(|| std::hint::black_box(run_continuation_search_script(10_000)));
        });
    }

    criterion_group!(
        benches,
        continuation_vec_search_one_thousand,
        recursive_vec_search_ten_thousand,
        continuation_vec_search_ten_thousand,
        recursive_vec_search_one_hundred,
        continuation_vec_search_one_hundred,
        float_one_hundred,
        float_one_thousand,
        float_ten_thousand,
    );

    criterion_main!(benches);

    pub fn run() {
        main();
    }
}

fn main() {
    #[cfg(target_arch = "x86_64")]
    instruction_count::run();

    #[cfg(not(target_arch = "x86_64"))]
    wall_clock::run();
}
