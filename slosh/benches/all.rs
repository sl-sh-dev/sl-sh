use std::path::PathBuf;

use compile_state::state::{new_slosh_vm, CompileState, SloshVm, SloshVmTrait};
use sl_compiler::pass1::pass1;
use sl_compiler::{compile, Reader};
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

pub fn run_bench(n: usize, m: f32) -> String {
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

fn run_float_script(n: usize, m: f32, expected: f64) {
    let vm = &mut new_slosh_vm();
    let fname = get_float_benchmark();
    match std::fs::File::open(&fname) {
        Ok(file) => {
            let mut reader = Reader::from_file(file, vm, "", 1, 0);
            let _ = run_reader(&mut reader);
        }
        Err(e) => {
            panic!("Could not open file: {:?}", e);
        }
    }
    let mut reader = Reader::from_string(run_bench(n, m), vm, "", 1, 0);
    let last = run_reader(&mut reader);
    match last {
        Ok(Value::Float(f)) => {
            assert_eq!(f.to_string(), expected.to_string());
            // assert_eq!(f64::from(f), expected);
        }
        _ => {
            panic!("Not a float");
        }
    }
}

#[cfg(target_arch = "x86_64")]
mod instruction_count {
    use super::*;
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

    main!(float_one_hundred, float_one_thousand, float_ten_thousand,);

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

    criterion_group!(
        benches,
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
