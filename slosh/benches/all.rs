#[cfg(not(target_arch = "x86_64"))]
use criterion::{criterion_group, criterion_main};
#[cfg(target_arch = "x86_64")]
use iai::main;

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

pub const FLOAT_BENCH: &'static str = r#"
(def defmacro
  (macro (name args & body)
      `(def ~name (macro ~args ~@body))))
(defmacro dotimes-i
    (idx-bind times & body)
    `(let (~idx-bind 0)
        (while (< ~idx-bind ~times)
            ~@body
            (inc! ~idx-bind))))
(def eval-pol (fn (n x)
  (let (su 0.0
        mu 10.0
        pu 0.0
        pol (make-vec 100 0.0))
    (dotimes-i i n
        (set! su 0.0)
        (dotimes-i j 100
             (set! mu (/ (+ mu 2.0) 2.0))
             (set! pol.~j mu))
        (dotimes-i j 100
          (set! su (+ pol.~j (* su x))))
            (set! pu (+ pu su)))
    pu)))
"#;

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

fn run_float_script(n: usize, m: f32, expected: f32) {
    let vm = &mut new_slosh_vm();
    let mut reader = Reader::from_static_string(FLOAT_BENCH, vm, "", 1, 0);
    _ = run_reader(&mut reader);

    let mut reader = Reader::from_string(run_bench(n, m), vm, "", 1, 0);
    let last = run_reader(&mut reader);
    match last {
        Ok(Value::Float(f)) => {
            assert_eq!(f.0, expected);
        }
        _ => {
            panic!("Not a float");
        }
    }
}

#[cfg(target_arch = "x86_64")]
mod instruction_count {
    use super::*;
    use iai::black_box;

    fn float_one_hundred() {
        black_box(run_float_script(100, 0.5, 400.0));
    }

    fn float_one_thousand() {
        black_box(run_float_script(1000, 0.05, 2105.2483));
    }

    fn float_ten_thousand() {
        black_box(run_float_script(10_000, 0.2, 25000.0));
    }
}

#[cfg(target_arch = "x86_64")]
main!(
    instruction_count::float_one_hundred,
    instruction_count::float_one_thousand
    instruction_count::float_ten_thousand
);

#[cfg(not(target_arch = "x86_64"))]
mod wall_clock {
    use super::*;
    use criterion::Criterion;

    pub fn float_one_hundred(c: &mut Criterion) {
        c.bench_function("float_ten", |bench| {
            bench.iter(|| std::hint::black_box(run_float_script(100, 0.5, 400.0)));
        });
    }

    pub fn float_one_thousand(c: &mut Criterion) {
        c.bench_function("float_one_thousand", |bench| {
            bench.iter(|| std::hint::black_box(run_float_script(1000, 0.05, 2105.2483)));
        });
    }

    pub fn float_ten_thousand(c: &mut Criterion) {
        c.bench_function("float_one_hundred", |bench| {
            bench.iter(|| std::hint::black_box(run_float_script(10_000, 0.2, 25000.0)));
        });
    }
}

#[cfg(not(target_arch = "x86_64"))]
criterion_group!(
    benches,
    wall_clock::float_one_hundred,
    wall_clock::float_one_thousand,
    wall_clock::float_ten_thousand,
);
#[cfg(not(target_arch = "x86_64"))]
criterion_main!(benches);
