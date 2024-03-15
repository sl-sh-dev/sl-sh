use std::path::PathBuf;

use compile_state::state::{CompileState, SloshVm, SloshVmTrait};
use sl_compiler::pass1::pass1;
use sl_compiler::{compile, Reader};
use slvm::{
    Chunk, VMError, VMResult, Value, Vm, ADD, CONST, DIV, GET, INC, JMPLT, MUL, RET, SETCOL, VECMKD,
};
use std::sync::Arc;

// TODO PC would be nice to not have to copy load_one_expression and run_reader.
// the refactor in issue #143 should help with this as both of these functions could
// be moved to a crate all.rs and the slosh crate depend on so they can be referenced.
// ALSO. might be nice to get more clarity on their usage. Is this the best way to be using them?
// are they only used in test and if so does it make sense to use them here or should we actually be
// using something else?

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
fn eval_pol(n: i32, x: f32, expected: f32) -> VMResult<()> {
    // algorithm from http://dan.corlan.net/bench.html
    // Do a lot of loops and simple math.
    /*
    (defn eval-pol (n x)
      (let ((su 0.0) (mu 10.0) (pu 0.0)
            (pol (make-vec 100 0.0)))
        (dotimes-i i n
          (do
            (set! su 0.0)
            (dotimes-i j 100
               (do
                 (set! mu (/ (+ mu 2.0) 2.0))
                 (vec-set! pol j mu)))
            (dotimes-i j 100
              (set! su (+ (vec-nth pol j) (* su x))))
            (set! pu (+ pu su))))
        (println pu)))
             */
    let mut vm = Vm::new();
    // XXX TODO- get rid of this pause when gc fixed.
    vm.pause_gc();
    let mut chunk = Chunk::new("no_file", 1);
    let n = chunk.add_constant(n.into()) as u16;
    let x = chunk.add_constant(x.into()) as u16;
    let su = chunk.add_constant(0.0.into()) as u16;
    let mu = chunk.add_constant(10.0.into()) as u16;
    let pu = chunk.add_constant(0.0.into()) as u16;
    let zero = chunk.add_constant(0.into()) as u16;
    let zerof = chunk.add_constant(0.0.into()) as u16;
    let twof = chunk.add_constant(2.0.into()) as u16;
    let hundred = chunk.add_constant(100.into()) as u16;
    let one = chunk.add_constant(1.into()) as u16;
    let line = 1;
    chunk.encode2(CONST, 1, n, Some(line))?;
    chunk.encode2(CONST, 2, x, Some(line))?;
    chunk.encode2(CONST, 3, su, Some(line))?;
    chunk.encode2(CONST, 4, mu, Some(line))?;
    chunk.encode2(CONST, 5, pu, Some(line))?;
    chunk.encode2(CONST, 6, zero, Some(line))?; // i
    chunk.encode2(CONST, 7, zero, Some(line))?; // j
    chunk.encode2(CONST, 8, twof, Some(line))?; // 2.0
    chunk.encode2(CONST, 100, hundred, Some(line))?;
    chunk.encode2(CONST, 101, one, Some(line))?;
    chunk.encode2(CONST, 103, zerof, Some(line))?;
    chunk.encode3(VECMKD, 10, 100, 103, Some(line))?; // pols
                                                      //chunk.encode2(VECELS, 10, 100, Some(line))?;
                                                      // loop i .. n
    let jmp0_idx = chunk.add_jump(chunk.code.len() as u32);
    chunk.encode2(CONST, 3, zerof, Some(line))?;
    chunk.encode2(CONST, 7, zero, Some(line))?; // j
    let jmp1_idx = chunk.add_jump(chunk.code.len() as u32);
    // loop j .. 100
    // (set! mu (/ (+ mu 2.0) 2.0))
    chunk.encode2(ADD, 4, 8, Some(line))?;
    chunk.encode2(DIV, 4, 8, Some(line))?;
    // (vec-set! pol j mu)))
    chunk.encode3(SETCOL, 4, 10, 7, Some(line))?;

    chunk.encode2(INC, 7, 1, Some(line))?;
    chunk.encode3(JMPLT, 7, 100, jmp1_idx as u16, Some(line))?;

    chunk.encode2(CONST, 7, zero, Some(line))?; // j
    let jmp2_idx = chunk.add_jump(chunk.code.len() as u32);
    // (dotimes-i j 100 (j2)
    //   (set! su (+ (vec-nth pol j) (* su x))))
    chunk.encode2(MUL, 3, 2, Some(line))?; // FROM 3
    chunk.encode3(GET, 51, 10, 7, Some(line))?;
    chunk.encode2(ADD, 3, 51, Some(line))?;

    chunk.encode2(INC, 7, 1, Some(line))?;
    chunk.encode3(JMPLT, 7, 100, jmp2_idx as u16, Some(line))?;
    // (set! pu (+ pu su))))
    chunk.encode2(ADD, 5, 3, Some(line))?;

    chunk.encode2(INC, 6, 1, Some(line))?;
    chunk.encode3(JMPLT, 6, 1, jmp0_idx as u16, Some(line))?;

    chunk.encode0(RET, Some(line))?;
    //chunk.disassemble_chunk(&vm)?;
    //assert!(false);

    let chunk = Arc::new(chunk);
    vm.execute(chunk)?;
    let result = vm.stack(5).get_float(&vm)?;

    // NOTE: converting the result to f32 because the f64 expects more precision than our F56 can provide.
    assert_eq!(result as f32, expected);

    Ok(())
}

#[cfg(target_arch = "x86_64")]
mod instruction_count {
    use super::*;
    use iai_callgrind::{library_benchmark, library_benchmark_group, main, LibraryBenchmarkConfig};
    use std::hint::black_box;

    #[library_benchmark]
    fn optimized_float_fifty_thousand() {
        black_box(eval_pol(50000, 0.2, 125000.0));
    }

    #[library_benchmark]
    fn optimized_float_one_hundred() {
        black_box(eval_pol(100, 0.5, 400.0));
    }

    #[library_benchmark]
    fn optimized_float_one_thousand() {
        black_box(eval_pol(1000, 0.05, 2105.26315904));
    }

    #[library_benchmark]
    fn optimized_float_ten_thousand() {
        black_box(eval_pol(10000, 0.2, 25000.0));
    }

    library_benchmark_group!(
        name = float;
        config = LibraryBenchmarkConfig::default()
            .raw_callgrind_args(["--cache-sim=yes"]);
        benchmarks = optimized_float_one_hundred,
        optimized_float_one_thousand,
        optimized_float_ten_thousand,
        optimized_float_fifty_thousand,
    );

    main!(
        library_benchmark_groups = recursion_and_continuations,
        float
    );

    pub fn run_public() {
        main();
    }
}

#[cfg(not(target_arch = "x86_64"))]
mod wall_clock {
    use super::*;
    use criterion::{criterion_group, criterion_main, Criterion};

    fn criterion_optimized_float_one_hundred(c: &mut Criterion) {
        c.bench_function("optimized_float_one_hundred", |bench| {
            bench.iter(|| std::hint::black_box(eval_pol(100, 0.5, 400.0)));
        });
    }

    fn criterion_optimized_float_one_thousand(c: &mut Criterion) {
        c.bench_function("optimized_float_one_thousand", |bench| {
            bench.iter(|| std::hint::black_box(eval_pol(1000, 0.05, 2105.26315904)));
        });
    }

    fn criterion_optimized_float_ten_thousand(c: &mut Criterion) {
        c.bench_function("optimized_float_ten_thousand", |bench| {
            bench.iter(|| std::hint::black_box(eval_pol(10_000, 0.2, 25000.0)));
        });
    }

    fn criterion_optimized_float_fifty_thousand(c: &mut Criterion) {
        c.bench_function("optimized_float_fifty_thousand", |bench| {
            bench.iter(|| std::hint::black_box(eval_pol(50_000, 0.2, 125000.0)));
        });
    }

    criterion_group!(
        benches,
        criterion_optimized_float_one_hundred,
        criterion_optimized_float_one_thousand,
        criterion_optimized_float_ten_thousand,
        criterion_optimized_float_fifty_thousand,
    );

    criterion_main!(benches);

    pub fn run() {
        main();
    }
}

fn main() {
    #[cfg(target_arch = "x86_64")]
    instruction_count::run_public();

    #[cfg(not(target_arch = "x86_64"))]
    wall_clock::run();
}
