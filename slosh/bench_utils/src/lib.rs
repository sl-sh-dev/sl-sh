use std::path::PathBuf;

use compile_state::state::{CompileState, SloshVm, SloshVmTrait};
use sl_compiler::pass1::pass1;
use sl_compiler::{compile, Reader};
use slosh_lib::new_slosh_vm_with_builtins;
use slvm::{
    Chunk, VMError, VMResult, Value, Vm, ADD, CONST, DIV, GET, INC, JMPLT, MUL, RET, SETCOL, VECMKD,
};
use std::sync::Arc;
use slosh_test_lib::run_reader;

pub fn run_float_bench(n: usize, m: f32) -> String {
    format!("(eval-pol {} {})", n, m)
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

pub fn load_file(fname: PathBuf, vm: &mut SloshVm) {
    match std::fs::File::open(fname) {
        Ok(file) => {
            let mut reader = Reader::from_file(file, vm, "", 1, 0);
            let _ = slosh_test_lib::run_reader(&mut reader);
        }
        Err(e) => {
            panic!("Could not open file: {:?}", e);
        }
    }
}

pub fn run_float_script(n: usize, m: f32, expected: f64) {
    let vm = &mut new_slosh_vm_with_builtins();
    let fname = get_float_benchmark();
    load_file(fname, vm);
    let mut reader = Reader::from_string(run_float_bench(n, m), vm, "", 1, 0);
    let last = run_reader(&mut reader);
    match last {
        Ok(Value::Float(f)) => {
            assert_eq!(expected as f32, f32::from(f));
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

pub fn run_bench_assert_true(n: usize, f: fn(n: usize) -> String, vm: &mut SloshVm) {
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

pub fn run_recursive_search_script(n: usize) {
    let vm = &mut new_slosh_vm_with_builtins();
    let fname = get_search_benchmark();
    load_file(fname, vm);

    run_bench_assert_true(n, run_recursive_search_bench, vm);
}

pub fn run_continuation_search_script(n: usize) {
    let vm = &mut new_slosh_vm_with_builtins();
    let fname = get_search_benchmark();
    load_file(fname, vm);

    run_bench_assert_true(n, run_continuation_search_bench, vm);
}

pub fn eval_pol(n: i32, x: f32, expected: f32) -> VMResult<()> {
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
