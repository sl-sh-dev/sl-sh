#[macro_use]
extern crate bencher;

use crate::load_one_expression;
use bencher::{black_box, Bencher};
use compile_state::state::{new_slosh_vm, SloshVm};
use shell::glob::{expand_glob, GlobOutput};
use sl_compiler::Reader;
use slvm::{VMError, Value};
use std::path::PathBuf;

fn glob_to_vec(pat: impl Into<PathBuf>) -> Vec<PathBuf> {
    match expand_glob(pat) {
        GlobOutput::Arg(p) => {
            vec![p]
        }
        GlobOutput::Args(ps) => ps,
    }
}
pub fn get_glob_from_dir(dir: PathBuf, glob: &str) -> Vec<PathBuf> {
    let test_glob = dir.join(glob);
    glob_to_vec(test_glob)
}

pub fn get_tests_directory() -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests");
    path
}

pub fn get_globs_from_test_directory(glob: &str) -> Vec<PathBuf> {
    let tests = get_tests_directory();
    get_glob_from_dir(tests, glob)
}

pub const FLOAT_BENCH: &'static str = r#"
(defn eval-pol (n x)
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
    pu))
    (eval-pol 100 0.5)
"#;

fn run_float_script() {
    let scripts = get_globs_from_test_directory("*.benchmark");
    for benchmark in scripts {
        let vm = &mut new_slosh_vm();
        let file = std::fs::File::open(benchmark).expect("benchmark file not found");
        let mut reader = Reader::from_file(file, vm, "", 1, 0);
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
            last = reader_vm.execute(chunk).unwrap();
        }
        println!("Value: {:?}", last);
    }
}

fn float(bench: &mut Bencher) {
    bench.iter(|| black_box(run_float_script()));
}

benchmark_group!(benches, float);
benchmark_main!(benches);
