use ::sl_sh::shell::*;

fn run_float_script_100() {
    assert_eq!(0, run_one_script("benches/float-bench-100.lisp", &[]));
}

fn run_float_script_1000() {
    assert_eq!(0, run_one_script("benches/float-bench-1000.lisp", &[]));
}

fn run_float_script_10000() {
    assert_eq!(0, run_one_script("benches/float-bench-10000.lisp", &[]));
}

#[cfg(target_arch = "x86_64")]
mod instruction_count {
    use super::*;
    use iai_callgrind::{library_benchmark, library_benchmark_group, main, LibraryBenchmarkConfig};
    use std::hint::black_box;

    #[library_benchmark]
    fn sl_sh_float_one_hundred() {
        black_box(run_float_script_100());
    }

    #[library_benchmark]
    fn sl_sh_float_one_thousand() {
        black_box(run_float_script_1000());
    }

    #[library_benchmark]
    fn sl_sh_float_ten_thousand() {
        black_box(run_float_script_10000());
    }

    library_benchmark_group!(
        name = bench_sl_sh_group;
        config = LibraryBenchmarkConfig::default()
            .raw_callgrind_args(["--cache-sim=yes"]);
        benchmarks = sl_sh_float_one_hundred,
        sl_sh_float_one_thousand,
        sl_sh_float_ten_thousand,
    );

    main!(library_benchmark_groups = bench_sl_sh_group);

    pub fn run_public() {
        main();
    }
}

#[cfg(not(target_arch = "x86_64"))]
mod wall_clock {
    use super::*;
    use criterion::{criterion_group, criterion_main, Criterion};

    fn sl_sh_float_one_hundred(c: &mut Criterion) {
        c.bench_function("sl_sh_float_one_hundred", |bench| {
            bench.iter(|| std::hint::black_box(run_float_script_100()));
        });
    }

    fn sl_sh_float_one_thousand(c: &mut Criterion) {
        c.bench_function("sl_sh_float_one_thousand", |bench| {
            bench.iter(|| std::hint::black_box(run_float_script_1000()));
        });
    }

    fn sl_sh_float_ten_thousand(c: &mut Criterion) {
        c.bench_function("sl_sh_float_ten_thousand", |bench| {
            bench.iter(|| std::hint::black_box(run_float_script_10000()));
        });
    }

    criterion_group!(
        benches,
        sl_sh_float_one_hundred,
        sl_sh_float_one_thousand,
        sl_sh_float_ten_thousand,
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
