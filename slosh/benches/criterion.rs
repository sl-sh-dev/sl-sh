extern crate utilities;

mod wall_clock {
    use super::*;
    use criterion::{criterion_group, criterion_main, Criterion};

    fn criterion_optimized_float_one_hundred(c: &mut Criterion) {
        c.bench_function("optimized_float_one_hundred", |bench| {
            bench.iter(|| std::hint::black_box(eval_pol(100, 0.5, 400.0).unwrap()));
        });
    }

    fn criterion_optimized_float_one_thousand(c: &mut Criterion) {
        c.bench_function("optimized_float_one_thousand", |bench| {
            bench.iter(|| std::hint::black_box(eval_pol(1000, 0.05, 2105.26315904).unwrap()));
        });
    }

    fn criterion_optimized_float_ten_thousand(c: &mut Criterion) {
        c.bench_function("optimized_float_ten_thousand", |bench| {
            bench.iter(|| std::hint::black_box(eval_pol(10_000, 0.2, 25000.0).unwrap()));
        });
    }

    fn criterion_optimized_float_fifty_thousand(c: &mut Criterion) {
        c.bench_function("optimized_float_fifty_thousand", |bench| {
            bench.iter(|| std::hint::black_box(eval_pol(50_000, 0.2, 125000.0).unwrap()));
        });
    }

    fn criterion_float_one_hundred(c: &mut Criterion) {
        c.bench_function("float_one_hundred", |bench| {
            bench.iter(|| std::hint::black_box(run_float_script(100, 0.5, 400.0)));
        });
    }

    fn criterion_float_one_thousand(c: &mut Criterion) {
        c.bench_function("float_one_thousand", |bench| {
            bench.iter(|| std::hint::black_box(run_float_script(1000, 0.05, 2105.26315904)));
        });
    }

    fn criterion_float_ten_thousand(c: &mut Criterion) {
        c.bench_function("float_ten_thousand", |bench| {
            bench.iter(|| std::hint::black_box(run_float_script(10_000, 0.2, 25000.0)));
        });
    }

    fn criterion_recursive_vec_search_one_hundred(c: &mut Criterion) {
        c.bench_function("recursive_vec_search_one_hundred", |bench| {
            bench.iter(|| std::hint::black_box(run_recursive_search_script(100)));
        });
    }

    fn criterion_recursive_vec_search_one_thousand(c: &mut Criterion) {
        c.bench_function("recursive_vec_search_one_thousand", |bench| {
            bench.iter(|| std::hint::black_box(run_recursive_search_script(1000)));
        });
    }

    fn criterion_recursive_vec_search_ten_thousand(c: &mut Criterion) {
        c.bench_function("recursive_vec_search_ten_thousand", |bench| {
            bench.iter(|| std::hint::black_box(run_recursive_search_script(10_000)));
        });
    }

    fn criterion_continuation_vec_search_one_hundred(c: &mut Criterion) {
        c.bench_function("continuation_vec_search_one_hundred", |bench| {
            bench.iter(|| std::hint::black_box(run_continuation_search_script(100)));
        });
    }

    fn criterion_continuation_vec_search_one_thousand(c: &mut Criterion) {
        c.bench_function("continuation_vec_search_one_thousand", |bench| {
            bench.iter(|| std::hint::black_box(run_continuation_search_script(1000)));
        });
    }

    fn criterion_continuation_vec_search_ten_thousand(c: &mut Criterion) {
        c.bench_function("continuation_vec_search_ten_thousand", |bench| {
            bench.iter(|| std::hint::black_box(run_continuation_search_script(10_000)));
        });
    }

    criterion_group!(
        benches,
        criterion_optimized_float_one_hundred,
        criterion_optimized_float_one_thousand,
        criterion_optimized_float_ten_thousand,
        criterion_optimized_float_fifty_thousand,
        criterion_float_one_hundred,
        criterion_float_one_thousand,
        criterion_float_ten_thousand,
        criterion_continuation_vec_search_one_hundred,
        criterion_recursive_vec_search_one_hundred,
        criterion_continuation_vec_search_one_thousand,
        criterion_recursive_vec_search_one_thousand,
        criterion_continuation_vec_search_ten_thousand,
        criterion_recursive_vec_search_ten_thousand,
    );

    criterion_main!(benches);

    pub fn run() {
        main();
    }
}

fn main() {
    wall_clock::run();
}
