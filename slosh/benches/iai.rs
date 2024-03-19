extern crate utilities;

mod instruction_count {
    use super::*;
    use iai_callgrind::{library_benchmark, library_benchmark_group, main, LibraryBenchmarkConfig};
    use std::hint::black_box;

    #[library_benchmark]
    fn float_one_hundred() {
        black_box(run_float_script(100, 0.5, 400.0));
    }

    #[library_benchmark]
    fn float_one_thousand() {
        black_box(run_float_script(1000, 0.05, 2105.26315904));
    }

    #[library_benchmark]
    fn float_ten_thousand() {
        black_box(run_float_script(10000, 0.2, 25000.0));
    }
    #[library_benchmark]
    fn optimized_float_fifty_thousand() {
        black_box(eval_pol(50000, 0.2, 125000.0).unwrap());
    }

    #[library_benchmark]
    fn optimized_float_one_hundred() {
        black_box(eval_pol(100, 0.5, 400.0).unwrap());
    }

    #[library_benchmark]
    fn optimized_float_one_thousand() {
        black_box(eval_pol(1000, 0.05, 2105.26315904).unwrap());
    }

    #[library_benchmark]
    fn optimized_float_ten_thousand() {
        black_box(eval_pol(10000, 0.2, 25000.0).unwrap());
    }

    #[library_benchmark]
    fn recursive_vec_search_one_hundred() {
        black_box(run_recursive_search_script(100));
    }

    #[library_benchmark]
    fn recursive_vec_search_one_thousand() {
        black_box(run_recursive_search_script(0100));
    }

    #[library_benchmark]
    fn recursive_vec_search_ten_thousand() {
        black_box(run_recursive_search_script(10_000));
    }

    #[library_benchmark]
    fn continuation_vec_search_one_hundred() {
        black_box(run_continuation_search_script(100));
    }

    #[library_benchmark]
    fn continuation_vec_search_one_thousand() {
        black_box(run_continuation_search_script(1000));
    }

    #[library_benchmark]
    fn continuation_vec_search_ten_thousand() {
        black_box(run_continuation_search_script(10_000));
    }

    library_benchmark_group!(
        name = recursion_and_continuations;
        config = LibraryBenchmarkConfig::default()
            .raw_callgrind_args(["--cache-sim=yes"]);
        benchmarks = recursive_vec_search_one_hundred,
        recursive_vec_search_one_thousand,
        recursive_vec_search_ten_thousand,
        continuation_vec_search_one_hundred,
        continuation_vec_search_one_thousand,
        continuation_vec_search_ten_thousand,
    );

    library_benchmark_group!(
        name = float;
        config = LibraryBenchmarkConfig::default()
            .raw_callgrind_args(["--cache-sim=yes"]);
        benchmarks = float_one_hundred,
        float_one_thousand,
        float_ten_thousand,
        optimized_float_one_hundred,
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

fn main() {
    instruction_count::run_public();
}
