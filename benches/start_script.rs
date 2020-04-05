use criterion::{criterion_group, criterion_main, Criterion};
use ::sl_sh::shell::*;

pub fn criterion_benchmark(c: &mut Criterion) {
    let command_args: Vec<String> = Vec::new();
    c.bench_function("start script", |b| {
        b.iter(|| run_one_script("benches/start.lisp", &command_args))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
