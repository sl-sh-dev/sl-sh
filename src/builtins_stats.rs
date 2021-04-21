use std::collections::HashMap;
use std::hash::BuildHasher;

use crate::builtins_util::*;
use crate::environment::*;
use crate::interner::*;
use crate::types::*;

fn float_to_expr(f: f64) -> Expression {
    Expression::alloc_data(ExpEnum::Float(f))
}

fn floats_to_expr(vec: Vec<f64>) -> Expression {
    let mut float_expr = Vec::with_capacity(vec.len());
    for v in vec {
        float_expr.push(Expression::alloc_data(ExpEnum::Float(v)));
    }
    Expression::alloc_data(ExpEnum::Vector(float_expr))
}

fn find_median(vec: &[f64]) -> f64 {
    match vec.len() % 2 {
        1 => {
            let mid = (vec.len() - 1) / 2;
            vec[mid]
        }
        _ => {
            let mid = (vec.len() - 1) / 2;
            (vec[mid] + vec[mid + 1]) / 2.0
        }
    }
}

struct SummaryStats<'a> {
    vec: &'a mut [f64],
    count: f64,
    mean: Option<f64>,
    std_dev: Option<f64>,
    mode: Option<Vec<f64>>,
    min: Option<f64>,
    quartile_one: Option<f64>,
    median: Option<f64>,
    quartile_three: Option<f64>,
    max: Option<f64>,
}

impl<'a> SummaryStats<'a> {
    fn new(vec: &mut [f64]) -> Result<SummaryStats, LispError> {
        let count = vec.len() as f64;
        if count > 0.0 {
            Ok(SummaryStats {
                vec,
                count,
                mean: None,
                std_dev: None,
                mode: None,
                min: None,
                quartile_one: None,
                median: None,
                quartile_three: None,
                max: None,
            })
        } else {
            Err(LispError::new("expected at least one number"))
        }
    }

    fn calc_mean(&mut self) -> f64 {
        match self.mean {
            Some(mean) => mean,
            None => {
                let sum = self.vec.iter().sum::<f64>();
                let mean = sum / self.count;
                self.mean = Some(mean);
                mean
            }
        }
    }

    fn calc_q1(&mut self) -> f64 {
        match self.quartile_one {
            Some(quartile_one) => quartile_one,
            None => {
                self.sorted_stats();
                self.quartile_one.unwrap()
            }
        }
    }

    fn calc_q3(&mut self) -> f64 {
        match self.quartile_three {
            Some(quartile_three) => quartile_three,
            None => {
                self.sorted_stats();
                self.quartile_three.unwrap()
            }
        }
    }

    fn calc_median(&mut self) -> f64 {
        match self.median {
            Some(median) => median,
            None => {
                self.sorted_stats();
                self.median.unwrap()
            }
        }
    }

    fn calc_min(&mut self) -> f64 {
        match self.min {
            Some(min) => min,
            None => {
                self.sorted_stats();
                self.min.unwrap()
            }
        }
    }

    fn calc_max(&mut self) -> f64 {
        match self.max {
            Some(max) => max,
            None => {
                self.sorted_stats();
                self.max.unwrap()
            }
        }
    }

    fn calc_std_dev(&mut self) -> f64 {
        match self.std_dev {
            Some(std_dev) => std_dev,
            None => {
                let mean = self.calc_mean();
                let std_dev = (self.vec.iter().fold(0.0, |accum: f64, elem: &f64| -> f64 {
                    accum + (mean - elem).powf(2.0)
                }) / (self.count - 1.0))
                    .sqrt();
                // https://en.wikipedia.org/wiki/Variance pop variance is unknown divide by n - 1
                self.std_dev = Some(std_dev);
                std_dev
            }
        }
    }

    fn calc_mode(&mut self) -> Vec<f64> {
        match &self.mode {
            Some(mode) => mode.to_vec(),
            _ => {
                let mut freqs: HashMap<u64, i32> = HashMap::new();
                for float in self.vec.iter() {
                    *freqs.entry(float.to_bits()).or_insert(0) += 1;
                }
                let mut counts: HashMap<i32, Vec<f64>> = HashMap::new();
                let mut max_count = 0;
                for freq in freqs.iter() {
                    let (float_as_int, count) = freq;
                    let float: f64 = f64::from_bits(*float_as_int);
                    let count: i32 = *count;
                    counts.entry(count).or_insert_with(Vec::new).push(float);
                    if count > max_count {
                        max_count = count;
                    }
                }
                let opt_modes = match counts.get(&max_count) {
                    Some(modes) => {
                        let mut modes = modes.to_vec();
                        modes.sort_by(|a, b| a.partial_cmp(b).unwrap());
                        Some(modes)
                    }
                    None => None,
                };
                let mode = opt_modes.unwrap();
                let ret = mode.to_vec();
                self.mode = Some(mode);
                ret
            }
        }
    }

    fn sorted_stats(&mut self) {
        self.vec.sort_by(|a, b| a.partial_cmp(b).unwrap());
        let mid_opt: Option<usize>;
        let median = match self.vec.len() % 2 {
            1 => {
                let mid = (self.vec.len() - 1) / 2;
                mid_opt = Some(mid);
                self.vec[mid]
            }
            _ => {
                let mid = (self.vec.len() - 1) / 2;
                mid_opt = Some(mid);
                (self.vec[mid] + self.vec[mid + 1]) / 2.0
            }
        };
        if let Some(mid) = mid_opt {
            if mid > 0 {
                let quartile_one = find_median(&self.vec[..mid]);
                self.quartile_one = Some(quartile_one);
                let quartile_three = find_median(&self.vec[mid + 1..]);
                self.quartile_three = Some(quartile_three);
            } else {
                self.quartile_one = Some(0.0);
                self.quartile_three = Some(0.0);
            }
        }
        self.median = Some(median);
        let min = self.vec[0];
        self.min = Some(min);
        let max = self.vec[(self.count - 1.0) as usize];
        self.max = Some(max);
    }

    fn calculate(&mut self) {
        self.calc_mean();
        self.calc_std_dev();
        self.calc_mode();
        self.sorted_stats();
    }
}

fn builtin_summary_stats(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let mut args = make_args(environment, args)?;
    let mut floats = parse_list_of_floats(environment, &mut args)?;
    let mut stats = SummaryStats::new(&mut floats)?;
    stats.calculate();

    let mut map: HashMap<&'static str, Expression> = HashMap::new();

    map.insert(":mean", float_to_expr(stats.calc_mean()));
    map.insert(":sd", float_to_expr(stats.calc_std_dev()));
    map.insert(":mode", floats_to_expr(stats.calc_mode()));
    map.insert(":min", float_to_expr(stats.calc_min()));
    map.insert(":q1", float_to_expr(stats.calc_q1()));
    map.insert(":med", float_to_expr(stats.calc_median()));
    map.insert(":q3", float_to_expr(stats.calc_q3()));
    map.insert(":max", float_to_expr(stats.calc_max()));
    map.insert(":vec", floats_to_expr(Vec::from(stats.vec)));

    Ok(Expression::alloc_data(ExpEnum::HashMap(map)))
}

fn builtin_mode(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let mut args = make_args(environment, args)?;
    let mut floats = parse_list_of_floats(environment, &mut args)?;
    let mut stats = SummaryStats::new(&mut floats)?;
    let modes = stats.calc_mode();
    let mut float_expr = Vec::with_capacity(modes.len());
    for m in modes {
        float_expr.push(Expression::alloc_data(ExpEnum::Float(m)));
    }
    Ok(Expression::alloc_data(ExpEnum::Vector(float_expr)))
}

fn builtin_std_dev(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let mut args = make_args(environment, args)?;
    let mut floats = parse_list_of_floats(environment, &mut args)?;
    let mut stats = SummaryStats::new(&mut floats)?;
    let std_dev = stats.calc_std_dev();
    Ok(Expression::alloc_data(ExpEnum::Float(std_dev)))
}

fn builtin_min(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let mut args = make_args(environment, args)?;
    let mut floats = parse_list_of_floats(environment, &mut args)?;
    let mut stats = SummaryStats::new(&mut floats)?;
    let min = stats.calc_min();
    Ok(Expression::alloc_data(ExpEnum::Float(min)))
}

fn builtin_max(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let mut args = make_args(environment, args)?;
    let mut floats = parse_list_of_floats(environment, &mut args)?;
    let mut stats = SummaryStats::new(&mut floats)?;
    let max = stats.calc_max();
    Ok(Expression::alloc_data(ExpEnum::Float(max)))
}

fn builtin_q1(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let mut args = make_args(environment, args)?;
    let mut floats = parse_list_of_floats(environment, &mut args)?;
    let mut stats = SummaryStats::new(&mut floats)?;
    let q1 = stats.calc_q1();
    Ok(Expression::alloc_data(ExpEnum::Float(q1)))
}

fn builtin_q3(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let mut args = make_args(environment, args)?;
    let mut floats = parse_list_of_floats(environment, &mut args)?;
    let mut stats = SummaryStats::new(&mut floats)?;
    let q3 = stats.calc_q3();
    Ok(Expression::alloc_data(ExpEnum::Float(q3)))
}

fn builtin_median(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let mut args = make_args(environment, args)?;
    let mut floats = parse_list_of_floats(environment, &mut args)?;
    let mut stats = SummaryStats::new(&mut floats)?;
    let median = stats.calc_median();
    Ok(Expression::alloc_data(ExpEnum::Float(median)))
}

fn builtin_mean(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let mut args = make_args(environment, args)?;
    let mut floats = parse_list_of_floats(environment, &mut args)?;
    let mut stats = SummaryStats::new(&mut floats)?;
    let mean = stats.calc_mean();
    Ok(Expression::alloc_data(ExpEnum::Float(mean)))
}

pub fn add_stats_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, (Expression, String), S>,
) {
    data.insert(
        interner.intern("median"),
        Expression::make_function(
            builtin_median,
            "Usage: (median number+)

Returns median of sequence of numbers.

Section: math

Example:
(test::assert-error-msg (median) \"expected at least one number\")
(test::assert-equal 5 (median 5))
(test::assert-equal 7.5 (median 10 5))
(test::assert-equal 5.5 (median 10 9 8 7 6 5 4 3 2 1))
(test::assert-equal 6 (median 10 4 8 7 6 5 9 3 2 1 11))",
        ),
    );

    data.insert(
        interner.intern("mean"),
        Expression::make_function(
            builtin_mean,
            "Usage: (mean number+)

Average a sequence of numbers.

Section: math

Example:
(test::assert-error-msg (mean) \"expected at least one number\")
(test::assert-equal 5 (mean 5))
(test::assert-equal 7.5 (mean 5 10))
(test::assert-equal 5.5 (mean 1 2 3 4 5 6 7 8 9 10))",
        ),
    );

    data.insert(
        interner.intern("mode"),
        Expression::make_function(
            builtin_mode,
            "Usage: (mode number+)

Returns mode of a sequence of numbers. Since distributions can be multimodal, mode returns a list.

Section: math

Example:
(test::assert-error-msg (mode) \"expected at least one number\")
(test::assert-equal (list 5) (mode 5))
(test::assert-equal (list 1 3 4 5 6 7 8 9 10) (mode 1 3 4 5 6 7 8 9 10))
(test::assert-equal (list 7.0) (mode 1 7.0 3 4 5 6 7 8 9 10))",
        ),
    );

    data.insert(
        interner.intern("std-dev"),
        Expression::make_function(
            builtin_std_dev,
            "Usage: (std-dev number+)

Returns standard deviation of a sequence of numbers.

Section: math

Example:
(test::assert-error-msg (std-dev) \"expected at least one number\")
(test::assert-equal 3.0276503540974917 (std-dev 1 2 3 4 5 6 7 8 9 10))",
        ),
    );

    data.insert(
        interner.intern("min"),
        Expression::make_function(
            builtin_min,
            "Usage: (min number+)

Returns minimum of provided arguments.

Section: math

Example:
(test::assert-error-msg (min) \"expected at least one number\")
(test::assert-equal 1 (min 10 4 8 7 6 5 9 3 2 1 11))
            ",
        ),
    );

    data.insert(
        interner.intern("max"),
        Expression::make_function(
            builtin_max,
            "Usage: (max number+)

Returns maximum of provided arguments.

Section: math

Example:
(test::assert-error-msg (max) \"expected at least one number\")
(test::assert-equal 11 (max 10 4 8 7 6 5 9 3 2 1 11))
            ",
        ),
    );

    data.insert(
        interner.intern("third-quartile"),
        Expression::make_function(
            builtin_q3,
            "Usage: (third-quartile number+)

Returns third quartile of distribution of provided arguments.

Section: math

Example:
(test::assert-error-msg (third-quartile) \"expected at least one number\")
(test::assert-equal 8 (third-quartile 10 4 8 7 6 5 9 3 2 1))
            ",
        ),
    );

    data.insert(
        interner.intern("first-quartile"),
        Expression::make_function(
            builtin_q1,
            "Usage: (first-quartile number+)

Returns first quartile of distribution of provided arguments.

Section: math

Example:
(test::assert-error-msg (first-quartile) \"expected at least one number\")
(test::assert-equal 2.5 (first-quartile 10 4 8 7 6 5 9 3 2 1))
            ",
        ),
    );

    data.insert(
        interner.intern("summary-stats"),
        Expression::make_function(
            builtin_summary_stats,
            "Usage: (summary-stats number+)

Returns hash map containing summary statistics and sorted array.

Section: math

Example:
(test::assert-error-msg (summary-stats) \"expected at least one number\")
(def distr (summary-stats 10 2 9 4 6 5 7 8 3 1))
(test::assert-equal '#(1 2 3 4 5 6 7 8 9 10) (hash-get distr :vec))
(test::assert-equal 5.5 (hash-get distr :med))
(test::assert-equal 10 (hash-get distr :max))
(test::assert-equal 3.0276503540974917 (hash-get distr :sd))
(test::assert-equal 5.5 (hash-get distr :mean))
(test::assert-equal '#(1 2 3 4 5 6 7 8 9 10) (hash-get distr :mode))
(test::assert-equal 1 (hash-get distr :min))
(test::assert-equal 8 (hash-get distr :q3))
(test::assert-equal 2.5 (hash-get distr :q1))
            ",
        ),
    );
}
// TODO need sample-stats hash map with
//  - can stats fcns take vectors as well?
//  - stats namespace
//  - merge w/ master
//  - tests for summary stats
//  - add min/max/q1/q3 functions
