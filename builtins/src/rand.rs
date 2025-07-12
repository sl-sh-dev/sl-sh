use rand::distributions::{Alphanumeric, Distribution};
use rand::{Rng, SeedableRng};
use std::iter;
use unicode_segmentation::UnicodeSegmentation;

use bridge_adapters::add_builtin;
use bridge_macros::sl_sh_fn;
use compile_state::state::SloshVm;
use rand::rngs::{StdRng, ThreadRng};
use slvm::{from_i56, VMError, VMResult, Value};
use std::borrow::Cow;
use std::hash::{DefaultHasher, Hasher};

fn builtin_random(_vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut rng = rand::thread_rng();
    let mut args = registers.iter();
    if let (Some(next_arg), None) = (args.next(), args.next()) {
        match next_arg {
            Value::Int(i) => {
                let i: i64 = from_i56(i);
                match i {
                    positive if positive > 0 => Ok(rng.gen_range(0..i).into()),
                    _ => Err(VMError::new("rand", "Expected positive number")),
                }
            }
            Value::Float(f) => {
                let f: f64 = (*f).into();
                match f {
                    positive if positive > 0.0 => Ok(rng.gen_range(0.0..f).into()),
                    _ => Err(VMError::new("rand", "Expected positive number")),
                }
            }
            _ => Err(VMError::new(
                "rand",
                "Expected positive number, float or int",
            )),
        }
    } else {
        Err(VMError::new(
            "rand",
            "Expected positive number, float or int",
        ))
    }
}

fn builtin_get_random_str(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut args = registers.iter();
    if let (Some(Value::Int(i)), Some(arg2)) = (args.next(), args.next()) {
        let i: i64 = from_i56(i);
        match i {
            positive if positive > 0 => get_random_str(vm, *arg2, positive as u64),
            _ => Err(VMError::new("rand", "Expected positive length")),
        }
    } else {
        Err(VMError::new(
            "rand",
            "Expected two arguments, length and charset",
        ))
    }
}

pub fn rand_alphanumeric_str(len: u64, rng: &mut ThreadRng) -> Cow<'static, str> {
    iter::repeat(())
        .map(|()| rng.sample(Alphanumeric))
        .map(char::from)
        .take(len as usize)
        .collect()
}

fn get_random_str(vm: &mut SloshVm, arg: Value, len: u64) -> VMResult<Value> {
    let mut rng = rand::thread_rng();
    match arg {
        Value::Keyword(i) => {
            let sym = vm.get_interned(i);
            match sym {
                "ascii" => Ok(vm.alloc_string(
                    iter::repeat(())
                        .map(|()| rng.sample(Ascii))
                        .map(char::from)
                        .take(len as usize)
                        .collect(),
                )),
                "alnum" => Ok(vm.alloc_string(rand_alphanumeric_str(len, &mut rng).to_string())),
                "hex" => Ok(vm.alloc_string(
                    iter::repeat(())
                        .map(|()| rng.sample(Hex))
                        .map(char::from)
                        .take(len as usize)
                        .collect(),
                )),
                _ => Err(VMError::new("rand", format!("Unknown symbol :{}", sym))),
            }
        }
        Value::String(h) => {
            let string = vm.get_string(h);
            let upg = UserProvidedGraphemes::new(string);
            Ok(vm.alloc_string(
                iter::repeat(())
                    .map(|()| rng.sample(&upg))
                    .take(len as usize)
                    .collect(),
            ))
        }
        Value::StringConst(i) => {
            let string = vm.get_interned(i);
            let upg = UserProvidedGraphemes::new(string);
            Ok(vm.alloc_string(
                iter::repeat(())
                    .map(|()| rng.sample(&upg))
                    .take(len as usize)
                    .collect(),
            ))
        }
        _ => Err(VMError::new(
            "rand",
            "Second argument must be keyword or string",
        )),
    }
}

#[derive(Debug)]
struct Ascii;

impl Distribution<u8> for Ascii {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> u8 {
        const RANGE: u32 = 26 + 26 + 10 + 32;
        const ASCII_PRINTABLE_CHARSET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZ\
                abcdefghijklmnopqrstuvwxyz\
                0123456789\
                !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~";
        // We can pick from 94 characters. This is so close to a power of 2, 128,
        // that we can do better than `Uniform`. Use a simple bitshift and
        // rejection sampling. We do not use a bitmask, because for small RNGs
        // the most significant bits are usually of higher quality.
        loop {
            let var = rng.next_u32() >> (32 - 7);
            if var < RANGE {
                return ASCII_PRINTABLE_CHARSET[var as usize];
            }
        }
    }
}

#[derive(Debug)]
struct Hex;

impl Distribution<u8> for Hex {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> u8 {
        const RANGE: u32 = 16;
        const HEX_CHARSET: &[u8] = b"abcdef0123456789";
        // We can pick from 16 characters. This is a power of 2. Use a
        // simple bitshift and rejection sampling. We do not use a bitmask,
        // because for small RNGs/ the most significant bits are usually
        // of higher quality.
        loop {
            let var = rng.next_u32() >> (32 - 4);
            if var < RANGE {
                return HEX_CHARSET[var as usize];
            }
        }
    }
}

#[derive(Debug)]
struct UserProvidedGraphemes {
    sample_space: Vec<String>,
    len: usize,
}

impl UserProvidedGraphemes {
    pub fn new(s: &str) -> UserProvidedGraphemes {
        let mut sample_space: Vec<String> = Vec::new();
        for cluster in UnicodeSegmentation::graphemes(s, true) {
            sample_space.push(cluster.to_string());
        }
        let len = sample_space.len();
        UserProvidedGraphemes { sample_space, len }
    }
}

impl Distribution<String> for UserProvidedGraphemes {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> String {
        self.sample_space
            .get(rng.gen_range(0..self.len))
            .unwrap()
            .to_owned()
    }
}

fn builtin_probool(_vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let tup: Option<(u32, u32)> = match (registers.first(), registers.get(1), registers.get(2)) {
        (None, None, None) => Some((1, 2)),
        (Some(Value::Int(first)), Some(Value::Int(second)), None) => {
            let i: i64 = from_i56(first);
            let j: i64 = from_i56(second);
            if i >= 0 && i < u32::MAX as i64 && j >= 0 && j < u32::MAX as i64 {
                Some((i as u32, j as u32))
            } else {
                None
            }
        }
        _ => None,
    };
    match tup {
        None => Err(VMError::new("rand", "Expected zero or two positive ints")),
        Some((_, 0)) => Err(VMError::new("rand", "Denominator can not be zero")),
        Some((i, j)) => match i / j {
            improper if improper > 1 => Ok(Value::True),
            _ => match rand::thread_rng().gen_ratio(i, j) {
                true => Ok(Value::True),
                false => Ok(Value::False),
            },
        },
    }
}

fn string_to_seed(str_seed: &str) -> [u8; 32] {
    let mut seed = [0u8; 32];

    // Hash the string multiple times with different prefixes
    for i in 0..4 {
        let mut hasher = DefaultHasher::new();
        hasher.write_u8(i as u8); // Different prefix each time
        hasher.write(str_seed.as_bytes());
        let hash = hasher.finish();
        seed[i * 8..(i + 1) * 8].copy_from_slice(&hash.to_le_bytes());
    }

    seed
}

/// Usage: (random-seq limit count [seed])
///
/// Returns Vector of size `count` of non-negative integer(s) less than limit. Optional seed can be used
/// for the random number generator which allows (random-seq) to behave as a pure function, each unique
/// triple of (`limit`, `count`, `seed`) is equivalent to one deterministic sequence. Without seed values in
/// returned Vector are randomly selected on each invocation.
///
/// Like (random) but with two additional parameters and is capable of returning a Vector of random values instead of just one.
///
/// Section: random
///
/// Example:
/// (test::assert-equal (vec 2 2) (random-seq 4 2 "42"))
#[sl_sh_fn(fn_name = "random-seq")]
fn generate_random_sequence(
    limit: usize,
    count: usize,
    seed_string: Option<String>,
) -> VMResult<Vec<usize>> {
    if let Some(seed_string) = seed_string {
        let seed = string_to_seed(&seed_string);
        let mut rng = StdRng::from_seed(seed);
        Ok((0..count).map(|_| rng.gen_range(0..=limit)).collect())
    } else {
        let mut rng = rand::thread_rng();
        Ok((0..count).map(|_| rng.gen_range(0..=limit)).collect())
    }
}

pub fn add_rand_builtins(env: &mut SloshVm) {
    intern_generate_random_sequence(env);
    add_builtin(
        env,
        "probool",
        builtin_probool,
        "Usage: (probool), (probool numerator denominator)

PRObability of a BOOLean.

If no arguments are given, returns #t 1/2 of the time, otherwise takes two
integers, numerator and denominator, and returns #t numerator/denominator of the
time. Throws an error if denominator is 0. If (>= (/ numerator denominator) 1)
probool always returns true. If numerator is 0 probool always returns false.

Section: random

Example:
(def val0 (probool))
(test::assert-true (or (= #t val0) (= nil val0)))
(def val1 (probool 17 42))
(test::assert-true (or (= #t val1) (= nil val1)))
(test::assert-true (probool 1 1))
(test::assert-false (probool 0 42))
(test::assert-error-msg (probool 0 0) :rand \"Denominator can not be zero\")
(test::assert-error-msg (probool 0 0 0) :rand \"Expected zero or two positive ints\")
",
    );

    add_builtin(
        env,
        "random-str",
        builtin_get_random_str,
        "Usage: (random-str str-length [char-set])

Takes a positive integer, str-length, and one of :hex, :ascii, :alnum, or
a string. Returns random string of provided str-length composed of second argument,
:hex results in random hex string, :ascii results in random string of all printable
ascii characters, :alnum results in random string of all alphanumeric characters,
and providing a string results in a random string composed by sampling input.

Section: random

Example:
(test::assert-error-msg (random-str) :rand \"Expected two arguments, length and charset\")
(test::assert-error-msg (random-str 10) :rand \"Expected two arguments, length and charset\")
(test::assert-error-msg (random-str -1 :hex) :rand \"Expected positive length\")
(test::assert-error-msg (random-str 10 1) :rand \"Second argument must be keyword or string\")
(test::assert-error-msg (random-str 1 :hexy) :rand \"Unknown symbol :hexy\")
(test::assert-equal 10 (len (random-str 10 :hex)))
(test::assert-true (str-contains (random-str 42 \"\u{2699}\") \"\u{2699}\"))
(test::assert-equal 19 (len (random-str 19 :ascii)))
(test::assert-equal 91 (len (random-str 91 :alnum)))
",
    );

    add_builtin(
        env,
        "random",
        builtin_random,
        "Usage: (random), (random limit)

Returns non-negative number less than limit and of the same type as limit.

Section: random

Example:
(def rand-int (random 100))
(test::assert-true (and (>= rand-int 0) (< rand-int 100)))
(def rand-float (random 1.0))
(test::assert-true (and (>= rand-float 0.0) (< rand-float 1.0)))
(test::assert-error-msg (random -1) :rand \"Expected positive number\")
(test::assert-error-msg (random 1 2) :rand \"Expected positive number, float or int\")
",
    );
}
