use rand::distributions::{Alphanumeric, Distribution};
use rand::Rng;
use std::collections::HashMap;
use std::hash::BuildHasher;
use std::iter;
use unicode_segmentation::UnicodeSegmentation;

use crate::builtins_util::*;
use crate::environment::*;
use crate::interner::*;
use crate::types::*;

fn builtin_random(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(next_arg) = eval_next(environment, args)? {
        let next_arg_d = next_arg.get();
        let mut rng = rand::thread_rng();
        match &next_arg_d.data {
            ExpEnum::Int(i) => match i {
                positive if positive > &0 => {
                    Ok(Expression::alloc_data(ExpEnum::Int(rng.gen_range(0..*i))))
                }
                _ => Err(LispError::new("Expected positive number")),
            },
            ExpEnum::Float(f) => match f {
                positive if positive > &0.0 => Ok(Expression::alloc_data(ExpEnum::Float(
                    rng.gen_range(0.0..*f),
                ))),
                _ => Err(LispError::new("Expected positive number")),
            },
            _ => Err(LispError::new("Expected positive number, float or int")),
        }
    } else {
        Err(LispError::new("Expected positive number"))
    }
}

fn builtin_get_random_str(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(next_arg) = eval_next(environment, args)? {
        if let ExpEnum::Int(i) = next_arg.get().data {
            match i {
                positive if positive > 0 => get_random_str(environment, args, positive),
                _ => Err(LispError::new("Expected positive number")),
            }
        } else {
            Err(LispError::new("Expected at least one number"))
        }
    } else {
        Err(LispError::new("Expected at least one number"))
    }
}

fn get_random_str(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
    len: i64,
) -> Result<Expression, LispError> {
    if let Some(opt_arg) = eval_next(environment, args)? {
        let opt_arg_d = opt_arg.get();
        let mut rng = rand::thread_rng();
        match &opt_arg_d.data {
            ExpEnum::Symbol(sym, _) => match *sym {
                ":ascii" => Ok(Expression::alloc_data(ExpEnum::String(
                    iter::repeat(())
                        .map(|()| rng.sample(Ascii))
                        .map(char::from)
                        .take(len as usize)
                        .collect(),
                    None,
                ))),
                ":alnum" => Ok(Expression::alloc_data(ExpEnum::String(
                    iter::repeat(())
                        .map(|()| rng.sample(Alphanumeric))
                        .map(char::from)
                        .take(len as usize)
                        .collect(),
                    None,
                ))),
                ":hex" => Ok(Expression::alloc_data(ExpEnum::String(
                    iter::repeat(())
                        .map(|()| rng.sample(Hex))
                        .map(char::from)
                        .take(len as usize)
                        .collect(),
                    None,
                ))),
                _ => Err(LispError::new(format!("Unknown symbol {}", sym))),
            },
            ExpEnum::String(string, _) => Ok(Expression::alloc_data(ExpEnum::String(
                iter::repeat(())
                    .map(|()| rng.sample(UserProvidedGraphemes::new(string)))
                    .take(len as usize)
                    .collect(),
                None,
            ))),
            _ => Err(LispError::new(
                "Expected Second argument must be keyword or string",
            )),
        }
    } else {
        Err(LispError::new(
            "Expected second argument to be a string, :hex, :ascii, or :alnum",
        ))
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

fn builtin_probool(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let mut args = make_args(environment, args)?;
    let ints = parse_list_of_ints(environment, &mut args)?;
    let count = ints.len();
    let tup: Option<(u32, u32)> = match count {
        0 => Some((1, 2)),
        2 => {
            let i = *ints.get(0).unwrap() as u32;
            let j = *ints.get(1).unwrap() as u32;
            Some((i, j))
        }
        _ => None,
    };
    match tup {
        None => Err(LispError::new("Expected zero or two numbers")),
        Some((_, 0)) => Err(LispError::new("Denominator can not be zero")),
        Some((i, j)) => match i / j {
            improper if improper > 1 => Ok(Expression::alloc_data(ExpEnum::True)),
            _ => match rand::thread_rng().gen_ratio(i, j) {
                true => Ok(Expression::alloc_data(ExpEnum::True)),
                false => Ok(Expression::alloc_data(ExpEnum::Nil)),
            },
        },
    }
}

pub fn add_rand_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, (Expression, String), S>,
) {
    data.insert(
        interner.intern("probool"),
        Expression::make_function(
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
(test::assert-error-msg (probool 0 0) \"Denominator can not be zero\")
(test::assert-error-msg (probool 0 0 0) \"Expected zero or two numbers\")
",
        ),
    );

    data.insert(
        interner.intern("random-str"),
        Expression::make_function(
            builtin_get_random_str,
            "Usage: (random-str str-length [char-set])

Takes a positive integer, str-length, and one of :hex, :ascii, :alnum, or
a string. Returns random string of provided str-length composed of second argument:
:hex results in random hex string, :ascii results in random string of all printable
ascii characters, :alnum results in random string of all alphanumeric characters,
and providing a string results in a random string composed by sampling input.

Section: random

Example:
(test::assert-error-msg (random-str) \"Expected at least one number\")
(test::assert-error-msg (random-str -1) \"Expected positive number\")
(test::assert-error-msg (random-str 10) \"Expected second argument to be a string, :hex, :ascii, or :alnum\")
(test::assert-equal 100 (length (random-str 10 :hex))
(test::assert-true (str-contains \"\u{2699}\" (random-str 42 \"\u{2699}\"))
(test::assert-equal 19 (length (random-str 19 :ascii)
(test::assert-equal 91 (length (random-str 91 :alnum)
",
        ),
    );

    data.insert(
        interner.intern("random"),
        Expression::make_function(
            builtin_random,
            "Usage: (random), (random limit)

Returns non-negative number less than limit and of the same type as limit.

Section: math

Example:
(def rand-int (random 100))
(test::assert-true (and (> rand-int 0) (< rand-int 100))
(def rand-float (random 1.0))
(test::assert-true (and (> rand-float 0) (< rand-float 1)))
(test::assert-error-msg (random -1) \"Expected positive integer\")
(test::assert-error-msg (random 1 2) \"Expected zero or one integers\")
",
        ),
    );
}
