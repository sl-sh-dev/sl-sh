use regex::{Captures, Regex};
use std::collections::HashMap;
use std::hash::{BuildHasher, Hash, Hasher};

use crate::environment::*;
use crate::interner::*;
use crate::types::*;
use crate::{param_eval, params_done};
use std::collections::hash_map::DefaultHasher;

const FOREGROUND_DEFAULT: &str = "\x1b[39m";

fn rgb(r: u8, g: u8, b: u8) -> String {
    format!("\x1b[38;2;{};{};{}m", r, g, b)
}

fn color(value: &str, capture_group: usize) -> String {
    // can create unique colors for up to 32 capture groups
    // given the bits from the str hash. as implemented means
    // that identical values for the 1st and 33rd capture groups
    // will have the same color.
    let shift = capture_group % 32;
    let mut s = DefaultHasher::new();
    value.hash(&mut s);
    let hash = s.finish();
    let r = (hash >> shift & 0xFF) as u8;
    let g = (hash >> (shift + 8) & 0xFF) as u8;
    let b = (hash >> (shift + 16) & 0xFF) as u8;
    rgb(r, g, b)
}

fn colorize_capture(value: &str, capture_group: usize, unique_colors: bool) -> String {
    let mut capture_group = capture_group;
    if !unique_colors {
        capture_group = 0;
    }
    format!(
        "{}{}{}",
        color(value, capture_group),
        value,
        FOREGROUND_DEFAULT
    )
}

fn matches(sample: &str, regex: &Regex) -> Expression {
    if regex.is_match(sample) {
        Expression::make_true()
    } else {
        Expression::make_false()
    }
}

fn captures_to_vec(caps: &Captures) -> Vec<Expression> {
    let mut captures: Vec<Expression> = Vec::new();
    for c in caps.iter().flatten() {
        captures.push(Expression::alloc_data(ExpEnum::String(
            c.as_str().to_string().into(),
            None,
        )));
    }
    captures
}

fn find_with_regex(sample: &str, regex: &Regex) -> Expression {
    if let Some(caps) = regex.captures_iter(sample).next() {
        Expression::alloc_data(ExpEnum::Vector(captures_to_vec(&caps)))
    } else {
        Expression::make_nil()
    }
}

fn find_all_with_regex(sample: &str, regex: &Regex) -> Vec<Expression> {
    let mut capture_groups: Vec<Expression> = Vec::new();
    for caps in regex.captures_iter(sample) {
        let captures = captures_to_vec(&caps);
        capture_groups.push(Expression::alloc_data(ExpEnum::Vector(captures)));
    }
    capture_groups
}

fn colorize_string_with_regex(sample: &str, regex: &Regex, unique_colors: bool) -> String {
    let mut offset = 0;
    regex
        .replace_all(sample, |caps: &Captures| {
            if caps.len() > 1 {
                // get the offset of each individual capture group into a vec.
                // this vec can then be used to cut the string and insert color
                // codes around a new output version of the sample.
                let mut offsets = vec![];
                for (idx, cap) in caps.iter().enumerate() {
                    if let Some(cap) = cap {
                        if idx == 0 {
                            offset = cap.start();
                        } else {
                            let start = cap.start() - offset;
                            let end = cap.end() - offset;
                            offsets.push((start, end));
                        }
                    }
                }
                let mut strings = String::with_capacity(
                    caps[0].len()
                        + (offsets.len() * (FOREGROUND_DEFAULT.len() + rgb(255, 255, 255).len())),
                );
                let mut last_end: Option<usize> = None;
                // store the whole capture group in capture, this string will be indexed according
                // to the offset array. If the portion of capture is within an offset it will be
                // assigned a unique color, otherwise it is just appeneded to the vector that
                // will be appended into the final output string
                let capture = String::from(&caps[0]);
                for (idx, (start, end)) in offsets.iter().enumerate() {
                    let slice = &capture[*start..*end];
                    if idx == 0 && *start == 0 {
                        strings.push_str(&colorize_capture(slice, idx, unique_colors));
                    } else if idx == 0 {
                        let begin = &capture[0..*start];
                        strings.push_str(begin);
                        strings.push_str(&colorize_capture(slice, idx, unique_colors));
                    } else if let Some(last_end) = last_end {
                        let begin = &capture[last_end..*start];
                        strings.push_str(begin);
                        strings.push_str(&colorize_capture(slice, idx, unique_colors));
                    }
                    last_end = Some(*end);
                }
                if let Some(last_end) = last_end {
                    if last_end < capture.len() {
                        let end = &capture[last_end..];
                        strings.push_str(end);
                    }
                }
                strings
            } else {
                colorize_capture(&caps[0], 0, unique_colors)
            }
        })
        .into()
}

fn builtin_make_regex(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let fn_name = "make-regex";
    let string = param_eval(environment, args, fn_name)?;
    let string = &string.get().data;
    params_done(args, fn_name)?;
    match string {
        ExpEnum::String(string, _) => {
            let re = Regex::new(string);
            match re {
                Ok(re) => Ok(Expression::alloc_data(ExpEnum::Regex(re))),
                Err(e) => Err(LispError::new(format!(
                    "{} requires a valid regular expression.\n{}",
                    fn_name, e
                ))),
            }
        }
        _ => Err(LispError::new(format!("{} takes a string", fn_name))),
    }
}

fn regex_replace(sample: &str, regex: &Regex, replacement: &str) -> Expression {
    let replaced = regex.replace_all(sample, replacement);
    Expression::alloc_data(ExpEnum::String(replaced.to_string().into(), None))
}

fn builtin_regex_replace(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let fn_name = "re-replace";
    let regex = param_eval(environment, args, fn_name)?;
    let regex = &regex.get().data;
    let string = param_eval(environment, args, fn_name)?;
    let string = &string.get().data;
    let replacement = param_eval(environment, args, fn_name)?;
    let replacement = &replacement.get().data;
    params_done(args, fn_name)?;
    match (regex, string, replacement) {
        (
            ExpEnum::String(regex, _),
            ExpEnum::String(string, _),
            ExpEnum::String(replacement, _),
        ) => {
            let regex = Regex::new(regex);
            match regex {
                Ok(regex) => Ok(regex_replace(string, &regex, replacement)),
                Err(e) => Err(LispError::new(format!(
                    "{} requires a valid regular expression.\n{}",
                    fn_name, e
                ))),
            }
        }
        (ExpEnum::Regex(regex), ExpEnum::String(string, _), ExpEnum::String(replacement, _)) => {
            Ok(regex_replace(string, regex, replacement))
        }
        (_, _, _) => Err(LispError::new(format!(
            "{} takes a regex, a string, and a replacement string",
            fn_name
        ))),
    }
}

fn builtin_regex_match(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let fn_name = "re-match";
    let regex = param_eval(environment, args, fn_name)?;
    let regex = &regex.get().data;
    let string = param_eval(environment, args, fn_name)?;
    let string = &string.get().data;
    params_done(args, fn_name)?;
    match (regex, string) {
        (ExpEnum::String(regex, _), ExpEnum::String(string, _)) => {
            let regex = Regex::new(regex);
            match regex {
                Ok(regex) => Ok(matches(string, &regex)),
                Err(e) => Err(LispError::new(format!(
                    "{} requires a valid regular expression.\n{}",
                    fn_name, e
                ))),
            }
        }
        (ExpEnum::Regex(regex), ExpEnum::String(string, _)) => Ok(matches(string, regex)),
        (_, _) => Err(LispError::new(format!(
            "{} takes a regex and a string",
            fn_name
        ))),
    }
}

fn builtin_regex_find(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let fn_name = "re-find";
    let regex = param_eval(environment, args, fn_name)?;
    let regex = &regex.get().data;
    let string = param_eval(environment, args, fn_name)?;
    let string = &string.get().data;
    params_done(args, fn_name)?;
    match (regex, string) {
        (ExpEnum::String(regex, _), ExpEnum::String(string, _)) => {
            let regex = Regex::new(regex);
            match regex {
                Ok(regex) => Ok(find_with_regex(string, &regex)),
                Err(e) => Err(LispError::new(format!(
                    "{} requires a valid regular expression.\n{}",
                    fn_name, e
                ))),
            }
        }
        (ExpEnum::Regex(regex), ExpEnum::String(string, _)) => Ok(find_with_regex(string, regex)),
        (_, _) => Err(LispError::new(format!(
            "{} takes a regex and a string",
            fn_name
        ))),
    }
}

fn builtin_regex_find_all(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let fn_name = "re-find-all";
    let regex = param_eval(environment, args, fn_name)?;
    let regex = &regex.get().data;
    let string = param_eval(environment, args, fn_name)?;
    let string = &string.get().data;
    params_done(args, fn_name)?;
    match (regex, string) {
        (ExpEnum::String(regex, _), ExpEnum::String(string, _)) => {
            let regex = Regex::new(regex);
            match regex {
                Ok(regex) => Ok(Expression::alloc_data(ExpEnum::Vector(
                    find_all_with_regex(string, &regex),
                ))),
                Err(e) => Err(LispError::new(format!(
                    "{} requires a valid regular expression.\n{}",
                    fn_name, e
                ))),
            }
        }
        (ExpEnum::Regex(regex), ExpEnum::String(string, _)) => Ok(Expression::alloc_data(
            ExpEnum::Vector(find_all_with_regex(string, regex)),
        )),
        (_, _) => Err(LispError::new(format!(
            "{} takes a regex and a string",
            fn_name
        ))),
    }
}

fn builtin_regex_color(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let fn_name = "re-color";
    let regex = param_eval(environment, args, fn_name)?;
    let regex = &regex.get().data;
    let string = param_eval(environment, args, fn_name)?;
    let string = &string.get().data;
    let mut unique_colors = false;
    if let Ok(unique) = param_eval(environment, args, fn_name) {
        let unique = &unique.get().data;
        if let ExpEnum::Symbol(str, _) = unique {
            if *str == ":unique" {
                unique_colors = true
            } else if *str == ":default" {
                unique_colors = false;
            } else {
                return Err(LispError::new(format!(
                    "Optional third param to {} must be the symbol :unique or :default",
                    fn_name
                )));
            }
        }
    }
    params_done(args, fn_name)?;
    match (regex, string) {
        (ExpEnum::String(regex, _), ExpEnum::String(string, _)) => {
            let regex = Regex::new(regex);
            match regex {
                Ok(regex) => {
                    let replaced = colorize_string_with_regex(string, &regex, unique_colors);
                    Ok(Expression::alloc_data(ExpEnum::String(
                        replaced.into(),
                        None,
                    )))
                }
                Err(e) => Err(LispError::new(format!(
                    "{} requires a valid regular expression.\n{}",
                    fn_name, e
                ))),
            }
        }
        (ExpEnum::Regex(regex), ExpEnum::String(string, _)) => {
            let replaced = colorize_string_with_regex(string, regex, unique_colors);
            Ok(Expression::alloc_data(ExpEnum::String(
                replaced.into(),
                None,
            )))
        }
        (_, _) => Err(LispError::new(format!(
            "{} takes a regex and a string",
            fn_name
        ))),
    }
}

pub fn add_regex_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, (Expression, String), S>,
) {
    data.insert(
        interner.intern("make-regex"),
        Expression::make_function(
            builtin_make_regex,
            r#"Usage: (make-regex regex) -> Regex

Given a valid regex string, return a sl-sh Regex. The syntax for regular
expressions in sl-sh is borrowed from the Rust regex library and is specified
[here](https://docs.rs/regex/latest/regex/#syntax).

Section: regex

Example:
(test::assert-equal "Regex" (type (make-regex ".*")))
"#,
        ),
    );
    data.insert(
        interner.intern("re-replace"),
        Expression::make_function(
            builtin_regex_replace,
            r#"Usage: (re-replace regex string replacement) -> String

Given a regex, a string, and a replacement string, return a modified version of
string where all occurrences of regex are edited according to the replacement
syntax. The replacement string syntax is borrowed from the Rust regex library
and is specified [here](https://docs.rs/regex/latest/regex/struct.Regex.html#replacement-string-syntax).
The regex argument can either be a regex string or a regex type
obtained from [make-regex](#regex::make-regex).

Section: regex

Example:
(test::assert-equal
    "Thon connection takes"
    (re-replace "is (.*) (.*)" "This connection takes on" "\$2 \$1"))
(test::assert-equal
    "10-20-2020 and then again on 12-18-2021 but not on 11-20-2020"
    (re-replace (make-regex "(?P<y>\d{4})-(?P<m>\d{2})-(?P<d>\d{2})") "2020-10-20 and then again on 2021-12-18 but not on 2020-11-20" "\$m-\$d-\$y"))
"#,
        ),
    );
    data.insert(
        interner.intern("re-match"),
        Expression::make_function(
            builtin_regex_match,
            r#"Usage: (re-match regex string) -> boolean?

Given a regex and a string, return true if regex is found in string, and return
false otherwise. The regex argument can either be a regex string or a regex type
obtained from [make-regex](#regex::make-regex).

Section: regex

Example:
(test::assert-true
    (re-match (make-regex "(\d{4})-(\d{2})-(\d{2})") "2020-12-20 and then again on 2021-12-18 but not on 2020-11-20"))
(test::assert-false
    (re-match "(\d{4})-(\d{2})-(\d{20})" "2020-12-20 and then again on 2021-12-18 but not on 2020-11-20"))
"#,
        ),
    );
    data.insert(
        interner.intern("re-find"),
        Expression::make_function(
            builtin_regex_find,
            r#"Usage: (re-find regex string) -> #([String, ..])

Given a regex and a string, find the first matching occurrence of regex in string 
and return a vector of capture groups. The 0th element of the vector is always a
string of the whole match. If N capture groups are provided, the Nth group's
value is placed in the Nth element of the vector, where N is one indexed. The regex
argument can either be a regex string or a regex type obtained from [make-regex](#regex::make-regex).

Section: regex

Example:
(def found-capture (re-find (make-regex "(\d{4})-(\d{2})-(\d{2})") "2020-12-20 and then again on 2021-12-18 but not on 2020-11-20"))
(test::assert-equal 4 (length found-capture))
(test::assert-equal '#("2020-12-20" "2020" "12" "20") found-capture))

(def found (re-find "\d{4}-\d{2}-\d{2}" "2020-12-20 and then again on 2021-12-18 but not on 2020-11-20"))
(test::assert-equal 1 (length found))
(test::assert-equal '#("2020-12-20") found))

(def found-none (re-find "\d{40}-\d{2}-\d{2}" "2020-12-20 and then again on 2021-12-18 but not on 2020-11-20"))
(test::assert-equal '#() found-none)
"#,
        ),
    );
    data.insert(
        interner.intern("re-find-all"),
        Expression::make_function(
            builtin_regex_find_all,
            r#"Usage: (re-find-all regex string) -> #(#([String, ..])..)

Given a regex and a string, find all matching occurrences of regex in string and
return a vector of a vector of capture groups. The 0th element of each nested vector
is always a string of the whole match. If N capture groups are provided, the
Nth group's value is placed in the Nth element of its respective match vector, where
N is one indexed. The regex argument can either be a regex string or a regex type
obtained from [make-regex](#regex::make-regex).

Section: regex

Example:
(def found (re-find-all (make-regex "(\d{4})-(\d{2})-(\d{2})") "2020-12-20 and then again on 2021-12-18 but not on 2020-11-20"))
(test::assert-equal 3 (length found))
(test::assert-equal '#("2020-12-20" "2020" "12" "20") (vec-nth found 0))
(test::assert-equal '#("2021-12-18" "2021" "12" "18") (vec-nth found 1))
(test::assert-equal '#("2020-11-20" "2020" "11" "20") (vec-nth found 2))

(def found-one (re-find-all "(\d{4})-(\d{2})-(\d{2})" "2020-12-20 and then again on"))
(test::assert-equal 1 (length found-one))
(test::assert-equal '#("2020-12-20" "2020" "12" "20") (vec-nth found-one 0))

(def found-none (re-find-all "(\d{40})-(\d{2})-(\d{2})" "2020-12-20 and then again on"))
(test::assert-equal '#() found-none)
"#,
        ),
    );
    data.insert(
        interner.intern("re-color"),
        Expression::make_function(
            builtin_regex_color,
            r#"Usage: (re-color regex string) -> t/nil

Given a regex and a string, colorize the portions of string that match regex,
giving unique capture group's values unique colors. Colors are chosen
deterministically based on the hash of the capture group's value. If no
capture groups are provided the whole regex is colorized uniquely based
on its value. Overlapping capture groups are not supported. The regex
argument can either be a regex string or a regex type obtained from [make-regex](#regex::make-regex).

Section: regex

Example:
(test::assert-equal
    (str "This c" (fg-color-rgb 35 98 130) "onnection takes on" shell::*fg-default*)
    (re-color (make-regex "is c(.*)") "This connection takes on"))
"#,
        ),
    );
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_colorize() {
        let sample = "2020-20-20 and then again on 2021-20-18 but not on 2020-18-20";
        let regex = Regex::new("(\\d{4})-(\\d{2})-(\\d{2})").unwrap();
        let unique_colors = false;
        let replaced = colorize_string_with_regex(sample, &regex, unique_colors);
        let expected = format!("{}2020{}-{}20{}-{}20{} and then again on {}2021{}-{}20{}-{}18{} but not on {}2020{}-{}18{}-{}20{}",
                               "\x1b[38;2;12;154;58m",
                               FOREGROUND_DEFAULT,
                               "\x1b[38;2;103;93;109m",
                               FOREGROUND_DEFAULT,
                               "\x1b[38;2;103;93;109m",
                               FOREGROUND_DEFAULT,
                               "\x1b[38;2;75;146;223m",
                               FOREGROUND_DEFAULT,
                               "\x1b[38;2;103;93;109m",
                               FOREGROUND_DEFAULT,
                               "\x1b[38;2;217;27;83m",
                               FOREGROUND_DEFAULT,
                               "\x1b[38;2;12;154;58m",
                               FOREGROUND_DEFAULT,
                               "\x1b[38;2;217;27;83m",
                               FOREGROUND_DEFAULT,
                               "\x1b[38;2;103;93;109m",
                               FOREGROUND_DEFAULT,
        );
        assert_eq!(expected, replaced);
    }

    #[test]
    fn test_colorize_unique() {
        let sample = "2020-20-20 and then again on 2021-20-18 but not on 2020-18-20";
        let regex = Regex::new("(\\d{4})-(\\d{2})-(\\d{2})").unwrap();
        let unique_colors = true;
        let replaced = colorize_string_with_regex(sample, &regex, unique_colors);
        let expected = format!("{}2020{}-{}20{}-{}20{} and then again on {}2021{}-{}20{}-{}18{} but not on {}2020{}-{}18{}-{}20{}",
                               "\x1b[38;2;12;154;58m",
                               FOREGROUND_DEFAULT,
                               "\x1b[38;2;179;174;182m",
                               FOREGROUND_DEFAULT,
                               "\x1b[38;2;89;87;219m",
                               FOREGROUND_DEFAULT,
                               "\x1b[38;2;75;146;223m",
                               FOREGROUND_DEFAULT,
                               "\x1b[38;2;179;174;182m",
                               FOREGROUND_DEFAULT,
                               "\x1b[38;2;246;198;212m",
                               FOREGROUND_DEFAULT,
                               "\x1b[38;2;12;154;58m",
                               FOREGROUND_DEFAULT,
                               "\x1b[38;2;236;141;169m",
                               FOREGROUND_DEFAULT,
                               "\x1b[38;2;89;87;219m",
                               FOREGROUND_DEFAULT,
        );
        assert_eq!(expected, replaced);
    }

    #[test]
    fn test_colorize_inner_capture_groups() {
        let sample = "\"name = eeestart_benchmarkeee\"";
        let regex = Regex::new(r" (=) eee(.*)eee").unwrap();
        let unique_colors = false;
        let replaced = colorize_string_with_regex(sample, &regex, unique_colors);
        let expected = format!(
            "\"name {}={} eee{}start_benchmark{}eee\"",
            "\x1b[38;2;165;86;20m",
            FOREGROUND_DEFAULT,
            "\x1b[38;2;140;144;179m",
            FOREGROUND_DEFAULT
        );
        assert_eq!(expected, replaced);
    }
}
