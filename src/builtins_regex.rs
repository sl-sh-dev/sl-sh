use regex::{Captures, Regex};
use std::collections::HashMap;
use std::hash::{BuildHasher, Hash, Hasher};

use crate::environment::*;
use crate::interner::*;
use crate::types::*;
use crate::{param_eval, params_done};
use std::collections::hash_map::DefaultHasher;

const FOREGROUND_DEFUALT: &str = "\x1b[39m";

fn rgb(r: u8, g: u8, b: u8) -> String {
    format!("\x1b[38;2;{};{};{}m", r, g, b)
}

fn color(str: &str, capture_group: usize) -> String {
    // can create unique colors for up to 32 capture groups
    // given the bits from the str hash. as implemented means
    // that identical values for the 1st and 33rd capture groups
    // will have the same color.
    let shift = capture_group % 32;
    let mut s = DefaultHasher::new();
    str.hash(&mut s);
    let hash = s.finish();
    let r = (hash >> shift & 0xFF) as u8;
    let g = (hash >> (shift + 8) & 0xFF) as u8;
    let b = (hash >> (shift + 16) & 0xFF) as u8;
    rgb(r, g, b)
}

fn colorize_capture(str: &str, capture_group: usize, unique_colors: bool) -> String {
    let mut capture_group = capture_group;
    if !unique_colors {
        capture_group = 0;
    }
    format!("{}{}{}", color(str, capture_group), str, FOREGROUND_DEFUALT)
}

fn matches(sample: &str, regex: &Regex) -> Expression {
    if regex.is_match(sample) {
        Expression::make_true()
    } else {
        Expression::make_false()
    }
}

fn captures_to_array(caps: &Captures) -> Vec<Expression> {
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
        Expression::alloc_data(ExpEnum::Vector(captures_to_array(&caps)))
    } else {
        Expression::make_nil()
    }
}
fn get_regex_captures(sample: &str, regex: &Regex) -> Vec<Expression> {
    let mut capture_groups: Vec<Expression> = Vec::new();
    for caps in regex.captures_iter(sample) {
        let captures = captures_to_array(&caps);
        capture_groups.push(Expression::alloc_data(ExpEnum::Vector(captures)));
    }
    capture_groups
}

fn colorize_string_with_regex(sample: &str, regex: &Regex, unique_colors: bool) -> String {
    let mut offset = 0;
    regex
        .replace_all(sample, |caps: &Captures| {
            if caps.len() > 1 {
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
                let mut strings = vec![];
                let mut last_end: Option<usize> = None;
                let capture = String::from(&caps[0]);
                for (idx, (start, end)) in offsets.iter().enumerate() {
                    let slice = &capture[*start..*end];
                    if idx == 0 && *start == 0 {
                        strings.push(colorize_capture(slice, idx, unique_colors));
                    } else if idx == 0 {
                        let begin = &capture[0..*start];
                        strings.push(begin.to_owned());
                        strings.push(colorize_capture(slice, idx, unique_colors));
                    } else if let Some(last_end) = last_end {
                        let begin = &capture[last_end..*start];
                        strings.push(begin.to_owned());
                        strings.push(colorize_capture(slice, idx, unique_colors));
                    }
                    last_end = Some(*end);
                }
                strings.join("")
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
            "{} takes a string, a regex, and a replacement string",
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
            "{} takes a string and a regex",
            fn_name
        ))),
    }
}

fn builtin_regex_search(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let fn_name = "re-search";
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
            "{} takes a string and a regex",
            fn_name
        ))),
    }
}

fn builtin_regex_search_all(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let fn_name = "re-search-all";
    let regex = param_eval(environment, args, fn_name)?;
    let regex = &regex.get().data;
    let string = param_eval(environment, args, fn_name)?;
    let string = &string.get().data;
    params_done(args, fn_name)?;
    match (regex, string) {
        (ExpEnum::String(regex, _), ExpEnum::String(string, _)) => {
            let regex = Regex::new(regex);
            match regex {
                Ok(regex) => Ok(Expression::alloc_data(ExpEnum::Vector(get_regex_captures(
                    string, &regex,
                )))),
                Err(e) => Err(LispError::new(format!(
                    "{} requires a valid regular expression.\n{}",
                    fn_name, e
                ))),
            }
        }
        (ExpEnum::Regex(regex), ExpEnum::String(string, _)) => Ok(Expression::alloc_data(
            ExpEnum::Vector(get_regex_captures(string, regex)),
        )),
        (_, _) => Err(LispError::new(format!(
            "{} takes a string and a regex",
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
            "{} takes a string and a regex",
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

Given a valid regex as a string return a sl-sh Regex.

Section: regex

Example:
#f
"#,
        ),
    );
    data.insert(
        interner.intern("re-replace"),
        Expression::make_function(
            builtin_regex_replace,
            r#"Usage: (re-replace regex string replacement) -> t/nil

Given a regex, a string, and a replacement string

Section: regex

Example:
#f
"#,
        ),
    );
    data.insert(
        interner.intern("re-match"),
        Expression::make_function(
            builtin_regex_match,
            r#"Usage: (re-match regex string) -> boolean?

Section: regex

Example:
#f
"#,
        ),
    );
    data.insert(
        interner.intern("re-search"),
        Expression::make_function(
            builtin_regex_search,
            r#"Usage: (re-search regex string) -> #([String, ..])

Section: regex

Example:
#f
"#,
        ),
    );
    data.insert(
        interner.intern("re-search-all"),
        Expression::make_function(
            builtin_regex_search_all,
            r#"Usage: (re-search-all regex string) -> #([String, ..])

Section: regex

Example:
#f
"#,
        ),
    );
    data.insert(
        interner.intern("re-color"),
        Expression::make_function(
            builtin_regex_color,
            r#"Usage: (re-color regex string) -> t/nil

Given a regex and a string, colorize the portions of string that matches regex.
Colors are chosen deterministically based on the hash of the target strings's
value. If no capture groups are provided the whole string is colorized uniquely
based on its value. If capture groups are provided each group is uniquely
colorized.

Section: regex

Example:
#f
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
        let regex = "(\\d{4})-(\\d{2})-(\\d{2})";
        let regex = Regex::new(regex).unwrap();
        let unique_colors = false;
        let replaced = colorize_string_with_regex(sample, &regex, unique_colors);
        let expected = "\x1b[38;2;12;154;58m2020\x1b[39m-\x1b[38;2;103;93;109m20\x1b[39m-\x1b[38;2;103;93;109m20\x1b[39m and then again on \x1b[38;2;75;146;223m2021\x1b[39m-\x1b[38;2;103;93;109m20\x1b[39m-\x1b[38;2;217;27;83m18\x1b[39m but not on \x1b[38;2;12;154;58m2020\x1b[39m-\x1b[38;2;217;27;83m18\x1b[39m-\x1b[38;2;103;93;109m20\x1b[39m";
        assert_eq!(expected, replaced);
    }
}