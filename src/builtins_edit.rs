use std::cmp::Ordering;
use std::collections::HashMap;
use std::env;
use std::hash::BuildHasher;
use std::io::{self, ErrorKind};

use sl_liner::vi::AlphanumericAndVariableKeywordRule;
use sl_liner::{
    keymap, last_non_ws_char_was_not_backslash, Buffer, ColorClosure, Context, DefaultEditorRules,
    NewlineRule, Prompt, WordDivideRule,
};

use crate::builtins_util::*;
use crate::completions::*;
use crate::environment::*;
use crate::eval::*;
use crate::interner::*;
use crate::types::*;
use crate::ExpEnum::HashMap as NativeHashMap;

fn load_repl_settings(repl_settings: &Expression) -> ReplSettings {
    let mut ret = ReplSettings::default();
    if let NativeHashMap(repl_settings) = &repl_settings.get().data {
        if let Some(keybindings) = repl_settings.get(":keybindings") {
            if let ExpEnum::Symbol(keybindings, _) = &keybindings.get().data {
                match &keybindings[..] {
                    ":vi" => ret.key_bindings = Keys::Vi,
                    ":emacs" => ret.key_bindings = Keys::Emacs,
                    _ => eprintln!("Invalid keybinding setting: {}", keybindings),
                }
            };
        }
        if let Some(max) = repl_settings.get(":max-history") {
            if let ExpEnum::Int(max) = &max.get().data {
                if *max >= 0 {
                    ret.max_history = *max as usize;
                } else {
                    eprintln!("Max history must be positive: {}", max);
                }
            } else {
                eprintln!("Max history must be a positive integer: {}", max);
            };
        }
        if let Some(vi_esc) = repl_settings.get(":vi_esc_sequence") {
            let mut i = vi_esc.iter();
            if let Some(arg0) = i.next() {
                if let ExpEnum::String(keys, _) = &arg0.get().data {
                    if let Some(arg1) = i.next() {
                        if let ExpEnum::Int(ms) = &arg1.get().data {
                            if keys.len() == 2 {
                                let mut chars = keys.chars();
                                ret.vi_esc_sequence = Some((
                                    chars.next().unwrap(),
                                    chars.next().unwrap(),
                                    *ms as u32,
                                ));
                            } else {
                                eprintln!(":vi_esc_sequence first value should be a string of two characters (two key sequence for escape)");
                            }
                        } else {
                            eprintln!(":vi_esc_sequence second value should be number (ms delay)");
                        }
                    } else {
                        eprintln!(":vi_esc_sequence second value should be number (ms delay)");
                    }
                } else {
                    eprintln!(
                    ":vi_esc_sequence first value should be a string (two key sequence for escape)"
                );
                }
            } else {
                eprintln!(
                    ":vi_esc_sequence first value should be a string (two key sequence for escape)"
                );
            }
        }
        if let Some(prefix) = repl_settings.get(":vi-normal-prompt-prefix") {
            if let ExpEnum::String(prefix, _) = &prefix.get().data {
                ret.vi_normal_prompt_prefix = Some(prefix.to_string());
            };
        }
        if let Some(suffix) = repl_settings.get(":vi-normal-prompt-suffix") {
            if let ExpEnum::String(suffix, _) = &suffix.get().data {
                ret.vi_normal_prompt_suffix = Some(suffix.to_string());
            };
        }
        if let Some(prefix) = repl_settings.get(":vi-insert-prompt-prefix") {
            if let ExpEnum::String(prefix, _) = &prefix.get().data {
                ret.vi_insert_prompt_prefix = Some(prefix.to_string());
            };
        }
        if let Some(suffix) = repl_settings.get(":vi-insert-prompt-suffix") {
            if let ExpEnum::String(suffix, _) = &suffix.get().data {
                ret.vi_insert_prompt_suffix = Some(suffix.to_string());
            };
        }
    }
    ret
}

// Like the liner default but make '(' and ')' their own words for cleaner completions.
fn get_liner_words(buf: &Buffer) -> Vec<(usize, usize)> {
    let mut res = Vec::new();

    let mut word_start = None;
    let mut just_had_backslash = false;

    for (i, str) in buf.range_graphemes_all().enumerate() {
        if str == "\\" {
            just_had_backslash = true;
            continue;
        }

        if let Some(start) = word_start {
            if (str == " " || str == "(" || str == ")") && !just_had_backslash {
                res.push((start, i));
                if str == "(" || str == ")" {
                    res.push((i, i + 1));
                }
                word_start = None;
            }
        } else if str == "(" || str == ")" {
            res.push((i, i + 1));
        } else if str != " " {
            word_start = Some(i);
        }

        just_had_backslash = false;
    }

    if let Some(start) = word_start {
        res.push((start, buf.num_graphemes()));
    }

    res
}

fn apply_repl_settings(con: &mut Context, repl_settings: &ReplSettings) {
    let keymap: Box<dyn keymap::KeyMap> = match repl_settings.key_bindings {
        Keys::Vi => {
            let mut vi = keymap::Vi::new();
            let vi_keywords = vec!["_", "-"];
            vi.set_keyword_rule(Box::new(AlphanumericAndVariableKeywordRule::new(
                vi_keywords,
            )));
            if let Some((ch1, ch2, timeout)) = repl_settings.vi_esc_sequence {
                vi.set_esc_sequence(ch1, ch2, timeout);
            }
            vi.set_normal_prompt_prefix(repl_settings.vi_normal_prompt_prefix.clone());
            vi.set_normal_prompt_suffix(repl_settings.vi_normal_prompt_suffix.clone());
            vi.set_insert_prompt_prefix(repl_settings.vi_insert_prompt_prefix.clone());
            vi.set_insert_prompt_suffix(repl_settings.vi_insert_prompt_suffix.clone());
            Box::new(vi)
        }
        Keys::Emacs => Box::new(keymap::Emacs::new()),
    };
    con.set_keymap(keymap);
    con.history.set_max_history_size(repl_settings.max_history);
}

/// Delimiters like ( and ) are open and close respectively. delimiters like " are identical.
#[derive(Debug, Clone, Copy)]
enum DelimiterIdx {
    Identical(usize),
    Open(usize),
    Close(usize),
}

impl DelimiterIdx {
    fn idx(&self, other: &Self) -> (usize, usize) {
        let self_loc = match self {
            DelimiterIdx::Identical(i) => i,
            DelimiterIdx::Open(i) => i,
            DelimiterIdx::Close(i) => i,
        };
        let other_loc = match other {
            DelimiterIdx::Identical(i) => i,
            DelimiterIdx::Open(i) => i,
            DelimiterIdx::Close(i) => i,
        };
        (*self_loc, *other_loc)
    }
}

impl PartialEq<Self> for DelimiterIdx {
    fn eq(&self, other: &Self) -> bool {
        let (this, other) = self.idx(other);
        this.eq(&other)
    }
}

impl PartialOrd for DelimiterIdx {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let (this, other) = self.idx(other);
        this.partial_cmp(&other)
    }
}

fn order_sorted_lists<T, U, V>(open_indices: T, close_indices: T) -> Vec<U>
where
    U: AsRef<V> + Copy,
    T: IntoIterator<Item = U>,
    V: PartialOrd,
{
    let mut vec = vec![];
    let mut open_indices = open_indices.into_iter();
    let mut close_indices = close_indices.into_iter();
    let (mut open_curr, mut close_curr) = (open_indices.next(), close_indices.next());
    loop {
        match (open_curr, close_curr) {
            (Some(open), Some(close)) => {
                if open.as_ref() <= close.as_ref() {
                    vec.push(open);
                    open_curr = open_indices.next();
                } else {
                    vec.push(close);
                    close_curr = close_indices.next();
                }
            }
            (Some(open), None) => {
                vec.push(open);
                open_curr = open_indices.next();
            }
            (None, Some(close)) => {
                vec.push(close);
                close_curr = close_indices.next();
            }
            (None, None) => {
                break;
            }
        }
    }
    vec
}

#[derive(Debug, Copy, Clone)]
enum DelimiterType<'a> {
    MultilineComment(&'a str),
    StringDelimiter(&'a str),
    BalancedDelimiter(&'a str),
}

#[derive(Debug, Copy, Clone)]
struct Delimiter<'a> {
    delimiter_type: DelimiterType<'a>,
    delimiter_place: DelimiterIdx,
}

impl AsRef<DelimiterIdx> for DelimiterIdx {
    fn as_ref(&self) -> &DelimiterIdx {
        self
    }
}

impl AsRef<DelimiterIdx> for Delimiter<'_> {
    fn as_ref(&self) -> &DelimiterIdx {
        &self.delimiter_place
    }
}

fn get_delimiters<'a>(
    str: &'a str,
    pat: &'a str,
    to_type: &dyn Fn() -> DelimiterType<'a>,
    to_idx: &dyn Fn(usize) -> DelimiterIdx,
) -> Vec<Delimiter<'a>> {
    str.match_indices(pat)
        .map(|(i, _)| Delimiter {
            delimiter_type: to_type(),
            delimiter_place: to_idx(i),
        })
        .collect::<Vec<Delimiter>>()
}

fn get_indices_of_tokens<'a>(
    str: &'a str,
    delimiters: &[(&'a str, &'a str)],
    multiline_comment: &'a (&str, &str),
    string_delimiter: &'a str,
) -> Vec<Delimiter<'a>> {
    let (open, close) = multiline_comment;
    let open_indices = get_delimiters(str, open, &|| DelimiterType::MultilineComment(open), &|i| {
        DelimiterIdx::Open(i)
    });
    let close_indices = get_delimiters(
        str,
        close,
        &|| DelimiterType::MultilineComment(close),
        &DelimiterIdx::Close,
    );
    let multiline_idx = order_sorted_lists(open_indices, close_indices);
    let string_indices = get_delimiters(
        str,
        string_delimiter,
        &|| DelimiterType::StringDelimiter(string_delimiter),
        &DelimiterIdx::Identical,
    );
    let mut vec = order_sorted_lists(multiline_idx, string_indices);
    for (open, close) in delimiters {
        let open_indices = get_delimiters(
            str,
            open,
            &|| DelimiterType::BalancedDelimiter(open),
            &DelimiterIdx::Open,
        );
        let close_indices = get_delimiters(
            str,
            close,
            &|| DelimiterType::BalancedDelimiter(close),
            &DelimiterIdx::Close,
        );
        let ordered_balanced = order_sorted_lists(open_indices, close_indices);
        vec = order_sorted_lists(ordered_balanced, vec);
    }
    vec
}

fn map_right_to_left_delimiters<'a>(
    delimiters: &[(&'a str, &'a str)],
) -> HashMap<&'a str, &'a str> {
    let mut delim_map = HashMap::new();
    for (left, right) in delimiters {
        delim_map.insert(*right, *left);
    }
    delim_map
}

#[derive(PartialEq, Debug, Clone, Copy)]
enum DelimiterState {
    InStringDelimiter,
    InMultilineComment(usize),
    Outside,
}

fn should_count_delimiters(delimiter: Delimiter, state: DelimiterState) -> DelimiterState {
    match state {
        DelimiterState::InStringDelimiter => {
            if let DelimiterType::StringDelimiter(_) = delimiter.delimiter_type {
                DelimiterState::Outside
            } else {
                DelimiterState::InStringDelimiter
            }
        }
        DelimiterState::InMultilineComment(x) => {
            if let DelimiterType::MultilineComment(_) = delimiter.delimiter_type {
                match delimiter.delimiter_place {
                    DelimiterIdx::Open(_) => DelimiterState::InMultilineComment(x + 1),
                    DelimiterIdx::Close(_) => {
                        if x > 1 {
                            DelimiterState::InMultilineComment(x - 1)
                        } else {
                            DelimiterState::Outside
                        }
                    }
                    DelimiterIdx::Identical(_) => {
                        unreachable!()
                    }
                }
            } else {
                DelimiterState::InMultilineComment(1)
            }
        }
        DelimiterState::Outside => match (delimiter.delimiter_type, delimiter.delimiter_place) {
            (DelimiterType::MultilineComment(_), DelimiterIdx::Open(_)) => {
                DelimiterState::InMultilineComment(1)
            }
            (DelimiterType::StringDelimiter(_), DelimiterIdx::Identical(_)) => {
                DelimiterState::InStringDelimiter
            }
            _ => state,
        },
    }
}

/// ensures all provided delimiters are balanced.
fn check_balanced_delimiters(
    str: &str,
    delimiters: &[(&str, &str)],
    multiline_comment: &(&str, &str),
    string_delimiter: &str,
) -> bool {
    let delim_map = map_right_to_left_delimiters(delimiters);
    let all_delimiters =
        get_indices_of_tokens(str, delimiters, multiline_comment, string_delimiter);

    let mut open_delims = HashMap::new();

    let mut prev_state = DelimiterState::Outside;
    for delimiter in all_delimiters {
        let new_state = should_count_delimiters(delimiter, prev_state);
        if let DelimiterState::Outside = new_state {
            match (delimiter.delimiter_type, delimiter.delimiter_place) {
                (DelimiterType::BalancedDelimiter(str), DelimiterIdx::Open(_)) => {
                    if let Some(&count) = open_delims.get(str) {
                        open_delims.insert(str, count + 1);
                    } else {
                        open_delims.insert(str, 1);
                    }
                }
                (DelimiterType::BalancedDelimiter(str), DelimiterIdx::Close(_)) => {
                    let opposite = delim_map.get(str);
                    if let Some(opposite) = opposite {
                        if let Some(&count) = open_delims.get(opposite) {
                            if count == 1 {
                                open_delims.remove(opposite);
                            } else {
                                open_delims.insert(opposite, count - 1);
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        prev_state = new_state;
    }
    prev_state == DelimiterState::Outside && open_delims.is_empty()
}

pub struct NewlineForBackSlashAndSlShSyntaxRule<'a> {
    string_delimiter: &'a str,
    delimiters: Vec<(&'a str, &'a str)>,
    multiline_comment: (&'a str, &'a str),
}

impl NewlineRule for NewlineForBackSlashAndSlShSyntaxRule<'_> {
    fn evaluate_on_newline(&self, buf: &Buffer) -> bool {
        last_non_ws_char_was_not_backslash(buf)
            && check_balanced_delimiters(
                buf.range_graphemes_all().slice(),
                &self.delimiters,
                &self.multiline_comment,
                self.string_delimiter,
            )
    }
}

pub struct LinerWordDividerRule {}

impl WordDivideRule for LinerWordDividerRule {
    fn divide_words(&self, buf: &Buffer) -> Vec<(usize, usize)> {
        get_liner_words(buf)
    }
}

fn make_con(environment: &mut Environment, history: Option<&str>) -> Context {
    let mut con = Context::new();
    // Do this before a history file load...
    apply_repl_settings(&mut con, &environment.repl_settings);
    let delimiters = vec![("{", "}"), ("(", ")"), ("[", "]")];
    let multiline_comment = ("#|", "|#");
    let string_delimiter = "\"";
    let editor_rules = DefaultEditorRules::custom(
        LinerWordDividerRule {},
        NewlineForBackSlashAndSlShSyntaxRule {
            delimiters,
            string_delimiter,
            multiline_comment,
        },
    );
    con.set_editor_rules(Box::new(editor_rules));
    let mut home = match env::var("HOME") {
        Ok(val) => val,
        Err(_) => ".".to_string(),
    };
    if home.ends_with('/') {
        home = home[..home.len() - 1].to_string();
    }
    if let Some(history) = history {
        let history_file = if history.starts_with('/') || history.starts_with('.') {
            history.to_string()
        } else {
            format!("{}/.local/share/sl-sh/{}", home, history)
        };
        if let Err(err) = con.history.set_file_name_and_load_history(&history_file) {
            eprintln!(
                "WARNING: Unable to load history file {}: {}",
                history_file, err
            );
        }
    }
    con
}

fn get_color_closure(environment: &mut Environment) -> Option<ColorClosure> {
    let line_exp = get_from_namespace(environment, "__line_handler");
    if let Some(fn_exp) = line_exp {
        // This unsafe should be OK because the returned object is used in a call to read_line and
        // dropped after.
        let environment = unsafe { &mut *(environment as *mut Environment) };
        Some(Box::new(move |input: &str| -> String {
            let exp = match &fn_exp.get().data {
                ExpEnum::Lambda(_) => {
                    let v = vec![
                        fn_exp.clone(),
                        Expression::alloc_data(ExpEnum::String(input.to_string().into(), None)),
                    ];
                    Expression::with_list(v)
                }
                _ => return input.to_string(),
            };
            environment.save_exit_status = false; // Do not overwrite last exit status with line_handler.
            let res = eval(environment, exp);
            environment.save_exit_status = true;
            res.unwrap_or_else(|e| {
                Expression::alloc_data(ExpEnum::String(format!("ERROR: {}", e).into(), None))
            })
            .as_string(environment)
            .unwrap_or_else(|_| "ERROR".to_string())
        }))
    } else {
        None
    }
}

pub fn read_prompt(
    environment: &mut Environment,
    prompt: &str,
    history: Option<&str>,
    liner_id: &'static str,
) -> io::Result<String> {
    let repl_settings = lookup_expression(environment, "*repl-settings*").unwrap();
    let new_repl_settings = load_repl_settings(&repl_settings);
    let mut load_settings = if environment.repl_settings != new_repl_settings {
        environment.repl_settings = new_repl_settings.clone();
        true
    } else {
        false
    };
    let mut con = if liner_id == ":new" {
        load_settings = false;
        make_con(environment, history)
    } else if environment.liners.contains_key(liner_id) {
        environment.liners.remove(liner_id).unwrap()
    } else {
        load_settings = false;
        make_con(environment, history)
    };
    if load_settings {
        apply_repl_settings(&mut con, &new_repl_settings);
    };
    // This unsafe should be OK because the con object this is set into is
    // stored in the environment (or dropped at the end of this function)
    // so environment should out live con.
    let env = unsafe { &mut *(environment as *mut Environment) };
    con.set_completer(Box::new(ShellCompleter::new(env)));
    let result = match con.read_line(Prompt::from(prompt), get_color_closure(environment)) {
        Ok(input) => {
            let input = input.trim();
            Ok(input.into())
        }
        Err(err) => Err(err),
    };
    if liner_id != ":new" {
        environment.liners.insert(liner_id, con);
    };
    result
}

fn builtin_prompt(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let (liner_id, prompt) = {
        let arg1 = param_eval(environment, args, "prompt")?;
        let arg_d = arg1.get();
        if let ExpEnum::Symbol(s, _) = arg_d.data {
            (s, param_eval(environment, args, "prompt")?)
        } else {
            drop(arg_d);
            (":new", arg1)
        }
    };
    let h_str;
    let history_file = if let Some(h) = args.next() {
        let hist = eval(environment, h)?;
        let hist_d = hist.get();
        if let ExpEnum::String(s, _) = &hist_d.data {
            h_str = match expand_tilde(s) {
                Some(p) => p,
                None => s.to_string(),
            };
            Some(&h_str[..])
        } else {
            return Err(LispError::new(
                "prompt: history file (if provided) must be a string.",
            ));
        }
    } else {
        None
    };
    params_done(args, "prompt")?;
    let prompt_d = prompt.get();
    if let ExpEnum::String(s, _) = &prompt_d.data {
        return match read_prompt(environment, s, history_file, liner_id) {
            Ok(input) => Ok(Expression::alloc_data(ExpEnum::String(input.into(), None))),
            Err(err) => match err.kind() {
                ErrorKind::UnexpectedEof => {
                    let input = Expression::alloc_data(ExpEnum::String("".into(), None));
                    let error =
                        Expression::alloc_data(ExpEnum::Symbol(":unexpected-eof", SymLoc::None));
                    Ok(Expression::alloc_data(ExpEnum::Values(vec![input, error])))
                }
                ErrorKind::Interrupted => {
                    let input = Expression::alloc_data(ExpEnum::String("".into(), None));
                    let error =
                        Expression::alloc_data(ExpEnum::Symbol(":interrupted", SymLoc::None));
                    Ok(Expression::alloc_data(ExpEnum::Values(vec![input, error])))
                }
                _ => {
                    eprintln!("Error on input: {}", err);
                    Err(LispError::new("Unexpected input error!"))
                }
            },
        };
    }
    Err(LispError::new(
        "prompt: requires a prompt string and option history file.",
    ))
}

fn get_liner_id(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
    form: &str,
) -> Result<&'static str, LispError> {
    let arg = param_eval(environment, args, form)?;
    let arg_d = arg.get();
    if let ExpEnum::Symbol(s, _) = arg_d.data {
        Ok(s)
    } else {
        Err(LispError::new(format!(
            "{}: context id must be a keyword.",
            form
        )))
    }
}

fn builtin_history_push(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let liner_id = get_liner_id(environment, args, "history-push")?;
    let item = {
        let arg = param_eval(environment, args, "history-push")?;
        let arg_d = arg.get();
        if let ExpEnum::String(s, _) = &arg_d.data {
            s.to_string()
        } else {
            return Err(LispError::new(
                "history-push: history item must be a string.",
            ));
        }
    };
    params_done(args, "history-push")?;
    let mut con = if environment.liners.contains_key(liner_id) {
        environment.liners.remove(liner_id).unwrap()
    } else {
        return Err(LispError::new("history-push: context id not found."));
    };
    let result = if let Err(err) = con.history.push(item) {
        eprintln!("Warning: failed to save history: {}", err);
        Ok(Expression::make_nil())
    } else {
        Ok(Expression::make_true())
    };
    environment.liners.insert(liner_id, con);
    result
}

fn builtin_history_push_throwaway(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let liner_id = get_liner_id(environment, args, "history-push-throwaway")?;
    let item = {
        let arg = param_eval(environment, args, "history-push-throwaway")?;
        let arg_d = arg.get();
        if let ExpEnum::String(s, _) = &arg_d.data {
            s.to_string()
        } else {
            return Err(LispError::new(
                "history-push-throwaway: history item must be a string.",
            ));
        }
    };
    params_done(args, "history-push-throwaway")?;
    let mut con = if environment.liners.contains_key(liner_id) {
        environment.liners.remove(liner_id).unwrap()
    } else {
        return Err(LispError::new(
            "history-push-throwaway: context id not found.",
        ));
    };
    let result = if let Err(err) = con.history.push_throwaway(item) {
        eprintln!("Warning: failed to save temp history: {}", err);
        Ok(Expression::make_nil())
    } else {
        Ok(Expression::make_true())
    };
    environment.liners.insert(liner_id, con);
    result
}

fn builtin_history_context(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let liner_id = get_liner_id(environment, args, "history-push-throwaway")?;
    let item = {
        let arg = param_eval(environment, args, "history-context")?;
        let arg_d = arg.get();
        match &arg_d.data {
            ExpEnum::String(s, _) => Some(s.to_string()),
            ExpEnum::Nil => None,
            _ => {
                return Err(LispError::new(
                    "history-context: history context item must be a string.",
                ))
            }
        }
    };
    params_done(args, "history-context")?;
    let mut con = if environment.liners.contains_key(liner_id) {
        environment.liners.remove(liner_id).unwrap()
    } else {
        return Ok(Expression::make_nil());
    };
    con.history.set_search_context(item);
    environment.liners.insert(liner_id, con);
    Ok(Expression::make_nil())
}

fn builtin_history_length(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let liner_id = get_liner_id(environment, args, "history-length")?;
    params_done(args, "history-length")?;
    let con = if environment.liners.contains_key(liner_id) {
        environment.liners.remove(liner_id).unwrap()
    } else {
        return Err(LispError::new("history-length: context id not found."));
    };
    let result = Ok(Expression::alloc_data(ExpEnum::Int(
        con.history.len() as i64
    )));
    environment.liners.insert(liner_id, con);
    result
}

fn builtin_history_is_empty(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let liner_id = get_liner_id(environment, args, "history-length")?;
    params_done(args, "history-length")?;
    let con = if environment.liners.contains_key(liner_id) {
        environment.liners.remove(liner_id).unwrap()
    } else {
        return Err(LispError::new("history-length: context id not found."));
    };
    let result = if con.history.is_empty() {
        Ok(Expression::make_true())
    } else {
        Ok(Expression::make_nil())
    };
    environment.liners.insert(liner_id, con);
    result
}

fn builtin_history_nth(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let liner_id = get_liner_id(environment, args, "history-nth")?;
    let idx = {
        let arg = param_eval(environment, args, "history-nth")?;
        let arg_d = arg.get();
        if let ExpEnum::Int(idx) = &arg_d.data {
            *idx
        } else {
            return Err(LispError::new("history-nth: history nth must be an int."));
        }
    };
    params_done(args, "history-nth")?;
    let con = if environment.liners.contains_key(liner_id) {
        environment.liners.remove(liner_id).unwrap()
    } else {
        return Err(LispError::new("history-nth: context id not found."));
    };
    if idx < 0 || idx as usize >= con.history.len() {
        environment.liners.insert(liner_id, con);
        return Err(LispError::new("history-nth: index out of bounds."));
    }
    let idx: usize = con.history.len() - (idx as usize) - 1;
    let result = Ok(Expression::alloc_data(ExpEnum::String(
        con.history[idx].to_string().into(),
        None,
    )));
    environment.liners.insert(liner_id, con);
    result
}

pub fn add_edit_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, (Expression, String), S>,
) {
    data.insert(
        interner.intern("prompt"),
        Expression::make_function(
            builtin_prompt,
            "Usage: (prompt string) -> string

Starts an interactive prompt (like the repl prompt) with the supplied prompt and
returns the input string.

Section: shell

Example:
;(def input-string (prompt \"prompt> \"))
#t
",
        ),
    );
    data.insert(
        interner.intern("history-push"),
        Expression::make_function(
            builtin_history_push,
            "Usage: (history-push :context_id string) -> nil/t

Pushes string onto the history for the prompt context :context_id.
Returns true on success or nil on failure.

Section: shell

Example:
;(history-push :repl \"Some command\")
#t
",
        ),
    );
    data.insert(
        interner.intern("history-push-throwaway"),
        Expression::make_function(
            builtin_history_push_throwaway,
            "Usage: (history-push-throwaway :context_id string) -> nil/t

Pushes string onto the history for the prompt context :context_id.  A throwaway
item will will only persist until the next command is read (use it to allow
editing of failed commands without them going into history).
Returns true on success or nil on failure.

Section: shell

Example:
;(history-push-throwaway :repl \"Some broken command\")
#t
",
        ),
    );
    data.insert(
        interner.intern("history-context"),
        Expression::make_function(
            builtin_history_context,
            "Usage: (history-context :context_id context-string) -> nil

Sets the history context for searches.  Usually the current path but can be any
string.  Pass nil to set it to nothing.

Section: shell

Example:
;(history-context :repl \"/home\")
#t
",
        ),
    );
    data.insert(
        interner.intern("history-length"),
        Expression::make_function(
            builtin_history_length,
            "Usage: (history-length :context_id) -> int

Returns the number of history items for the given context.

Section: shell

Example:
;(history-length :repl)
#t
",
        ),
    );
    data.insert(
        interner.intern("history-empty?"),
        Expression::make_function(
            builtin_history_is_empty,
            "Usage: (history-empty? :context_id) -> t/nil

Returns true if history for context_id is empty, nil otherwise.

Section: shell

Example:
;(history-empty? :repl)
#t
",
        ),
    );
    data.insert(
        interner.intern("history-nth"),
        Expression::make_function(
            builtin_history_nth,
            "Usage: (history-nth :context_id nth) -> String

Returns the history at index nth (the newest is 0).

Section: shell

Example:
;(history-nth :repl 0)
#t
",
        ),
    );
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn test_check_balanced_delimiters() {
        let string_delimiter = "\"";
        let multiline_comment = ("#|", "|#");
        let delimiters = vec![("(", ")"), ("[", "]"), ("{", "}")];
        let buf =
            Buffer::from("(ok #| outside (double quotes\" inside) double_quotes|#\" and )this.");
        let is_balanced = check_balanced_delimiters(
            buf.range_graphemes_all().slice(),
            &delimiters,
            &multiline_comment,
            string_delimiter,
        );
        assert!(!is_balanced);

        let buf = Buffer::from("outside double quotes\" inside double_quotes\" and this.");
        let is_balanced = check_balanced_delimiters(
            buf.range_graphemes_all().slice(),
            &delimiters,
            &multiline_comment,
            string_delimiter,
        );
        assert!(is_balanced);

        let buf = Buffer::from("outside double quotes\" inside double_quotes\" ok \"");
        let is_balanced = check_balanced_delimiters(
            buf.range_graphemes_all().slice(),
            &delimiters,
            &multiline_comment,
            string_delimiter,
        );
        assert!(!is_balanced);

        let buf = Buffer::from("ok now here \" this is #| |# |# #| #| |# |# |#\" we go #| doing things in comments |# sometimes");
        let is_balanced = check_balanced_delimiters(
            buf.range_graphemes_all().slice(),
            &delimiters,
            &multiline_comment,
            string_delimiter,
        );
        assert!(is_balanced);

        let buf = Buffer::from("#|ok now here \" this is #| |# |# #| #| |# |# |#\" we go #| doing things in comments |# sometimes");
        let is_balanced = check_balanced_delimiters(
            buf.range_graphemes_all().slice(),
            &delimiters,
            &multiline_comment,
            string_delimiter,
        );
        assert!(!is_balanced);

        let buf = Buffer::from("))))) ( [ [ [ } { ] ] } ) ]");
        let is_balanced = check_balanced_delimiters(
            buf.range_graphemes_all().slice(),
            &delimiters,
            &multiline_comment,
            string_delimiter,
        );
        assert!(is_balanced);

        let buf = Buffer::from("))))) ( [ [ [ } \"{ ] ] } ) ] ] )\" }})) [ }}} ]");
        let is_balanced = check_balanced_delimiters(
            buf.range_graphemes_all().slice(),
            &delimiters,
            &multiline_comment,
            string_delimiter,
        );
        assert!(!is_balanced);
    }
}
