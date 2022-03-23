use std::collections::{HashMap, HashSet};
use std::env;
use std::hash::BuildHasher;
use std::io::{self, ErrorKind};
use std::iter::FromIterator;

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

#[derive(PartialOrd, Ord, PartialEq, Eq, Debug, Clone, Copy)]
enum DelimiterType {
    Open(usize),
    Close(usize),
}

/// checks buf to see if the count of the provided string_delimiter is even
fn has_balanced_string_delimiter(buf: &Buffer, string_delimiter: &str) -> bool {
    (buf.range_graphemes_all()
        .slice()
        .match_indices(string_delimiter)
        .count()
        % 2)
        == 0
}

fn order_delimiters_in_str(str: &str, open: &str, close: &str) -> Vec<DelimiterType> {
    let mut vec = vec![];
    let mut open_indices = str.match_indices(open).map(|(i, _)| DelimiterType::Open(i));
    let mut close_indices = str
        .match_indices(close)
        .map(|(i, _)| DelimiterType::Close(i));
    let (mut open_curr, mut close_curr) = (open_indices.next(), close_indices.next());
    loop {
        match (open_curr, close_curr) {
            (Some(open), Some(close)) => {
                if open <= close {
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

/// Checks buf to see if vec of multi grapheme cluster &str pairs are balanced. Precondition for
/// evaluation ensures that string_delimiter is balanced, as only delimiter pairs outside
/// string_delimiter are searched.
fn has_balanced_multi_delimiters(
    buf: &Buffer,
    multiline_delimiters: &[(&str, &str)],
    string_delimiter: &str,
) -> bool {
    if !has_balanced_string_delimiter(buf, string_delimiter) {
        false
    } else {
        let filtered_str = buf
            .range_graphemes_all()
            .slice()
            .split(string_delimiter)
            .step_by(2)
            .collect::<String>();
        let mut count = 0;
        for delimiter_pair in multiline_delimiters {
            let (open, close) = delimiter_pair;
            let vec = order_delimiters_in_str(&filtered_str, open, close);
            for delim in vec {
                match delim {
                    DelimiterType::Open(_) => {
                        count += 1;
                    }
                    DelimiterType::Close(_) => {
                        if count > 0 {
                            count -= 1;
                        }
                    }
                }
            }
        }
        count == 0
    }
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

/// Checks buf to see if vec of grapheme cluster &str pairs are balanced. Precondition for
/// evaluation ensures that string_delimiter is balanced, as only delimiter pairs outside
/// string_delimiter are searched.
fn has_balanced_delimiters(
    buf: &Buffer,
    delimiters: &[(&str, &str)],
    string_delimiter: &str,
) -> bool {
    let delim_map = map_right_to_left_delimiters(delimiters);
    let left_delimiters: HashSet<&str> =
        HashSet::from_iter(delimiters.iter().map(|(left, _)| *left));
    let right_delimiters: HashSet<&str> =
        HashSet::from_iter(delimiters.iter().map(|(_, right)| *right));
    let mut open_delims = HashMap::new();
    let mut outside_string_delimiter = true;
    for str in buf.range_graphemes_all() {
        match str {
            str if outside_string_delimiter && left_delimiters.contains(str) => {
                if let Some(&count) = open_delims.get(str) {
                    open_delims.insert(str, count + 1);
                } else {
                    open_delims.insert(str, 1);
                }
            }
            str if outside_string_delimiter && right_delimiters.contains(str) => {
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
            _ if str == string_delimiter => {
                outside_string_delimiter = !outside_string_delimiter;
            }
            _ => {}
        }
    }
    outside_string_delimiter && open_delims.is_empty()
}

/// ensures all provided delimiters are balanced.
pub fn check_balanced_delimiters(
    buf: &Buffer,
    string_delimiter: &str,
    delimiters: &[(&str, &str)],
    multiline_delimiters: &[(&str, &str)],
) -> bool {
    let has_balanced_delimiters = has_balanced_delimiters(buf, delimiters, string_delimiter);
    let has_balanced_multi_delimiters =
        has_balanced_multi_delimiters(buf, multiline_delimiters, string_delimiter);
    has_balanced_delimiters && has_balanced_multi_delimiters
}

pub struct NewlineForBackslashAndOpenDelimRule<'a> {
    string_delimiter: &'a str,
    delimiters: Vec<(&'a str, &'a str)>,
    multichar_delimiters: Vec<(&'a str, &'a str)>,
}

impl NewlineRule for NewlineForBackslashAndOpenDelimRule<'_> {
    fn evaluate_on_newline(&self, buf: &Buffer) -> bool {
        last_non_ws_char_was_not_backslash(buf)
            && check_balanced_delimiters(
                buf,
                self.string_delimiter,
                &self.delimiters,
                &self.multichar_delimiters,
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
    let multichar_delimiters = vec![("#|", "|#")];
    let string_delimiter = "\"";
    let editor_rules = DefaultEditorRules::custom(
        LinerWordDividerRule {},
        NewlineForBackslashAndOpenDelimRule {
            delimiters,
            string_delimiter,
            multichar_delimiters,
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
    fn test_has_balanced_delimiters() {
        let string_delimiter = "\"";
        let vec = vec![("(", ")"), ("[", "]"), ("{", "}")];

        let buf = Buffer::from("))))) ( [ [ [ } { ] ] } ) ]");
        let ret = has_balanced_delimiters(&buf, &vec, string_delimiter);
        assert!(ret);

        let buf = Buffer::from("))))) ( [ [ [ } \"{ ] ] } ) ] ] )\" }})) [ }}} ]");
        let ret = has_balanced_delimiters(&buf, &vec, string_delimiter);
        assert!(!ret);
    }

    #[test]
    fn test_has_balanced_multi_delimiters() {
        let string_delimiter = "\"";
        let vec = vec![("#|", "|#")];

        let buf = Buffer::from("ok now here \" this is #| |# |# #| #| |# |# |#\" we go #| doing things in comments |# sometimes");
        let ret = has_balanced_multi_delimiters(&buf, &vec, string_delimiter);
        assert!(ret);

        let buf = Buffer::from("#|ok now here \" this is #| |# |# #| #| |# |# |#\" we go #| doing things in comments |# sometimes");
        let ret = has_balanced_multi_delimiters(&buf, &vec, string_delimiter);
        assert!(!ret);
    }

    #[test]
    fn test_string_delimiter_balanced() {
        let string_delimiter = "\"";
        let buf = Buffer::from("outside double quotes\" inside double_quotes\" and this.");
        assert!(has_balanced_string_delimiter(buf, string_delimiter));

        let buf = Buffer::from("outside double quotes\" inside double_quotes\" ok \"");
        assert!(!has_balanced_string_delimiter(buf, string_delimiter));
    }
}
