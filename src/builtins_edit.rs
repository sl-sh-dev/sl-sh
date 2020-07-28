use std::collections::HashMap;
use std::env;
use std::hash::BuildHasher;
use std::io;

use liner::{Context, Prompt};

use crate::builtins_util::*;
use crate::environment::*;
use crate::eval::*;
use crate::interner::*;
use crate::shell::apply_repl_settings;
use crate::shell::get_liner_words;
use crate::types::*;

pub fn read_prompt(
    environment: &mut Environment,
    prompt: &str,
    history: Option<&str>,
) -> io::Result<String> {
    let mut con = Context::new();
    apply_repl_settings(&mut con, &environment.repl_settings);
    con.set_word_divider(Box::new(get_liner_words));
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
    //con.set_completer(Box::new(ShellCompleter::new(environment.clone())));
    match con.read_line(Prompt::from(prompt), None) {
        Ok(input) => {
            let input = input.trim();
            if history.is_some() {
                if let Err(err) = con.history.push(input) {
                    eprintln!("read-line: Error saving history: {}", err);
                }
            }
            Ok(input.into())
        }
        Err(err) => Err(io::Error::new(io::ErrorKind::Other, err)),
    }
}

fn builtin_read_prompt(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    let prompt = param_eval(environment, args, "read-prompt")?;
    let h_str;
    let history_file = if let Some(h) = args.next() {
        let hist = eval(environment, h)?;
        let hist_d = hist.get();
        if let ExpEnum::Atom(Atom::String(s, _)) = &hist_d.data {
            h_str = match expand_tilde(s) {
                Some(p) => p,
                None => s.to_string(),
            };
            Some(&h_str[..])
        } else {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "read-prompt: history file (if provided) must be a string.",
            ));
        }
    } else {
        None
    };
    params_done(args, "read-prompt")?;
    let prompt_d = prompt.get();
    if let ExpEnum::Atom(Atom::String(s, _)) = &prompt_d.data {
        let input = read_prompt(environment, s, history_file)?;
        return Ok(Expression::alloc_data(ExpEnum::Atom(Atom::String(
            input.into(),
            None,
        ))));
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "read-prompt: requires a prompt string and option history file.",
    ))
}

pub fn add_edit_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, Reference, S>,
) {
    let root = interner.intern("root");
    data.insert(
        interner.intern("read-prompt"),
        Expression::make_function(
            builtin_read_prompt,
            "Usage: (read-prompt string) -> string

Starts an interactive prompt (like the repl prompt) with the supplied prompt and
returns the input string.

Section: file

Example:
;(def 'input-string (read-prompt \"prompt> \"))
t
",
            root,
        ),
    );
}
