use glob::glob;
use liner::{Completer, CursorPosition, Event, EventKind, FilenameCompleter};
use std::env;

use crate::builtins_util::expand_tilde;
use crate::environment::*;
use crate::types::*;

#[derive(Debug)]
enum CompType {
    Nothing,
    Command,
    CommandParen,
    EnvVar,
    Symbols,
    Other,
}

pub struct ShellCompleter<'a> {
    environment: &'a Environment<'a>,
    comp_type: CompType,
}

impl<'a> ShellCompleter<'a> {
    pub fn new(environment: &'a Environment<'a>) -> ShellCompleter<'a> {
        ShellCompleter {
            environment,
            comp_type: CompType::Nothing,
        }
    }
}

impl<'a> Completer for ShellCompleter<'a> {
    fn completions(&mut self, start: &str) -> Vec<String> {
        match self.comp_type {
            CompType::Nothing => Vec::new(),
            CompType::Command => {
                let mut ret = get_dir_matches(start);
                find_lisp_fns(self.environment, &mut ret, start);
                find_exes(&mut ret, start);
                ret
            }
            CompType::CommandParen => {
                let mut ret: Vec<String> = Vec::new();
                find_lisp_fns(self.environment, &mut ret, start);
                find_exes(&mut ret, start);
                ret
            }
            CompType::EnvVar => {
                let env_start = if start.starts_with('$') {
                    &start[1..]
                } else {
                    start
                };
                let mut ret: Vec<String> = Vec::new();
                for (key, _value) in env::vars() {
                    if key.starts_with(env_start) {
                        ret.push(format!("${}", key));
                    }
                }
                ret
            }
            CompType::Symbols => {
                let mut ret: Vec<String> = Vec::new();
                find_lisp_symbols(self.environment, &mut ret, start);
                ret
            }
            CompType::Other => {
                let mut ret = get_dir_matches(start);
                find_lisp_symbols(self.environment, &mut ret, start);
                ret
            }
        }
    }

    fn on_event<W: std::io::Write>(&mut self, event: Event<'_, '_, W>) {
        if let EventKind::BeforeComplete = event.kind {
            let (words, pos) = event.editor.get_words_and_cursor_position();
            self.comp_type = match pos {
                _ if words.is_empty() => CompType::Nothing,
                CursorPosition::InWord(0) => CompType::Command,
                CursorPosition::OnWordRightEdge(index) => {
                    if index == 0 {
                        CompType::Command
                    } else {
                        let word_limits = words.into_iter().nth(index);
                        let is_form_start = word_limits
                            .map(|(start, end)| event.editor.current_buffer().range(start, end))
                            .filter(|filename| filename.starts_with('('))
                            .is_some();
                        if is_form_start {
                            CompType::CommandParen
                        } else {
                            let is_env_var = word_limits
                                .map(|(start, end)| event.editor.current_buffer().range(start, end))
                                .filter(|filename| filename.starts_with('$'))
                                .is_some();
                            if is_env_var {
                                CompType::EnvVar
                            } else {
                                let is_symbol = word_limits
                                    .map(|(start, end)| {
                                        event.editor.current_buffer().range(start, end)
                                    })
                                    .filter(|filename| filename.starts_with('\''))
                                    .is_some();
                                if is_symbol {
                                    CompType::Symbols
                                } else {
                                    CompType::Other
                                }
                            }
                        }
                    }
                }
                _ => CompType::Other,
            };
        }
    }
}

fn get_dir_matches(start: &str) -> Vec<String> {
    match env::current_dir() {
        Ok(p) => {
            let mut fc = FilenameCompleter::new(Some(p));
            match expand_tilde(start) {
                Some(s) => fc.completions(&s),
                None => fc.completions(start),
            }
        }
        Err(_err) => Vec::new(),
    }
}

fn find_lisp_fns(environment: &Environment, comps: &mut Vec<String>, org_start: &str) {
    let (start, need_paren) = if org_start.starts_with('(') {
        (&org_start[1..], true)
    } else {
        (org_start, false)
    };
    for key in environment.data.keys() {
        if key.starts_with(start) {
            let val = if need_paren {
                format!("({}", key)
            } else {
                key.to_string()
            };
            match environment.data.get(key).unwrap() {
                Expression::Func(_) => comps.push(val),
                Expression::Atom(Atom::Lambda(_)) => comps.push(val),
                Expression::Atom(Atom::Macro(_)) => comps.push(val),
                _ => {}
            }
        }
    }
    if let Some(env) = environment.outer {
        find_lisp_fns(env, comps, org_start);
    }
}

fn find_lisp_symbols(environment: &Environment, comps: &mut Vec<String>, org_start: &str) {
    let (start, need_quote) = if org_start.starts_with('\'') {
        (&org_start[1..], true)
    } else {
        (org_start, false)
    };
    for key in environment.data.keys() {
        if key.starts_with(start) {
            let val = if need_quote {
                format!("'{}", key)
            } else {
                key.to_string()
            };
            match environment.data.get(key).unwrap() {
                Expression::Atom(Atom::Lambda(_)) => {}
                Expression::Atom(Atom::Macro(_)) => {}
                Expression::Func(_) => {}
                _ => {
                    comps.push(val);
                }
            }
        }
    }
    if let Some(env) = environment.outer {
        find_lisp_fns(env, comps, org_start);
    }
}

fn find_exes(comps: &mut Vec<String>, start: &str) {
    let (start, need_paren) = if start.starts_with('(') {
        (&start[1..], true)
    } else {
        (start, false)
    };
    let paths = if let Some(paths) = env::var_os("PATH") {
        env::split_paths(&paths)
            .map(|s| {
                if !s.to_string_lossy().ends_with('/') {
                    let mut oss = s.into_os_string();
                    oss.push("/");
                    oss.into()
                } else {
                    s
                }
            })
            .collect()
    } else {
        Vec::new()
    };

    for p in paths {
        if let Some(p) = p.to_str() {
            let pat = format!("{}*", p);
            match glob(&pat) {
                Ok(paths) => {
                    for p in paths {
                        match p {
                            Ok(p) => {
                                if let Some(p) = p.file_name() {
                                    if let Some(p) = p.to_str() {
                                        if p.starts_with(start) {
                                            let com = if need_paren {
                                                format!("({}", p)
                                            } else {
                                                p.to_string()
                                            };
                                            comps.push(com);
                                        }
                                    }
                                }
                            }
                            Err(_err) => {}
                        }
                    }
                }
                Err(_err) => {}
            }
        }
    }
}
