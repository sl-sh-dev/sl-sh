use liner::{Completer, FilenameCompleter};
use std::env;

use crate::builtins_util::expand_tilde;

pub struct ShellCompleter;

impl Completer for ShellCompleter {
    fn completions(&mut self, start: &str) -> Vec<String> {
        match env::current_dir() {
            Ok(p) => {
                let mut fc = FilenameCompleter::new(Some(p));
                match expand_tilde(start) {
                    Some(s) => fc.completions(&s),
                    None => fc.completions(start),
                }
            }
            Err(err) => {
                eprintln!("Error getting current working directory: {}", err);
                Vec::new()
            }
        }
    }
}
