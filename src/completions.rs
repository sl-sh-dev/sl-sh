use liner::{Completer, FilenameCompleter};
use std::env;

pub struct ShellCompleter;

impl Completer for ShellCompleter {
    fn completions(&mut self, start: &str) -> Vec<String> {
        match env::current_dir() {
            Ok(p) => {
                let mut fc = FilenameCompleter::new(Some(p));
                fc.completions(start)
            }
            Err(err) => {
                eprintln!("Error getting current working directory: {}", err);
                Vec::new()
            }
        }
    }
}
