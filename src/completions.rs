use liner::Completer;

pub struct ShellCompleter;

impl Completer for ShellCompleter {
    fn completions(&mut self, _start: &str) -> Vec<String> {
        Vec::new()
    }
}
