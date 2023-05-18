use crate::command_data::{CommandWithArgs, Redirects, Run};
use crate::unix::pipe;
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::Write;
use std::iter::Peekable;
use std::os::fd::FromRawFd;
use std::str::Chars;

/// Type of the current sequence being parsed.
#[derive(Copy, Clone, Debug)]
enum SeqType {
    Command,
    Pipe,
    Sequence,
    And,
    Or,
}

/// A list of commands to run in a pipe as a job.
#[derive(Clone, Debug)]
pub struct ParsedJob {
    // Only wrapped in an optional to use take() in push methods, must always be Some.
    commands: Option<Run>,
}

impl Display for ParsedJob {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(run) = &self.commands {
            write!(f, "{run}")?;
        }
        Ok(())
    }
}

impl ParsedJob {
    fn new() -> Self {
        Self {
            commands: Some(Run::Empty),
        }
    }

    fn push_command(&mut self, new_run: Run) {
        if let Some(run) = self.commands.take() {
            self.commands = Some(run.push_run(new_run));
        }
    }

    fn push_pipe(&mut self, new_run: Run) {
        if let Some(run) = self.commands.take() {
            self.commands = Some(run.push_pipe(new_run));
        }
    }

    fn push_sequence(&mut self, new_run: Run) {
        if let Some(run) = self.commands.take() {
            self.commands = Some(run.push_sequence(new_run));
        }
    }

    fn push_and(&mut self, new_run: Run) {
        if let Some(run) = self.commands.take() {
            self.commands = Some(run.push_and(new_run));
        }
    }

    fn push_or(&mut self, new_run: Run) {
        if let Some(run) = self.commands.take() {
            self.commands = Some(run.push_or(new_run));
        }
    }

    /// Slice of the individual commands in pipe order.
    pub fn commands(&self) -> &Run {
        self.commands.as_ref().expect("missing command")
    }
}

fn push_next_seq_run(job: &mut ParsedJob, new_run: Run, seq_type: SeqType) {
    match seq_type {
        SeqType::Command => job.push_command(new_run),
        SeqType::Pipe => job.push_pipe(new_run),
        SeqType::Sequence => job.push_sequence(new_run),
        SeqType::And => job.push_and(new_run),
        SeqType::Or => job.push_or(new_run),
    }
}

fn push_next_seq_item(
    job: &mut ParsedJob,
    command: CommandWithArgs,
    seq_type: SeqType,
    background: bool,
) {
    let new_run = if background {
        Run::BackgroundCommand(command)
    } else {
        Run::Command(command)
    };
    match seq_type {
        SeqType::Command => job.push_command(new_run),
        SeqType::Pipe => job.push_pipe(new_run),
        SeqType::Sequence => job.push_sequence(new_run),
        SeqType::And => job.push_and(new_run),
        SeqType::Or => job.push_or(new_run),
    }
}

#[derive(Copy, Clone, Debug)]
enum TokenState {
    Normal,
    StdInPath,
    StdInDirect,
    StdOutCreate,
    StdOutAppend,
    StdErrCreate,
    StdErrAppend,
    StdOutErrCreate,
    StdOutErrAppend,
}

#[derive(Debug)]
struct ParseState {
    ret: ParsedJob,
    // Should not be None, is Option to allow ownership to change.
    command: Option<CommandWithArgs>,
    stdio: Redirects,
    in_string: bool,
    in_stringd: bool,
    // Should not be None, is Option to allow ownership to change.
    token: Option<String>,
    last_ch: char,
    current_seq: SeqType,
    token_state: TokenState,
}

impl ParseState {
    fn new() -> Self {
        let ret = ParsedJob::new();
        let command = Some(CommandWithArgs::new());
        let in_string = false;
        let in_stringd = false;
        let token = Some(String::new());
        let last_ch = ' ';
        let current_seq = SeqType::Command;
        Self {
            ret,
            command,
            stdio: Redirects::default(),
            in_string,
            in_stringd,
            token,
            last_ch,
            current_seq,
            token_state: TokenState::Normal,
        }
    }

    fn command(&mut self) -> &mut CommandWithArgs {
        self.command.as_mut().expect("invalid empty command")
    }

    fn token(&mut self) -> &mut String {
        self.token.as_mut().expect("invalid empty token")
    }

    fn take_token(&mut self) -> String {
        let result = self.token.take().expect("invalid empty token");
        self.token = Some(String::new());
        result
    }

    fn proc_token(&mut self) {
        let token = self.take_token();
        if !token.is_empty() {
            match self.token_state {
                TokenState::Normal => {
                    self.command().push_arg(token.into());
                    self.command().stop_compound_arg();
                }
                TokenState::StdInPath => self.stdio.set_in_path(0, token.into()),
                TokenState::StdInDirect => {
                    if let Ok((pread, pwrite)) = pipe() {
                        unsafe {
                            let mut file = File::from_raw_fd(pwrite);
                            if let Err(e) = file.write_all(token.as_bytes()) {
                                eprintln!("Error writing {token} to stdin: {e}");
                            }
                        }
                        self.stdio.set_in_internal_fd(0, pread, true);
                    }
                }
                TokenState::StdOutCreate => self.stdio.set_out_path(1, token.into(), true),
                TokenState::StdOutAppend => self.stdio.set_out_path(1, token.into(), false),
                TokenState::StdErrCreate => self.stdio.set_out_path(2, token.into(), true),
                TokenState::StdErrAppend => self.stdio.set_out_path(2, token.into(), false),
                TokenState::StdOutErrCreate => {
                    self.stdio.set_out_path(1, token.into(), true);
                    self.stdio.set_out_internal_fd(2, 1, true);
                }
                TokenState::StdOutErrAppend => {
                    self.stdio.set_out_path(1, token.into(), false);
                    self.stdio.set_out_internal_fd(2, 1, true);
                }
            }
            self.token_state = TokenState::Normal;
        }
    }

    fn end_command(&mut self, background: bool) {
        if !self.command().is_empty() {
            if let Some(mut command) = self.command.take() {
                command.set_stdios(self.stdio.clone());
                self.stdio.clear();
                push_next_seq_item(&mut self.ret, command, self.current_seq, background);
            }
            self.command = Some(CommandWithArgs::new());
        }
    }

    fn in_string(&self) -> bool {
        self.in_string || self.in_stringd
    }

    fn str_single(&mut self, ch: char) {
        self.in_string = !self.in_string;
        if !self.in_string {
            self.proc_token();
        }
        self.last_ch = ch;
    }

    fn str_double(&mut self, ch: char) {
        self.in_stringd = !self.in_stringd;
        if !self.in_stringd {
            self.proc_token();
        }
        self.last_ch = ch;
    }

    fn pipe_or(&mut self, ch: char, next_char: char) {
        if self.last_ch == '\\' {
            self.token().push('|');
            self.last_ch = ' ';
        } else if self.last_ch == '|' {
            self.proc_token();
            self.end_command(false);
            self.current_seq = SeqType::Or;
            self.last_ch = ' ';
        } else if next_char == '|' {
            // If the next char is not a '|' then we have a pipe, else will loop and become an OR.
            self.last_ch = ch;
        } else {
            self.proc_token();
            self.end_command(false);
            self.current_seq = SeqType::Pipe;
            self.last_ch = ' ';
        }
    }

    fn seq(&mut self) {
        self.proc_token();
        self.end_command(false);
        self.current_seq = SeqType::Sequence;
    }

    fn and(&mut self) {
        self.proc_token();
        self.end_command(false);
        self.current_seq = SeqType::And;
        self.last_ch = ' ';
    }

    fn redir_out(&mut self, next_char: char, chars: &mut dyn Iterator<Item = char>) {
        if self.last_ch == '\\' {
            self.token().push('>');
            self.last_ch = ' ';
        } else if next_char == '>' {
            self.proc_token();
            if self.last_ch == '2' {
                self.token_state = TokenState::StdErrAppend;
            } else if self.last_ch == '1' {
                self.token_state = TokenState::StdOutAppend;
            } else if self.last_ch == '&' {
                self.token_state = TokenState::StdOutErrAppend;
            } else {
                self.token_state = TokenState::StdOutAppend
            }
            chars.next();
            self.last_ch = ' ';
        } else {
            self.proc_token();
            if self.last_ch == '2' {
                if next_char == '1' {
                    // '2>1'- stderr to stdout.
                    chars.next(); // Consume the '1'.
                    self.stdio.set_out_internal_fd(2, 1, true);
                } else {
                    self.token_state = TokenState::StdErrCreate;
                }
            } else if self.last_ch == '1' {
                if next_char == '2' {
                    // '1>2'- stdout to stderr.
                    chars.next(); // Consume the '2'.
                    self.stdio.set_out_internal_fd(1, 2, true);
                } else {
                    self.token_state = TokenState::StdOutCreate
                }
            } else if self.last_ch == '&' {
                self.token_state = TokenState::StdOutErrCreate
            } else {
                self.token_state = TokenState::StdOutCreate
            }
            self.last_ch = ' ';
        }
    }

    fn redir_in(&mut self, next_char: char, chars: &mut dyn Iterator<Item = char>) {
        if self.last_ch == '\\' {
            self.token().push('<');
            self.last_ch = ' ';
        } else if next_char == '<' {
            self.proc_token();
            chars.next();
            self.last_ch = ' ';
            self.token_state = TokenState::StdInDirect;
        } else {
            self.proc_token();
            self.token_state = TokenState::StdInPath;
            self.last_ch = ' ';
        }
    }

    fn special_arg(&mut self, chars: &mut Peekable<Chars>, end_char: Option<char>) {
        if let Some('(') = chars.peek() {
            // Subshell to capture
            chars.next();
            self.proc_token();
            let mut sub = parse_line_inner(chars, Some(')'));
            if let Some(sub) = sub.commands.take() {
                self.command().push_run_arg(sub);
            }
            if chars.peek().unwrap_or(&' ').is_whitespace() {
                self.command().stop_compound_arg();
            } else {
                self.command().start_compound_arg();
            }
        } else {
            // Env var
            self.proc_token();
            let name = read_env_var_name(chars, end_char);
            if !name.is_empty() {
                self.command().push_env_var_arg(name.into());
            }
            if chars.peek().unwrap_or(&' ').is_whitespace() {
                self.command().stop_compound_arg();
            } else {
                self.command().start_compound_arg();
            }
        }
    }
}

impl From<ParseState> for ParsedJob {
    fn from(value: ParseState) -> Self {
        value.ret
    }
}

fn read_env_var_name(chars: &mut Peekable<Chars>, end_char: Option<char>) -> String {
    let end_char = end_char.unwrap_or(' ');
    let mut res = String::new();
    let mut next_ch = chars.peek();
    while let Some(ch) = next_ch {
        let ch = *ch;
        if !ch.is_whitespace() && ch != '=' && ch != '/' && ch != end_char {
            chars.next();
            res.push(ch);
            next_ch = chars.peek();
        } else {
            next_ch = None;
        }
    }
    res
}

fn parse_line_inner(chars: &mut Peekable<Chars>, end_char: Option<char>) -> ParsedJob {
    let mut state = ParseState::new();
    while let Some(ch) = chars.next() {
        if state.in_string() {
            match ch {
                '\'' if state.last_ch != '\\' && !state.in_stringd => {
                    state.str_single(ch);
                }
                '"' if state.last_ch != '\\' && !state.in_string => {
                    state.str_double(ch);
                }
                _ => {
                    state.token().push(ch);
                    state.last_ch = ch;
                }
            }
        } else {
            if let Some(end_ch) = end_char {
                if ch == end_ch {
                    break;
                }
            }
            let next_char = *chars.peek().unwrap_or(&' ');
            if ch.is_whitespace() {
                state.proc_token();
            } else {
                match ch {
                    '\'' if state.last_ch != '\\' && !state.in_stringd => {
                        state.str_single(ch);
                    }
                    '"' if state.last_ch != '\\' && !state.in_string => {
                        state.str_double(ch);
                    }
                    '|' => {
                        state.pipe_or(ch, next_char);
                    }
                    ';' if state.last_ch != '\\' => {
                        state.seq();
                    }
                    '>' => {
                        state.redir_out(next_char, chars);
                    }
                    '<' => {
                        state.redir_in(next_char, chars);
                    }
                    '&' if state.last_ch == '\\' => {
                        state.token().push('&');
                        state.last_ch = ' ';
                    }
                    '&' if next_char == '>' || next_char == '&' => {
                        state.last_ch = '&';
                    }
                    '&' if state.last_ch == '&' => {
                        state.and();
                    }
                    '&' => {
                        state.proc_token();
                        state.end_command(true);
                        state.last_ch = ' ';
                    }
                    '1' if next_char == '>' => {
                        state.last_ch = '1';
                    }
                    '2' if next_char == '>' => {
                        state.last_ch = '2';
                    }
                    '\\' => {
                        if state.last_ch == '\\' {
                            state.token().push('\\');
                            state.last_ch = ' ';
                        } else {
                            state.last_ch = ch;
                        }
                    }
                    '(' => {
                        state.proc_token();
                        state.end_command(false);
                        let mut sub = parse_line_inner(chars, Some(')'));
                        if let Some(sub) = sub.commands.take() {
                            push_next_seq_run(
                                &mut state.ret,
                                Run::Subshell(Box::new(sub)),
                                state.current_seq,
                            );
                        }
                    }
                    '$' => state.special_arg(chars, end_char),
                    _ => {
                        state.token().push(ch);
                        state.last_ch = ch;
                    }
                }
            }
        }
    }
    state.proc_token();
    state.end_command(false);
    state.into()
}

pub fn parse_line(input: &str) -> ParsedJob {
    let mut chars = input.chars().peekable();
    parse_line_inner(&mut chars, None)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_parse(input: &str, expected: &str) {
        let pj = parse_line(input);
        let pj_str = pj.to_string();
        assert_eq!(&pj_str, expected);
        let pj = parse_line(&pj_str);
        assert_eq!(&pj.to_string(), expected);
    }

    #[test]
    fn test_basic_parse() {
        test_parse("ls", "ls");
        test_parse("ls -al", "ls -al");
        test_parse("ls -al|grep lisp", "ls -al | grep lisp");
        test_parse("<in_file ls -al|grep lisp", "ls -al <in_file | grep lisp");

        test_parse("ls -al;grep lisp", "ls -al ; grep lisp");

        test_parse("ls -al&&grep lisp", "ls -al && grep lisp");

        test_parse("ls -al||grep lisp", "ls -al || grep lisp");

        test_parse(
            "</some/file grep test|grep lisp>/out_file",
            "grep test </some/file | grep lisp >/out_file",
        );

        test_parse(
            "</some/file > out_file grep test;<in_file grep lisp>/out_file;ls",
            "grep test </some/file >out_file ; grep lisp <in_file >/out_file ; ls",
        );

        test_parse(
            "</some/file 2>1 > out_file grep test;<in_file grep lisp 1>2 >/out_file;ls",
            "grep test </some/file 2>1 >out_file ; grep lisp <in_file 1>2 >/out_file ; ls",
        );

        test_parse(
            "</some/file 2>1 > out_file grep test;ls|<in_file grep lisp 1>2 >/out_file;ls",
            "grep test </some/file 2>1 >out_file ; ls | grep lisp <in_file 1>2 >/out_file ; ls",
        );
    }

    #[test]
    fn test_subshell_parse() {
        test_parse("(ls -al)", "(ls -al)");
        test_parse("(ls -al)|grep May", "(ls -al) | grep May");
        test_parse(
            "(ls -al)|(grep May|grep 7)|grep 00",
            "(ls -al) | (grep May | grep 7) | grep 00",
        )
    }
}
