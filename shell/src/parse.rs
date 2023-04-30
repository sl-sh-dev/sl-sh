use crate::command_data::{CommandWithArgs, Run, StdIos};
use crate::unix::pipe;
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::Write;
use std::os::fd::FromRawFd;

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
    background: bool,
}

impl Display for ParsedJob {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(run) = &self.commands {
            write!(f, "{run}")?;
            if self.background {
                write!(f, " &")?;
            }
        }
        Ok(())
    }
}

impl ParsedJob {
    fn new() -> Self {
        Self {
            commands: Some(Run::Empty),
            background: false,
        }
    }

    fn push_command(&mut self, command: CommandWithArgs) {
        if let Some(run) = self.commands.take() {
            self.commands = Some(run.push_command(command));
        }
    }

    fn push_pipe(&mut self, command: CommandWithArgs) {
        if let Some(run) = self.commands.take() {
            self.commands = Some(run.push_pipe(command));
        }
    }

    fn push_sequence(&mut self, command: CommandWithArgs) {
        if let Some(run) = self.commands.take() {
            self.commands = Some(run.push_sequence(command));
        }
    }

    fn push_and(&mut self, command: CommandWithArgs) {
        if let Some(run) = self.commands.take() {
            self.commands = Some(run.push_and(command));
        }
    }

    fn push_or(&mut self, command: CommandWithArgs) {
        if let Some(run) = self.commands.take() {
            self.commands = Some(run.push_or(command));
        }
    }

    fn set_background(&mut self, background: bool) {
        self.background = background;
    }

    /// Should this job run in the background?
    pub fn background(&self) -> bool {
        self.background
    }

    /// Slice of the individual commands in pipe order.
    pub fn commands(&self) -> &Run {
        self.commands.as_ref().expect("missing command")
    }
}

fn push_next_seq_item(job: &mut ParsedJob, command: CommandWithArgs, seq_type: SeqType) {
    match seq_type {
        SeqType::Command => job.push_command(command),
        SeqType::Pipe => job.push_pipe(command),
        SeqType::Sequence => job.push_sequence(command),
        SeqType::And => job.push_and(command),
        SeqType::Or => job.push_or(command),
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
    stdio: StdIos,
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
            stdio: StdIos::default(),
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
                TokenState::Normal => self.command().push_arg(token.into()),
                TokenState::StdInPath => self.stdio.set_in_path(token.into(), false),
                TokenState::StdInDirect => {
                    if let Ok((pread, pwrite)) = pipe() {
                        unsafe {
                            let mut file = File::from_raw_fd(pwrite);
                            if let Err(e) = file.write_all(token.as_bytes()) {
                                eprintln!("Error writing {token} to stdin: {e}");
                            }
                        }
                        self.stdio.set_in_fd(pread, true);
                    }
                }
                TokenState::StdOutCreate => self.stdio.set_out_path(token.into(), true),
                TokenState::StdOutAppend => self.stdio.set_out_path(token.into(), false),
                TokenState::StdErrCreate => self.stdio.set_err_path(token.into(), true),
                TokenState::StdErrAppend => self.stdio.set_err_path(token.into(), false),
                TokenState::StdOutErrCreate => {
                    self.stdio.set_out_path(token.into(), true);
                    self.stdio.set_err_fd(1, true);
                }
                TokenState::StdOutErrAppend => {
                    self.stdio.set_out_path(token.into(), false);
                    self.stdio.set_err_fd(1, true);
                }
            }
            self.token_state = TokenState::Normal;
        }
    }

    fn end_command(&mut self) {
        if !self.command().is_empty() {
            if let Some(mut command) = self.command.take() {
                command.set_stdios(self.stdio.clone());
                self.stdio.clear();
                push_next_seq_item(&mut self.ret, command, self.current_seq);
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
            self.end_command();
            self.current_seq = SeqType::Or;
            self.last_ch = ' ';
        } else if next_char == '|' {
            // If the next char is not a '|' then we have a pipe, else will loop and become an OR.
            self.last_ch = ch;
        } else {
            self.proc_token();
            self.end_command();
            self.current_seq = SeqType::Pipe;
            self.last_ch = ' ';
        }
    }

    fn seq(&mut self) {
        self.proc_token();
        self.end_command();
        self.current_seq = SeqType::Sequence;
    }

    fn and(&mut self, mut ch: char) {
        if self.last_ch == '\\' {
            self.token().push('&');
            ch = ' ';
        } else if self.last_ch == '&' {
            self.proc_token();
            self.end_command();
            self.current_seq = SeqType::And;
            ch = ' ';
        } else {
            self.proc_token();
        }
        self.last_ch = ch;
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
                    self.stdio.set_err_fd(1, true);
                } else {
                    self.token_state = TokenState::StdErrCreate;
                }
            } else if self.last_ch == '1' {
                if next_char == '2' {
                    // '1>2'- stdout to stderr.
                    chars.next(); // Consume the '2'.
                    self.stdio.set_out_fd(2, true);
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
}

impl From<ParseState> for ParsedJob {
    fn from(value: ParseState) -> Self {
        value.ret
    }
}

pub fn parse_line(input: &str) -> ParsedJob {
    let mut chars = input.chars().peekable();
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
            let next_char = *chars.peek().unwrap_or(&' ');
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
                    state.redir_out(next_char, &mut chars);
                }
                '<' => {
                    state.redir_in(next_char, &mut chars);
                }
                '&' if state.last_ch == '\\' => {
                    state.token().push('&');
                    state.last_ch = ' ';
                }
                '&' if next_char == '>' => {
                    state.last_ch = '&';
                }
                '1' if next_char == '>' => {
                    state.last_ch = '1';
                }
                '2' if next_char == '>' => {
                    state.last_ch = '2';
                }
                '&' if state.last_ch != '\\' => {
                    state.and(ch);
                }
                '\\' => {
                    if state.last_ch == '\\' {
                        state.token().push('\\');
                        state.last_ch = ' ';
                    } else {
                        state.last_ch = ch;
                    }
                }
                ' ' => {
                    state.proc_token();
                }
                _ => {
                    state.token().push(ch);
                    state.last_ch = ch;
                }
            }
        }
    }
    if !state.token().is_empty() {
        if state.token() == "&" {
            state.ret.set_background(true);
        } else {
            state.proc_token();
        }
    }
    state.end_command();
    state.into()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_parse() {
        let pj = parse_line("ls");
        assert_eq!(&pj.to_string(), "ls");
        let pj = parse_line("ls -al");
        assert_eq!(&pj.to_string(), "ls -al");
        let pj = parse_line("ls -al|grep lisp");
        assert_eq!(&pj.to_string(), "ls -al | grep lisp");
        let pj = parse_line(&pj.to_string());
        assert_eq!(&pj.to_string(), "ls -al | grep lisp");
        let pj = parse_line("<in_file ls -al|grep lisp");
        assert_eq!(&pj.to_string(), "ls -al <in_file | grep lisp");
        let pj = parse_line(&pj.to_string());
        assert_eq!(&pj.to_string(), "ls -al <in_file | grep lisp");

        let pj = parse_line("ls -al;grep lisp");
        assert_eq!(&pj.to_string(), "ls -al ; grep lisp");
        let pj = parse_line(&pj.to_string());
        assert_eq!(&pj.to_string(), "ls -al ; grep lisp");

        let pj = parse_line("ls -al&&grep lisp");
        assert_eq!(&pj.to_string(), "ls -al && grep lisp");
        let pj = parse_line(&pj.to_string());
        assert_eq!(&pj.to_string(), "ls -al && grep lisp");

        let pj = parse_line("ls -al||grep lisp");
        assert_eq!(&pj.to_string(), "ls -al || grep lisp");
        let pj = parse_line(&pj.to_string());
        assert_eq!(&pj.to_string(), "ls -al || grep lisp");

        let pj = parse_line("</some/file grep test|grep lisp>/out_file");
        assert_eq!(
            &pj.to_string(),
            "grep test </some/file | grep lisp >/out_file"
        );
        let pj = parse_line(&pj.to_string());
        assert_eq!(
            &pj.to_string(),
            "grep test </some/file | grep lisp >/out_file"
        );

        let pj = parse_line("</some/file > out_file grep test;<in_file grep lisp>/out_file;ls");
        assert_eq!(
            &pj.to_string(),
            "grep test </some/file >out_file ; grep lisp <in_file >/out_file ; ls"
        );
        let pj = parse_line(&pj.to_string());
        assert_eq!(
            &pj.to_string(),
            "grep test </some/file >out_file ; grep lisp <in_file >/out_file ; ls"
        );

        let pj =
            parse_line("</some/file 2>1 > out_file grep test;<in_file grep lisp 1>2 >/out_file;ls");
        assert_eq!(
            &pj.to_string(),
            "grep test </some/file 2>1 >out_file ; grep lisp <in_file 1>2 >/out_file ; ls"
        );
        let pj = parse_line(&pj.to_string());
        assert_eq!(
            &pj.to_string(),
            "grep test </some/file 2>1 >out_file ; grep lisp <in_file 1>2 >/out_file ; ls"
        );

        let pj = parse_line(
            "</some/file 2>1 > out_file grep test;ls|<in_file grep lisp 1>2 >/out_file;ls",
        );
        assert_eq!(
            &pj.to_string(),
            "grep test </some/file 2>1 >out_file ; ls | grep lisp <in_file 1>2 >/out_file ; ls"
        );
        let pj = parse_line(&pj.to_string());
        assert_eq!(
            &pj.to_string(),
            "grep test </some/file 2>1 >out_file ; ls | grep lisp <in_file 1>2 >/out_file ; ls"
        );
    }
}
