use crate::command_data::{CommandWithArgs, Run, StdIos};
use crate::unix::pipe;
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
    redir_create: bool,
    redir_append: bool,
    redir_err_create: bool,
    redir_err_append: bool,
    redir_in: bool,
    redir_in_direct: bool,
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
            redir_create: false,
            redir_append: false,
            redir_err_create: false,
            redir_err_append: false,
            redir_in: false,
            redir_in_direct: false,
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

    fn clear_redirs(&mut self) {
        self.redir_create = false;
        self.redir_append = false;
        self.redir_in = false;
        self.redir_in_direct = false;
        self.redir_err_create = false;
        self.redir_err_append = false;
    }

    fn proc_token(&mut self) {
        let token = self.take_token();
        if !token.is_empty() {
            let command = if self.redir_create {
                self.stdio.set_out_path(token.clone().into(), true);
                false
            } else if self.redir_append {
                self.stdio.set_out_path(token.clone().into(), false);
                false
            } else if self.redir_in {
                self.stdio.set_in_path(token.clone().into(), false);
                false
            } else if self.redir_in_direct {
                if let Ok((pread, pwrite)) = pipe() {
                    unsafe {
                        let mut file = File::from_raw_fd(pwrite);
                        if let Err(e) = file.write_all(token.as_bytes()) {
                            eprintln!("Error writing {token} to stdin: {e}");
                        }
                    }
                    self.stdio.set_in_fd(pread, true);
                }
                false
            } else {
                true
            };
            if self.redir_err_create {
                if self.redir_create {
                    self.stdio.set_err_fd(1, true);
                } else {
                    self.stdio.set_err_path(token.into(), true);
                }
            } else if self.redir_err_append {
                if self.redir_append {
                    self.stdio.set_err_fd(1, true);
                } else {
                    self.stdio.set_err_path(token.into(), false);
                }
            } else if command {
                self.command().push_arg(token.into());
            }
            self.clear_redirs();
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
        }
        // If the next char is not a '|' then we have a pipe, else will loop and become an OR.
        if let '|' = next_char {
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
                self.redir_err_append = true;
            } else if self.last_ch == '1' {
                self.redir_append = true;
            } else if self.last_ch == '&' {
                self.redir_err_append = true;
                self.redir_append = true;
            } else {
                self.redir_append = true;
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
                    self.redir_err_create = true;
                }
            } else if self.last_ch == '1' {
                if next_char == '2' {
                    // '1>2'- stdout to stderr.
                    chars.next(); // Consume the '2'.
                    self.stdio.set_out_fd(2, true);
                } else {
                    self.redir_create = true;
                }
            } else if self.last_ch == '&' {
                self.redir_err_create = true;
                self.redir_create = true;
            } else {
                self.redir_create = true;
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
            self.redir_in_direct = true;
        } else {
            self.proc_token();
            self.redir_in = true;
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
