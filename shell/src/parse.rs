use crate::command_data::{BoxedIos, CommandWithArgs, Run};

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

    fn push_command(&mut self, command: CommandWithArgs, ios: BoxedIos) {
        if let Some(run) = self.commands.take() {
            self.commands = Some(run.push_command(command, ios));
        }
    }

    fn push_pipe(&mut self, command: CommandWithArgs, ios: BoxedIos) {
        if let Some(run) = self.commands.take() {
            self.commands = Some(run.push_pipe(command, ios));
        }
    }

    fn push_sequence(&mut self, command: CommandWithArgs, ios: BoxedIos) {
        if let Some(run) = self.commands.take() {
            self.commands = Some(run.push_sequence(command, ios));
        }
    }

    fn push_and(&mut self, command: CommandWithArgs, ios: BoxedIos) {
        if let Some(run) = self.commands.take() {
            self.commands = Some(run.push_and(command, ios));
        }
    }

    fn push_or(&mut self, command: CommandWithArgs, ios: BoxedIos) {
        if let Some(run) = self.commands.take() {
            self.commands = Some(run.push_or(command, ios));
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

fn push_next_seq_item(
    job: &mut ParsedJob,
    command: CommandWithArgs,
    ios: BoxedIos,
    seq_type: SeqType,
) {
    match seq_type {
        SeqType::Command => job.push_command(command, ios),
        SeqType::Pipe => job.push_pipe(command, ios),
        SeqType::Sequence => job.push_sequence(command, ios),
        SeqType::And => job.push_and(command, ios),
        SeqType::Or => job.push_or(command, ios),
    }
}

struct ParseState {
    ret: ParsedJob,
    // Should not be None, is Option to allow ownership to change.
    command: Option<CommandWithArgs>,
    in_string: bool,
    in_stringd: bool,
    // Should not be None, is Option to allow ownership to change.
    token: Option<String>,
    last_ch: char,
    current_seq: SeqType,
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
            in_string,
            in_stringd,
            token,
            last_ch,
            current_seq,
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

    fn in_string(&self) -> bool {
        self.in_string || self.in_stringd
    }

    fn str_single(&mut self, ch: char) {
        self.in_string = !self.in_string;
        if !self.in_string {
            let token = self.take_token();
            self.command().push_arg(token.into());
        }
        self.last_ch = ch;
    }

    fn str_double(&mut self, ch: char) {
        self.in_stringd = !self.in_stringd;
        if !self.in_stringd {
            let token = self.take_token();
            self.command().push_arg(token.into());
        }
        self.last_ch = ch;
    }

    fn pipe_or(&mut self, ch: char, next_char: Option<&char>) {
        if self.last_ch == '\\' {
            self.token().push('|');
            self.last_ch = ' ';
        } else if self.last_ch == '|' {
            if !self.token().is_empty() {
                let token = self.take_token();
                self.command().push_arg(token.into());
            }
            if !self.command().is_empty() {
                if let Some(command) = self.command.take() {
                    push_next_seq_item(&mut self.ret, command, None, self.current_seq);
                }
                self.command = Some(CommandWithArgs::new());
            }
            self.current_seq = SeqType::Or;
            self.last_ch = ' ';
        }
        // If the next char is not a '|' then we have a pipe, else will loop and become an OR.
        if let Some(&'|') = next_char {
            self.last_ch = ch;
        } else {
            if !self.token().is_empty() {
                let token = self.take_token();
                self.command().push_arg(token.into());
            }
            if !self.command().is_empty() {
                if let Some(command) = self.command.take() {
                    push_next_seq_item(&mut self.ret, command, None, self.current_seq);
                }
                self.command = Some(CommandWithArgs::new());
            }
            self.current_seq = SeqType::Pipe;
            self.last_ch = ' ';
        }
    }

    fn seq(&mut self) {
        if !self.token().is_empty() {
            let token = self.take_token();
            self.command().push_arg(token.into());
        }
        if !self.command().is_empty() {
            if let Some(command) = self.command.take() {
                push_next_seq_item(&mut self.ret, command, None, self.current_seq);
            }
            self.command = Some(CommandWithArgs::new());
        }
        self.current_seq = SeqType::Sequence;
    }

    fn and(&mut self, mut ch: char) {
        if self.last_ch == '\\' {
            self.token().push('&');
            ch = ' ';
        } else if self.last_ch == '&' {
            if !self.token().is_empty() {
                let token = self.take_token();
                self.command().push_arg(token.into());
            }
            if !self.command().is_empty() {
                if let Some(command) = self.command.take() {
                    push_next_seq_item(&mut self.ret, command, None, self.current_seq);
                }
                self.command = Some(CommandWithArgs::new());
            }
            self.current_seq = SeqType::And;
            ch = ' ';
        } else if !self.token().is_empty() {
            let token = self.take_token();
            self.command().push_arg(token.into());
        }
        self.last_ch = ch;
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
        match ch {
            '\'' if state.last_ch != '\\' && !state.in_stringd => {
                state.str_single(ch);
            }
            '"' if state.last_ch != '\\' && !state.in_string => {
                state.str_double(ch);
            }
            '|' if !state.in_string() => {
                state.pipe_or(ch, chars.peek());
            }
            ';' if state.last_ch != '\\' && !state.in_string() => {
                state.seq();
            }
            '&' if state.last_ch != '\\' && !state.in_string() => {
                state.and(ch);
            }
            '\\' if !state.in_string() => {
                if state.last_ch == '\\' {
                    state.token().push('\\');
                    state.last_ch = ' ';
                } else {
                    state.last_ch = ch;
                }
            }
            ' ' if !state.in_string() => {
                if !state.token().is_empty() {
                    let token = state.take_token();
                    state.command().push_arg(token.into());
                }
            }
            _ => {
                state.token().push(ch);
                state.last_ch = ch;
            }
        }
    }
    if !state.token().is_empty() {
        if state.token() == "&" {
            state.ret.set_background(true);
        } else {
            let token = state.take_token();
            state.command().push_arg(token.into());
        }
    }
    if let Some(command) = state.command.take() {
        push_next_seq_item(&mut state.ret, command, None, state.current_seq);
    }
    state.into()
}
