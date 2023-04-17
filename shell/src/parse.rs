use std::ffi::{OsStr, OsString};

/// Arg to a command, either a direct string or a sub command to run to get the arg.
#[derive(Clone, Debug)]
pub enum Arg {
    Str(OsString),
    Command(Run),
}

/// Optional file descriptors for standard in, out and error.
#[derive(Clone, Debug)]
pub struct StdIos {
    pub stdin: Option<i32>,
    pub stdout: Option<i32>,
    pub stderr: Option<i32>,
}

/// An individual command with args.
#[derive(Clone, Debug)]
pub struct CommandWithArgs {
    /// args[0] is the command.
    args: Vec<Arg>,
}

impl CommandWithArgs {
    fn new() -> Self {
        Self { args: vec![] }
    }

    fn push_arg(&mut self, arg: OsString) {
        self.args.push(Arg::Str(arg));
    }

    /// Empty, not even the command is set.
    pub fn is_empty(&self) -> bool {
        self.args.is_empty()
    }

    /// Command name, None if no command name set (args are empty).
    pub fn command(&self) -> Option<&OsStr> {
        if let Some(Arg::Str(command)) = self.args.get(0) {
            Some(command)
        } else {
            None
        }
    }

    /// Args to the command.
    pub fn args(&self) -> &[Arg] {
        if self.args.is_empty() {
            &self.args[..]
        } else {
            &self.args[1..]
        }
    }

    pub fn args_iter(&self) -> CommandArgs {
        CommandArgs {
            args: self.args(),
            temps: vec![],
            index: 0,
        }
    }
}

pub struct CommandArgs<'args> {
    args: &'args [Arg],
    temps: Vec<OsString>,
    index: usize,
}

impl<'args> Iterator for CommandArgs<'args> {
    type Item = &'args OsStr;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.args.len() {
            let r = match &self.args[self.index] {
                Arg::Str(s) => s.as_ref(),
                Arg::Command(_run) => {
                    self.temps.push("XXX".into());
                    self.last().unwrap()
                }
            };
            self.index += 1;
            Some(r)
        } else {
            None
        }
    }
}

pub type BoxedIos = Option<Box<StdIos>>;

/// Command(s) ready to run with context.
#[derive(Clone, Debug)]
pub enum Run {
    Command(CommandWithArgs, BoxedIos),
    Pipe(Vec<Run>, BoxedIos),
    Sequence(Vec<Run>, BoxedIos),
    And(Vec<Run>, BoxedIos),
    Or(Vec<Run>, BoxedIos),
    Empty,
}

impl Run {
    fn push_command(self, command: CommandWithArgs, com_ios: BoxedIos) -> Self {
        match self {
            Run::Command(current, ios) => Run::Sequence(
                vec![Run::Command(current, ios), Run::Command(command, com_ios)],
                None,
            ),
            Run::Pipe(pipe, ios) => Run::Sequence(
                vec![Run::Pipe(pipe, ios), Run::Command(command, com_ios)],
                None,
            ),
            Run::Sequence(mut seq, ios) => {
                seq.push(Run::Command(command, com_ios));
                Run::Sequence(seq, ios)
            }
            Run::And(seq, ios) => Run::Sequence(
                vec![Run::And(seq, ios), Run::Command(command, com_ios)],
                None,
            ),
            Run::Or(seq, ios) => Run::Sequence(
                vec![Run::Or(seq, ios), Run::Command(command, com_ios)],
                None,
            ),
            Run::Empty => Run::Command(command, com_ios),
        }
    }

    fn _push_to_last(self, command: CommandWithArgs, com_ios: BoxedIos) -> Self {
        match self {
            Run::Command(current, ios) => Run::Sequence(
                vec![Run::Command(current, ios), Run::Command(command, com_ios)],
                None,
            ),
            Run::Pipe(mut pipe, ios) => {
                pipe.push(Run::Command(command, com_ios));
                Run::Pipe(pipe, ios)
            }
            Run::Sequence(mut seq, ios) => {
                seq.push(Run::Command(command, com_ios));
                Run::Sequence(seq, ios)
            }
            Run::And(mut seq, ios) => {
                seq.push(Run::Command(command, com_ios));
                Run::And(seq, ios)
            }
            Run::Or(mut seq, ios) => {
                seq.push(Run::Command(command, com_ios));
                Run::Or(seq, ios)
            }
            Run::Empty => Run::Command(command, com_ios),
        }
    }

    fn push_pipe(self, command: CommandWithArgs, com_ios: BoxedIos) -> Self {
        match self {
            Run::Command(current, ios) => Run::Pipe(
                vec![Run::Command(current, ios), Run::Command(command, com_ios)],
                None,
            ),
            Run::Pipe(mut pipe, ios) => {
                pipe.push(Run::Command(command, com_ios));
                Run::Pipe(pipe, ios)
            }
            Run::Sequence(seq, ios) => Run::Pipe(
                vec![Run::Sequence(seq, ios), Run::Command(command, com_ios)],
                None,
            ),
            Run::And(seq, ios) => Run::Pipe(
                vec![Run::And(seq, ios), Run::Command(command, com_ios)],
                None,
            ),
            Run::Or(seq, ios) => Run::Pipe(
                vec![Run::Or(seq, ios), Run::Command(command, com_ios)],
                None,
            ),
            Run::Empty => Run::Command(command, com_ios),
        }
    }

    fn push_sequence(self, command: CommandWithArgs, com_ios: BoxedIos) -> Self {
        match self {
            Run::Command(current, ios) => Run::Sequence(
                vec![Run::Command(current, ios), Run::Command(command, com_ios)],
                None,
            ),
            Run::Pipe(pipe, ios) => Run::Sequence(
                vec![Run::Pipe(pipe, ios), Run::Command(command, com_ios)],
                None,
            ),
            Run::Sequence(mut seq, ios) => {
                seq.push(Run::Command(command, com_ios));
                Run::Sequence(seq, ios)
            }
            Run::And(seq, ios) => Run::Sequence(
                vec![Run::And(seq, ios), Run::Command(command, com_ios)],
                None,
            ),
            Run::Or(seq, ios) => Run::Sequence(
                vec![Run::Or(seq, ios), Run::Command(command, com_ios)],
                None,
            ),
            Run::Empty => Run::Command(command, com_ios),
        }
    }

    fn push_and(self, command: CommandWithArgs, com_ios: BoxedIos) -> Self {
        match self {
            Run::Command(current, ios) => Run::And(
                vec![Run::Command(current, ios), Run::Command(command, com_ios)],
                None,
            ),
            Run::Pipe(pipe, ios) => Run::And(
                vec![Run::Pipe(pipe, ios), Run::Command(command, com_ios)],
                None,
            ),
            Run::Sequence(seq, ios) => Run::And(
                vec![Run::Sequence(seq, ios), Run::Command(command, com_ios)],
                None,
            ),
            Run::And(mut seq, ios) => {
                seq.push(Run::Command(command, com_ios));
                Run::And(seq, ios)
            }
            Run::Or(seq, ios) => Run::And(
                vec![Run::Or(seq, ios), Run::Command(command, com_ios)],
                None,
            ),
            Run::Empty => Run::Command(command, com_ios),
        }
    }

    fn push_or(self, command: CommandWithArgs, com_ios: BoxedIos) -> Self {
        match self {
            Run::Command(current, ios) => Run::Or(
                vec![Run::Command(current, ios), Run::Command(command, com_ios)],
                None,
            ),
            Run::Pipe(pipe, ios) => Run::Or(
                vec![Run::Pipe(pipe, ios), Run::Command(command, com_ios)],
                None,
            ),
            Run::Sequence(seq, ios) => Run::Or(
                vec![Run::Or(seq, ios), Run::Command(command, com_ios)],
                None,
            ),
            Run::And(seq, ios) => Run::Or(
                vec![Run::And(seq, ios), Run::Command(command, com_ios)],
                None,
            ),
            Run::Or(mut seq, ios) => {
                seq.push(Run::Command(command, com_ios));
                Run::Or(seq, ios)
            }
            Run::Empty => Run::Command(command, com_ios),
        }
    }

    pub fn stdin(&self) -> Option<i32> {
        match self {
            Run::Command(_, ios) => ios.as_ref().and_then(|io| io.stdin),
            Run::Pipe(_, ios) => ios.as_ref().and_then(|io| io.stdin),
            Run::Sequence(_, ios) => ios.as_ref().and_then(|io| io.stdin),
            Run::And(_, ios) => ios.as_ref().and_then(|io| io.stdin),
            Run::Or(_, ios) => ios.as_ref().and_then(|io| io.stdin),
            Run::Empty => None,
        }
    }

    pub fn stdout(&self) -> Option<i32> {
        match self {
            Run::Command(_, ios) => ios.as_ref().and_then(|io| io.stdout),
            Run::Pipe(_, ios) => ios.as_ref().and_then(|io| io.stdout),
            Run::Sequence(_, ios) => ios.as_ref().and_then(|io| io.stdout),
            Run::And(_, ios) => ios.as_ref().and_then(|io| io.stdout),
            Run::Or(_, ios) => ios.as_ref().and_then(|io| io.stdout),
            Run::Empty => None,
        }
    }

    pub fn stderr(&self) -> Option<i32> {
        match self {
            Run::Command(_, ios) => ios.as_ref().and_then(|io| io.stderr),
            Run::Pipe(_, ios) => ios.as_ref().and_then(|io| io.stderr),
            Run::Sequence(_, ios) => ios.as_ref().and_then(|io| io.stderr),
            Run::And(_, ios) => ios.as_ref().and_then(|io| io.stderr),
            Run::Or(_, ios) => ios.as_ref().and_then(|io| io.stderr),
            Run::Empty => None,
        }
    }

    fn set_io_inner(
        ios: &mut BoxedIos,
        stdin: Option<i32>,
        stdout: Option<i32>,
        stderr: Option<i32>,
    ) {
        match ios {
            Some(ios) => {
                ios.stdin = stdin;
                ios.stdout = stdout;
                ios.stderr = stderr;
            }
            None => {
                let nios = Box::new(StdIos {
                    stdin,
                    stdout,
                    stderr,
                });
                *ios = Some(nios);
            }
        }
    }

    pub fn set_io(&mut self, stdin: Option<i32>, stdout: Option<i32>, stderr: Option<i32>) {
        match self {
            Run::Command(_, ios) => Self::set_io_inner(ios, stdin, stdout, stderr),
            Run::Pipe(_, ios) => Self::set_io_inner(ios, stdin, stdout, stderr),
            Run::Sequence(_, ios) => Self::set_io_inner(ios, stdin, stdout, stderr),
            Run::And(_, ios) => Self::set_io_inner(ios, stdin, stdout, stderr),
            Run::Or(_, ios) => Self::set_io_inner(ios, stdin, stdout, stderr),
            Run::Empty => {}
        }
    }
}

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

pub fn parse_line(input: &str) -> ParsedJob {
    let mut ret = ParsedJob::new();
    let mut command = CommandWithArgs::new();
    let mut in_string = false;
    let mut in_stringd = false;
    let mut token = String::new();
    let mut last_ch = ' ';
    let mut current_seq = SeqType::Command;
    let mut chars = input.chars().peekable();
    while let Some(ch) = chars.next() {
        let next_char = chars.peek();
        if ch == '\'' && last_ch != '\\' {
            in_string = !in_string;
            if !in_string {
                command.push_arg(token.into());
                token = String::new();
            }
            last_ch = ch;
            continue;
        }
        if ch == '"' && last_ch != '\\' {
            in_stringd = !in_stringd;
            if !in_stringd {
                command.push_arg(token.into());
                token = String::new();
            }
            last_ch = ch;
            continue;
        }
        if ch == '|' {
            if last_ch == '\\' {
                token.push('|');
                last_ch = ' ';
                continue;
            } else if last_ch == '|' {
                if !token.is_empty() {
                    command.push_arg(token.into());
                    token = String::new();
                }
                if !command.is_empty() {
                    push_next_seq_item(&mut ret, command, None, current_seq);
                    command = CommandWithArgs::new();
                }
                current_seq = SeqType::Or;
                last_ch = ' ';
                continue;
            }
            // If the next char is not a '|' then we have a pipe, else will loop and become an OR.
            if let Some(&'|') = next_char {
                last_ch = ch;
                continue;
            } else {
                if !token.is_empty() {
                    command.push_arg(token.into());
                    token = String::new();
                }
                if !command.is_empty() {
                    push_next_seq_item(&mut ret, command, None, current_seq);
                    command = CommandWithArgs::new();
                }
                current_seq = SeqType::Pipe;
                last_ch = ' ';
                continue;
            }
        }
        if ch == ';' && last_ch != '\\' {
            if !token.is_empty() {
                command.push_arg(token.into());
                token = String::new();
            }
            if !command.is_empty() {
                push_next_seq_item(&mut ret, command, None, current_seq);
                command = CommandWithArgs::new();
            }
            current_seq = SeqType::Sequence;
            continue;
        }
        if ch == '&' {
            if last_ch == '\\' {
                token.push('&');
                last_ch = ' ';
                continue;
            } else if last_ch == '&' {
                if !token.is_empty() {
                    command.push_arg(token.into());
                    token = String::new();
                }
                if !command.is_empty() {
                    push_next_seq_item(&mut ret, command, None, current_seq);
                    command = CommandWithArgs::new();
                }
                current_seq = SeqType::And;
                last_ch = ' ';
                continue;
            }
            if !token.is_empty() {
                command.push_arg(token.into());
                token = String::new();
            }
            last_ch = ch;
            continue;
        }
        if ch == '\\' {
            if last_ch == '\\' {
                token.push('\\');
                last_ch = ' ';
            } else {
                last_ch = ch;
            }
            continue;
        }
        if in_string || in_stringd {
            token.push(ch);
        } else if ch == ' ' {
            if !token.is_empty() {
                command.push_arg(token.into());
                token = String::new();
            }
        } else {
            token.push(ch);
        }
        last_ch = ch;
    }
    if !token.is_empty() {
        if token == "&" {
            ret.set_background(true);
        } else {
            command.push_arg(token.into());
        }
    }
    push_next_seq_item(&mut ret, command, None, current_seq);
    ret
}
