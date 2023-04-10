/// An individual command with args.
pub struct CommandWithArgs {
    /// args[0] is the command.
    args: Vec<String>,
}

impl CommandWithArgs {
    fn new() -> Self {
        Self { args: vec![] }
    }

    fn push_arg(&mut self, arg: String) {
        self.args.push(arg);
    }

    /// Empty, not even the command is set.
    pub fn is_empty(&self) -> bool {
        self.args.is_empty()
    }

    /// Command name, None if no command name set (args are empty).
    pub fn command(&self) -> Option<&str> {
        if let Some(command) = self.args.get(0) {
            Some(command)
        } else {
            None
        }
    }

    /// Argss to the command.
    pub fn args(&self) -> &[String] {
        if self.args.is_empty() {
            &self.args[..]
        } else {
            &self.args[1..]
        }
    }
}

/// A list of commands to run in a pipe as a job.
pub struct ParsedJob {
    commands: Vec<CommandWithArgs>,
    background: bool,
}

impl ParsedJob {
    fn new() -> Self {
        Self {
            commands: vec![],
            background: false,
        }
    }

    fn push_command(&mut self, command: CommandWithArgs) {
        self.commands.push(command);
    }

    fn set_background(&mut self, background: bool) {
        self.background = background;
    }

    /// Should this job run in the background?
    pub fn background(&self) -> bool {
        self.background
    }

    /// Slice of the individual commands in pipe order.
    pub fn commands(&self) -> &[CommandWithArgs] {
        &self.commands[..]
    }

    /// Number of commands in this Job.
    pub fn len(&self) -> usize {
        self.commands.len()
    }
}

pub fn parse_line(input: &str) -> ParsedJob {
    let mut ret = ParsedJob::new();
    let mut command = CommandWithArgs::new();
    let mut in_string = false;
    let mut in_stringd = false;
    let mut token = String::new();
    let mut last_ch = ' ';
    for ch in input.chars() {
        if ch == '\'' && last_ch != '\\' {
            in_string = !in_string;
            if !in_string {
                command.push_arg(token);
                token = String::new();
            }
            last_ch = ch;
            continue;
        }
        if ch == '"' && last_ch != '\\' {
            in_stringd = !in_stringd;
            if !in_stringd {
                command.push_arg(token);
                token = String::new();
            }
            last_ch = ch;
            continue;
        }
        if ch == '|' && last_ch != '\\' {
            if !token.is_empty() {
                command.push_arg(token);
                token = String::new();
            }
            if !command.is_empty() {
                ret.push_command(command);
                command = CommandWithArgs::new();
            } else {
                // XXXX Error...
            }
            continue;
        }
        if in_string || in_stringd {
            token.push(ch);
        } else if ch == ' ' {
            if !token.is_empty() {
                command.push_arg(token);
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
            command.push_arg(token);
        }
    }
    ret.push_command(command);
    ret
}
