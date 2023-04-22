use std::ffi::{OsStr, OsString};
use std::fs::File;
use std::os::fd::IntoRawFd;
use std::path::PathBuf;

/// Arg to a command, either a direct string or a sub command to run to get the arg.
#[derive(Clone, Debug)]
pub enum Arg {
    Str(OsString),
    Command(Run),
}

/// Optional file descriptors for standard in, out and error.
#[derive(Clone, Debug)]
pub struct StdIos {
    stdin: Option<i32>,
    stdout: Option<i32>,
    stderr: Option<i32>,
    in_name: Option<Box<(PathBuf, bool)>>,
    out_name: Option<Box<(PathBuf, bool)>>,
    err_name: Option<Box<(PathBuf, bool)>>,
}

fn extract_fd(fd: &Option<i32>, file_info: Option<&(PathBuf, bool)>, stdin: bool) -> Option<i32> {
    if let Some(fd) = fd {
        Some(*fd)
    } else if let Some((name, overwrite)) = file_info {
        let f = if stdin {
            File::open(name)
        } else if *overwrite {
            File::create(name)
        } else {
            File::options().append(true).create(true).open(name)
        };
        if let Ok(f) = f {
            Some(f.into_raw_fd())
        } else {
            None
        }
    } else {
        None
    }
}

impl StdIos {
    /// Stdin file descriptor.
    pub fn stdin(&self) -> Option<i32> {
        extract_fd(&self.stdin, self.in_name.as_deref(), true)
    }

    /// Stdout file descriptor.
    pub fn stdout(&self) -> Option<i32> {
        extract_fd(&self.stdout, self.out_name.as_deref(), false)
    }

    /// Stderr file descriptor.
    pub fn stderr(&self) -> Option<i32> {
        extract_fd(&self.stderr, self.err_name.as_deref(), false)
    }
}

/// An individual command with args.
#[derive(Clone, Debug)]
pub struct CommandWithArgs {
    /// args[0] is the command.
    args: Vec<Arg>,
}

impl CommandWithArgs {
    /// Create a new empty command and args.
    pub fn new() -> Self {
        Self { args: vec![] }
    }

    /// Push a new arg onto the command, the first "arg" is the command itself.
    pub fn push_arg(&mut self, arg: OsString) {
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

    /// Iterator over the argumants for the command.
    pub fn args_iter(&self) -> CommandArgs {
        CommandArgs {
            args: self.args(),
            temps: vec![],
            index: 0,
        }
    }
}

impl Default for CommandWithArgs {
    fn default() -> Self {
        Self::new()
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
    /// Push a single command onto the Run.  If it is not the first command will add to or create a sequence.
    pub fn push_command(self, command: CommandWithArgs, com_ios: BoxedIos) -> Self {
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

    /// Push onto the existing current sequence (creates a sequence if this is the second command).
    pub fn _push_to_last(self, command: CommandWithArgs, com_ios: BoxedIos) -> Self {
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

    /// Push onto an existing or create a new pipe sequence.
    pub fn push_pipe(self, command: CommandWithArgs, com_ios: BoxedIos) -> Self {
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

    /// Push onto an existing or create a new sequence.
    pub fn push_sequence(self, command: CommandWithArgs, com_ios: BoxedIos) -> Self {
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

    /// Push onto an existing or create a new AND sequence.
    pub fn push_and(self, command: CommandWithArgs, com_ios: BoxedIos) -> Self {
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

    /// Push onto an existing or create a new OR sequence.
    pub fn push_or(self, command: CommandWithArgs, com_ios: BoxedIos) -> Self {
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

    /// Return the stdin for this Run.
    pub fn stdin(&self) -> Option<i32> {
        match self {
            Run::Command(_, ios) => ios.as_ref().and_then(|io| io.stdin()),
            Run::Pipe(_, ios) => ios.as_ref().and_then(|io| io.stdin()),
            Run::Sequence(_, ios) => ios.as_ref().and_then(|io| io.stdin()),
            Run::And(_, ios) => ios.as_ref().and_then(|io| io.stdin()),
            Run::Or(_, ios) => ios.as_ref().and_then(|io| io.stdin()),
            Run::Empty => None,
        }
    }

    /// Return the stdout for this Run.
    pub fn stdout(&self) -> Option<i32> {
        match self {
            Run::Command(_, ios) => ios.as_ref().and_then(|io| io.stdout()),
            Run::Pipe(_, ios) => ios.as_ref().and_then(|io| io.stdout()),
            Run::Sequence(_, ios) => ios.as_ref().and_then(|io| io.stdout()),
            Run::And(_, ios) => ios.as_ref().and_then(|io| io.stdout()),
            Run::Or(_, ios) => ios.as_ref().and_then(|io| io.stdout()),
            Run::Empty => None,
        }
    }

    /// Return the stderr for this Run.
    pub fn stderr(&self) -> Option<i32> {
        match self {
            Run::Command(_, ios) => ios.as_ref().and_then(|io| io.stderr()),
            Run::Pipe(_, ios) => ios.as_ref().and_then(|io| io.stderr()),
            Run::Sequence(_, ios) => ios.as_ref().and_then(|io| io.stderr()),
            Run::And(_, ios) => ios.as_ref().and_then(|io| io.stderr()),
            Run::Or(_, ios) => ios.as_ref().and_then(|io| io.stderr()),
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
                    in_name: None,
                    out_name: None,
                    err_name: None,
                });
                *ios = Some(nios);
            }
        }
    }

    fn set_io_inner_names(
        ios: &mut BoxedIos,
        in_name: Option<Box<(PathBuf, bool)>>,
        out_name: Option<Box<(PathBuf, bool)>>,
        err_name: Option<Box<(PathBuf, bool)>>,
    ) {
        match ios {
            Some(ios) => {
                if in_name.is_some() {
                    ios.in_name = in_name;
                }
                if out_name.is_some() {
                    ios.out_name = out_name;
                }
                if err_name.is_some() {
                    ios.err_name = err_name;
                }
            }
            None => {
                let nios = Box::new(StdIos {
                    stdin: None,
                    stdout: None,
                    stderr: None,
                    in_name,
                    out_name,
                    err_name,
                });
                *ios = Some(nios);
            }
        }
    }

    /// Set the IOs for this Run.
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

    /// Set the stdin file name for this Run.
    pub fn set_stdin_path(&mut self, stdin: PathBuf, overwrite: bool) {
        let inner = Some(Box::new((stdin, overwrite)));
        match self {
            Run::Command(_, ios) => Self::set_io_inner_names(ios, inner, None, None),
            Run::Pipe(_, ios) => Self::set_io_inner_names(ios, inner, None, None),
            Run::Sequence(seq, _ios) => {
                if let Some(last) = seq.last_mut() {
                    last.set_stdin_path((*inner.unwrap()).0, overwrite);
                }
            }
            Run::And(seq, _ios) => {
                if let Some(last) = seq.last_mut() {
                    last.set_stdin_path((*inner.unwrap()).0, overwrite);
                }
            }
            Run::Or(seq, _ios) => {
                if let Some(last) = seq.last_mut() {
                    last.set_stdin_path((*inner.unwrap()).0, overwrite);
                }
            }
            Run::Empty => {}
        }
    }

    /// Set the stdout file name for this Run.
    pub fn set_stdout_path(&mut self, stdout: PathBuf, overwrite: bool) {
        let inner = Some(Box::new((stdout, overwrite)));
        match self {
            Run::Command(_, ios) => Self::set_io_inner_names(ios, None, inner, None),
            Run::Pipe(_, ios) => Self::set_io_inner_names(ios, None, inner, None),
            Run::Sequence(seq, _ios) => {
                if let Some(last) = seq.last_mut() {
                    last.set_stdout_path((*inner.unwrap()).0, overwrite);
                }
            }
            Run::And(seq, _ios) => {
                if let Some(last) = seq.last_mut() {
                    last.set_stdout_path((*inner.unwrap()).0, overwrite);
                }
            }
            Run::Or(seq, _ios) => {
                if let Some(last) = seq.last_mut() {
                    last.set_stdout_path((*inner.unwrap()).0, overwrite);
                }
            }
            Run::Empty => {}
        }
    }

    /// Set the stderr file name for this Run.
    pub fn set_stderr_path(&mut self, stderr: PathBuf, overwrite: bool) {
        let inner = Some(Box::new((stderr, overwrite)));
        match self {
            Run::Command(_, ios) => Self::set_io_inner_names(ios, None, None, inner),
            Run::Pipe(_, ios) => Self::set_io_inner_names(ios, None, None, inner),
            Run::Sequence(seq, _ios) => {
                if let Some(last) = seq.last_mut() {
                    last.set_stderr_path((*inner.unwrap()).0, overwrite);
                }
            }
            Run::And(seq, _ios) => {
                if let Some(last) = seq.last_mut() {
                    last.set_stderr_path((*inner.unwrap()).0, overwrite);
                }
            }
            Run::Or(seq, _ios) => {
                if let Some(last) = seq.last_mut() {
                    last.set_stderr_path((*inner.unwrap()).0, overwrite);
                }
            }
            Run::Empty => {}
        }
    }
}
