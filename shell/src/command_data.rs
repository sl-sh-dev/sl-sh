use crate::unix::{close_fd, dup_fd};
use std::ffi::{OsStr, OsString};
use std::fs::File;
use std::os::fd::IntoRawFd;
use std::path::{Path, PathBuf};

/// Arg to a command, either a direct string or a sub command to run to get the arg.
#[derive(Clone, Debug)]
pub enum Arg {
    Str(OsString),
    Command(Run),
}

#[derive(Clone, Debug)]
enum IoType {
    FileDescriptor(i32),
    FilePath(PathBuf, bool), //  Path, overwrite file?
}

#[derive(Clone, Debug)]
enum StdIoType {
    Stdin(IoType),
    Stdout(IoType),
    Stderr(IoType),
}

/// Optional file descriptors for standard in, out and error.
#[derive(Clone, Debug)]
pub struct StdIos {
    redirects: Vec<StdIoType>,
}

fn path_fd(name: &Path, overwrite: bool, is_stdin: bool) -> Option<i32> {
    let f = if is_stdin {
        File::open(name)
    } else if overwrite {
        File::create(name)
    } else {
        File::options().append(true).create(true).open(name)
    };
    match f {
        Ok(f) => Some(f.into_raw_fd()),
        Err(err) => {
            eprintln!("Error opening {}: {err}", name.display());
            None
        }
    }
}

fn dup_stdio(fd: i32, std: Option<i32>) -> i32 {
    if let Some(std_fd) = std {
        if let Ok(new_fd) = dup_fd(std_fd) {
            new_fd
        } else {
            fd
        }
    } else {
        fd
    }
}

fn set_io(stdio: &mut Option<i32>, fd: i32) {
    if let Some(old_fd) = stdio {
        // Close the previous FD.
        let _ = close_fd(*old_fd);
    }
    *stdio = Some(fd);
}

impl StdIos {
    pub fn stdio(&self) -> (Option<i32>, Option<i32>, Option<i32>) {
        let mut stdin: Option<i32> = None;
        let mut stdout: Option<i32> = None;
        let mut stderr: Option<i32> = None;

        for s in &self.redirects {
            match s {
                StdIoType::Stdin(IoType::FileDescriptor(fd)) => set_io(&mut stdin, *fd),
                StdIoType::Stdout(IoType::FileDescriptor(fd)) => {
                    let new_fd = if *fd == 2 {
                        // Set equal to current stderr
                        dup_stdio(*fd, stderr)
                    } else {
                        *fd
                    };
                    set_io(&mut stdout, new_fd);
                }
                StdIoType::Stderr(IoType::FileDescriptor(fd)) => {
                    let new_fd = if *fd == 1 {
                        // Set equal to current stdout
                        dup_stdio(*fd, stdout)
                    } else {
                        *fd
                    };
                    set_io(&mut stderr, new_fd);
                }
                StdIoType::Stdin(IoType::FilePath(path, overwrite)) => {
                    let new_fd = path_fd(path, *overwrite, true);
                    if let Some(new_fd) = new_fd {
                        set_io(&mut stdin, new_fd);
                    }
                }
                StdIoType::Stdout(IoType::FilePath(path, overwrite)) => {
                    let new_fd = path_fd(path, *overwrite, false);
                    if let Some(new_fd) = new_fd {
                        set_io(&mut stdout, new_fd);
                    }
                }
                StdIoType::Stderr(IoType::FilePath(path, overwrite)) => {
                    let new_fd = path_fd(path, *overwrite, false);
                    if let Some(new_fd) = new_fd {
                        set_io(&mut stderr, new_fd);
                    }
                }
            }
        }
        (stdin, stdout, stderr)
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

    /// Return the stdios for this Run.
    pub fn stdio(&self) -> (Option<i32>, Option<i32>, Option<i32>) {
        match self {
            Run::Command(_, ios) => ios.as_ref().map(|io| io.stdio()).unwrap_or_default(),
            Run::Pipe(_, ios) => ios.as_ref().map(|io| io.stdio()).unwrap_or_default(),
            Run::Sequence(_, ios) => ios.as_ref().map(|io| io.stdio()).unwrap_or_default(),
            Run::And(_, ios) => ios.as_ref().map(|io| io.stdio()).unwrap_or_default(),
            Run::Or(_, ios) => ios.as_ref().map(|io| io.stdio()).unwrap_or_default(),
            Run::Empty => (None, None, None),
        }
    }

    /// Set the FDs this Ios.  Will NOT overwrite existing values (FDs or names), this is important
    /// for the proper interaction of pipes and redirects.
    fn set_io_inner(
        ios: &mut BoxedIos,
        stdin: Option<i32>,
        stdout: Option<i32>,
        stderr: Option<i32>,
        push_back: bool,
    ) {
        match ios {
            Some(ios) => {
                if let Some(fd) = stdin {
                    if push_back {
                        ios.redirects
                            .push(StdIoType::Stdin(IoType::FileDescriptor(fd)));
                    } else {
                        ios.redirects
                            .insert(0, StdIoType::Stdin(IoType::FileDescriptor(fd)));
                    }
                }
                if let Some(fd) = stdout {
                    if push_back {
                        ios.redirects
                            .push(StdIoType::Stdout(IoType::FileDescriptor(fd)));
                    } else {
                        ios.redirects
                            .insert(0, StdIoType::Stdout(IoType::FileDescriptor(fd)));
                    }
                }
                if let Some(fd) = stderr {
                    if push_back {
                        ios.redirects
                            .push(StdIoType::Stderr(IoType::FileDescriptor(fd)));
                    } else {
                        ios.redirects
                            .insert(0, StdIoType::Stderr(IoType::FileDescriptor(fd)));
                    }
                }
            }
            None => {
                let nios = Box::new(StdIos { redirects: vec![] });
                *ios = Some(nios);
                Self::set_io_inner(ios, stdin, stdout, stderr, push_back)
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
                if let Some(name) = in_name {
                    ios.redirects
                        .push(StdIoType::Stdin(IoType::FilePath((*name).0, (*name).1)));
                }
                if let Some(name) = out_name {
                    ios.redirects
                        .push(StdIoType::Stdout(IoType::FilePath((*name).0, (*name).1)));
                }
                if let Some(name) = err_name {
                    ios.redirects
                        .push(StdIoType::Stderr(IoType::FilePath((*name).0, (*name).1)));
                }
            }
            None => {
                let nios = Box::new(StdIos { redirects: vec![] });
                *ios = Some(nios);
                Self::set_io_inner_names(ios, in_name, out_name, err_name)
            }
        }
    }

    /// Set the IOs for this Run.
    pub fn set_io(&mut self, stdin: Option<i32>, stdout: Option<i32>, stderr: Option<i32>) {
        match self {
            Run::Command(_, ios) => Self::set_io_inner(ios, stdin, stdout, stderr, true),
            Run::Pipe(_, ios) => Self::set_io_inner(ios, stdin, stdout, stderr, true),
            Run::Sequence(_, ios) => Self::set_io_inner(ios, stdin, stdout, stderr, true),
            Run::And(_, ios) => Self::set_io_inner(ios, stdin, stdout, stderr, true),
            Run::Or(_, ios) => Self::set_io_inner(ios, stdin, stdout, stderr, true),
            Run::Empty => {}
        }
    }

    /// Set the IOs for this Run- Put at front of the queue for pipes.
    pub fn set_io_first(&mut self, stdin: Option<i32>, stdout: Option<i32>, stderr: Option<i32>) {
        match self {
            Run::Command(_, ios) => Self::set_io_inner(ios, stdin, stdout, stderr, false),
            Run::Pipe(_, ios) => Self::set_io_inner(ios, stdin, stdout, stderr, false),
            Run::Sequence(_, ios) => Self::set_io_inner(ios, stdin, stdout, stderr, false),
            Run::And(_, ios) => Self::set_io_inner(ios, stdin, stdout, stderr, false),
            Run::Or(_, ios) => Self::set_io_inner(ios, stdin, stdout, stderr, false),
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
            Run::Pipe(seq, _ios) => {
                if let Some(last) = seq.last_mut() {
                    last.set_stdout_path((*inner.unwrap()).0, overwrite);
                }
            }
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
