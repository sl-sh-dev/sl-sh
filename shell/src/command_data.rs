use crate::unix::{close_fd, dup_fd};
use std::collections::HashSet;
use std::ffi::{OsStr, OsString};
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::os::fd::IntoRawFd;
use std::path::{Path, PathBuf};

/// Arg to a command, either a direct string or a sub command to run to get the arg.
#[derive(Clone, Debug)]
pub enum Arg {
    Str(OsString),
    Command(Run),
}

impl Display for Arg {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Str(os_str) => write!(f, "{}", os_str.to_string_lossy()),
            Self::Command(run) => write!(f, "$({run})"),
        }
    }
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

impl Display for StdIoType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Stdin(IoType::FileDescriptor(fd)) => write!(f, "<{fd}"),
            Self::Stdin(IoType::FilePath(path, _overwrite)) => write!(f, "<{}", path.display()),
            Self::Stdout(IoType::FileDescriptor(fd)) => write!(f, "1>{fd}"),
            Self::Stdout(IoType::FilePath(path, true)) => write!(f, ">{}", path.display()),
            Self::Stdout(IoType::FilePath(path, false)) => write!(f, ">>{}", path.display()),
            Self::Stderr(IoType::FileDescriptor(fd)) => write!(f, "2>{fd}"),
            Self::Stderr(IoType::FilePath(path, true)) => write!(f, "2>{}", path.display()),
            Self::Stderr(IoType::FilePath(path, false)) => write!(f, "2>>{}", path.display()),
        }
    }
}

/// Optional file descriptors for standard in, out and error.
#[derive(Clone, Debug)]
pub struct StdIos {
    redirects: Vec<StdIoType>,
}

impl Display for StdIos {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for redir in &self.redirects {
            write!(f, " {redir}")?;
        }
        Ok(())
    }
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
    /// Create a new IO redirect stack.
    pub fn new() -> Self {
        Self { redirects: vec![] }
    }

    /// Resolves the redirect stack and returns the computed stdin, stdout and stderr.
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

    /// Push a stdin fd to the redirect stack.
    /// If push_back is true then push to the end else put on the front (useful for pipes).
    pub fn set_in_fd(&mut self, fd: i32, push_back: bool) {
        if push_back {
            self.redirects
                .push(StdIoType::Stdin(IoType::FileDescriptor(fd)));
        } else {
            self.redirects
                .insert(0, StdIoType::Stdin(IoType::FileDescriptor(fd)));
        }
    }

    /// Push a stdout fd to the redirect stack.
    /// If push_back is true then push to the end else put on the front (useful for pipes).
    pub fn set_out_fd(&mut self, fd: i32, push_back: bool) {
        if push_back {
            self.redirects
                .push(StdIoType::Stdout(IoType::FileDescriptor(fd)));
        } else {
            self.redirects
                .insert(0, StdIoType::Stdout(IoType::FileDescriptor(fd)));
        }
    }

    /// Push a stderr fd to the redirect stack.
    /// If push_back is true then push to the end else put on the front (useful for pipes).
    pub fn set_err_fd(&mut self, fd: i32, push_back: bool) {
        if push_back {
            self.redirects
                .push(StdIoType::Stderr(IoType::FileDescriptor(fd)));
        } else {
            self.redirects
                .insert(0, StdIoType::Stderr(IoType::FileDescriptor(fd)));
        }
    }

    /// Push a stdin file path to the redirect stack.
    pub fn set_in_path(&mut self, path: PathBuf, overwrite: bool) {
        self.redirects
            .push(StdIoType::Stdin(IoType::FilePath(path, overwrite)));
    }

    /// Push a stdin file path to the redirect stack.
    pub fn set_out_path(&mut self, path: PathBuf, overwrite: bool) {
        self.redirects
            .push(StdIoType::Stdout(IoType::FilePath(path, overwrite)));
    }

    /// Push a stderr file path to the redirect stack.
    pub fn set_err_path(&mut self, path: PathBuf, overwrite: bool) {
        self.redirects
            .push(StdIoType::Stderr(IoType::FilePath(path, overwrite)));
    }

    /// Clear the redirect stack.
    pub fn clear(&mut self) {
        self.redirects.clear();
    }

    fn collect_fds(&self, fd_set: &mut HashSet<i32>) {
        for r in &self.redirects {
            match r {
                StdIoType::Stdin(io) | StdIoType::Stdout(io) | StdIoType::Stderr(io) => match io {
                    IoType::FileDescriptor(fd) => {
                        fd_set.insert(*fd);
                    }
                    IoType::FilePath(_, _) => {}
                },
            }
        }
    }
}

impl Default for StdIos {
    fn default() -> Self {
        Self::new()
    }
}

/// An individual command with args.
#[derive(Clone, Debug)]
pub struct CommandWithArgs {
    /// args[0] is the command.
    args: Vec<Arg>,
    stdios: Option<StdIos>,
}

impl Display for CommandWithArgs {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut first = true;
        for arg in &self.args {
            if first {
                write!(f, "{arg}")?;
                first = false;
            } else {
                write!(f, " {arg}")?;
            }
        }
        if let Some(ios) = &self.stdios {
            write!(f, "{ios}")?;
        }
        Ok(())
    }
}

impl CommandWithArgs {
    /// Create a new empty command and args.
    pub fn new() -> Self {
        Self {
            args: vec![],
            stdios: None,
        }
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

    /// Set the stdio redirect stack for this command.
    pub fn set_stdios(&mut self, stdios: StdIos) {
        self.stdios = Some(stdios);
    }

    /// Resolve the stdios redirect stack and return stdin/out/err if they exist.
    pub fn stdios(&self) -> (Option<i32>, Option<i32>, Option<i32>) {
        self.stdios
            .as_ref()
            .map(|io| io.stdio())
            .unwrap_or_default()
    }

    /// If fd is Some value then put it at the front of the redir queue for this command.
    pub fn push_stdin_front(&mut self, fd: Option<i32>) {
        if let Some(fd) = fd {
            if let Some(stdios) = self.stdios.as_mut() {
                stdios.set_in_fd(fd, false);
            } else {
                let mut stdios = StdIos::default();
                stdios.set_in_fd(fd, true);
                self.stdios = Some(stdios);
            }
        }
    }

    /// If fd is Some value then put it at the front of the redir queue for this command.
    pub fn push_stdout_front(&mut self, fd: Option<i32>) {
        if let Some(fd) = fd {
            if let Some(stdios) = self.stdios.as_mut() {
                stdios.set_out_fd(fd, false);
            } else {
                let mut stdios = StdIos::default();
                stdios.set_out_fd(fd, true);
                self.stdios = Some(stdios);
            }
        }
    }

    fn collect_fds(&self, fd_set: &mut HashSet<i32>) {
        if let Some(stdios) = &self.stdios {
            stdios.collect_fds(fd_set);
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
                    self.temps.push("XXXX".into());
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

/// Command(s) ready to run with context.
#[derive(Clone, Debug)]
pub enum Run {
    Command(CommandWithArgs),
    BackgroundCommand(CommandWithArgs),
    Pipe(Vec<Run>),
    Sequence(Vec<Run>),
    And(Vec<Run>),
    Or(Vec<Run>),
    Subshell(Box<Run>),
    Empty,
}

fn write_seq(f: &mut Formatter<'_>, seq: &[Run], sep: &str) -> std::fmt::Result {
    let mut first = true;
    for s in seq {
        if first {
            write!(f, "{s}")?;
            first = false;
        } else {
            write!(f, " {sep} {s}")?;
        }
    }
    Ok(())
}

impl Display for Run {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Command(command) => write!(f, "{command}")?,
            Self::BackgroundCommand(command) => write!(f, "{command} &")?,
            Self::Pipe(seq) => write_seq(f, &seq[..], "|")?,
            Self::Sequence(seq) => write_seq(f, &seq[..], ";")?,
            Self::And(seq) => write_seq(f, &seq[..], "&&")?,
            Self::Or(seq) => write_seq(f, &seq[..], "||")?,
            Self::Subshell(sub_run) => write!(f, "({sub_run})")?,
            Self::Empty => {}
        }
        Ok(())
    }
}

impl Run {
    /// Push a new Run onto the Run.  If it is not the first command will add to or create a sequence.
    pub fn push_run(self, new_run: Run) -> Self {
        match self {
            Run::Command(current) => Run::Sequence(vec![Run::Command(current), new_run]),
            Run::BackgroundCommand(current) => {
                Run::Sequence(vec![Run::BackgroundCommand(current), new_run])
            }
            Run::Pipe(pipe) => Run::Sequence(vec![Run::Pipe(pipe), new_run]),
            Run::Sequence(mut seq) => {
                seq.push(new_run);
                Run::Sequence(seq)
            }
            Run::And(seq) => Run::Sequence(vec![Run::And(seq), new_run]),
            Run::Or(seq) => Run::Sequence(vec![Run::Or(seq), new_run]),
            Run::Subshell(current) => Run::Sequence(vec![Run::Subshell(current), new_run]),
            Run::Empty => new_run,
        }
    }

    /// Push Run onto an existing or create a new pipe sequence.
    pub fn push_pipe(self, new_run: Run) -> Self {
        match self {
            Run::Command(current) => Run::Pipe(vec![Run::Command(current), new_run]),
            Run::BackgroundCommand(current) => {
                Run::Pipe(vec![Run::BackgroundCommand(current), new_run])
            }
            Run::Pipe(mut pipe) => {
                pipe.push(new_run);
                Run::Pipe(pipe)
            }
            Run::Sequence(seq) => Run::Pipe(vec![Run::Sequence(seq), new_run]),
            Run::And(seq) => Run::Pipe(vec![Run::And(seq), new_run]),
            Run::Or(seq) => Run::Pipe(vec![Run::Or(seq), new_run]),
            Run::Subshell(current) => Run::Pipe(vec![Run::Subshell(current), new_run]),
            Run::Empty => new_run,
        }
    }

    /// Push new Run onto an existing or create a new sequence.
    pub fn push_sequence(self, new_run: Run) -> Self {
        match self {
            Run::Command(current) => Run::Sequence(vec![Run::Command(current), new_run]),
            Run::BackgroundCommand(current) => {
                Run::Sequence(vec![Run::BackgroundCommand(current), new_run])
            }
            Run::Pipe(pipe) => Run::Sequence(vec![Run::Pipe(pipe), new_run]),
            Run::Sequence(mut seq) => {
                seq.push(new_run);
                Run::Sequence(seq)
            }
            Run::And(seq) => Run::Sequence(vec![Run::And(seq), new_run]),
            Run::Or(seq) => Run::Sequence(vec![Run::Or(seq), new_run]),
            Run::Subshell(current) => Run::Sequence(vec![Run::Subshell(current), new_run]),
            Run::Empty => new_run,
        }
    }

    /// Push new Run onto an existing or create a new AND sequence.
    pub fn push_and(self, new_run: Run) -> Self {
        match self {
            Run::Command(current) => Run::And(vec![Run::Command(current), new_run]),
            Run::BackgroundCommand(current) => {
                Run::And(vec![Run::BackgroundCommand(current), new_run])
            }
            Run::Pipe(pipe) => Run::And(vec![Run::Pipe(pipe), new_run]),
            Run::Sequence(seq) => Run::And(vec![Run::Sequence(seq), new_run]),
            Run::And(mut seq) => {
                seq.push(new_run);
                Run::And(seq)
            }
            Run::Or(seq) => Run::And(vec![Run::Or(seq), new_run]),
            Run::Subshell(current) => Run::And(vec![Run::Subshell(current), new_run]),
            Run::Empty => new_run,
        }
    }

    /// Push new Run onto an existing or create a new OR sequence.
    pub fn push_or(self, new_run: Run) -> Self {
        match self {
            Run::Command(current) => Run::Or(vec![Run::Command(current), new_run]),
            Run::BackgroundCommand(current) => {
                Run::Or(vec![Run::BackgroundCommand(current), new_run])
            }
            Run::Pipe(pipe) => Run::Or(vec![Run::Pipe(pipe), new_run]),
            Run::Sequence(seq) => Run::Or(vec![Run::Or(seq), new_run]),
            Run::And(seq) => Run::Or(vec![Run::And(seq), new_run]),
            Run::Or(mut seq) => {
                seq.push(new_run);
                Run::Or(seq)
            }
            Run::Subshell(current) => Run::Or(vec![Run::Subshell(current), new_run]),
            Run::Empty => new_run,
        }
    }

    /// If fd is Some value then put it at the front of the redir queue for the first command in the Run.
    pub fn push_stdin_front(&mut self, fd: Option<i32>) {
        if let Some(fd) = fd {
            match self {
                Run::Command(current) => current.push_stdin_front(Some(fd)),
                Run::BackgroundCommand(current) => current.push_stdin_front(Some(fd)),
                Run::Pipe(ref mut seq)
                | Run::Sequence(ref mut seq)
                | Run::And(ref mut seq)
                | Run::Or(ref mut seq) => {
                    if let Some(run) = seq.first_mut() {
                        run.push_stdin_front(Some(fd));
                    }
                }
                Run::Subshell(ref mut current) => current.push_stdin_front(Some(fd)),
                Run::Empty => {}
            }
        }
    }

    /// If fd is Some value then put it at the front of the redir queue for the last command in the Run.
    pub fn push_stdout_front(&mut self, fd: Option<i32>) {
        if let Some(fd) = fd {
            match self {
                Run::Command(current) => current.push_stdout_front(Some(fd)),
                Run::BackgroundCommand(current) => current.push_stdout_front(Some(fd)),
                Run::Pipe(ref mut seq)
                | Run::Sequence(ref mut seq)
                | Run::And(ref mut seq)
                | Run::Or(ref mut seq) => {
                    if let Some(run) = seq.last_mut() {
                        run.push_stdout_front(Some(fd));
                    }
                }
                Run::Subshell(ref mut current) => current.push_stdout_front(Some(fd)),
                Run::Empty => {}
            }
        }
    }

    fn collect_fds(&self, fd_set: &mut HashSet<i32>) {
        match self {
            Run::Command(current) => current.collect_fds(fd_set),
            Run::BackgroundCommand(current) => current.collect_fds(fd_set),
            Run::Pipe(ref seq) | Run::Sequence(ref seq) | Run::And(ref seq) | Run::Or(ref seq) => {
                for run in seq {
                    run.collect_fds(fd_set);
                }
            }
            Run::Subshell(ref current) => current.collect_fds(fd_set),
            Run::Empty => {}
        }
    }

    pub fn get_redir_fds(&self) -> HashSet<i32> {
        let mut res = HashSet::new();
        self.collect_fds(&mut res);
        res
    }
}
