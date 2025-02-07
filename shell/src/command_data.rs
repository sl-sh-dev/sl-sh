use crate::jobs::Jobs;
use crate::platform::{FileDesc, FromFileDesc, Platform, Sys, STDIN_FILENO, STDOUT_FILENO};
use std::collections::HashSet;
use std::ffi::OsString;
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io;
use std::io::{BufRead, ErrorKind, Write};
use std::str::FromStr;

/// Arg to a command, either a direct string or a sub command to run to get the arg.
#[derive(Clone, Debug)]
pub enum Arg {
    /// Single string arg.
    Str(OsString),
    /// A command to run to get the string arg.
    Command(Run),
    /// Env variable to use to set the arg.
    Var(OsString),
    /// List of args that will be concatenated to make the arg.
    Compound(Vec<Arg>),
}

impl Arg {
    pub fn resolve_arg(&self, jobs: &mut Jobs) -> io::Result<OsString> {
        match self {
            Self::Str(val) => Ok(val.clone()),
            Self::Command(run) => {
                let (input, output) = Sys::anon_pipe()?;
                let mut run = run.clone();
                run.push_stdout_front(Some(output));
                let mut job = jobs.new_job();
                Sys::fork_run(&run, &mut job, jobs)?;
                let lines = io::BufReader::new(unsafe { File::from_file_desc(input) }).lines();
                let mut val = String::new();
                for (i, line) in lines.enumerate() {
                    if i > 0 {
                        val.push(' ');
                    }
                    let line = line?;
                    val.push_str(line.trim());
                }
                Ok(val.into())
            }
            Self::Var(var_name) => {
                if let Some(val) = jobs.get_env_or_local_var(var_name) {
                    Ok(val)
                } else {
                    Ok("".into())
                }
            }
            Self::Compound(cargs) => {
                let mut val = String::new();
                for a in cargs {
                    val.push_str(a.resolve_arg(jobs)?.to_string_lossy().as_ref());
                }
                Ok(val.into())
            }
        }
    }
}

impl TryFrom<&mut Arg> for FileDesc {
    type Error = ();

    fn try_from(arg: &mut Arg) -> Result<Self, Self::Error> {
        if let Arg::Str(targ) = arg {
            let fd = targ.to_string_lossy();
            if fd.ends_with('-') {
                match FileDesc::from_str(&fd[0..fd.len() - 1]) {
                    Ok(fd) => Ok(fd),
                    Err(_) => Err(()),
                }
            } else {
                match FileDesc::from_str(&fd) {
                    Ok(fd) => Ok(fd),
                    Err(_) => Err(()),
                }
            }
        } else {
            Err(())
        }
    }
}

impl Display for Arg {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Str(os_str) => write!(f, "{}", os_str.to_string_lossy()),
            Self::Command(run) => write!(f, "$({run})"),
            Self::Var(var) => write!(f, "${}", var.to_string_lossy()),
            Self::Compound(cargs) => {
                for a in cargs {
                    write!(f, "{a}")?;
                }
                Ok(())
            }
        }
    }
}

/// An argument for a redirect (the source).
#[derive(Clone, Debug)]
enum RedirArg {
    /// Arg should resolve to a file path.
    Path(Arg),
    /// Arg should resolve to file descriptor (positive integer or '-' to close).
    Fd(Arg),
    /// A file descriptor created and managed by the shell (for instance for pipes).
    InternalFd(FileDesc),
}

impl Display for RedirArg {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RedirArg::Path(arg) => write!(f, "{arg}"),
            RedirArg::Fd(arg) => write!(f, "&{arg}"),
            RedirArg::InternalFd(_fd) => write!(f, ""),
        }
    }
}

/// An individual redirect, first element is always the target file descriptor.
#[derive(Clone, Debug)]
enum RedirType {
    /// An input file to open and dup to fd.
    In(FileDesc, RedirArg),
    /// Inject Arg as data into the fd.
    InDirect(FileDesc, Arg),
    /// An output file to open (append) and dup to fd.
    Out(FileDesc, RedirArg),
    /// An output file to open (create/trunc) and dup to fd.
    OutTrunc(FileDesc, RedirArg),
    /// An input/output file to open (append) and dup to fd.
    InOut(FileDesc, RedirArg),
}

impl Display for RedirType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RedirType::In(fd, arg) => write!(f, " {fd}<{arg}"),
            RedirType::InDirect(fd, arg) => write!(f, " {fd}<<{arg}"),
            RedirType::Out(fd, arg) => write!(f, " {fd}>>{arg}"),
            RedirType::OutTrunc(fd, arg) => write!(f, " {fd}>{arg}"),
            RedirType::InOut(fd, arg) => write!(f, " {fd}<>{arg}"),
        }
    }
}

impl RedirType {
    fn process_source_fd(
        dest_fd: FileDesc,
        arg: &Arg,
        jobs: &mut Jobs,
    ) -> Result<FileDesc, io::Error> {
        let targ = arg.resolve_arg(jobs)?;
        let source_fd = targ.to_string_lossy();
        if source_fd == "-" {
            Sys::close_fd(dest_fd)?;
            Ok(dest_fd)
        } else if source_fd.ends_with('-') {
            match FileDesc::from_str(&source_fd[0..source_fd.len() - 1]) {
                Ok(source_fd) => {
                    Sys::dup2_fd(source_fd, dest_fd)?;
                    Sys::close_fd(source_fd)?;
                    Ok(dest_fd)
                }
                Err(err) => Err(io::Error::new(ErrorKind::Other, err)),
            }
        } else {
            match FileDesc::from_str(&source_fd) {
                Ok(source_fd) => Sys::dup2_fd(source_fd, dest_fd),
                Err(err) => Err(io::Error::new(ErrorKind::Other, err)),
            }
        }
    }

    fn process(&self, jobs: &mut Jobs) -> Result<FileDesc, io::Error> {
        match self {
            RedirType::In(fd, RedirArg::Path(arg)) => {
                let path = arg.resolve_arg(jobs)?;
                let f = File::open(path)?;
                // Use as_raw_fd ot nto_raw_fd so f will close when dropped.
                Sys::dup2_fd(f.into(), *fd)?;
                Ok(*fd)
            }
            RedirType::In(fd, RedirArg::Fd(arg)) => Self::process_source_fd(*fd, arg, jobs),
            RedirType::InDirect(fd, arg) => {
                let tdata = arg.resolve_arg(jobs)?;
                let data = tdata.to_string_lossy();
                if let Ok((pread, pwrite)) = Sys::anon_pipe() {
                    Sys::dup2_fd(pread, *fd)?;
                    Sys::close_fd(pread)?;
                    unsafe {
                        let mut file = File::from_file_desc(pwrite);
                        if let Err(e) = file.write_all(data.as_bytes()) {
                            eprintln!("Error writing {data} to fd {fd}: {e}");
                        }
                    }
                }
                Ok(*fd)
            }
            RedirType::In(dest_fd, RedirArg::InternalFd(source_fd)) => {
                Sys::dup2_fd(*source_fd, *dest_fd)
            }
            RedirType::Out(fd, RedirArg::Path(arg)) => {
                let path = arg.resolve_arg(jobs)?;
                let f = File::options().append(true).create(true).open(path)?;
                // Use as_raw_fd ot nto_raw_fd so f will close when dropped.
                Sys::dup2_fd(f.into(), *fd)?;
                Ok(*fd)
            }
            RedirType::Out(fd, RedirArg::Fd(arg)) => Self::process_source_fd(*fd, arg, jobs),
            RedirType::Out(dest_fd, RedirArg::InternalFd(source_fd)) => {
                Sys::dup2_fd(*source_fd, *dest_fd)
            }
            RedirType::OutTrunc(fd, RedirArg::Path(arg)) => {
                let path = arg.resolve_arg(jobs)?;
                let f = File::create(path)?;
                // Use as_raw_fd ot nto_raw_fd so f will close when dropped.
                Sys::dup2_fd(f.into(), *fd)?;
                Ok(*fd)
            }
            RedirType::OutTrunc(fd, RedirArg::Fd(arg)) => Self::process_source_fd(*fd, arg, jobs),
            RedirType::OutTrunc(dest_fd, RedirArg::InternalFd(source_fd)) => {
                Sys::dup2_fd(*source_fd, *dest_fd)
            }
            RedirType::InOut(fd, RedirArg::Path(arg)) => {
                let path = arg.resolve_arg(jobs)?;
                let f = File::options()
                    .append(true)
                    .create(true)
                    .read(true)
                    .open(path)?;
                // Use as_raw_fd ot nto_raw_fd so f will close when dropped.
                Sys::dup2_fd(f.into(), *fd)?;
                Ok(*fd)
            }
            RedirType::InOut(fd, RedirArg::Fd(arg)) => Self::process_source_fd(*fd, arg, jobs),
            RedirType::InOut(dest_fd, RedirArg::InternalFd(source_fd)) => {
                Sys::dup2_fd(*source_fd, *dest_fd)
            }
        }
    }
}

/// Contains a stack or redirects that can be processed in order to setup the file descriptors for
/// a new process.
#[derive(Clone, Debug)]
pub struct Redirects {
    redir_stack: Vec<RedirType>,
}

impl Display for Redirects {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for r in &self.redir_stack {
            write!(f, "{r}")?;
        }
        Ok(())
    }
}

impl Redirects {
    /// Create a new empty redirect stack.
    pub fn new() -> Self {
        Self {
            redir_stack: Vec::new(),
        }
    }

    /// Process the stack in order and setup all the requested file descriptors.
    /// Returns a Set containing all the file descriptors setup (to avoid closing them on process start).
    pub fn process(&self, jobs: &mut Jobs) -> Result<HashSet<FileDesc>, io::Error> {
        let mut fd_set = HashSet::new();
        for r in &self.redir_stack {
            fd_set.insert(r.process(jobs)?);
        }
        Ok(fd_set)
    }

    /// Push a fd to fd redirect onto the stack.
    /// This is for internally managed source FDs (for pipes etc).
    /// If push_back is true then push to the end else put on the front (useful for pipes).
    pub fn set_in_internal_fd(&mut self, dest_fd: FileDesc, source_fd: FileDesc, push_back: bool) {
        let redir = RedirType::In(dest_fd, RedirArg::InternalFd(source_fd));
        if push_back {
            self.redir_stack.push(redir);
        } else {
            self.redir_stack.insert(0, redir);
        }
    }

    /// Push a fd to fd redirect onto the stack.
    /// This is for internally managed source FDs (for pipes etc).
    /// If push_back is true then push to the end else put on the front (useful for pipes).
    pub fn set_out_internal_fd(&mut self, dest_fd: FileDesc, source_fd: FileDesc, push_back: bool) {
        let redir = RedirType::Out(dest_fd, RedirArg::InternalFd(source_fd));
        if push_back {
            self.redir_stack.push(redir);
        } else {
            self.redir_stack.insert(0, redir);
        }
    }

    /// Push a fd to fd redirect onto the stack.
    /// If push_back is true then push to the end else put on the front (useful for pipes).
    pub fn set_in_fd(&mut self, dest_fd: FileDesc, source_fd: Arg, push_back: bool) {
        let redir = RedirType::In(dest_fd, RedirArg::Fd(source_fd));
        if push_back {
            self.redir_stack.push(redir);
        } else {
            self.redir_stack.insert(0, redir);
        }
    }

    /// Push a fd to fd redirect onto the stack.
    /// If push_back is true then push to the end else put on the front (useful for pipes).
    pub fn set_out_fd(&mut self, dest_fd: FileDesc, source_fd: Arg, push_back: bool) {
        let redir = RedirType::OutTrunc(dest_fd, RedirArg::Fd(source_fd));
        if push_back {
            self.redir_stack.push(redir);
        } else {
            self.redir_stack.insert(0, redir);
        }
    }

    /// Push a fd to fd redirect onto the stack.
    pub fn set_in_out_fd(&mut self, dest_fd: FileDesc, source_fd: Arg) {
        let redir = RedirType::InOut(dest_fd, RedirArg::Fd(source_fd));
        self.redir_stack.push(redir);
    }

    /// Push an input file path to the redirect stack for fd.
    pub fn set_in_out_path(&mut self, dest_fd: FileDesc, path: Arg) {
        let redir = RedirType::InOut(dest_fd, RedirArg::Path(path));
        self.redir_stack.push(redir);
    }

    /// Push an input file path to the redirect stack for fd.
    pub fn set_in_path(&mut self, dest_fd: FileDesc, path: Arg) {
        let redir = RedirType::In(dest_fd, RedirArg::Path(path));
        self.redir_stack.push(redir);
    }

    /// Push input data to the redirect stack for fd.
    pub fn set_in_direct(&mut self, dest_fd: FileDesc, data: Arg) {
        let redir = RedirType::InDirect(dest_fd, data);
        self.redir_stack.push(redir);
    }

    /// Push an output file path to the redirect stack for fd.
    pub fn set_out_path(&mut self, dest_fd: FileDesc, path: Arg, overwrite: bool) {
        let redir = if overwrite {
            RedirType::OutTrunc(dest_fd, RedirArg::Path(path))
        } else {
            RedirType::Out(dest_fd, RedirArg::Path(path))
        };
        self.redir_stack.push(redir);
    }

    /// Clear the redirect stack.
    pub fn clear(&mut self) {
        self.redir_stack.clear();
    }

    /// Extend this redir stack with the redirs from other.
    pub fn extend(&mut self, other: &Redirects) {
        self.redir_stack.extend(other.redir_stack.iter().cloned());
    }

    fn collect_internal_fds(&self, fd_set: &mut HashSet<FileDesc>) {
        for r in &self.redir_stack {
            match r {
                RedirType::In(_, RedirArg::InternalFd(fd)) => {
                    fd_set.insert(*fd);
                }
                RedirType::In(_, _) => {}
                RedirType::InDirect(_, _) => {}
                RedirType::Out(_, RedirArg::InternalFd(fd)) => {
                    fd_set.insert(*fd);
                }
                RedirType::Out(_, _) => {}
                RedirType::OutTrunc(_, RedirArg::InternalFd(fd)) => {
                    fd_set.insert(*fd);
                }
                RedirType::OutTrunc(_, _) => {}
                RedirType::InOut(_, RedirArg::InternalFd(fd)) => {
                    fd_set.insert(*fd);
                }
                RedirType::InOut(_, _) => {}
            }
        }
    }

    fn fds_to_internal(&mut self, fd_set: &HashSet<FileDesc>) {
        for r in self.redir_stack.iter_mut() {
            match r {
                RedirType::In(ifd, RedirArg::Fd(fd_arg)) => {
                    if let Ok(fd) = fd_arg.try_into() {
                        if fd_set.contains(&fd) {
                            *r = RedirType::In(*ifd, RedirArg::InternalFd(fd));
                        }
                    }
                }
                RedirType::In(_, _) => {}
                RedirType::InDirect(_, _) => {}
                RedirType::Out(ofd, RedirArg::Fd(fd_arg)) => {
                    if let Ok(fd) = fd_arg.try_into() {
                        if fd_set.contains(&fd) {
                            *r = RedirType::Out(*ofd, RedirArg::InternalFd(fd));
                        }
                    }
                }
                RedirType::Out(_, _) => {}
                RedirType::OutTrunc(ofd, RedirArg::Fd(fd_arg)) => {
                    if let Ok(fd) = fd_arg.try_into() {
                        if fd_set.contains(&fd) {
                            *r = RedirType::OutTrunc(*ofd, RedirArg::InternalFd(fd));
                        }
                    }
                }
                RedirType::OutTrunc(_, _) => {}
                RedirType::InOut(iofd, RedirArg::Fd(fd_arg)) => {
                    if let Ok(fd) = fd_arg.try_into() {
                        if fd_set.contains(&fd) {
                            *r = RedirType::InOut(*iofd, RedirArg::InternalFd(fd));
                        }
                    }
                }
                RedirType::InOut(_, _) => {}
            }
        }
    }
}

impl Default for Redirects {
    fn default() -> Self {
        Self::new()
    }
}

/// An individual command with args.
#[derive(Clone, Debug)]
pub struct CommandWithArgs {
    #[allow(rustdoc::broken_intra_doc_links)]
    /// args[0] is the command.
    args: Vec<Arg>,
    stdios: Option<Redirects>,
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
    pub fn push_arg(&mut self, arg: Arg) {
        self.args.push(arg);
    }

    /// Push a new env var arg onto the command, the first "arg" is the command itself.
    pub fn push_env_var_arg(&mut self, arg: OsString) {
        self.args.push(Arg::Var(arg));
    }

    /// Push a new env var arg onto the command, the first "arg" is the command itself.
    pub fn push_run_arg(&mut self, run: Run) {
        self.args.push(Arg::Command(run));
    }

    /// Empty, not even the command is set.
    pub fn is_empty(&self) -> bool {
        self.args.is_empty()
    }

    /// Command name, None if no command name set (args are empty).
    pub fn command(&self, jobs: &mut Jobs) -> Option<io::Result<OsString>> {
        self.args.first().map(|v| v.resolve_arg(jobs))
    }

    /// Args to the command.
    pub fn args(&self) -> &[Arg] {
        if self.args.is_empty() {
            &self.args[..]
        } else {
            &self.args[1..]
        }
    }

    /// Iterator over the arguments for the command.
    pub fn args_iter(&self) -> CommandArgs {
        CommandArgs {
            args: self.args(),
            index: 0,
        }
    }

    /// Set the stdio redirect stack for this command.
    pub fn set_stdios(&mut self, stdios: Redirects) {
        self.stdios = Some(stdios);
    }

    /// Set the stdio redirect stack for this command.
    pub fn stdios(&self) -> &Option<Redirects> {
        &self.stdios
    }

    /// Extend the redirect stack for this command with stdio.
    pub fn extend_stdios(&mut self, stdios: &Redirects) {
        let mut current_stdios = self.stdios.take().unwrap_or_default();
        current_stdios.extend(stdios);
        self.stdios = Some(current_stdios);
    }

    /// If fd is Some value then put it at the front of the redir queue for this command.
    pub fn push_stdin_front(&mut self, fd: Option<FileDesc>) {
        if let Some(fd) = fd {
            if let Some(stdios) = self.stdios.as_mut() {
                stdios.set_in_internal_fd(STDIN_FILENO, fd, false);
            } else {
                let mut stdios = Redirects::default();
                stdios.set_in_internal_fd(STDIN_FILENO, fd, true);
                self.stdios = Some(stdios);
            }
        }
    }

    /// If fd is Some value then put it at the front of the redir queue for this command.
    pub fn push_stdout_front(&mut self, fd: Option<FileDesc>) {
        if let Some(fd) = fd {
            if let Some(stdios) = self.stdios.as_mut() {
                stdios.set_out_internal_fd(STDOUT_FILENO, fd, false);
            } else {
                let mut stdios = Redirects::default();
                stdios.set_out_internal_fd(STDOUT_FILENO, fd, true);
                self.stdios = Some(stdios);
            }
        }
    }

    /// Process redirects.
    pub fn process_redirects(&self, jobs: &mut Jobs) -> Result<HashSet<FileDesc>, io::Error> {
        if let Some(redirects) = &self.stdios {
            redirects.process(jobs)
        } else {
            Ok(HashSet::new())
        }
    }

    fn collect_internal_fds(&self, fd_set: &mut HashSet<FileDesc>) {
        if let Some(stdios) = &self.stdios {
            stdios.collect_internal_fds(fd_set);
        }
    }

    pub fn fds_to_internal(&mut self, fd_set: &HashSet<FileDesc>) {
        if let Some(stdios) = &mut self.stdios {
            stdios.fds_to_internal(fd_set);
        }
    }

    /// Return a set of all the 'internal' file descriptors (for pipes etc).
    pub fn get_internal_fds(&self) -> HashSet<FileDesc> {
        let mut res = HashSet::new();
        self.collect_internal_fds(&mut res);
        res
    }
}

impl Default for CommandWithArgs {
    fn default() -> Self {
        Self::new()
    }
}

pub struct CommandArgs<'args> {
    args: &'args [Arg],
    index: usize,
}

impl<'args> Iterator for CommandArgs<'args> {
    type Item = &'args Arg;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.args.len() {
            let r = &self.args[self.index];
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
    pub fn push_stdin_front(&mut self, fd: Option<FileDesc>) {
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
    pub fn push_stdout_front(&mut self, fd: Option<FileDesc>) {
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

    fn collect_internal_fds(&self, fd_set: &mut HashSet<FileDesc>) {
        match self {
            Run::Command(current) => current.collect_internal_fds(fd_set),
            Run::BackgroundCommand(current) => current.collect_internal_fds(fd_set),
            Run::Pipe(ref seq) | Run::Sequence(ref seq) | Run::And(ref seq) | Run::Or(ref seq) => {
                for run in seq {
                    run.collect_internal_fds(fd_set);
                }
            }
            Run::Subshell(ref current) => current.collect_internal_fds(fd_set),
            Run::Empty => {}
        }
    }

    pub fn fds_to_internal(&mut self, fd_set: &HashSet<FileDesc>) {
        match self {
            Run::Command(current) => current.fds_to_internal(fd_set),
            Run::BackgroundCommand(current) => current.fds_to_internal(fd_set),
            Run::Pipe(ref mut seq)
            | Run::Sequence(ref mut seq)
            | Run::And(ref mut seq)
            | Run::Or(ref mut seq) => {
                for run in seq {
                    run.fds_to_internal(fd_set);
                }
            }
            Run::Subshell(ref mut current) => current.fds_to_internal(fd_set),
            Run::Empty => {}
        }
    }

    /// Return a set of all the 'internal' file descriptors (for pipes etc).
    pub fn get_internal_fds(&self) -> HashSet<FileDesc> {
        let mut res = HashSet::new();
        self.collect_internal_fds(&mut res);
        res
    }

    /// If fd is Some value then put it at the front of the redir queue for the last command in the Run.
    pub fn push_arg_end(&mut self, arg: Arg) {
        match self {
            Run::Command(current) => current.push_arg(arg),
            Run::BackgroundCommand(current) => current.push_arg(arg),
            Run::Pipe(ref mut seq)
            | Run::Sequence(ref mut seq)
            | Run::And(ref mut seq)
            | Run::Or(ref mut seq) => {
                if let Some(run) = seq.last_mut() {
                    run.push_arg_end(arg);
                }
            }
            Run::Subshell(ref mut current) => current.push_arg_end(arg),
            Run::Empty => {}
        }
    }

    /// If fd is Some value then put it at the front of the redir queue for the last command in the Run.
    pub fn extend_redirs_end(&mut self, redirs: &Redirects) {
        match self {
            Run::Command(current) => current.extend_stdios(redirs),
            Run::BackgroundCommand(current) => current.extend_stdios(redirs),
            Run::Pipe(ref mut seq)
            | Run::Sequence(ref mut seq)
            | Run::And(ref mut seq)
            | Run::Or(ref mut seq) => {
                if let Some(run) = seq.last_mut() {
                    run.extend_redirs_end(redirs);
                }
            }
            Run::Subshell(ref mut current) => current.extend_redirs_end(redirs),
            Run::Empty => {}
        }
    }
}
