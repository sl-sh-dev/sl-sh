use std::collections::HashMap;
use std::env;
use std::hash::BuildHasher;
use std::io::{self, BufReader, Read, Write};
use std::path::Path;

use glob::glob;

use crate::builtins_util::*;
use crate::environment::*;
use crate::eval::*;
use crate::interner::*;
use crate::process::*;
use crate::types::*;
use crate::unix::*;

fn cd_expand_all_dots(cd: String) -> String {
    let mut all_dots = false;
    if cd.len() > 2 {
        all_dots = true;
        for ch in cd.chars() {
            if ch != '.' {
                all_dots = false;
                break;
            }
        }
    }
    if all_dots {
        let mut new_cd = String::new();
        let paths_up = cd.len() - 2;
        new_cd.push_str("../");
        for _i in 0..paths_up {
            new_cd.push_str("../");
        }
        new_cd
    } else {
        cd
    }
}

fn builtin_cd(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let home = match env::var("HOME") {
        Ok(val) => val,
        Err(_) => "/".to_string(),
    };
    let old_dir = match env::var("OLDPWD") {
        Ok(val) => val,
        Err(_) => home.to_string(),
    };
    let new_dir = if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg_d = arg.get();
            let new_arg = match &arg_d.data {
                ExpEnum::Symbol(s, _) => match get_expression(environment, arg.clone()) {
                    Some(exp) => match &exp.get().data {
                        ExpEnum::Function(_) => eval(
                            environment,
                            Expression::alloc_data(ExpEnum::String((*s).into(), None)),
                        )?,
                        ExpEnum::Lambda(_) => eval(
                            environment,
                            Expression::alloc_data(ExpEnum::String((*s).into(), None)),
                        )?,
                        ExpEnum::Macro(_) => eval(
                            environment,
                            Expression::alloc_data(ExpEnum::String((*s).into(), None)),
                        )?,
                        _ => {
                            drop(arg_d);
                            eval(environment, &arg)?
                        }
                    },
                    _ => {
                        drop(arg_d);
                        eval(environment, &arg)?
                    }
                },
                _ => {
                    drop(arg_d);
                    eval(environment, &arg)?
                }
            }
            .as_string(environment)?;
            if let Some(h) = expand_tilde(&new_arg) {
                h
            } else {
                new_arg
            }
        } else {
            return Err(LispError::new("cd can not have more then one form"));
        }
    } else {
        home
    };
    let new_dir = if new_dir == "-" { &old_dir } else { &new_dir };
    let new_dir = cd_expand_all_dots(new_dir.to_string());
    let root = Path::new(&new_dir);
    if let Ok(oldpwd) = env::current_dir() {
        env::set_var("OLDPWD", oldpwd);
    }
    if let Err(e) = env::set_current_dir(&root) {
        eprintln!("Error changing to {}, {}", root.display(), e);
        Ok(Expression::make_nil())
    } else {
        env::set_var("PWD", env::current_dir()?);
        Ok(Expression::make_true())
    }
}

fn file_test(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
    test: fn(path: &Path) -> bool,
    fn_name: &str,
) -> Result<Expression, LispError> {
    if let Some(p) = args.next() {
        if args.next().is_none() {
            let p = match &eval(environment, p)?.get().data {
                ExpEnum::String(p, _) => {
                    match expand_tilde(&p) {
                        Some(p) => p,
                        None => p.to_string(), // XXX not great.
                    }
                }
                _ => {
                    let msg = format!("{} path must be a string", fn_name);
                    return Err(LispError::new(msg));
                }
            };
            let path = Path::new(&p);
            if test(path) {
                return Ok(Expression::make_true());
            } else {
                return Ok(Expression::make_nil());
            }
        }
    }
    let msg = format!("{} takes a string (a path)", fn_name);
    Err(LispError::new(msg))
}

fn builtin_path_exists(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    file_test(environment, args, |path| path.exists(), "fs-exists?")
}

fn builtin_is_file(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    file_test(environment, args, |path| path.is_file(), "fs-file?")
}

fn builtin_is_dir(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    file_test(environment, args, |path| path.is_dir(), "fs-dir?")
}

fn pipe_write_file(pipe_in: i32, writer: &mut dyn Write) -> Result<(), LispError> {
    let mut inf = BufReader::new(fd_to_file(pipe_in));
    let mut buf = [0; 10240];
    let mut n = inf.read(&mut buf[..])?;
    while n > 0 {
        writer.write_all(&buf[..n])?;
        n = inf.read(&mut buf[..])?;
    }
    Ok(())
}

fn builtin_pipe(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    pub struct GrabStdIn {
        pub old_stdin: Option<i32>,
    }

    pub fn grab_stdin(new_stdin: Option<i32>) -> Result<GrabStdIn, LispError> {
        let old_stdin = if let Some(new_stdin) = new_stdin {
            Some(replace_stdin(new_stdin)?)
        } else {
            None
        };
        Ok(GrabStdIn { old_stdin })
    }

    impl Drop for GrabStdIn {
        fn drop(&mut self) {
            if let Some(old_stdin) = self.old_stdin {
                if let Err(err) = dup_stdin(old_stdin) {
                    eprintln!("Error restoring stdin after pipe: {}", err);
                }
            }
        }
    }

    let mut pipe = args.next();
    let mut last_pid: Option<u32> = None;
    let mut read = None;
    let mut write;
    let mut next_read;
    let mut res = Ok(Expression::make_nil());
    let gpo = set_grab_proc_output(environment, false);
    while let Some(p) = pipe {
        let next_pipe = args.next();
        if next_pipe.is_none() {
            // Last thing in the pipe so do not run in background.
            let _old_stdin = grab_stdin(read)?; // RAII guard for stdin
            gpo.environment.grab_proc_output = gpo.old_grab_proc_output;
            res = eval(gpo.environment, p);
            // If pipe ended in a file then dump final output into it.
            match &res {
                Ok(res_in) => {
                    let res_d = res_in.get();
                    if let ExpEnum::File(file) = &res_d.data {
                        let mut file_b = file.borrow_mut();
                        match &mut *file_b {
                            FileState::Stdout => {
                                let stdout = io::stdout();
                                let mut handle = stdout.lock();
                                pipe_write_file(0, &mut handle)?;
                            }
                            FileState::Stderr => {
                                let stderr = io::stderr();
                                let mut handle = stderr.lock();
                                pipe_write_file(0, &mut handle)?;
                            }
                            FileState::Write(f) => {
                                pipe_write_file(0, f)?;
                            }
                            _ => {
                                drop(file_b);
                                drop(res_d);
                                res = Err(LispError::new("File at pipe end must be writable."));
                            }
                        }
                    }
                }
                Err(_err) => {}
            }
        } else {
            let (read_fd, write_fd) = anon_pipe()?;
            write = Some(write_fd);
            next_read = Some(read_fd);
            let pid = fork(gpo.environment, p, read, write)?;
            last_pid = Some(pid);
            let res_proc = Expression::alloc_data(ExpEnum::Process(ProcessState::Running(pid)));
            add_process(gpo.environment, pid, (res_proc, None));
            if gpo.environment.pipe_pgid.is_none() {
                gpo.environment.pipe_pgid = last_pid;
            }
            read = next_read;
        }
        pipe = next_pipe;
    }
    gpo.environment.pipe_pgid = None;
    if res.is_err() {
        if let Some(pid) = last_pid {
            // Send a sigint to the feeding job so it does not hang on a full output buffer.
            if let Err(err) = nix::sys::signal::kill(
                nix::unistd::Pid::from_raw(pid as i32),
                nix::sys::signal::Signal::SIGINT,
            ) {
                eprintln!("ERROR, sending SIGINT to pid {}: {}", pid, err);
            }
        }
    }
    res
}

fn builtin_wait(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg0) = args.next() {
        if args.next().is_none() {
            let arg0 = eval(environment, arg0)?;
            let arg0_d = arg0.get();
            return match &arg0_d.data {
                ExpEnum::Process(ProcessState::Running(pid)) => {
                    let pid = *pid;
                    drop(arg0_d);
                    match wait_pid(environment, pid, None) {
                        Some(exit_status) => {
                            Ok(Expression::alloc_data(ExpEnum::Int(i64::from(exit_status))))
                        }
                        None => Ok(Expression::make_nil()),
                    }
                }
                ExpEnum::Process(ProcessState::Over(_pid, exit_status)) => Ok(
                    Expression::alloc_data(ExpEnum::Int(i64::from(*exit_status))),
                ),
                ExpEnum::Int(pid) => {
                    let pid = *pid;
                    drop(arg0_d);
                    match wait_pid(environment, pid as u32, None) {
                        Some(exit_status) => {
                            Ok(Expression::alloc_data(ExpEnum::Int(i64::from(exit_status))))
                        }
                        None => Ok(Expression::make_nil()),
                    }
                }
                _ => Err(LispError::new("wait error: not a pid")),
            };
        }
    }
    Err(LispError::new("wait takes one form (a pid to wait on)"))
}

fn builtin_pid(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg0) = args.next() {
        if args.next().is_none() {
            let arg0 = eval(environment, arg0)?;
            return match arg0.get().data {
                ExpEnum::Process(ProcessState::Running(pid)) => {
                    Ok(Expression::alloc_data(ExpEnum::Int(i64::from(pid))))
                }
                ExpEnum::Process(ProcessState::Over(pid, _exit_status)) => {
                    Ok(Expression::alloc_data(ExpEnum::Int(i64::from(pid))))
                }
                _ => Err(LispError::new("pid error: not a process")),
            };
        }
    }
    Err(LispError::new("pid takes one form (a process)"))
}

fn builtin_glob(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    fn remove_escapes(pat: &str) -> String {
        let mut ret = String::new();
        let mut last_esc = false;
        for ch in pat.chars() {
            match ch {
                '\\' if last_esc => {
                    ret.push('\\');
                    last_esc = false;
                }
                '\\' => last_esc = true,
                '*' if last_esc => {
                    ret.push('*');
                    last_esc = false;
                }
                '?' if last_esc => {
                    ret.push('?');
                    last_esc = false;
                }
                '[' if last_esc => {
                    ret.push('[');
                    last_esc = false;
                }
                ']' if last_esc => {
                    ret.push(']');
                    last_esc = false;
                }
                _ => {
                    if last_esc {
                        ret.push('\\');
                        ret.push(ch);
                    } else {
                        ret.push(ch);
                    }
                }
            }
        }
        ret
    }
    let mut files = Vec::new();
    for pat in args {
        let pat = match &eval(environment, pat)?.get().data {
            ExpEnum::String(s, _) => s.to_string(),
            _ => return Err(LispError::new("globs need to be strings")),
        };
        let pat = match expand_tilde(&pat) {
            Some(p) => p,
            None => pat,
        };
        if let Ok(paths) = glob(&pat) {
            for p in paths {
                match p {
                    Ok(p) => {
                        if let Some(p) = p.to_str() {
                            files.push(Expression::alloc_data(ExpEnum::String(
                                p.to_string().into(),
                                None,
                            )));
                        }
                    }
                    Err(err) => {
                        let msg = format!("glob error on while iterating {}, {}", pat, err);
                        return Err(LispError::new(msg));
                    }
                }
            }
            if files.is_empty() {
                // Got nothing so fall back on pattern.
                if pat.contains('\\') {
                    files.push(Expression::alloc_data(ExpEnum::String(
                        remove_escapes(&pat).into(),
                        None,
                    )));
                } else {
                    files.push(Expression::alloc_data(ExpEnum::String(pat.into(), None)));
                }
            }
        } else if pat.contains('\\') {
            files.push(Expression::alloc_data(ExpEnum::String(
                remove_escapes(&pat).into(),
                None,
            )));
        } else {
            files.push(Expression::alloc_data(ExpEnum::String(pat.into(), None)));
        }
    }
    Ok(Expression::with_list(files))
}

pub fn add_file_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, (Expression, String), S>,
) {
    data.insert(
        interner.intern("cd"),
        Expression::make_function(
            builtin_cd,
            r#"Usage: (cd dir-to-change-to)

Change directory.

Section: shell

Example:
(syscall mkdir "/tmp/tst-fs-cd")
(syscall touch "/tmp/tst-fs-cd/fs-cd-marker")
(test::assert-false (fs-exists? "fs-cd-marker"))
(pushd "/tmp/tst-fs-cd")
(root::cd "/tmp")
(root::cd "/tmp/tst-fs-cd")
(test::assert-true (fs-exists? "fs-cd-marker"))
(syscall rm "/tmp/tst-fs-cd/fs-cd-marker")
(popd)
(syscall rmdir "/tmp/tst-fs-cd")
"#,
        ),
    );
    data.insert(
        interner.intern("fs-exists?"),
        Expression::make_function(
            builtin_path_exists,
            r#"Usage: (fs-exists? path-to-test)

Does the given path exist?

Section: shell

Example:
$(mkdir /tmp/tst-fs-exists)
$(touch /tmp/tst-fs-exists/fs-exists)
(test::assert-true (fs-exists? "/tmp/tst-fs-exists/fs-exists"))
(test::assert-true (fs-exists? "/tmp/tst-fs-exists"))
(test::assert-false (fs-exists? "/tmp/tst-fs-exists/fs-exists-nope"))
$(rm /tmp/tst-fs-exists/fs-exists)
$(rmdir /tmp/tst-fs-exists)
"#,
        ),
    );
    data.insert(
        interner.intern("fs-file?"),
        Expression::make_function(
            builtin_is_file,
            r#"Usage: (fs-file? path-to-test)

Is the given path a file?

Section: shell

Example:
$(mkdir /tmp/tst-fs-file)
$(touch "/tmp/tst-fs-file/fs-file")
(test::assert-true (fs-file? "/tmp/tst-fs-file/fs-file"))
(test::assert-false (fs-file? "/tmp/tst-fs-file"))
(test::assert-false (fs-file? "/tmp/tst-fs-file/fs-file-nope"))
$(rm "/tmp/tst-fs-file/fs-file")
$(rmdir /tmp/tst-fs-file)
"#,
        ),
    );
    data.insert(
        interner.intern("fs-dir?"),
        Expression::make_function(
            builtin_is_dir,
            r#"Usage: (fs-dir? path-to-test)

Is the given path a directory?

Section: shell

Example:
$(mkdir /tmp/tst-fs-dir)
$(touch /tmp/tst-fs-dir/fs-dir-file)
(test::assert-false (fs-dir? "/tmp/tst-fs-dir/fs-dir-file"))
(test::assert-true (fs-dir? "/tmp/tst-fs-dir"))
(test::assert-false (fs-dir? "/tmp/tst-fs-dir/fs-dir-nope"))
$(rm /tmp/tst-fs-dir/fs-dir-file)
$(rmdir /tmp/tst-fs-dir)
"#,
        ),
    );
    data.insert(
        interner.intern("pipe"),
        Expression::make_function(
            builtin_pipe,
            r#"Usage: (pipe [expression]+)

Setup a pipe between processes or expressions.  Pipe will take one or more
expressions, each one but the last will be forked into a new process with it's
stdin being the output of the last expression.  The first expression uses the
current stdin and the last expression outputs to the current stdout.  Pipe works
with system commands as well as sl-sh forms (lambdas, etc).  Note it connects
the stdin/stdout of processes so if used with a lambda it should read stdin to
get the previous output and write to stdout to pass to the next expression in
the pipe (i.e. pipe will not interact with parameters or anything else).

Pipes also support using a read file as the first expression (the file contents
become stdin for the next form) and a write file as the last expression
(previous output will be written to the file).  For instance pipe can be used
to copy a file with (pipe (open IN_FILE :read)(open OUT_FILE :create)), note
this example does not close the files.

Pipes can be nested including piping through a lambda that itself uses pipes.

Section: shell

Example:
(def pipe-test (str (pipe $(echo "one
two
three")$(grep two))))
(test::assert-equal "two
" pipe-test)
(def pipe-test (str (pipe (pipe $(echo "one
two
twotwo
three")$(grep two))$(grep twotwo))))
(test::assert-equal "twotwo
" pipe-test)
$(mkdir "/tmp/tst-pipe-dir")
(def tsync (open "/tmp/tst-pipe-dir/test1" :create))
(pipe (print "one
two
two2
three") $(grep two) tsync)
(close tsync)
(def topen (open "/tmp/tst-pipe-dir/test1" :read))
(test::assert-equal "two
" (read-line topen))
(test::assert-equal "two2
" (read-line topen))
(test::assert-false (read-line topen))
(close topen)
(def topen (open "/tmp/tst-pipe-dir/test1" :read))
(def pipe-test (str (pipe topen $(grep two2))))
(close topen)
(test::assert-equal "two2
" pipe-test)
$(rm "/tmp/tst-pipe-dir/test1")
$(rmdir "/tmp/tst-pipe-dir")
"#,
        ),
    );
    data.insert(
        interner.intern("wait"),
        Expression::make_function(
            builtin_wait,
            r#"Usage: (wait proc-to-wait-for)

Wait for a process to end and return it's exit status.
Wait can be called multiple times if it is given a process
object (not just a numeric pid).

Section: shell

Example:
(def wait-test (wait (err>null $(ls /does/not/exist/123))))
(test::assert-true (> wait-test 0))
(def wait-test2 (fork (* 11 5)))
(test::assert-equal 55 (wait wait-test2))
(test::assert-equal 55 (wait wait-test2))
(test::assert-equal 55 (wait wait-test2))
"#,
        ),
    );
    data.insert(
        interner.intern("pid"),
        Expression::make_function(
            builtin_pid,
            r#"Usage: (pid proc)

Return the pid of a process.

Section: shell

Example:
(def pid-test $(echo -n))
(test::assert-true (int? (pid pid-test)))
(test::assert-error (pid 1))
"#,
        ),
    );
    data.insert(
        interner.intern("glob"),
        Expression::make_function(
            builtin_glob,
            r#"Usage: (glob /path/with/*)

Takes a list/varargs of globs and return the list of them expanded.

Section: shell

Example:
(syscall mkdir "/tmp/tst-fs-glob")
(syscall touch "/tmp/tst-fs-glob/g1")
(syscall touch "/tmp/tst-fs-glob/g2")
(syscall touch "/tmp/tst-fs-glob/g3")
(test::assert-equal '("/tmp/tst-fs-glob/g1" "/tmp/tst-fs-glob/g2" "/tmp/tst-fs-glob/g3") (glob "/tmp/tst-fs-glob/*"))
(syscall rm "/tmp/tst-fs-glob/g1")
(syscall rm "/tmp/tst-fs-glob/g2")
(syscall rm "/tmp/tst-fs-glob/g3")
(syscall rmdir "/tmp/tst-fs-glob")
"#,
        ),
    );
}
