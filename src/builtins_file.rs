use std::collections::HashMap;
use std::hash::BuildHasher;
use std::path::{Path, PathBuf};
use std::{env, fs, io};

use glob::glob;

use crate::builtins_util::*;
use crate::environment::*;
use crate::eval::*;
use crate::interner::*;
use crate::types::*;
use core::iter;
use same_file;
use std::fs::Metadata;
use std::time::SystemTime;
use walkdir::{DirEntry, WalkDir};

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

pub fn get_file(environment: &mut Environment, p: Expression) -> Option<PathBuf> {
    let p = match &eval(environment, p).ok()?.get().data {
        ExpEnum::String(p, _) => {
            match expand_tilde(p) {
                Some(p) => p,
                None => p.to_string(), // XXX not great.
            }
        }
        _ => return None,
    };
    let p = Path::new(&p);
    Some((*p.to_path_buf()).to_owned())
}

fn file_test(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
    test: fn(path: &Path) -> bool,
    fn_name: &str,
) -> Result<Expression, LispError> {
    if let Some(p) = args.next() {
        if args.next().is_none() {
            if let Some(path) = get_file(environment, p) {
                if test(path.as_path()) {
                    return Ok(Expression::make_true());
                } else {
                    return Ok(Expression::make_nil());
                }
            } else {
                let msg = format!("{} takes a string (a path)", fn_name);
                return Err(LispError::new(msg));
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
                    }
                    ret.push(ch);
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

fn builtin_same_file(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let fn_name = "fs-same?";
    let arg0 = param_eval(environment, args, fn_name)?;
    let arg1 = param_eval(environment, args, fn_name)?;
    params_done(args, fn_name)?;
    let path_0 = match get_file(environment, arg0) {
        Some(path) => path,
        None => {
            let msg = format!("{} first arg is not a valid path", fn_name);
            return Err(LispError::new(msg));
        }
    };

    let path_1 = match get_file(environment, arg1) {
        Some(path) => path,
        None => {
            let msg = format!("{} second arg is not a valid path", fn_name);
            return Err(LispError::new(msg));
        }
    };

    if let Ok(b) = same_file::is_same_file(path_0.as_path(), path_1.as_path()) {
        if b {
            Ok(Expression::make_true())
        } else {
            Ok(Expression::make_false())
        }
    } else {
        let msg = format!(
            "{} one or more paths does not exist, or there were not enough \
                permissions.",
            fn_name
        );
        Err(LispError::new(msg))
    }
}

fn builtin_fs_crawl(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let fn_name = "fs-crawl";
    let arg0 = param_eval(environment, args, fn_name)?;
    let file_or_dir = get_file(environment, arg0);
    let lambda_exp = param_eval(environment, args, fn_name)?;
    let mut depth = None;
    let mut sym_links = None;
    for _ in 0..2 {
        if let Ok(depth_or_sym_link) = param_eval(environment, args, fn_name) {
            if let Ok(d) = depth_or_sym_link.make_int(environment) {
                depth = Some(d);
            } else if let Ok(s) = depth_or_sym_link.make_string(environment) {
                if s == ":follow-syms" {
                    sym_links = Some(true);
                }
            }
        };
    }
    // TODO cb masks fact that call_lambda can fail,
    // this is bad, an error should be (optionally?) passed up
    // not swallowed s.t. silent failure occurs.
    let mut cb = |entry: &DirEntry| {
        let path = entry.path();
        if let Some(path) = path.to_str() {
            let path = Expression::alloc_data(ExpEnum::String(
                environment.interner.intern(path).into(),
                None,
            ));
            let mut args = iter::once(path);
            let _ = call_lambda(environment, lambda_exp.copy(), &mut args, true);
        }
    };
    let mut iterate = |file_or_dir, depth, sym_links| match (depth, sym_links) {
        (Some(depth), Some(sym_links)) => {
            for entry in WalkDir::new(file_or_dir)
                .max_depth(depth as usize)
                .follow_links(sym_links)
                .into_iter()
                .filter_map(|e| e.ok())
            {
                cb(&entry);
            }
        }
        (Some(depth), None) => {
            for entry in WalkDir::new(file_or_dir)
                .max_depth(depth as usize)
                .into_iter()
                .filter_map(|e| e.ok())
            {
                cb(&entry);
            }
        }
        (None, Some(sym_links)) => {
            for entry in WalkDir::new(file_or_dir)
                .follow_links(sym_links)
                .into_iter()
                .filter_map(|e| e.ok())
            {
                cb(&entry);
            }
        }
        (None, None) => {
            for entry in WalkDir::new(file_or_dir).into_iter().filter_map(|e| e.ok()) {
                cb(&entry);
            }
        }
    };
    let lambda_exp_d = &lambda_exp.get().data;
    params_done(args, fn_name)?;
    match lambda_exp_d {
        ExpEnum::Lambda(_) => {
            if let Some(file_or_dir) = file_or_dir {
                iterate(file_or_dir, depth, sym_links);
                Ok(Expression::make_true())
            } else {
                let msg = format!("{} provided path does not exist", fn_name);
                Err(LispError::new(msg))
            }
        }
        _ => {
            let msg = format!("{} second argument must be function", fn_name);
            Err(LispError::new(msg))
        }
    }
}

fn builtin_fs_len(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let fn_name = "fs-len";
    let arg0 = param_eval(environment, args, fn_name)?;
    let file_or_dir = get_file(environment, arg0);
    params_done(args, fn_name)?;
    if let Some(file_or_dir) = file_or_dir {
        if let Ok(metadata) = fs::metadata(file_or_dir) {
            let len = metadata.len();
            Ok(Expression::alloc_data(ExpEnum::Float(len as f64)))
        } else {
            let msg = format!("{} can not fetch metadata at provided path", fn_name);
            Err(LispError::new(msg))
        }
    } else {
        let msg = format!("{} provided path does not exist", fn_name);
        Err(LispError::new(msg))
    }
}

fn get_file_time(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
    fn_name: &str,
    to_time: fn(Metadata) -> io::Result<SystemTime>,
) -> Result<Expression, LispError> {
    let arg0 = param_eval(environment, args, fn_name)?;
    let file_or_dir = get_file(environment, arg0);
    params_done(args, fn_name)?;
    if let Some(file_or_dir) = file_or_dir {
        if let Ok(metadata) = fs::metadata(file_or_dir) {
            if let Ok(sys_time) = to_time(metadata) {
                match sys_time.duration_since(SystemTime::UNIX_EPOCH) {
                    Ok(n) => {
                        let n = n.as_millis() as i64;
                        Ok(Expression::alloc_data(ExpEnum::Int(n)))
                    }
                    Err(_) => {
                        let msg = format!("{} can not parse time", fn_name);
                        Err(LispError::new(msg))
                    }
                }
            } else {
                let msg = format!("{} can not fetch time", fn_name);
                Err(LispError::new(msg))
            }
        } else {
            let msg = format!("{} can not fetch metadata at provided path", fn_name);
            Err(LispError::new(msg))
        }
    } else {
        let msg = format!("{} provided path does not exist", fn_name);
        Err(LispError::new(msg))
    }
}

fn builtin_fs_modified(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    get_file_time(environment, args, "fs-modified", |md| md.modified())
}

fn builtin_fs_accessed(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    get_file_time(environment, args, "fs-accessed", |md| md.accessed())
}

fn builtin_fs_created(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    get_file_time(environment, args, "fs-created", |md| md.created())
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

Section: file

Example:
(syscall 'mkdir "/tmp/tst-fs-cd")
(syscall 'touch "/tmp/tst-fs-cd/fs-cd-marker")
(test::assert-false (fs-exists? "fs-cd-marker"))
(pushd "/tmp/tst-fs-cd")
(root::cd "/tmp")
(root::cd "/tmp/tst-fs-cd")
(test::assert-true (fs-exists? "fs-cd-marker"))
(syscall 'rm "/tmp/tst-fs-cd/fs-cd-marker")
(popd)
(syscall 'rmdir "/tmp/tst-fs-cd")
"#,
        ),
    );
    data.insert(
        interner.intern("fs-exists?"),
        Expression::make_function(
            builtin_path_exists,
            r#"Usage: (fs-exists? path-to-test)

Does the given path exist?

Section: file

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

Section: file

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

Section: file

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
        interner.intern("glob"),
        Expression::make_function(
            builtin_glob,
            r#"Usage: (glob /path/with/*)

Takes a list/varargs of globs and return the list of them expanded.

Section: file

Example:
(syscall 'mkdir "/tmp/tst-fs-glob")
(syscall 'touch "/tmp/tst-fs-glob/g1")
(syscall 'touch "/tmp/tst-fs-glob/g2")
(syscall 'touch "/tmp/tst-fs-glob/g3")
(test::assert-equal '("/tmp/tst-fs-glob/g1" "/tmp/tst-fs-glob/g2" "/tmp/tst-fs-glob/g3") (glob "/tmp/tst-fs-glob/*"))
(syscall 'rm "/tmp/tst-fs-glob/g1")
(syscall 'rm "/tmp/tst-fs-glob/g2")
(syscall 'rm "/tmp/tst-fs-glob/g3")
(syscall 'rmdir "/tmp/tst-fs-glob")
"#,
        ),
    );
    data.insert(
        interner.intern("fs-crawl"),
        Expression::make_function(
            builtin_fs_crawl,
            r#"Usage: (fs-crawl /path/to/file/or/dir (fn (x) (println "found path" x) [max-depth]
             [:follow-syms])

If a directory is provided the path is resursively searched and every
file and directory is called as an argument to the provided function.
If a file is provided the path is provided as an argument to the provided
function. Takes two optional arguments (in any order) an integer,
representing max depth to traverse if file is a directory, or the
symbol, :follow-syms, to follow symbol links when traversing if
desired.


Section: file
"#,
        ),
    );
    data.insert(
        interner.intern("fs-same?"),
        Expression::make_function(
            builtin_same_file,
            r#"Usage: (fs-same? /path/to/file/or/dir /path/to/file/or/dir)

Returns true if the two provided file paths refer to the same file or directory.

Section: file
"#,
        ),
    );
    data.insert(
        interner.intern("fs-len"),
        Expression::make_function(
            builtin_fs_len,
            r#"Usage: (fs-len /path/to/file/or/dir)

Returns the size of the file in bytes.

Section: file
"#,
        ),
    );
    data.insert(
        interner.intern("fs-modified"),
        Expression::make_function(
            builtin_fs_modified,
            r#"Usage: (fs-modified /path/to/file/or/dir)

Returns the time file last modified in ms.

Section: file
"#,
        ),
    );
    data.insert(
        interner.intern("fs-accessed"),
        Expression::make_function(
            builtin_fs_accessed,
            r#"Usage: (fs-accessed /path/to/file/or/dir)

Returns the time file last accessed in ms.

Section: file
"#,
        ),
    );
    data.insert(
        interner.intern("fs-created"),
        Expression::make_function(
            builtin_fs_created,
            r#"Usage: (fs-created /path/to/file/or/dir)

Returns the time file was created in ms.

Section: file
"#,
        ),
    );
    //TODO what does CL name these things?
    // TODO should epoch and times be in ms or s?
}
