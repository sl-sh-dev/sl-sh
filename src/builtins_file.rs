use sl_sh_proc_macros::sl_sh_fn;
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
use crate::{rand_alphanumeric_str, LispResult, VarArgs};
use core::iter;
use same_file;
use std::ffi::OsStr;
use std::fs::{File, Metadata};
use std::time::SystemTime;
use walkdir::{DirEntry, WalkDir};

const LEN: i64 = 5;

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

/// Usage: (cd dir-to-change-to)
///
/// Change directory.
///
/// Section: file
///
/// Example:
/// (syscall 'mkdir "/tmp/tst-fs-cd")
/// (syscall 'touch "/tmp/tst-fs-cd/fs-cd-marker")
/// (test::assert-false (fs-exists? "fs-cd-marker"))
/// (pushd "/tmp/tst-fs-cd")
/// (root::cd "/tmp")
/// (root::cd "/tmp/tst-fs-cd")
/// (test::assert-true (fs-exists? "fs-cd-marker"))
/// (syscall 'rm "/tmp/tst-fs-cd/fs-cd-marker")
/// (popd)
/// (syscall 'rmdir "/tmp/tst-fs-cd")
#[sl_sh_fn(fn_name = "cd", takes_env = true)]
fn cd(environment: &mut Environment, arg: Option<String>) -> LispResult<Expression> {
    let fn_name = "cd";
    let home = match env::var("HOME") {
        Ok(val) => val,
        Err(_) => "/".to_string(),
    };
    let old_dir = match env::var("OLDPWD") {
        Ok(val) => val,
        Err(_) => home.to_string(),
    };
    let new_dir = match arg {
        Some(arg) => {
            let new_arg =
                Expression::alloc_data(ExpEnum::String(arg.into(), None)).as_string(environment)?;
            if let Some(h) = expand_tilde(&new_arg) {
                h
            } else {
                new_arg
            }
        }
        None => home,
    };
    let new_dir = if new_dir == "-" { &old_dir } else { &new_dir };
    let new_dir = cd_expand_all_dots(new_dir.to_string());
    let root = Path::new(&new_dir);
    if let Ok(oldpwd) = env::current_dir() {
        env::set_var("OLDPWD", oldpwd);
    }
    if let Err(e) = env::set_current_dir(&root) {
        eprintln!("{} Error changing to {}, {}", fn_name, root.display(), e);
        Ok(Expression::make_nil())
    } else {
        env::set_var("PWD", env::current_dir()?);
        Ok(Expression::make_true())
    }
}

pub fn get_file(p: String) -> Option<PathBuf> {
    let p = expand_tilde(p.as_str()).unwrap_or(p);
    let p = Path::new(&p);
    Some((*p.to_path_buf()).to_owned())
}

fn file_test(path: String, test: fn(path: &Path) -> bool, fn_name: &str) -> LispResult<Expression> {
    if let Some(path) = get_file(path) {
        if test(path.as_path()) {
            Ok(Expression::make_true())
        } else {
            Ok(Expression::make_nil())
        }
    } else {
        let msg = format!("{} takes a string (a path)", fn_name);
        Err(LispError::new(msg))
    }
}

/// Usage: (fs-exists? path-to-test)
///
/// Does the given path exist?
///
/// Section: file
///
/// Example:
/// $(mkdir /tmp/tst-fs-exists)
/// $(touch /tmp/tst-fs-exists/fs-exists)
/// (test::assert-true (fs-exists? "/tmp/tst-fs-exists/fs-exists"))
/// (test::assert-true (fs-exists? "/tmp/tst-fs-exists"))
/// (test::assert-false (fs-exists? "/tmp/tst-fs-exists/fs-exists-nope"))
/// $(rm /tmp/tst-fs-exists/fs-exists)
/// $(rmdir /tmp/tst-fs-exists)
#[sl_sh_fn(fn_name = "fs-exists?")]
fn path_exists(path: String) -> LispResult<Expression> {
    file_test(path, |path| path.exists(), "fs-exists?")
}

/// Usage: (fs-file? path-to-test)
///
/// Is the given path a file?
///
/// Section: file
///
/// Example:
/// $(mkdir /tmp/tst-fs-file)
/// $(touch "/tmp/tst-fs-file/fs-file")
/// (test::assert-true (fs-file? "/tmp/tst-fs-file/fs-file"))
/// (test::assert-false (fs-file? "/tmp/tst-fs-file"))
/// (test::assert-false (fs-file? "/tmp/tst-fs-file/fs-file-nope"))
/// $(rm "/tmp/tst-fs-file/fs-file")
/// $(rmdir /tmp/tst-fs-file)
#[sl_sh_fn(fn_name = "fs-file?")]
fn is_file(path: String) -> LispResult<Expression> {
    file_test(path, |path| path.is_file(), "fs-file?")
}

/// Usage: (fs-dir? path-to-test)
///
/// Is the given path a directory?
///
/// Section: file
///
/// Example:
/// $(mkdir /tmp/tst-fs-dir)
/// $(touch /tmp/tst-fs-dir/fs-dir-file)
/// (test::assert-false (fs-dir? "/tmp/tst-fs-dir/fs-dir-file"))
/// (test::assert-true (fs-dir? "/tmp/tst-fs-dir"))
/// (test::assert-false (fs-dir? "/tmp/tst-fs-dir/fs-dir-nope"))
/// $(rm /tmp/tst-fs-dir/fs-dir-file)
/// $(rmdir /tmp/tst-fs-dir)
#[sl_sh_fn(fn_name = "fs-dir?")]
fn is_dir(path: String) -> LispResult<Expression> {
    file_test(path, |path| path.is_dir(), "fs-dir?")
}

/// Usage: (glob /path/with/*)
///
/// Takes a list/varargs of globs and return the list of them expanded.
///
/// Section: file
///
/// Example:
/// (syscall 'mkdir "/tmp/tst-fs-glob")
/// (syscall 'touch "/tmp/tst-fs-glob/g1")
/// (syscall 'touch "/tmp/tst-fs-glob/g2")
/// (syscall 'touch "/tmp/tst-fs-glob/g3")
/// (test::assert-equal '("/tmp/tst-fs-glob/g1" "/tmp/tst-fs-glob/g2" "/tmp/tst-fs-glob/g3") (glob "/tmp/tst-fs-glob/*"))
/// (syscall 'rm "/tmp/tst-fs-glob/g1")
/// (syscall 'rm "/tmp/tst-fs-glob/g2")
/// (syscall 'rm "/tmp/tst-fs-glob/g3")
/// (syscall 'rmdir "/tmp/tst-fs-glob")
#[sl_sh_fn(fn_name = "glob")]
fn do_glob(args: VarArgs<String>) -> LispResult<Expression> {
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

/// Usage: (fs-parent /path/to/file/or/dir)
///
/// Returns base name of file or directory passed to function.
///
/// Section: file
/// Example:
/// (with-temp (fn (tmp)
/// (let ((tmp-file (get-temp-file tmp)))
/// (test::assert-true (fs-same? (fs-parent tmp-file) tmp)))))
#[sl_sh_fn(fn_name = "fs-parent")]
fn fs_parent(path: String) -> LispResult<String> {
    let fn_name = "fs-parent";
    if let Some(path) = get_file(path) {
        let mut path = path.canonicalize().map_err(|_| {
            let msg = format!("{} failed to get full filepath of parent", fn_name);
            LispError::new(msg)
        })?;
        let _ = path.pop();
        let path = path.as_path().to_str().ok_or_else(|| {
            let msg = format!("{} failed to get parent path", fn_name);
            LispError::new(msg)
        })?;
        Ok(path.to_string())
    } else {
        let msg = format!("{} first arg is not a valid path", fn_name);
        Err(LispError::new(msg))
    }
}

/// Usage: (fs-base /path/to/file/or/dir)
///
/// Returns base name of file or directory passed to function.
///
/// Section: file
/// Example:
/// (with-temp (fn (tmp)
/// (let ((tmp-file (temp-file tmp)))
/// (test::assert-equal (length \".tmp01234\") (length (fs-base tmp-file))))))
#[sl_sh_fn(fn_name = "fs-base")]
fn fs_base(path: String) -> LispResult<String> {
    let fn_name = "fs-base";
    match get_file(path) {
        Some(path) => {
            let path = path.file_name().and_then(|s| s.to_str()).ok_or_else(|| {
                let msg = format!("{} failed to extract name of file", fn_name);
                LispError::new(msg)
            })?;
            Ok(path.to_string())
        }
        None => {
            let msg = format!("{} first arg is not a valid path", fn_name);
            Err(LispError::new(msg))
        }
    }
}

/// Usage: (fs-same? /path/to/file/or/dir /path/to/file/or/dir)
///
/// Returns true if the two provided file paths refer to the same file or directory.
///
/// Section: file
///
/// Example:
/// (with-temp-file (fn (tmp-file)
/// (test::assert-true (fs-same? tmp-file tmp-file)))
#[sl_sh_fn(fn_name = "fs-same?")]
fn is_same_file(path_0: String, path_1: String) -> LispResult<Expression> {
    let fn_name = "fs-same?";
    match (get_file(path_0), get_file(path_1)) {
        (Some(path_0), Some(path_1)) => {
            if let Ok(b) = same_file::is_same_file(path_0.as_path(), path_1.as_path()) {
                if b {
                    Ok(Expression::make_true())
                } else {
                    Ok(Expression::make_false())
                }
            } else {
                let msg = format!(
                    "{} there were insufficient permissions to access one or both of the provided files.",
                    fn_name
                );
                Err(LispError::new(msg))
            }
        }
        (_, _) => {
            let msg = format!("{} one or more paths does not exist.", fn_name);
            Err(LispError::new(msg))
        }
    }
}

/// Usage: (fs-crawl /path/to/file/or/dir (fn (x) (println "found path" x) [max-depth]
///              [:follow-syms])
///
/// If a directory is provided the path is recursively searched and every
/// file and directory is called as an argument to the provided function.
/// If a file is provided the path is provided as an argument to the provided
/// function. Takes two optional arguments (in any order) an integer,
/// representing max depth to traverse if file is a directory, or the
/// symbol, :follow-syms, to follow symbol links when traversing if
/// desired.
///
///
/// Section: file
///
/// Example:
///
/// (with-temp-file (fn (tmp-file)
/// 	(def cnt 0)
/// 	(fs-crawl tmp-file (fn (x)
/// 		(test::assert-equal (fs-base tmp-file) (fs-base x))
/// 		(set! cnt (+ 1 cnt))))
/// 	(test::assert-equal 1 cnt)))
///
/// (defn create-in (in-dir num-files visited)
/// 	(dotimes-i i num-files
/// 		 (hash-set! visited (get-temp-file in-dir) nil)))
///
/// (defn create-dir (tmp-dir visited)
/// 	(let ((new-tmp (get-temp tmp-dir)))
/// 		(hash-set! visited new-tmp nil)
/// 		new-tmp))
///
/// (with-temp (fn (root-tmp-dir)
/// 	(let ((tmp-file-count 5)
/// 		  (visited (make-hash)))
/// 	(def cnt 0)
/// 	(hash-set! visited root-tmp-dir nil)
/// 	(create-in root-tmp-dir tmp-file-count visited)
/// 	(let* ((tmp-dir (create-dir root-tmp-dir visited))
/// 			(new-files (create-in tmp-dir tmp-file-count visited))
/// 			(tmp-dir (create-dir tmp-dir visited))
/// 			(new-files (create-in tmp-dir tmp-file-count visited)))
/// 	(fs-crawl root-tmp-dir (fn (x)
/// 		(let ((file (hash-get visited x)))
/// 			(test::assert-true (not file)) ;; also tests double counting
/// 			(hash-set! visited x #t)
/// 			(set! cnt (+ 1 cnt)))))
/// 	(test::assert-equal (+ 3 (* 3 tmp-file-count)) cnt)
/// 	(test::assert-equal (+ 3 (* 3 tmp-file-count)) (length (hash-keys visited)))
/// 	(iterator::map (fn (x) (test::assert-true (hash-get visited y))) (hash-keys visited))))))
///
/// (with-temp (fn (root-tmp-dir)
/// 	(let ((tmp-file-count 5)
/// 		  (visited (make-hash)))
/// 	(def cnt 0)
/// 	(hash-set! visited root-tmp-dir nil)
/// 	(create-in root-tmp-dir tmp-file-count visited)
/// 	(let* ((tmp-dir (create-dir root-tmp-dir visited))
/// 			(new-files (create-in tmp-dir tmp-file-count visited))
/// 			(tmp-dir (create-dir tmp-dir (make-hash)))
/// 			(new-files (create-in tmp-dir tmp-file-count (make-hash))))
/// 	(fs-crawl root-tmp-dir (fn (x)
/// 		(let ((file (hash-get visited x)))
/// 			(test::assert-true (not file)) ;; also tests double counting
/// 			(hash-set! visited x #t)
/// 			(set! cnt (+ 1 cnt)))) 2)
/// 	(test::assert-equal (+ 3 (* 2 tmp-file-count)) cnt)
/// 	(test::assert-equal (+ 3 (* 2 tmp-file-count)) (length (hash-keys visited)))
/// 	(iterator::map (fn (x) (test::assert-true (hash-get visited y))) (hash-keys visited))))))
///
/// (with-temp (fn (root-tmp-dir)
/// 	(let ((tmp-file-count 5)
/// 		  (visited (make-hash)))
/// 	(def cnt 0)
/// 	(hash-set! visited root-tmp-dir nil)
/// 	(create-in root-tmp-dir tmp-file-count visited)
/// 	(let* ((tmp-dir (create-dir root-tmp-dir (make-hash)))
/// 			(new-files (create-in tmp-dir tmp-file-count (make-hash)))
/// 			(tmp-dir (create-dir tmp-dir (make-hash)))
/// 			(new-files (create-in tmp-dir tmp-file-count (make-hash))))
/// 	(fs-crawl root-tmp-dir (fn (x)
/// 		(let ((file (hash-get visited x)))
/// 			(test::assert-true (not file)) ;; also tests double counting
/// 			(hash-set! visited x #t)
/// 			(set! cnt (+ 1 cnt)))) 1)
/// 	(test::assert-equal (+ 2 tmp-file-count) cnt)
/// 	(test::assert-equal (+ 2 tmp-file-count) (length (hash-keys visited)))
/// 	(iterator::map (fn (x) (test::assert-true (hash-get visited y))) (hash-keys visited))))))
#[sl_sh_fn(fn_name = "fs-crawl", takes_env = true)]
fn fs_crawl(
    environment: &mut Environment,
    path: String,
    lambda_exp: Expression,
    optional_depth_or_symlink: VarArgs<Expression>,
) -> LispResult<Expression> {
    let fn_name = "fs-crawl";
    let file_or_dir = get_file(path);
    let mut depth = None;
    let mut sym_links = None;
    for depth_or_symlink in optional_depth_or_symlink {
        if let Ok(d) = depth_or_symlink.make_int(environment) {
            depth = Some(d);
        } else if let Ok(s) = depth_or_symlink.make_string(environment) {
            if s == ":follow-syms" {
                sym_links = Some(true);
            }
        }
    }
    let lambda_exp_d = &lambda_exp.get().data;
    match lambda_exp_d {
        ExpEnum::Lambda(_) => {
            if let Some(file_or_dir) = file_or_dir {
                let mut cb = |entry: &DirEntry| -> LispResult<()> {
                    let path = entry.path();
                    if let Some(path) = path.to_str() {
                        let path =
                            Expression::alloc_data(ExpEnum::String(path.to_string().into(), None));
                        let mut args = iter::once(path);
                        call_lambda(environment, lambda_exp.copy(), &mut args, true)?;
                    }
                    Ok(())
                };
                match (depth, sym_links) {
                    (Some(depth), Some(sym_links)) => {
                        for entry in WalkDir::new(file_or_dir)
                            .max_depth(depth as usize)
                            .follow_links(sym_links)
                            .into_iter()
                            .filter_map(|e| e.ok())
                        {
                            cb(&entry)?;
                        }
                    }
                    (Some(depth), None) => {
                        for entry in WalkDir::new(file_or_dir)
                            .max_depth(depth as usize)
                            .into_iter()
                            .filter_map(|e| e.ok())
                        {
                            cb(&entry)?;
                        }
                    }
                    (None, Some(sym_links)) => {
                        for entry in WalkDir::new(file_or_dir)
                            .follow_links(sym_links)
                            .into_iter()
                            .filter_map(|e| e.ok())
                        {
                            cb(&entry)?;
                        }
                    }
                    (None, None) => {
                        for entry in WalkDir::new(file_or_dir).into_iter().filter_map(|e| e.ok()) {
                            cb(&entry)?;
                        }
                    }
                }
                Ok(Expression::make_true())
            } else {
                let msg = format!("{} provided path does not exist", fn_name);
                Err(LispError::new(msg))
            }
        }
        _ => {
            let msg = format!("{} second argument must be a lambda", fn_name);
            Err(LispError::new(msg))
        }
    }
}

/// Usage: (fs-len /path/to/file/or/dir)
///
/// Returns the size of the file in bytes.
///
/// Section: file
///
/// Example:
/// (with-temp-file (fn (tmp)
///     (let ((tst-file (open tmp :create :truncate)))
///         (write-line tst-file \"Test Line Read Line One\")
///         (write-string tst-file \"Test Line Read Line Two\")
///         (flush tst-file)
///         (close tst-file)
///         (println \"fs-len is: \" (fs-len tst-file))
///         (test::assert-equal 47 (fs-len tst-file)))))
#[sl_sh_fn(fn_name = "fs-len")]
fn fs_len(file_or_dir: String) -> LispResult<Expression> {
    let fn_name = "fs-len";
    let file_or_dir = get_file(file_or_dir);
    if let Some(file_or_dir) = file_or_dir {
        if let Ok(metadata) = fs::metadata(file_or_dir) {
            let len = metadata.len();
            Ok(Expression::alloc_data(ExpEnum::Int(len as i64)))
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
    file_or_dir: Option<PathBuf>,
    fn_name: &str,
    to_time: fn(Metadata) -> io::Result<SystemTime>,
) -> Result<Expression, LispError> {
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

/// Usage: (fs-modified /path/to/file/or/dir)
///
/// Returns the unix time file last modified in ms.
///
/// Section: file
///
/// Example:
/// (with-temp-file (fn (tmp)
/// (let ((tst-file (open tmp :create :truncate))
/// (last-mod (fs-modified tmp)))
/// (write-line tst-file \"Test Line Read Line One\")
///         (write-string tst-file \"Test Line Read Line Two\")
///         (flush tst-file)
///         (close tst-file)
///         (test::assert-true (> (fs-modified tmp) last-mod)))))
#[sl_sh_fn(fn_name = "fs-modified")]
fn fs_modified(file_or_dir: String) -> LispResult<Expression> {
    let file_or_dir = get_file(file_or_dir);
    get_file_time(file_or_dir, "fs-modified", |md| md.modified())
}

/// Usage: (fs-accessed /path/to/file/or/dir)
///
/// Returns the unix time file last accessed in ms.
///
/// Section: file
///
/// Example:
/// (with-temp-file (fn (tmp)
/// (let ((tst-file (open tmp :read))
/// (last-acc (fs-accessed tmp)))
/// (close tst-file)
/// (let ((tst-file (open tmp :read)))
/// (test::assert-true (> (fs-accessed tmp) last-acc))
/// (close tst-file))))
#[sl_sh_fn(fn_name = "fs-accessed")]
fn fs_accessed(file_or_dir: String) -> LispResult<Expression> {
    let file_or_dir = get_file(file_or_dir);
    get_file_time(file_or_dir, "fs-accessed", |md| md.accessed())
}

fn temp_dir() -> PathBuf {
    env::temp_dir()
}

/// Usage: (get-temp [\"/path/to/directory/to/use/as/base\" \"optional-prefix\" \"optional-suffix\" length])
///
/// Creates a directory inside of an OS specific temporary directory. See [temp-dir](root::temp-dir)
/// for OS specific notes. Also accepts an optional prefix, an optional suffix, and an optional
/// length for the random number of characters in the temporary file created. Defaults to prefix of
/// \".tmp\", no suffix, and five random characters.
///
/// Section: file
///
/// Example:
/// (test::assert-true (str-contains (temp-dir) (get-temp)))
///
/// (with-temp (fn (tmp)
///         (let ((tmp-dir (get-temp tmp)))
///             (test::assert-true (str-contains tmp tmp-dir)))))
///
/// (with-temp (fn (tmp)
///         (let ((tmp-dir (get-temp tmp \"some-prefix\")))
///             (test::assert-true (str-contains tmp tmp-dir))
///             (test::assert-true (str-contains \"some-prefix\" tmp-dir)))))
///
/// (with-temp (fn (tmp)
///         (let ((tmp-dir (get-temp tmp \"some-prefix\" \"some-suffix\")))
///             (test::assert-true (str-contains tmp tmp-dir))
///             (test::assert-true (str-contains \"some-prefix\" tmp-dir))
///             (test::assert-true (str-contains \"some-suffix\" tmp-dir)))))
///
/// (with-temp (fn (tmp)
///         (let ((tmp-dir (get-temp tmp \"some-prefix\" \"some-suffix\" 6)))
///             (test::assert-true (str-contains tmp tmp-dir))
///             (test::assert-true (str-contains \"some-prefix\" tmp-dir))
///             (test::assert-true (str-contains \"some-suffix\" tmp-dir))
///             (test::assert-equal (length \"some-prefix012345some-suffix\") (length (fs-base tmp-dir))))))
#[sl_sh_fn(fn_name = "get-temp")]
fn get_temp(
    path: Option<String>,
    prefix: Option<String>,
    suffix: Option<String>,
    len: Option<i64>,
) -> LispResult<Expression> {
    let fn_name = "get-temp";
    let dir = get_provided_or_default_temp(path, fn_name)?;
    let prefix = prefix.as_deref().unwrap_or_default();
    let suffix = suffix.as_deref().unwrap_or_default();
    let len = len.unwrap_or(LEN);
    let dir = create_temp_dir(dir.as_path(), prefix, suffix, len, fn_name)?;
    if let Some(path) = dir.to_str() {
        let path = Expression::alloc_data(ExpEnum::String(path.to_string().into(), None));
        Ok(path)
    } else {
        let msg = format!("{} unable to provide temporary directory", fn_name);
        Err(LispError::new(msg))
    }
}

fn random_name(prefix: &str, suffix: &str, len: i64) -> String {
    let prefix = if prefix.is_empty() { ".tmp" } else { prefix };
    let mut rng = rand::thread_rng();
    let name = rand_alphanumeric_str(len.unsigned_abs() as u64, &mut rng);
    format!("{}{}{}", prefix, name, suffix)
}

fn get_provided_or_default_temp(path: Option<String>, fn_name: &str) -> LispResult<PathBuf> {
    match path {
        None => Ok(temp_dir()),
        Some(path) => match get_file(path) {
            None => {
                let msg = format!("{} unable to access provided file", fn_name);
                Err(LispError::new(msg))
            }
            Some(dir) => {
                let p = dir.as_path();
                if p.exists() && p.is_dir() {
                    Ok(dir)
                } else {
                    let msg = format!(
                        "{} unable to provide temporary file in provided directory",
                        fn_name
                    );
                    Err(LispError::new(msg))
                }
            }
        },
    }
}

fn create_temp_dir(
    path: &Path,
    prefix: &str,
    suffix: &str,
    len: i64,
    fn_name: &str,
) -> Result<PathBuf, LispError> {
    if path.exists() && path.is_dir() {
        let dir_name = random_name(prefix, suffix, len);
        let dir = Path::new::<OsStr>(dir_name.as_ref());
        let dir = path.join(dir);
        fs::create_dir(dir.as_path()).map_err(|err| {
            let msg = format!("{} unable to create temporary directory inside default temporary directory ({:?}), reason: {:?}", fn_name, dir.as_path(), err);
            LispError::new(msg)
        })?;
        Ok(dir)
    } else {
        let msg = format!("{} unable to provide temporary directory", fn_name);
        Err(LispError::new(msg))
    }
}

/// Usage: (with-temp (fn (x) (println \"given temp dir:\" x)) [\"optional-prefix\" \"optional-suffix\" length])
///
/// Takes a function that accepts a temporary directory. This directory will be recursively removed
/// when the provided function is finished executing. Also accepts an optional prefix, an optional
/// suffix, and an optional length for the random number of characters in the temporary directory
/// created. Defaults to prefix of \".tmp\", no suffix, and five random characters.
///
/// Section: file
///
/// Example:
/// (def fp nil)
/// (with-temp (fn (tmp-dir)
///     (let* ((tmp-file (str tmp-dir \"/sl-sh-tmp-file.txt\"))
///         (a-file (open tmp-file :create :truncate)))
///         (test::assert-true (fs-exists? tmp-file))
///         (set! fp tmp-file)
///         (close a-file))))
/// (test::assert-false (nil? fp))
/// (test::assert-false (fs-exists? fp))
///
/// (with-temp
///     (fn (tmp)
///         (test::assert-true (str-contains \"some-prefix\" tmp)))
///     \"some-prefix\")
///
/// (with-temp
///     (fn (tmp)
///         (test::assert-true (str-contains \"some-prefix\" tmp))
///         (test::assert-true (str-contains \"some-suffix\" tmp)))
///     \"some-prefix\"
///     \"some-suffix\")
///
/// (with-temp
///     (fn (tmp)
///         (test::assert-true (str-contains \"some-prefix\" tmp))
///         (test::assert-true (str-contains \"some-suffix\" tmp))
///         (test::assert-equal (length \"some-prefix0123456789some-suffix\") (length (fs-base tmp))))
///     \"some-prefix\"
///     \"some-suffix\"
///     10)
#[sl_sh_fn(fn_name = "with-temp", takes_env = true)]
fn with_temp_dir(
    environment: &mut Environment,
    lambda_exp: Expression,
    prefix: Option<String>,
    suffix: Option<String>,
    len: Option<i64>,
) -> LispResult<Expression> {
    let fn_name = "with-temp";
    let prefix = prefix.as_deref().unwrap_or_default();
    let suffix = suffix.as_deref().unwrap_or_default();
    let len = len.unwrap_or(LEN);
    let lambda_exp_d = &lambda_exp.get().data;
    match lambda_exp_d {
        ExpEnum::Lambda(_) => {
            let p = &temp_dir();
            let dir = create_temp_dir(p, prefix, suffix, len, fn_name)?;
            if let Some(path) = dir.as_path().to_str() {
                let path = Expression::alloc_data(ExpEnum::String(path.to_string().into(), None));
                let mut args = iter::once(path);
                let ret = call_lambda(environment, lambda_exp.copy(), &mut args, true);
                let _ = fs::remove_dir_all(dir.as_path());
                ret
            } else {
                let msg = format!("{} unable to provide temporary directory", fn_name);
                Err(LispError::new(msg))
            }
        }
        _ => {
            let msg = format!("{} first argument must be a lambda", fn_name);
            Err(LispError::new(msg))
        }
    }
}

fn create_temp_file(
    dir: PathBuf,
    prefix: &Option<String>,
    suffix: &Option<String>,
    len: Option<i64>,
    fn_name: &str,
) -> LispResult<PathBuf> {
    let prefix = prefix.as_ref().map(|x| x.as_str()).unwrap_or_default();
    let suffix = suffix.as_ref().map(|x| x.as_str()).unwrap_or_default();
    let len = len.unwrap_or(LEN);
    let filename = random_name(prefix, suffix, len);
    let p = Path::new::<OsStr>(filename.as_ref());
    let file = dir.join(p);
    let p = file.as_path();
    File::create(p).map_err(|err| {
        let msg = format!(
            "{} unable to create temporary file inside temporary directory ({:?}), reason: {:?}",
            fn_name,
            dir.as_path(),
            err
        );
        LispError::new(msg)
    })?;
    Ok(file)
}

/// Usage: (get-temp-file [\"/path/to/directory/to/use/as/base\" \"optional-prefix\" \"optional-suffix\" length])
///
/// Returns name of file created inside temporary directory. Optionally takes a directory to use as
/// the parent directory of the temporary file. Also accepts an optional prefix, an optional suffix,
/// and an optional length for the random number of characters in the temporary files created. Defaults
/// to prefix of \".tmp\", no suffix, and five random characters.
///
/// Section: file
///
/// Example:
/// (test::assert-true (str-contains (temp-dir) (get-temp-file)))
///
/// (with-temp (fn (tmp)
///         (let ((tmp-file (get-temp-file tmp)))
///             (test::assert-true (str-contains tmp tmp-file)))))
///
/// (with-temp (fn (tmp)
///         (let ((tmp-file (get-temp-file tmp \"some-prefix\")))
///             (test::assert-true (str-contains \"some-prefix\" tmp-file)))))
///
/// (with-temp (fn (tmp)
///         (let ((tmp-file (get-temp-file tmp \"some-prefix\" \"some-suffix\")))
///             (test::assert-true (str-contains \"some-prefix\" tmp-file))
///             (test::assert-true (str-contains \"some-suffix\" tmp-file)))))
///
/// (with-temp (fn (tmp)
///         (let ((tmp-file (get-temp-file tmp \"some-prefix\" \"some-suffix\" 10)))
///             (test::assert-true (str-contains \"some-prefix\" tmp-file))
///             (test::assert-true (str-contains \"some-suffix\" tmp-file))
///             (test::assert-equal (length \"some-prefix0123456789some-suffix\") (length (fs-base tmp-file))))))
#[sl_sh_fn(fn_name = "get-temp-file")]
fn get_temp_file(
    path: Option<String>,
    prefix: Option<String>,
    suffix: Option<String>,
    len: Option<i64>,
) -> LispResult<Expression> {
    let fn_name = "get-temp-file";
    let dir = get_provided_or_default_temp(path, fn_name)?;
    let file = create_temp_file(dir, &prefix, &suffix, len, fn_name)?;
    if let Some(path) = file.as_path().to_str() {
        let path = Expression::alloc_data(ExpEnum::String(path.to_string().into(), None));
        Ok(path)
    } else {
        let msg = format!("{} unable to provide temporary file", fn_name);
        Err(LispError::new(msg))
    }
}

/// Usage: (with-temp-file (fn (x) (println \"given temp file:\" x)) [\"optional-prefix\" \"optional-suffix\" length])
///
/// Takes a function that accepts a temporary file. This file will be removed when the provided function
/// is finished executing. Also accepts an optional prefix, an optional suffix, and an optional
/// length for the random number of characters in the temporary file created. Defaults to prefix of
/// \".tmp\", no suffix, and five random characters.
///
/// Section: file
///
/// Example:
/// (def fp nil)
/// (with-temp-file (fn (tmp-file)
///     (let* ((a-file (open tmp-file :create :truncate)))
///         (test::assert-true (fs-exists? tmp-file))
///         (set! fp tmp-file)
///         (close a-file))))
/// (test::assert-false (nil? fp))
/// (test::assert-false (fs-exists? fp))
///
/// (with-temp-file
///     (fn (tmp)
///         (test::assert-true (str-contains \"some-prefix\" tmp)))
///     \"some-prefix\")
///
/// (with-temp-file
///     (fn (tmp)
///         (test::assert-true (str-contains \"some-prefix\" tmp))
///         (test::assert-true (str-contains \"some-suffix\" tmp)))
///     \"some-prefix\"
///     \"some-suffix\")
///
/// (with-temp-file
///     (fn (tmp)
///         (test::assert-true (str-contains \"some-prefix\" tmp))
///         (test::assert-true (str-contains \"some-suffix\" tmp))
///         (test::assert-equal (length \"some-prefix0123456789some-suffix\") (length (fs-base tmp))))
///     \"some-prefix\"
///     \"some-suffix\"
///     10)
#[sl_sh_fn(fn_name = "with-temp-file", takes_env = true)]
fn with_temp_file(
    environment: &mut Environment,
    lambda_exp: Expression,
    prefix: Option<String>,
    suffix: Option<String>,
    len: Option<i64>,
) -> LispResult<Expression> {
    let fn_name = "with-temp-file";
    let lambda_exp_d = &lambda_exp.get().data;
    match lambda_exp_d {
        ExpEnum::Lambda(_) => {
            let p = &temp_dir();
            let dir = {
                let prefix = prefix.as_deref().unwrap_or_default();
                let suffix = suffix.as_deref().unwrap_or_default();
                let len = len.unwrap_or(LEN);
                create_temp_dir(p, prefix, suffix, len, fn_name)?
            };
            let file = create_temp_file(dir, &prefix, &suffix, len, fn_name)?;
            if let Some(path) = file.as_path().to_str() {
                let path = Expression::alloc_data(ExpEnum::String(path.to_string().into(), None));
                let mut args = iter::once(path);
                let ret = call_lambda(environment, lambda_exp.copy(), &mut args, true);
                let _ = fs::remove_file(file.as_path());
                ret
            } else {
                let msg = format!("{} unable to provide temporary file", fn_name);
                Err(LispError::new(msg))
            }
        }
        _ => {
            let msg = format!("{} first argument must be a lambda", fn_name);
            Err(LispError::new(msg))
        }
    }
}

/// Usage: (temp-dir)
///
/// Returns a string representing the temporary directory. See [get-temp](root::get-temp) for higher
/// level temporary directory creation mechanism.
///
/// On Unix:
/// Returns the value of the TMPDIR environment variable if it is set, otherwise for non-Android it
/// returns /tmp. If Android, since there is no global temporary folder (it is usually allocated
/// per-app), it returns /data/local/tmp.
///
/// On Windows:
/// Returns the value of, in order, the TMP, TEMP, USERPROFILE environment variable if any are set and
/// not the empty string. Otherwise, temp_dir returns the path of the Windows directory. This behavior
/// is identical to that of GetTempPath, which this function uses internally.
///
/// Section: file
///
/// Example:
/// (test::assert-true (fs-dir? (temp-dir)))
#[sl_sh_fn(fn_name = "temp-dir")]
fn builtin_temp_dir() -> LispResult<Expression> {
    if let Some(path) = temp_dir().to_str() {
        let path = Expression::alloc_data(ExpEnum::String(path.to_string().into(), None));
        Ok(path)
    } else {
        Err(LispError::new(
            "temp-dir: unable to provide temporary directory".to_string(),
        ))
    }
}

pub fn add_file_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, (Expression, String), S>,
) {
    intern_cd(interner, data);
    intern_path_exists(interner, data);
    intern_is_file(interner, data);
    intern_is_dir(interner, data);
    intern_do_glob(interner, data);
    intern_fs_crawl(interner, data);
    intern_is_same_file(interner, data);
    intern_fs_base(interner, data);
    intern_fs_parent(interner, data);
    intern_fs_len(interner, data);
    intern_fs_modified(interner, data);
    intern_fs_accessed(interner, data);
    intern_get_temp(interner, data);
    intern_get_temp_file(interner, data);
    intern_with_temp_file(interner, data);
    intern_with_temp_dir(interner, data);
    intern_builtin_temp_dir(interner, data);
}
