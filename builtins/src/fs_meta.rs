use bridge_macros::sl_sh_fn;
use bridge_types::VarArgs;
use compile_state::state::SloshVm;
use shell::builtins::expand_tilde;
use sl_compiler::load_eval::apply_callable;
use slvm::{from_i56, VMError, VMResult, Value};
use std::path::{Path, PathBuf};
use std::{env, fs, io, time};

use glob::glob;

use bridge_adapters::add_builtin;
use same_file;
use slvm::vm_hashmap::VMHashMap;
use std::fs::{File, Metadata};
use std::thread;
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

/// Usage: (sleep milliseconds) -> nil
///
/// Sleep for *at least* the provided milliseconds (must be a positive integer),
/// otherwise function will no-op.
///
/// Section: system
///
// Example:
// ;; TODO sls implement time
// (def test-sleep-var (time (sleep 1000)))
// (assert-true (> test-sleep-var 1.0))
#[sl_sh_fn(fn_name = "sleep")]
fn sleep(millis: i64) -> VMResult<()> {
    if millis > 0 {
        let millis = time::Duration::from_millis(millis as u64);
        thread::sleep(millis);
    }
    Ok(())
}

/// Usage: (cd dir-to-change-to)
///
/// Change directory.
///
/// Section: file
///
/// Example:
/// (with-temp (fn (tmp)
///     (fclose (fopen (str tmp "/fs-cd-marker") :create :truncate))
///     (test::assert-false (fs-exists? "fs-cd-marker"))
///     (cd tmp)
///     (test::assert-true (fs-exists? "fs-cd-marker"))
///     (cd)))
#[sl_sh_fn(fn_name = "cd")]
fn cd(arg: Option<String>) -> VMResult<Value> {
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
        Some(arg) => expand_tilde(arg.into()).to_string_lossy().to_string(),
        None => home,
    };
    let new_dir = if new_dir == "-" { &old_dir } else { &new_dir };
    let new_dir = cd_expand_all_dots(new_dir.to_string());
    let root = Path::new(&new_dir);
    if let Ok(oldpwd) = env::current_dir() {
        unsafe { env::set_var("OLDPWD", oldpwd); }
    }
    if let Err(e) = env::set_current_dir(root) {
        eprintln!("{} Error changing to {}, {}", fn_name, root.display(), e);
        Ok(Value::Nil)
    } else {
        unsafe { env::set_var("PWD", env::current_dir()?); }
        Ok(Value::True)
    }
}

pub fn get_file(p: &str) -> Option<PathBuf> {
    let p = expand_tilde(p.into());
    Some(p.to_path_buf())
}

fn file_test(path: &str, test: fn(path: &Path) -> bool, fn_name: &str) -> VMResult<Value> {
    if let Some(path) = get_file(path) {
        if test(path.as_path()) {
            Ok(Value::True)
        } else {
            Ok(Value::False)
        }
    } else {
        let msg = format!("{} takes a string (a path)", fn_name);
        Err(VMError::new("io", msg))
    }
}

/// Usage: (fs-fullpath "~/some/filepath/../") -> nil
///
/// Return absolute 'canonical' file path.
///
/// Section: file
///
/// Example:
/// #t
#[sl_sh_fn(fn_name = "fs-fullpath")]
fn fs_fullpath(path: &str) -> Option<String> {
    let path = expand_tilde(path.into());
    Path::new(&path)
        .canonicalize()
        .ok()
        .map(|path| path.to_string_lossy().into_owned())
}

/// Usage: (fs-exists? path-to-test)
///
/// Does the given path exist?
///
/// Section: file
///
/// Example:
/// (with-temp (fn (tmp)
///     (fclose (fopen (str tmp "/fs-exists") :create :truncate))
///     (test::assert-true (fs-exists? (str tmp "/fs-exists")))
///     (test::assert-true (fs-exists? tmp))
///     (test::assert-false (fs-exists? (str tmp "/fs-exists-nope")))))
#[sl_sh_fn(fn_name = "fs-exists?")]
fn path_exists(path: &str) -> VMResult<Value> {
    file_test(path, |path| path.exists(), "fs-exists?")
}

/// Usage: (fs-file? path-to-test)
///
/// Is the given path a file?
///
/// Section: file
///
/// Example:
/// (with-temp (fn (tmp)
///     (fclose (fopen (str tmp "/fs-file") :create :truncate))
///     (test::assert-true (fs-file? (str tmp "/fs-file")))
///     (test::assert-false (fs-file? tmp))
///     (test::assert-false (fs-file? (str tmp "/fs-file-nope")))))
#[sl_sh_fn(fn_name = "fs-file?")]
fn is_file(path: &str) -> VMResult<Value> {
    file_test(path, |path| path.is_file(), "fs-file?")
}

/// Usage: (fs-dir? path-to-test)
///
/// Is the given path a directory?
///
/// Section: file
///
/// Example:
/// (with-temp (fn (tmp)
///     (fclose (fopen (str tmp "/fs-dir-file") :create :truncate))
///     (test::assert-false (fs-dir? (str tmp "/fs-dir-file")))
///     (test::assert-true (fs-dir? tmp))
///     (test::assert-false (fs-file? (str tmp "/fs-dir-nope")))))
#[sl_sh_fn(fn_name = "fs-dir?")]
fn is_dir(path: &str) -> VMResult<Value> {
    file_test(path, |path| path.is_dir(), "fs-dir?")
}

/// Usage: (glob /path/with/*)
///
/// Takes a list/varargs of globs and return the list of them expanded.
///
/// Section: file
///
/// Example:
/// (with-temp (fn (tmp)
///     (fclose (fopen (str tmp "/g1") :create :truncate))
///     (fclose (fopen (str tmp "/g2") :create :truncate))
///     (fclose (fopen (str tmp "/g3") :create :truncate))
///     (test::assert-equal [(str tmp "/g1") (str tmp "/g2") (str tmp "/g3")] (glob (str tmp "/*")))))
#[sl_sh_fn(fn_name = "glob", takes_env = true)]
fn do_glob(environment: &mut SloshVm, args: VarArgs<String>) -> VMResult<Value> {
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
        let pat = expand_tilde(pat.into()).to_string_lossy().to_string();
        if let Ok(paths) = glob(&pat) {
            for p in paths {
                match p {
                    Ok(p) => {
                        if let Some(p) = p.to_str() {
                            files.push(environment.alloc_string(p.to_string()));
                        }
                    }
                    Err(err) => {
                        let msg = format!("glob error on while iterating {}, {}", pat, err);
                        return Err(VMError::new("io", msg));
                    }
                }
            }
            if files.is_empty() {
                // Got nothing so fall back on pattern.
                if pat.contains('\\') {
                    files.push(environment.alloc_string(remove_escapes(&pat)));
                } else {
                    files.push(environment.alloc_string(pat));
                }
            }
        } else if pat.contains('\\') {
            files.push(environment.alloc_string(remove_escapes(&pat)));
        } else {
            files.push(environment.alloc_string(pat));
        }
    }
    Ok(environment.alloc_vector(files))
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
fn fs_parent(path: &str) -> VMResult<String> {
    let fn_name = "fs-parent";
    if let Some(path) = get_file(path) {
        let mut path = path.canonicalize().map_err(|_| {
            let msg = format!("{} failed to get full filepath of parent", fn_name);
            VMError::new("io", msg)
        })?;
        let _ = path.pop();
        let path = path.as_path().to_str().ok_or_else(|| {
            let msg = format!("{} failed to get parent path", fn_name);
            VMError::new("io", msg)
        })?;
        Ok(path.to_string())
    } else {
        let msg = format!("{} first arg is not a valid path", fn_name);
        Err(VMError::new("io", msg))
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
fn fs_base(path: &str) -> VMResult<String> {
    let fn_name = "fs-base";
    match get_file(path) {
        Some(path) => {
            let path = path.file_name().and_then(|s| s.to_str()).ok_or_else(|| {
                let msg = format!("{} failed to extract name of file", fn_name);
                VMError::new("io", msg)
            })?;
            Ok(path.to_string())
        }
        None => {
            let msg = format!("{} first arg is not a valid path", fn_name);
            Err(VMError::new("io", msg))
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
///     (test::assert-true (fs-same? tmp-file tmp-file))))
#[sl_sh_fn(fn_name = "fs-same?")]
fn is_same_file(path_0: &str, path_1: &str) -> VMResult<Value> {
    let fn_name = "fs-same?";
    match (get_file(path_0), get_file(path_1)) {
        (Some(path_0), Some(path_1)) => {
            if let Ok(b) = same_file::is_same_file(path_0.as_path(), path_1.as_path()) {
                if b {
                    Ok(Value::True)
                } else {
                    Ok(Value::False)
                }
            } else {
                let msg = format!(
                    "{} there were insufficient permissions to access one or both of the provided files.",
                    fn_name
                );
                Err(VMError::new("io", msg))
            }
        }
        (_, _) => {
            let msg = format!("{} one or more paths does not exist.", fn_name);
            Err(VMError::new("io", msg))
        }
    }
}

/// Usage: (fs-crawl /path/to/file/or/dir (fn (x) (prn "found path" x) [max-depth]
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
/// 	(let (cnt 0)
/// 	(fs-crawl tmp-file (fn (x)
/// 		(test::assert-equal (fs-base tmp-file) (fs-base x))
/// 		(set! cnt (+ 1 cnt))))
/// 	(test::assert-equal 1 cnt))))
///
///
/// (defn create-in (in-dir num-files visited)
/// 	(dotimes-i i num-files
/// 		 (let (tmp-file (get-temp-file in-dir))
/// 		 (set! visited.~tmp-file #f))))
///
/// (defn create-dir (tmp-dir visited)
/// 	(let (new-tmp (get-temp tmp-dir))
/// 		(set! visited.~new-tmp #f)
/// 		new-tmp))
///
/// (with-temp (fn (root-tmp-dir)
/// 	(let (tmp-file-count 5
/// 		  visited {}
/// 		  cnt 0)
/// 	(set! visited.~root-tmp-dir #f)
/// 	(create-in root-tmp-dir tmp-file-count visited)
/// 	(let (tmp-dir (create-dir root-tmp-dir visited)
/// 			new-files (create-in tmp-dir tmp-file-count visited)
/// 			tmp-dir (create-dir tmp-dir visited)
/// 			new-files (create-in tmp-dir tmp-file-count visited))
/// 	(fs-crawl root-tmp-dir (fn (x)
/// 		(let (file visited.~x)
/// 			(test::assert-true (not file)) ;; also tests double counting
/// 			(set! visited.~x #t)
/// 			(inc! cnt))))
/// 	(test::assert-equal (+ 3 (* 3 tmp-file-count)) cnt)
/// 	(test::assert-equal (+ 3 (* 3 tmp-file-count)) (len visited))
/// 	(seq-for key in (hash-keys visited) (test::assert-true visited.~key))))))
///
/// (with-temp (fn (root-tmp-dir)
/// 	(let (tmp-file-count 5
/// 		  visited {}
/// 		  cnt 0)
/// 	(set! visited.~root-tmp-dir #f)
/// 	(create-in root-tmp-dir tmp-file-count visited)
/// 	(let (tmp-dir (create-dir root-tmp-dir visited)
/// 		  new-files (create-in tmp-dir tmp-file-count visited)
/// 		  tmp-dir (create-dir tmp-dir {})
/// 		  new-files (do (set! visited.~tmp-dir #f)(create-in tmp-dir tmp-file-count {})))
/// 	(fs-crawl root-tmp-dir (fn (x)
/// 		(let (file visited.~x)
/// 			(test::assert-true (not file)) ;; also tests double counting
/// 			(set! visited.~x #t)
/// 			(inc! cnt))) 2)
/// 	(test::assert-equal (+ 3 (* 2 tmp-file-count)) cnt)
/// 	(test::assert-equal (+ 3 (* 2 tmp-file-count)) (len visited))
/// 	(seq-for key in (hash-keys visited) (test::assert-true visited.~key))))))
///
/// (with-temp (fn (root-tmp-dir)
/// 	(let (tmp-file-count 5
/// 		  visited {}
/// 		  cnt 0)
/// 	(set! visited.~root-tmp-dir #f)
/// 	(create-in root-tmp-dir tmp-file-count visited)
/// 	(let (tmp-dir (create-dir root-tmp-dir {})
/// 		  new-files (do (set! visited.~tmp-dir #f)(create-in tmp-dir tmp-file-count {}))
/// 		  tmp-dir (create-dir tmp-dir {})
/// 		  new-files (create-in tmp-dir tmp-file-count {}))
/// 	(fs-crawl root-tmp-dir (fn (x)
/// 		(let (file visited.~x)
/// 			(test::assert-true (not file)) ;; also tests double counting
/// 			(set! visited.~x #t)
/// 			(inc! cnt))) 1)
/// 	(test::assert-equal (+ 2 tmp-file-count) cnt)
/// 	(test::assert-equal (+ 2 tmp-file-count) (len visited))
/// 	(seq-for key in (hash-keys visited) (test::assert-true visited.~key))))))
#[sl_sh_fn(fn_name = "fs-crawl", takes_env = true)]
fn fs_crawl(
    environment: &mut SloshVm,
    path: String,
    lambda_exp: Value,
    optional_depth_or_symlink: VarArgs<Value>,
) -> VMResult<Value> {
    let fn_name = "fs-crawl";
    let file_or_dir = get_file(&path);
    let mut depth = None;
    let mut sym_links = None;
    for depth_or_symlink in optional_depth_or_symlink {
        match depth_or_symlink {
            Value::Int(i) => {
                let i: i64 = from_i56(&i);
                depth = Some(i)
            }
            Value::Keyword(i) if environment.get_interned(i) == "follow-syms" => {
                sym_links = Some(true);
            }
            _ => {
                return Err(VMError::new(
                    "io",
                    format!(
                        "invalid argument {}",
                        depth_or_symlink.display_value(environment)
                    ),
                ))
            }
        }
    }
    match lambda_exp {
        Value::Lambda(_) | Value::Closure(_) => {
            if let Some(file_or_dir) = file_or_dir {
                let mut cb = |entry: &DirEntry| -> VMResult<()> {
                    let path = entry.path();
                    if let Some(path) = path.to_str() {
                        let path = environment.alloc_string(path.to_string());
                        apply_callable(environment, lambda_exp, &[path])?;
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
                Ok(Value::True)
            } else {
                let msg = format!("{} provided path does not exist", fn_name);
                Err(VMError::new("io", msg))
            }
        }
        _ => {
            let msg = format!("{} second argument must be a lambda", fn_name);
            Err(VMError::new("io", msg))
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
///     (let (tst-file (fopen tmp :create :truncate))
///         (fprn tst-file "Test Line Read Line One")
///         (fpr tst-file "Test Line Read Line Two")
///         (fclose tst-file)
///         (test::assert-equal 47 (fs-len tmp)))))
#[sl_sh_fn(fn_name = "fs-len")]
fn fs_len(file_or_dir: &str) -> VMResult<i64> {
    let fn_name = "fs-len";
    let file_or_dir = get_file(file_or_dir);
    if let Some(file_or_dir) = file_or_dir {
        if let Ok(metadata) = fs::metadata(file_or_dir) {
            let len = metadata.len();
            Ok(len as i64)
        } else {
            let msg = format!("{} can not fetch metadata at provided path", fn_name);
            Err(VMError::new("io", msg))
        }
    } else {
        let msg = format!("{} provided path does not exist", fn_name);
        Err(VMError::new("io", msg))
    }
}

fn get_file_time(
    file_or_dir: Option<PathBuf>,
    fn_name: &str,
    to_time: fn(Metadata) -> io::Result<SystemTime>,
) -> VMResult<i64> {
    if let Some(file_or_dir) = file_or_dir {
        if let Ok(metadata) = fs::metadata(file_or_dir) {
            if let Ok(sys_time) = to_time(metadata) {
                match sys_time.duration_since(SystemTime::UNIX_EPOCH) {
                    Ok(n) => {
                        let n = n.as_millis() as i64;
                        Ok(n)
                    }
                    Err(_) => {
                        let msg = format!("{} can not parse time", fn_name);
                        Err(VMError::new("io", msg))
                    }
                }
            } else {
                let msg = format!("{} can not fetch time", fn_name);
                Err(VMError::new("io", msg))
            }
        } else {
            let msg = format!("{} can not fetch metadata at provided path", fn_name);
            Err(VMError::new("io", msg))
        }
    } else {
        let msg = format!("{} provided path does not exist", fn_name);
        Err(VMError::new("io", msg))
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
///     (let (tst-file (fopen tmp :create :truncate)
///           last-mod (fs-modified tmp))
///         (fprn tst-file "Test Line Read Line One")
///         (fpr tst-file "Test Line Read Line Two")
///         (fflush tst-file)
///         (fclose tst-file)
///         (test::assert-true (>= (fs-modified tmp) last-mod)))))
#[sl_sh_fn(fn_name = "fs-modified")]
fn fs_modified(file_or_dir: &str) -> VMResult<i64> {
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
///     (let (tst-file (fopen tmp :create)
///           last-acc (fs-accessed tmp))
///         (fclose tst-file)
///         (let (tst-file (fopen tmp :read))
///             (test::assert-true (>= (fs-accessed tmp) last-acc))
///             (fclose tst-file)))))
#[sl_sh_fn(fn_name = "fs-accessed")]
fn fs_accessed(file_or_dir: &str) -> VMResult<i64> {
    let file_or_dir = get_file(file_or_dir);
    get_file_time(file_or_dir, "fs-accessed", |md| md.accessed())
}

fn fs_meta(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(string), None) = (i.next(), i.next()) {
        let name = string.pretty_value(vm);
        let file = File::open(name)?;
        let meta = file.metadata()?;
        let mut map = VMHashMap::new();
        let ftype = if meta.is_dir() {
            "dir"
        } else if meta.is_file() {
            "file"
        } else if meta.is_symlink() {
            "symlink"
        } else {
            "unknown"
        };
        let ro = if meta.permissions().readonly() {
            Value::True
        } else {
            Value::False
        };
        let key = Value::Keyword(vm.intern_static("readonly"));
        map.insert(vm, key, ro);
        let key = Value::Keyword(vm.intern_static("len"));
        let val: Value = (meta.len() as i64).into();
        map.insert(vm, key, val);
        let key = Value::Keyword(vm.intern_static("type"));
        let val = Value::Keyword(vm.intern_static(ftype));
        map.insert(vm, key, val);
        // XXX TODO- include times.
        Ok(vm.alloc_map(map))
    } else {
        Err(VMError::new(
            "io",
            "fs-meta: takes a filename as only arg".to_string(),
        ))
    }
}

pub fn add_fs_meta_builtins(env: &mut SloshVm) {
    intern_cd(env);
    intern_fs_fullpath(env);
    intern_path_exists(env);
    intern_is_file(env);
    intern_is_dir(env);
    intern_do_glob(env);
    intern_fs_crawl(env);
    intern_is_same_file(env);
    intern_fs_base(env);
    intern_fs_parent(env);
    intern_fs_len(env);
    intern_fs_modified(env);
    intern_fs_accessed(env);
    intern_sleep(env);

    add_builtin(
        env,
        "fs-meta",
        fs_meta,
        r#"Usage: (fs-meta [FILENAME]) -> map

Returns a map of a files meta data.

Section: io
"#,
    );
}
