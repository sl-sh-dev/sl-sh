use crate::rand::rand_alphanumeric_str;
use bridge_macros::sl_sh_fn;
use compile_state::state::SloshVm;
use rand;
use shell::builtins::expand_tilde;
use slvm::{VMError, VMResult};
use std::ffi::OsStr;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::{env, fs};

const LEN: i64 = 5;

/// Usage: (get-temp & base-path prefix suffix length) => directory-path
///
/// Creates a directory inside of an OS specific temporary directory.
///
/// Arguments:
/// - base-path: A string (optional). Directory to use as base instead of system temp directory.
/// - prefix: A string (optional). Prefix for the directory name (defaults to ".tmp").
/// - suffix: A string (optional). Suffix for the directory name (defaults to empty).
/// - length: An integer (optional). Number of random characters (defaults to 5).
/// - directory-path: A string. Path to the newly created temporary directory.
///
/// See [temp-dir](root::temp-dir) for OS specific notes. The directory name is
/// constructed as: prefix + random-characters + suffix.
///
/// Section: file
///
/// Example:
/// (test::assert-true (str-contains (get-temp) (temp-dir)))
///
/// (with-temp (fn (tmp)
///         (let (tmp-dir (get-temp tmp))
///             (test::assert-true (str-contains tmp-dir tmp)))))
///
/// (with-temp (fn (tmp)
///         (let (tmp-dir (get-temp tmp "some-prefix"))
///             (test::assert-true (str-contains tmp-dir tmp))
///             (test::assert-true (str-contains tmp-dir "some-prefix")))))
///
/// (with-temp (fn (tmp)
///         (let (tmp-dir (get-temp tmp "some-prefix" "some-suffix"))
///             (test::assert-true (str-contains tmp-dir tmp))
///             (test::assert-true (str-contains tmp-dir "some-prefix"))
///             (test::assert-true (str-contains tmp-dir "some-suffix")))))
///
/// (with-temp (fn (tmp)
///         (let (tmp-dir (get-temp tmp "some-prefix" "some-suffix" 6))
///             (test::assert-true (str-contains tmp-dir tmp))
///             (test::assert-true (str-contains tmp-dir "some-prefix"))
///             (test::assert-true (str-contains tmp-dir "some-suffix"))
///             (test::assert-equal (len "some-prefix012345some-suffix") (len (fs-base tmp-dir))))))
#[sl_sh_fn(fn_name = "get-temp")]
fn get_temp(
    path: Option<String>,
    prefix: Option<String>,
    suffix: Option<String>,
    len: Option<i64>,
) -> VMResult<String> {
    let fn_name = "get-temp";
    let dir = get_provided_or_default_temp(path, fn_name)?;
    let prefix = prefix.as_deref().unwrap_or_default();
    let suffix = suffix.as_deref().unwrap_or_default();
    let len = len.unwrap_or(LEN);
    let dir = create_temp_dir(dir.as_path(), prefix, suffix, len, fn_name)?;
    if let Some(path) = dir.to_str() {
        let path = path.to_string();
        Ok(path)
    } else {
        let msg = format!("{} unable to provide temporary directory", fn_name);
        Err(VMError::new("io", msg))
    }
}

/// Usage: (temp-dir) => directory-path
///
/// Returns the OS-specific temporary directory path.
///
/// Arguments:
/// - directory-path: A string. The system temporary directory path.
///
/// See [get-temp](root::get-temp) for higher level temporary directory creation mechanism.
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
fn builtin_temp_dir() -> VMResult<String> {
    if let Some(path) = temp_dir().to_str() {
        Ok(path.to_string())
    } else {
        Err(VMError::new(
            "io",
            "temp-dir: unable to provide temporary directory".to_string(),
        ))
    }
}

fn get_provided_or_default_temp(path: Option<String>, fn_name: &str) -> VMResult<PathBuf> {
    match path {
        None => Ok(temp_dir()),
        Some(path) => {
            let dir = expand_tilde(path.into());
            let p = dir.as_path();
            if p.exists() && p.is_dir() {
                Ok(dir)
            } else {
                let msg = format!(
                    "{} unable to provide temporary file in provided directory",
                    fn_name
                );
                Err(VMError::new("io", msg))
            }
        }
    }
}

fn create_temp_dir(
    path: &Path,
    prefix: &str,
    suffix: &str,
    len: i64,
    fn_name: &str,
) -> VMResult<PathBuf> {
    if path.exists() && path.is_dir() {
        let dir_name = random_name(prefix, suffix, len);
        let dir = Path::new::<OsStr>(dir_name.as_ref());
        let dir = path.join(dir);
        fs::create_dir(dir.as_path()).map_err(|err| {
            let msg = format!("{} unable to create temporary directory inside default temporary directory ({:?}), reason: {:?}", fn_name, dir.as_path(), err);
            VMError::new("io", msg)
        })?;
        Ok(dir)
    } else {
        let msg = format!("{} unable to provide temporary directory", fn_name);
        Err(VMError::new("io", msg))
    }
}

fn random_name(prefix: &str, suffix: &str, len: i64) -> String {
    let prefix = if prefix.is_empty() { ".tmp" } else { prefix };
    let mut rng = rand::thread_rng();
    let name = rand_alphanumeric_str(len.unsigned_abs(), &mut rng);
    format!("{}{}{}", prefix, name, suffix)
}

fn temp_dir() -> PathBuf {
    env::temp_dir()
}

fn create_temp_file(
    dir: PathBuf,
    prefix: &Option<String>,
    suffix: &Option<String>,
    len: Option<i64>,
    fn_name: &str,
) -> VMResult<PathBuf> {
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
        VMError::new("io", msg)
    })?;
    Ok(file)
}

/// Usage: (get-temp-file & base-path prefix suffix length) => file-path
///
/// Creates a temporary file and returns its path.
///
/// Arguments:
/// - base-path: A string (optional). Directory to create the file in (defaults to system temp).
/// - prefix: A string (optional). Prefix for the file name (defaults to ".tmp").
/// - suffix: A string (optional). Suffix for the file name (defaults to empty).
/// - length: An integer (optional). Number of random characters (defaults to 5).
/// - file-path: A string. Path to the newly created temporary file.
///
/// The file name is constructed as: prefix + random-characters + suffix.
/// The file is created empty and ready for writing.
///
/// Section: file
///
/// Example:
/// (test::assert-true (str-contains (get-temp-file) (temp-dir)))
///
/// (with-temp (fn (tmp)
///         (let (tmp-file (get-temp-file tmp))
///             (test::assert-true (str-contains tmp-file tmp)))))
///
/// (with-temp (fn (tmp)
///         (let (tmp-file (get-temp-file tmp "some-prefix"))
///             (test::assert-true (str-contains tmp-file "some-prefix")))))
///
/// (with-temp (fn (tmp)
///         (let (tmp-file (get-temp-file tmp "some-prefix" "some-suffix"))
///             (test::assert-true (str-contains tmp-file "some-prefix"))
///             (test::assert-true (str-contains tmp-file "some-suffix")))))
///
/// (with-temp (fn (tmp)
///         (let (tmp-file (get-temp-file tmp "some-prefix" "some-suffix" 10))
///             (test::assert-true (str-contains tmp-file "some-prefix"))
///             (test::assert-true (str-contains tmp-file "some-suffix"))
///             (test::assert-equal (len "some-prefix0123456789some-suffix") (len (fs-base tmp-file))))))
#[sl_sh_fn(fn_name = "get-temp-file")]
fn get_temp_file(
    path: Option<String>,
    prefix: Option<String>,
    suffix: Option<String>,
    len: Option<i64>,
) -> VMResult<String> {
    let fn_name = "get-temp-file";
    let dir = get_provided_or_default_temp(path, fn_name)?;
    let file = create_temp_file(dir, &prefix, &suffix, len, fn_name)?;
    if let Some(path) = file.as_path().to_str() {
        Ok(path.to_string())
    } else {
        let msg = format!("{} unable to provide temporary file", fn_name);
        Err(VMError::new("io", msg))
    }
}

pub fn add_fs_temp_builtins(env: &mut SloshVm) {
    intern_get_temp(env);
    intern_get_temp_file(env);
    intern_builtin_temp_dir(env);
}
