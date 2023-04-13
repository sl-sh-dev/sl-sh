use std::env;
use std::ffi::OsString;
use std::path::{Path, PathBuf};

pub fn cd(arg: Option<PathBuf>) -> i32 {
    let home: OsString = match env::var_os("HOME") {
        Some(val) => val,
        None => "/".into(),
    };
    let old_dir: OsString = match env::var_os("OLDPWD") {
        Some(val) => val,
        None => home.clone(),
    };
    let new_dir: PathBuf = match arg {
        Some(arg) => expand_tilde(arg),
        None => home.into(),
    };
    let new_dir = if new_dir.as_os_str() == "-" {
        old_dir.into()
    } else {
        new_dir
    };
    let new_dir = cd_expand_all_dots(new_dir);
    let root = Path::new(&new_dir);
    if let Ok(oldpwd) = env::current_dir() {
        env::set_var("OLDPWD", oldpwd);
    }
    if let Err(e) = env::set_current_dir(root) {
        eprintln!("cd: Error changing to {}, {}", root.display(), e);
        -1
    } else {
        match env::current_dir() {
            Ok(dir) => env::set_var("PWD", dir),
            Err(err) => eprintln!("cd: error setting PWD: {err}"),
        }
        0
    }
}

/// Takes a PathBuf, if it contains ~ then replaces it with HOME and returns the new PathBuf, else
/// returns path.  If path is not utf-8 then it will not expand ~.
pub fn expand_tilde(path: PathBuf) -> PathBuf {
    if let Some(path_str) = path.to_str() {
        if path_str.ends_with('~') || path_str.contains("~/") {
            let home: PathBuf = match env::var_os("HOME") {
                Some(val) => val.into(),
                None => "/".into(),
            };
            let mut new_path = OsString::new();
            let mut last_ch = ' ';
            let mut buf = [0_u8; 4];
            for ch in path_str.chars() {
                if ch == '~' && (last_ch == ' ' || last_ch == ':') {
                    // Strip the trailing / if it exists.
                    new_path.push(home.components().as_path().as_os_str());
                } else {
                    if last_ch == '\\' {
                        new_path.push("\\");
                    }
                    if ch != '\\' {
                        new_path.push(ch.encode_utf8(&mut buf));
                    }
                }
                last_ch = ch;
            }
            if last_ch == '\\' {
                new_path.push("\\");
            }
            new_path.into()
        } else {
            path
        }
    } else {
        path
    }
}

fn cd_expand_all_dots(cd: PathBuf) -> PathBuf {
    let mut all_dots = false;
    let cd_ref = cd.to_string_lossy();
    if cd_ref.len() > 2 {
        all_dots = true;
        for ch in cd_ref.chars() {
            if ch != '.' {
                all_dots = false;
                break;
            }
        }
    }
    if all_dots {
        let mut new_cd = OsString::new();
        let paths_up = cd_ref.len() - 2;
        new_cd.push("../");
        for _i in 0..paths_up {
            new_cd.push("../");
        }
        new_cd.into()
    } else {
        cd
    }
}
