use crate::command_data::Arg;
use crate::jobs::Jobs;
use std::env;
use std::ffi::{OsStr, OsString};
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

/// If path start with HOME then replace with ~.
pub fn compress_tilde(path: &str) -> Option<String> {
    if let Ok(mut home) = env::var("HOME") {
        if home.ends_with('/') {
            home.pop();
        }
        if path.starts_with(&home) {
            Some(path.replace(&home, "~"))
        } else {
            None
        }
    } else {
        None
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

fn export(arg: OsString, arg2: Option<OsString>) {
    let arg = arg.to_string_lossy();
    if !arg.contains('=') {
        eprintln!("export: VAR_NAME=VALUE");
        return;
    }
    let mut key_val = arg.split('=');
    if let Some(key) = key_val.next() {
        if let Some(val) = key_val.next() {
            if val.is_empty() {
                if let Some(val) = arg2 {
                    env::set_var(key, val);
                }
            } else {
                env::set_var(key, val);
            }
        } else {
            eprintln!("export: VAR_NAME=VALUE");
        }
    } else {
        eprintln!("export: VAR_NAME=VALUE");
    }
}

fn alias<I>(args: I, jobs: &mut Jobs)
where
    I: Iterator<Item = OsString>,
{
    let mut empty = true;
    let mut args = args.map(|a| a.to_string_lossy().to_string());
    while let Some(a) = args.next() {
        empty = false;
        if a.contains('=') {
            let mut key_val = a.split('=');
            if let (Some(key), val, None) = (key_val.next(), key_val.next(), key_val.next()) {
                let val = if let Some(val) = val {
                    if val.is_empty() {
                        args.next().unwrap_or_default()
                    } else {
                        val.to_string()
                    }
                } else {
                    args.next().unwrap_or_default()
                };
                if let Err(e) = jobs.add_alias(key.to_string(), val) {
                    eprintln!("alias: error setting {key}: {e}");
                }
            } else {
                eprintln!("alias: invalid arg {a}, use ALIAS_NAME=\"command\"");
            }
        } else {
            jobs.print_alias(a.to_string());
        }
    }
    if empty {
        jobs.print_all_alias();
    }
}

pub fn run_builtin<'arg, I>(command: &OsStr, args: &mut I, jobs: &mut Jobs) -> bool
where
    I: Iterator<Item = &'arg Arg>,
{
    let mut args = args.map(|v| v.resolve_arg(jobs).unwrap_or_default());
    if command == "cd" {
        let arg = args.next();
        if args.next().is_none() {
            cd(arg.map(|s| s.into()));
        } else {
            eprintln!("cd: too many arguments!");
        }
        true
    } else if command == "fg" {
        if let Some(arg) = args.next() {
            if args.next().is_none() {
                if let Some(Ok(job_num)) = arg.to_str().map(|s| s.parse()) {
                    jobs.foreground_job(job_num);
                }
            } else {
                eprintln!("fg: takes one argument!");
            }
        } else {
            eprintln!("fg: takes one argument!");
        }
        true
    } else if command == "bg" {
        if let Some(arg) = args.next() {
            if args.next().is_none() {
                if let Some(Ok(job_num)) = arg.to_str().map(|s| s.parse()) {
                    jobs.background_job(job_num);
                }
            } else {
                eprintln!("fg: takes one argument!");
            }
        } else {
            eprintln!("fg: takes one argument!");
        }
        true
    } else if command == "jobs" {
        if args.next().is_some() {
            eprintln!("jobs: too many arguments!");
        } else {
            println!("{jobs}");
        }
        true
    } else if command == "export" {
        if let Some(arg) = args.next() {
            export(arg, args.next());
        } else {
            eprintln!("export: VAR_NAME=VALUE");
        }
        true
    } else if command == "alias" {
        let args: Vec<OsString> = args.collect();
        alias(args.into_iter(), jobs);
        true
    } else {
        false
    }
}
