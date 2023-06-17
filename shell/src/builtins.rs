use crate::command_data::Arg;
use crate::jobs::Jobs;
use crate::platform::{Platform, RLimit, RLimitVals, Sys};
use std::collections::HashSet;
use std::env;
use std::ffi::{OsStr, OsString};
use std::path::{Path, PathBuf};

fn set_arg_flags<S: AsRef<str>>(
    args: &mut HashSet<char>,
    allowed: &str,
    arg: S,
) -> Result<(), String> {
    let mut arg = arg.as_ref().chars();
    arg.next(); // Skip over the leading '-'
    for c in arg {
        if allowed.contains(c) {
            args.insert(c);
        } else {
            return Err(format!("invalid arg {c}"));
        }
    }
    Ok(())
}

fn ulimit_parm_list(args: &HashSet<char>) -> Result<Vec<RLimit>, String> {
    let mut limits = Vec::new();
    for ch in args {
        match ch {
            'H' | 'S' | 'a' => {}
            'b' => limits.push(RLimit::SocketBufferSize),
            'c' => limits.push(RLimit::CoreSize),
            'd' => limits.push(RLimit::DataSize),
            'e' => limits.push(RLimit::Nice),
            'f' => limits.push(RLimit::FileSize),
            'i' => limits.push(RLimit::SigPending),
            'k' => limits.push(RLimit::KQueues),
            'l' => limits.push(RLimit::MemLock),
            'm' => limits.push(RLimit::RSS),
            'n' => limits.push(RLimit::MaxFiles),
            //'p' => limits.push(RLimit::SocketBufferSize),
            'q' => limits.push(RLimit::MessageQueueByte),
            'r' => limits.push(RLimit::RealTimePriority),
            's' => limits.push(RLimit::StackSize),
            't' => limits.push(RLimit::CpuTime),
            'u' => limits.push(RLimit::MaxProcs),
            'v' => limits.push(RLimit::MaxMemory),
            'x' => limits.push(RLimit::MaxFileLocks),
            'P' => limits.push(RLimit::MaxPtty),
            'R' => limits.push(RLimit::MaxRealTime),
            'T' => limits.push(RLimit::MaxThreads),
            _ => return Err(format!("unknown option {ch}")),
        }
    }
    Ok(limits)
}

fn ulimit<I>(args: I, _jobs: &mut Jobs) -> i32
where
    I: Iterator<Item = OsString>,
{
    let mut args_flags = HashSet::new();
    let mut limit = None;
    for arg in args {
        if arg.to_string_lossy().starts_with('-') {
            if let Err(err) = set_arg_flags(
                &mut args_flags,
                "SHabcdefiklmnpqrstuvxPT",
                arg.to_string_lossy(),
            ) {
                eprintln!("ulimit: {err}");
                eprintln!("ulimit: usage: ulimit [-SHabcdefiklmnpqrstuvxPRT] [limit]");
                return 1;
            }
        } else if limit.is_none() {
            limit = Some(arg.to_string_lossy().to_string());
        } else {
            eprintln!("ulimit: invalid parameters");
            eprintln!("ulimit: usage: ulimit [-SHabcdefiklmnpqrstuvxPT] [limit]");
            return 1;
        }
    }
    let limits = match ulimit_parm_list(&args_flags) {
        Ok(limits) => limits,
        Err(err) => {
            eprintln!("ulimit: {err}");
            eprintln!("ulimit: usage: ulimit [-SHabcdefiklmnpqrstuvxPRT] [limit]");
            return 1;
        }
    };
    if let Some(limit) = limit {
        let limit = match limit.as_ref() {
            "unlimited" => u64::MAX,
            _ => match limit.parse() {
                Ok(limit) => limit,
                Err(_err) => {
                    eprintln!("ulimit: invalid limit");
                    return 1;
                }
            },
        };
        let vals = RLimitVals {
            current: limit,
            max: limit,
        };
        for l in limits {
            if let Err(err) = Sys::set_rlimit(l, vals) {
                eprintln!("ulimit: {err}");
                return 1;
            }
        }
    } else {
        for l in limits {
            match Sys::get_rlimit(l) {
                Ok(lim) => {
                    match (lim.current, lim.max) {
                        (u64::MAX, u64::MAX) => println!("unlimited"),
                        (u64::MAX, max) => println!("unlimited/{max}"), // WTF...
                        (current, u64::MAX) => println!("{current}/unlimited"),
                        (current, max) if current == max => println!("{current}"),
                        (current, max) => println!("{current}/{max}"),
                    }
                }
                Err(err) => eprintln!("ulimit: {err}"),
            }
        }
    }
    0
}

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

fn export(key: OsString, val: OsString) -> i32 {
    let key_str = key.to_string_lossy();
    if key.is_empty() || key_str.contains('=') || key_str.contains('\0') {
        eprintln!("export: Invalid key");
        return 1;
    }
    let val_str = val.to_string_lossy();
    if val_str.contains('\0') {
        eprintln!("export: Invalid val (contains NUL character ('\\0')'");
        return 1;
    }
    let val = if val.to_string_lossy().contains('~') {
        expand_tilde(val.into()).into()
    } else {
        val
    };
    env::set_var(key, val);
    0
}

fn set_var(jobs: &mut Jobs, key: OsString, val: OsString) -> i32 {
    let key_str = key.to_string_lossy();
    if key.is_empty() || key_str.contains('=') || key_str.contains('\0') {
        eprintln!("export: Invalid key");
        return 1;
    }
    let val_str = val.to_string_lossy();
    if val_str.contains('\0') {
        eprintln!("export: Invalid val (contains NUL character ('\\0')'");
        return 1;
    }
    if env::var_os(&key).is_some() {
        env::set_var(key, val);
    } else {
        jobs.set_local_var(key, val);
    }
    0
}

fn alias<I>(args: I, jobs: &mut Jobs) -> i32
where
    I: Iterator<Item = OsString>,
{
    let mut status = 0;
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
                    status = 1;
                }
            } else {
                eprintln!("alias: invalid arg {a}, use ALIAS_NAME=\"command\"");
                status = 1;
            }
        } else {
            jobs.print_alias(a.to_string());
        }
    }
    if empty {
        jobs.print_all_alias();
    }
    status
}

fn unalias<I>(args: I, jobs: &mut Jobs) -> i32
where
    I: Iterator<Item = OsString>,
{
    for arg in args {
        let arg = arg.to_string_lossy();
        if arg == "-a" {
            jobs.clear_aliases();
        } else {
            jobs.remove_alias(&arg)
        }
    }
    0
}

pub fn run_builtin<'arg, I>(command: &OsStr, args: &mut I, jobs: &mut Jobs) -> Option<i32>
where
    I: Iterator<Item = &'arg Arg>,
{
    let mut args = args.map(|v| v.resolve_arg(jobs).unwrap_or_default());
    let command_str = command.to_string_lossy();
    let status = match &*command_str {
        "cd" => {
            let arg = args.next();
            if args.next().is_none() {
                cd(arg.map(|s| s.into()))
            } else {
                eprintln!("cd: too many arguments!");
                1
            }
        }
        "fg" => {
            if let Some(arg) = args.next() {
                if args.next().is_none() {
                    if let Some(Ok(job_num)) = arg.to_str().map(|s| s.parse()) {
                        jobs.foreground_job(job_num);
                    }
                    0
                } else {
                    eprintln!("fg: takes one argument!");
                    1
                }
            } else {
                eprintln!("fg: takes one argument!");
                1
            }
        }
        "bg" => {
            if let Some(arg) = args.next() {
                if args.next().is_none() {
                    if let Some(Ok(job_num)) = arg.to_str().map(|s| s.parse()) {
                        jobs.background_job(job_num);
                    }
                    0
                } else {
                    eprintln!("fg: takes one argument!");
                    1
                }
            } else {
                eprintln!("fg: takes one argument!");
                1
            }
        }
        "jobs" => {
            if args.next().is_some() {
                eprintln!("jobs: too many arguments!");
                1
            } else {
                println!("{jobs}");
                0
            }
        }
        "export" => {
            fn split_export<S: AsRef<str>>(arg: S) -> i32 {
                let arg = arg.as_ref();
                let mut key_val = arg.split('=');
                if let (Some(key), Some(val), None) =
                    (key_val.next(), key_val.next(), key_val.next())
                {
                    export(key.into(), val.into())
                } else {
                    eprintln!("export: VAR_NAME=VALUE");
                    1
                }
            }
            let ceq: OsString = "=".into();
            match (args.next(), args.next(), args.next(), args.next()) {
                (Some(arg), None, None, None) => {
                    if arg.to_string_lossy().contains('=') {
                        split_export(arg.to_string_lossy())
                    } else {
                        if let Some(val) = jobs.remove_local_var(&arg) {
                            env::set_var(arg, val)
                        }
                        0
                    }
                }
                (Some(arg1), Some(arg2), None, None) => {
                    let arg = arg1.to_string_lossy() + arg2.to_string_lossy();
                    split_export(arg)
                }
                (Some(arg), Some(eq), Some(val), None) if eq == ceq => export(arg, val),
                _ => {
                    eprintln!("export: VAR_NAME=VALUE");
                    1
                }
            }
        }
        "unset" => match (args.next(), args.next()) {
            (Some(key), None) => {
                let key_str = key.to_string_lossy();
                if key.is_empty() || key_str.contains('=') || key_str.contains('\x00') {
                    eprintln!("unset: Invalid key");
                    1
                } else {
                    jobs.remove_local_var(&key);
                    env::remove_var(key);
                    0
                }
            }
            _ => {
                eprintln!("unset: VAR_NAME");
                1
            }
        },
        "alias" => {
            let args: Vec<OsString> = args.collect();
            alias(args.into_iter(), jobs)
        }
        "unalias" => {
            let args: Vec<OsString> = args.collect();
            unalias(args.into_iter(), jobs)
        }
        "ulimit" => {
            let args: Vec<OsString> = args.collect();
            ulimit(args.into_iter(), jobs)
        }
        // Check for VAR_NAME=val before returning.
        _ => match (args.next(), args.next()) {
            (None, None) if command_str.contains('=') => {
                let mut key_val = command_str.split('=');
                if let (Some(key), Some(val), None) =
                    (key_val.next(), key_val.next(), key_val.next())
                {
                    set_var(jobs, key.into(), val.into())
                } else {
                    return None;
                }
            }
            (Some(val), None) if command_str.ends_with('=') => {
                if let Some(key) = command_str.strip_suffix('=') {
                    set_var(jobs, key.into(), val)
                } else {
                    return None;
                }
            }
            _ => return None,
        },
    };
    Some(status)
}
