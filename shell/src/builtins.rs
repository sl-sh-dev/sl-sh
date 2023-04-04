use std::env;
use std::path::Path;

pub fn cd(arg: Option<String>) -> i32 {
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
            if let Some(h) = expand_tilde(&arg) {
                h
            } else {
                arg
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

fn expand_tilde(path: &str) -> Option<String> {
    if path.ends_with('~') || path.contains("~/") {
        let mut home = match env::var("HOME") {
            Ok(val) => val,
            Err(_) => "/".to_string(),
        };
        if home.ends_with('/') {
            home.pop();
        }
        let mut new_path = String::new();
        let mut last_ch = ' ';
        for ch in path.chars() {
            if ch == '~' && (last_ch == ' ' || last_ch == ':') {
                if last_ch == '\\' {
                    new_path.push('~');
                } else {
                    new_path.push_str(&home);
                }
            } else {
                if last_ch == '\\' {
                    new_path.push('\\');
                }
                if ch != '\\' {
                    new_path.push(ch);
                }
            }
            last_ch = ch;
        }
        if last_ch == '\\' {
            new_path.push('\\');
        }
        Some(new_path)
    } else {
        None
    }
}

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
