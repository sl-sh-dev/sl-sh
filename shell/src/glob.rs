use std::path::PathBuf;

pub enum GlobOutput {
    Arg(PathBuf),
    Args(Vec<PathBuf>),
}

pub fn expand_glob(pat: impl Into<PathBuf>) -> GlobOutput {
    let pat: PathBuf = pat.into();
    let mut files = Vec::new();
    if let Ok(paths) = glob::glob(&pat.to_string_lossy()) {
        for p in paths {
            match p {
                Ok(p) => {
                    files.push(p);
                }
                Err(err) => {
                    eprintln!("glob error while iterating {}, {}", pat.display(), err);
                }
            }
        }
        if files.is_empty() {
            // Got nothing so fall back on pattern.
            if pat.to_str().unwrap_or("").contains('\\') {
                GlobOutput::Arg(remove_escapes(pat.to_str().unwrap_or("")).into())
            } else {
                GlobOutput::Arg(pat)
            }
        } else if pat.starts_with("./") {
            GlobOutput::Args(
                files
                    .iter()
                    .cloned()
                    .map(|f| {
                        let mut nf = PathBuf::new();
                        nf.push("./");
                        nf.push(f);
                        nf
                    })
                    .collect(),
            )
        } else {
            GlobOutput::Args(files)
        }
    } else if pat.to_str().unwrap_or("").contains('\\') {
        GlobOutput::Arg(remove_escapes(pat.to_str().unwrap_or("")).into())
    } else {
        GlobOutput::Arg(pat)
    }
}

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
                last_esc = false;
            }
        }
    }
    ret
}
