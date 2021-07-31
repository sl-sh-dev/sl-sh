use walkdir::WalkDir;

use crate::eval::*;
use crate::{
    expand_tilde, param_eval, params_done, Environment, ExpEnum, Expression, Interner, LispError,
};
use core::iter;
use std::collections::HashMap;
use std::fs;
use std::hash::BuildHasher;
use std::path::{Path, PathBuf};

enum Event {
    Create(String),
    Write(String),
    Chmod(String),
    Remove(String),
    Rename(String, String),
}

fn get_event(file_or_dir: PathBuf) -> Result<Event, LispError> {
    if file_or_dir.is_dir() {
        for entry in WalkDir::new("foo").into_iter().filter_map(|e| e.ok()) {
            println!("{}", entry.path().display());
        }
        Ok(Event::Create(format!("Event::Create: {:?}", file_or_dir)))
    } else {
        match fs::metadata(file_or_dir.as_path()) {
            Ok(md) => {
                println!("Metadata:: {:?}.", md);
                Ok(Event::Create(format!("Event::Create: {:?}", file_or_dir)))
            }
            Err(e) => Err(LispError::new(format!(
                "unable to get metadata for file: {:?}. Errror: {:?}",
                file_or_dir, e
            ))),
        }
    }
}

fn get_file(
    environment: &mut Environment,
    p: Expression,
    fn_name: &str,
) -> Result<PathBuf, LispError> {
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
    let p = Path::new(&p);
    Ok((*p.to_path_buf()).to_owned())
}

fn handle_event(event: Event, lambda_exp: Expression, environment: &mut Environment) {
    let event = match event {
        Event::Create(path) => Some((":notice-create", format!("{:?}", path))),
        Event::Write(path) => Some((":notice-write", format!("{:?}", path))),
        Event::Chmod(path) => Some((":notice-chmod", format!("{:?}", path))),
        Event::Remove(path) => Some((":notice-remove", format!("{:?}", path))),
        Event::Rename(src, dest) => Some((":notice-rename", format!("{:?}", dest))),
    };
    if let Some(event) = event {
        let (event_type, path) = event;
        let event_type = Expression::alloc_data(ExpEnum::String(
            environment.interner.intern(event_type).into(),
            None,
        ));
        let mut args = iter::once(event_type);
        let val = call_lambda(environment, lambda_exp, &mut args, true);
        println!("Expression!: {:?}.", val);
    }
}

fn builtin_notify(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let arg0 = param_eval(environment, args, "notify")?;
    let file_or_dir = get_file(environment, arg0, "notify")?;
    let lambda_exp = param_eval(environment, args, "notify")?;
    let lambda_exp_d = &lambda_exp.get().data;
    params_done(args, "notify")?;
    match lambda_exp_d {
        ExpEnum::Lambda(_) => {
            let p = file_or_dir.as_path();
            if p.exists() {
                let event = get_event(file_or_dir)?;

                handle_event(event, lambda_exp.copy(), environment);
                Ok(Expression::make_true())
            } else {
                Err(LispError::new("provided path does not exist"))
            }
        }
        _ => Err(LispError::new("second argument must be function")),
    }
}

pub fn add_notify_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, (Expression, String), S>,
) {
    data.insert(
        interner.intern("notify"),
        Expression::make_function(
            builtin_notify,
            r#"Usage: (notify /path/to/file/or/dir fn)

Takes a path to a file or a directory and a function to call. If changes occur in file or
recursively in directory provided function is called with sequence of changed files.

Section: shell

Example:
"#,
        ),
    );
}
