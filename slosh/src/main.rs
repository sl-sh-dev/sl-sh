extern crate sl_liner;

use std::cell::RefCell;
use std::env;
use std::ffi::OsString;
use std::fs::create_dir_all;
use std::io::{BufRead, ErrorKind, Write};
use std::path::PathBuf;
use std::sync::Arc;

use slvm::opcodes::*;

use compile_state::state::*;
use sl_compiler::compile::*;
use sl_compiler::reader::*;

use builtins::collections::setup_collection_builtins;
use builtins::conversions::add_conv_builtins;
use builtins::io::add_io_builtins;
use builtins::print::{add_print_builtins, display_value};
use builtins::string::add_str_builtins;
use builtins::{add_global_value, add_misc_builtins};
use sl_liner::vi::AlphanumericAndVariableKeywordRule;
use sl_liner::{keymap, ColorClosure, Context, Prompt};

mod completions;
mod config;
pub mod debug;
mod liner_rules;
mod load_eval;
mod shell_builtins;

use crate::completions::ShellCompleter;
use crate::liner_rules::make_editor_rules;
use crate::load_eval::{add_load_builtins, load_internal, SLSHRC};
use crate::shell_builtins::add_shell_builtins;
use config::*;
use debug::*;
use shell::platform::{Platform, Sys, STDIN_FILENO};
use sl_compiler::pass1::pass1;
use slvm::{Value, INT_BITS, INT_MAX, INT_MIN};

thread_local! {
    /// Env (job control status, etc) for the shell.
    pub static SHELL_ENV: RefCell<shell::jobs::Jobs> = RefCell::new(shell::jobs::Jobs::new(true));
}

thread_local! {
    /// Env (job control status, etc) for the shell.
    pub static ENV: RefCell<SloshVm> = RefCell::new(new_slosh_vm());
}

const PROMPT_FN: &str = "prompt";

fn get_prompt(env: &mut SloshVm) -> String {
    let i_val = env.intern("__prompt");
    if let Some(idx) = env.global_intern_slot(i_val) {
        match env.get_global(idx) {
            Value::Lambda(h) => {
                let l = env.get_lambda(h);
                match env.do_call(l, &[], None) {
                    Ok(v) => match v {
                        Value::StringConst(i) => env.get_interned(i).to_string(),
                        Value::String(h) => env.get_string(h).to_string(),
                        _ => v.display_value(env),
                    },
                    Err(e) => {
                        eprintln!("Error getting prompt: {e}");
                        "slosh> ".to_string()
                    }
                }
            }
            Value::Closure(h) => {
                let (l, tcaps) = env.get_closure(h);
                let caps = Vec::from(tcaps);
                match env.do_call(l, &[], Some(&caps[..])) {
                    Ok(v) => match v {
                        Value::StringConst(i) => env.get_interned(i).to_string(),
                        Value::String(h) => env.get_string(h).to_string(),
                        _ => v.display_value(env),
                    },
                    Err(e) => {
                        eprintln!("Error getting prompt: {e}");
                        "slosh> ".to_string()
                    }
                }
            }
            _ => env.get_global(idx).display_value(env),
        }
    } else {
        "slosh> ".to_string()
    }
}

fn load_sloshrc() {
    if let Ok(mut rcfile) = env::var("HOME") {
        if rcfile.ends_with('/') {
            rcfile.push_str(".config/slosh");
        } else {
            rcfile.push_str("/.config/slosh");
        }

        ENV.with(|renv| {
            let mut env = renv.borrow_mut();
            let i_path = env.intern(&rcfile);
            let v = vec![Value::StringConst(i_path)];
            let path = env.alloc_vector(v);
            add_global_value(
                &mut env,
                "*load-path*",
                path,
                "Usage: (set '*load-path* '(\"/path/one\" \"/path/two\"))

Set the a list of paths to search for loading scripts with the load form.

Section: scripting

Example:
;(set '*load-path '(\"/path\"))
;(load \"script-in-path\")
t
",
            );
            let rcpath: PathBuf = rcfile.clone().into();
            if !rcpath.exists() {
                match create_dir_all(&rcpath) {
                    Ok(_) => {}
                    Err(e) => eprintln!(
                        "error creating default config directory {}: {e}",
                        rcpath.to_string_lossy()
                    ),
                }
            }
            rcfile.push_str("/init.slosh");
            let rcpath: PathBuf = rcfile.clone().into();
            if !rcpath.exists() {
                match std::fs::File::create(&rcpath) {
                    Ok(mut f) => match f.write_all(SLSHRC.as_bytes()) {
                        Ok(_) => {}
                        Err(e) => eprintln!(
                            "error writing default config {}: {e}",
                            rcpath.to_string_lossy()
                        ),
                    },
                    Err(e) => eprintln!(
                        "error creating default config {}: {e}",
                        rcpath.to_string_lossy()
                    ),
                }
            }
            let script = env.intern(&rcfile);
            let script = env.get_interned(script);
            match load_internal(&mut env, script) {
                Ok(_) => {}
                Err(err) => println!("ERROR: {err}"),
            }
        });
    }
}

fn history_file() -> String {
    let mut share_dir = if let Ok(mut home) = env::var("HOME") {
        if home.ends_with('/') {
            home.push_str(".local/share/slosh");
        } else {
            home.push_str("/.local/share/slosh");
        }
        home
    } else {
        "./.local/share/slosh".to_string()
    };
    if let Err(err) = create_dir_all(&share_dir) {
        eprintln!(
            "WARNING: Unable to create share directory: {}- {}",
            share_dir, err
        );
    }
    share_dir.push_str("/history");
    share_dir
}

fn get_color_closure() -> Option<ColorClosure> {
    ENV.with(move |renv| -> Option<ColorClosure> {
        let mut env = renv.borrow_mut();
        let handler_interned = env.intern("__line_handler");
        if let Some(idx) = env.global_intern_slot(handler_interned) {
            match env.get_global(idx) {
                Value::Lambda(h) => Some(Box::new(move |input: &str| -> String {
                    ENV.with(|renv| {
                        let mut env = renv.borrow_mut();
                        let line_handler = env.get_lambda(h);
                        let param = env.alloc_string(input.to_string());
                        env.heap_sticky(param);
                        let res = match env.do_call(line_handler, &[param], None) {
                            Ok(v) => match v {
                                Value::StringConst(i) => env.get_interned(i).to_string(),
                                Value::String(h) => env.get_string(h).to_string(),
                                _ => v.display_value(&env),
                            },
                            Err(e) => {
                                format!("ERROR {e}")
                            }
                        };
                        env.heap_unsticky(param);
                        res
                    })
                })),
                Value::Closure(h) => Some(Box::new(move |input: &str| -> String {
                    ENV.with(|renv| {
                        let mut env = renv.borrow_mut();
                        let (line_handler, tcaps) = env.get_closure(h);
                        let caps = Vec::from(tcaps);
                        let param = env.alloc_string(input.to_string());
                        env.heap_sticky(param);
                        let res = match env.do_call(line_handler, &[param], Some(&caps[..])) {
                            Ok(v) => match v {
                                Value::StringConst(i) => env.get_interned(i).to_string(),
                                Value::String(h) => env.get_string(h).to_string(),
                                _ => v.display_value(&env),
                            },
                            Err(e) => {
                                format!("ERROR {e}")
                            }
                        };
                        env.heap_unsticky(param);
                        res
                    })
                })),
                _ => None,
            }
        } else {
            None
        }
    })
}

fn set_builtins(env: &mut SloshVm) {
    add_shell_builtins(env);
    setup_collection_builtins(env);
    add_print_builtins(env);
    add_load_builtins(env);
    add_str_builtins(env);
    add_misc_builtins(env);
    add_io_builtins(env);
    add_conv_builtins(env);
    env.set_global_builtin("dump-regs", builtin_dump_regs);
    let uid = Sys::current_uid();
    let euid = Sys::effective_uid();
    env::set_var("UID", format!("{uid}"));
    env::set_var("EUID", format!("{euid}"));
    env.set_named_global("*uid*", uid.into());
    env.set_named_global("*euid*", euid.into());
    env.set_named_global("*last-status*", 0.into());
    env.set_named_global("*int-bits*", (INT_BITS as i64).into());
    env.set_named_global("*int-max*", INT_MAX.into());
    env.set_named_global("*int-min*", INT_MIN.into());
    // Initialize the HOST variable
    let host: OsString = Sys::gethostname().unwrap_or_else(|| "Operating system hostname is not a string capable of being parsed by native platform???".into());
    env::set_var("HOST", host);
    if let Ok(dir) = env::current_dir() {
        env::set_var("PWD", dir);
    }
}

fn main() {
    if let Some(config) = get_config() {
        ENV.with(|renv| {
            let mut env = renv.borrow_mut();
            set_builtins(&mut env);
        });
        if config.command.is_none() && config.script.is_none() {
            load_sloshrc();
            if Sys::is_tty(STDIN_FILENO) {
                let mut con = Context::new();
                //con.set_completer(Box::new(FilenameCompleter::new(Some("."))));
                con.set_completer(Box::new(ShellCompleter::new()));
                con.set_editor_rules(make_editor_rules());
                let mut vi = keymap::Vi::new();
                let vi_keywords = vec!["_", "-"];
                vi.set_keyword_rule(Box::new(AlphanumericAndVariableKeywordRule::new(
                    vi_keywords,
                )));
                /*if let Some((ch1, ch2, timeout)) = repl_settings.vi_esc_sequence {
                    vi.set_esc_sequence(ch1, ch2, timeout);
                }
                vi.set_normal_prompt_prefix(repl_settings.vi_normal_prompt_prefix.clone());
                vi.set_normal_prompt_suffix(repl_settings.vi_normal_prompt_suffix.clone());
                vi.set_insert_prompt_prefix(repl_settings.vi_insert_prompt_prefix.clone());
                vi.set_insert_prompt_suffix(repl_settings.vi_insert_prompt_suffix.clone());*/
                //Box::new(keymap::Emacs::new())
                con.set_keymap(Box::new(vi));

                if let Err(e) = con.history.set_file_name_and_load_history(&history_file()) {
                    println!("Error loading history: {e}");
                }
                shell::run::setup_shell_tty(STDIN_FILENO);
                SHELL_ENV.with(|jobs| {
                    jobs.borrow_mut().cap_term();
                });
                loop {
                    SHELL_ENV.with(|jobs| {
                        jobs.borrow_mut().reap_procs();
                    });
                    let prompt = ENV.with(|env| get_prompt(&mut env.borrow_mut()));
                    let res = match con.read_line(Prompt::from(prompt), get_color_closure()) {
                        Ok(input) => input,
                        Err(err) => match err.kind() {
                            ErrorKind::UnexpectedEof => {
                                break;
                            }
                            ErrorKind::Interrupted => {
                                continue;
                            }
                            _ => {
                                // Usually can just restore the tty and be back in action.
                                SHELL_ENV.with(|jobs| {
                                    jobs.borrow_mut().restore_terminal();
                                });
                                eprintln!("Error on input: {err}");
                                continue;
                            }
                        },
                    };

                    if res.is_empty() {
                        continue;
                    }

                    let res = if res.contains("\\\n") {
                        res.replace("\\\n", "")
                    } else {
                        res
                    };
                    con.history.push(&res).expect("Failed to push history.");
                    if res.starts_with('(') {
                        ENV.with(|env| {
                            exec_expression(res, &mut env.borrow_mut());
                        });
                    } else {
                        let status = SHELL_ENV.with(|jobs| {
                            match shell::run::run_one_command(&res, &mut jobs.borrow_mut()) {
                                Ok(status) => status,
                                Err(err) => {
                                    eprintln!("ERROR executing {res}: {err}");
                                    1
                                }
                            }
                        });
                        ENV.with(|env| {
                            env.borrow_mut()
                                .set_named_global("*last-status*", status.into());
                        })
                    }
                }
            } else {
                // No tty so just grab lines from stdin and try to use them....
                let mut res = String::new();
                let stdin = std::io::stdin();
                while let Ok(bytes) = stdin.lock().read_line(&mut res) {
                    SHELL_ENV.with(|jobs| {
                        jobs.borrow_mut().reap_procs();
                    });
                    if bytes == 0 {
                        break;
                    }
                    if res.is_empty() {
                        continue;
                    }
                    if res.starts_with('(') {
                        ENV.with(|env| {
                            exec_expression(res.clone(), &mut env.borrow_mut());
                        });
                    } else {
                        let status = SHELL_ENV.with(|jobs| {
                            match shell::run::run_one_command(&res, &mut jobs.borrow_mut()) {
                                Ok(status) => status,
                                Err(err) => {
                                    eprintln!("ERROR executing {res}: {err}");
                                    1
                                }
                            }
                        });
                        ENV.with(|env| {
                            env.borrow_mut()
                                .set_named_global("*last-status*", status.into());
                        })
                    }
                    res.clear();
                }
                SHELL_ENV.with(|jobs| {
                    jobs.borrow_mut().reap_procs();
                });
            }
        } else if let Some(mut command) = config.command {
            for a in &config.args {
                command.push(' ');
                command.push_str(a);
            }
            if Sys::is_tty(STDIN_FILENO) {
                shell::run::setup_shell_tty(STDIN_FILENO);
            }
            let status = SHELL_ENV.with(|jobs| {
                match shell::run::run_one_command(&command, &mut jobs.borrow_mut()) {
                    Ok(status) => status,
                    Err(err) => {
                        eprintln!("ERROR executing {command}: {err}");
                        1
                    }
                }
            });
            SHELL_ENV.with(|jobs| {
                jobs.borrow_mut().reap_procs();
            });
            std::process::exit(status);
        } else if let Some(script) = config.script {
            load_sloshrc();
            ENV.with(|renv| {
                let mut env = renv.borrow_mut();
                let script = env.intern(&script);
                let script = env.get_interned(script);
                match load_internal(&mut env, script) {
                    Ok(_) => {}
                    Err(err) => println!("ERROR: {err}"),
                }
            });
        }
    }
}

fn exec_expression(res: String, env: &mut SloshVm) {
    let reader = Reader::from_string(res, env, "", 1, 0);
    let exps: Result<Vec<Value>, ReadError> = reader.collect();
    match exps {
        Ok(exps) => {
            for exp in exps {
                let line_num = env.line_num();
                let mut state = CompileState::new_state(PROMPT_FN, line_num, None);
                if let Err(e) = pass1(env, &mut state, exp) {
                    eprintln!("Compile error (pass1), line {}: {}", env.line_num(), e);
                    return;
                }
                if let Err(e) = compile(env, &mut state, exp, 0) {
                    eprintln!("Compile error, line {}: {}", env.line_num(), e);
                    return;
                }
                if let Err(e) = state.chunk.encode0(RET, env.own_line()) {
                    eprintln!(
                        "Compile error (failed to add return...), line {}: {}",
                        env.line_num(),
                        e
                    );
                    return;
                }
                let chunk = Arc::new(state.chunk.clone());
                match env.execute(chunk.clone()) {
                    Ok(res) => {
                        if !res.is_nil() {
                            println!("{}", display_value(env, res));
                        }
                    }
                    Err(err) => {
                        eprintln!("ERROR: {}", err.display(env));
                        if let Some(err_frame) = env.err_frame() {
                            let line = err_frame.current_line().unwrap_or(0);
                            eprintln!(
                                "{} line: {} ip: {:#010x}",
                                err_frame.chunk.file_name,
                                line,
                                err_frame.current_offset()
                            );
                        }
                        debug(env);
                    }
                }
            }
        }
        Err(err) => println!("Reader error: {err}"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use compile_state::state::{new_slosh_vm, SloshVmTrait};
    use lazy_static::lazy_static;
    use regex::Regex;
    use std::error::Error;
    use std::fmt::{Debug, Display, Formatter};
    //use sl_compiler::test_utils::exec;

    lazy_static! {
        static ref DOC_REGEX: Regex =
            Regex::new(r"^Usage:(.*)\n\n.*^Section:(.*)\n\n^Example:(.*)").unwrap();
    }

    #[derive(Debug, Clone, Eq, Hash, PartialEq)]
    enum Namespace {
        Global,
        Other(String),
    }

    impl Namespace {
        fn add_docs(&self, docs: &mut Vec<SloshDoc>, vm: &mut SloshVm) -> DocResult<()> {
            let docstring_key = vm.intern_static("doc-string");
            match self {
                Namespace::Global => {
                    for g in vm.globals().keys() {
                        let sym = Value::Symbol(*g);
                        let sym_str = sym.display_value(&vm);
                        let slot = vm.global_intern_slot(*g).unwrap();
                        let prop = vm
                            .get_global_property(slot, docstring_key)
                            .map_or(None, |x| {
                                if let Value::String(h) = x {
                                    Some(vm.get_string(h).to_string())
                                } else {
                                    None
                                }
                            });
                        match prop {
                            None => {
                                //TODO PC put this back because it SHOULD have a doc string
                                //return Err(DocError::NoDocString { symbol: sym_str });
                            }
                            Some(raw_doc_string) => {
                                docs.push(SloshDoc::new(
                                    sym_str,
                                    sym.display_type(&vm).to_string(),
                                    self.clone(),
                                    raw_doc_string,
                                )?);
                            }
                        }
                    }
                }
                Namespace::Other(_) => {
                    unimplemented!("No other docs yet exist besides global!");
                }
            }
            Ok(())
        }
    }

    #[derive(Debug, Clone, Eq, Hash, PartialEq)]
    struct DocStringSection {
        usage: String,
        description: String,
        section: String,
        example: Option<String>,
    }

    impl Display for DocStringSection {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            let example = self.example.clone().unwrap_or_default();
            write!(
                f,
                "Usage: {usage}\n\n{description}\n\nSection: {section}\n\nExample: {example}",
                usage = self.usage,
                description = self.description,
                section = self.section,
                example = example,
            )
        }
    }

    struct SloshDoc {
        symbol: String,
        symbol_type: String,
        namespace: Namespace,
        doc_string: DocStringSection,
    }

    impl SloshDoc {
        fn new(
            name: String,
            symbol_type: String,
            namespace: Namespace,
            raw_doc_string: String,
        ) -> DocResult<SloshDoc> {
            let doc_string = SloshDoc::parse_doc_string(raw_doc_string)?;
            Ok(SloshDoc {
                symbol: name,
                symbol_type,
                namespace,
                doc_string,
            })
        }

        fn parse_doc_string(raw_doc_string: String) -> DocResult<DocStringSection> {
            let strs = DOC_REGEX.captures(raw_doc_string.as_str());
            let usage: String = "fixme".to_string();
            let description: String = "fixme".to_string();
            let section: String = "fixme".to_string();
            let example: Option<String> = None;
            match strs {
                None => {}
                Some(s) => {
                    println!("      line: {s:?}");
                }
            }
            Ok(DocStringSection {
                usage,
                description,
                section,
                example,
            })
        }
    }

    enum DocError {
        NoDocString { symbol: String },
        DocStringMissingSection { symbol: String, section: String },
        DocStringMustStartWithUsage { symbol: String },
    }

    impl Debug for DocError {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            Display::fmt(self, f)
        }
    }

    impl Display for DocError {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            let str = match self {
                DocError::NoDocString{ symbol} => {
                    format!(
                        "No documentation string provided for symbol {symbol}, all slosh functions written in Rust must have a valid documentation string."
                    )
                }
                DocError::DocStringMissingSection{ symbol, section} => {
                    format!("Invalid documentation string for symbol {symbol}, missing required section {section:?}")
                }
                DocError::DocStringMustStartWithUsage{ symbol } => {
                    format!(
                        "Invalid documentation string for symbol {symbol}, first line must start with \"Usage:\""
                    )
                }
            }
            .to_string();
            write!(f, "{}", str)
        }
    }

    impl Error for DocError {}

    type DocResult<T> = Result<T, DocError>;

    #[test]
    fn test_docs() {
        let mut env = new_slosh_vm();
        set_builtins(&mut env);

        let mut docs: Vec<SloshDoc> = vec![];
        Namespace::Global.add_docs(&mut docs, &mut env).unwrap();

        for doc in docs {
            eprintln!("ns: {:?}", doc.namespace);
            eprintln!("  sym: {}", doc.symbol);
            eprintln!("  type: {}", doc.symbol_type);
            eprintln!("      doc_string: {:?}", doc.doc_string);
        }
    }
}
