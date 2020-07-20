use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::env;
use std::fs::OpenOptions;
use std::hash::BuildHasher;
use std::io::{self, BufRead, BufReader, BufWriter, Read, Write};
use std::os::unix::io::AsRawFd;
use std::rc::Rc;

use liner::{Context, Prompt};
use unicode_segmentation::UnicodeSegmentation;
extern crate unicode_reader;
use unicode_reader::Graphemes;

use crate::builtins_util::expand_tilde;
use crate::environment::*;
use crate::eval::*;
use crate::interner::*;
use crate::reader::*;
use crate::shell::apply_repl_settings;
use crate::shell::get_liner_words;
use crate::types::*;

fn builtin_open(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(a) = args.next() {
        let a = eval(environment, a)?;
        if let ExpEnum::Atom(Atom::Symbol(sym)) = &a.get().data {
            let ret = match &sym[..] {
                ":stdin" => Some(ExpEnum::File(Rc::new(RefCell::new(FileState::Stdin)))),
                ":stdout" => Some(ExpEnum::File(Rc::new(RefCell::new(FileState::Stdout)))),
                ":stderr" => Some(ExpEnum::File(Rc::new(RefCell::new(FileState::Stderr)))),
                _ => None,
            };
            if let Some(ret) = ret {
                if args.next().is_some() {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "open: if first form is a symbol then other forms not valid",
                    ));
                }
                return Ok(Expression::alloc_data(ret));
            }
        }
        let file_name = match &a.get().data {
            ExpEnum::Atom(Atom::String(name, _)) => name.to_string(),
            _ => {
                return Err(io::Error::new(
                io::ErrorKind::Other,
                "open: first form must evaluate to a string (filename) or :stdin, :stdout, :stderr",
            ));
            }
        };
        let file_name = match expand_tilde(&file_name) {
            Some(p) => p,
            None => file_name,
        };
        let mut opts = OpenOptions::new();
        let mut is_read = false;
        let mut is_write = false;
        let mut error_nil = false;
        for a in args {
            let a = eval(environment, a)?;
            let a_d = a.get();
            if let ExpEnum::Atom(Atom::Symbol(sym)) = &a_d.data {
                match &sym[..] {
                    ":read" => {
                        is_read = true;
                        opts.read(true);
                    }
                    ":write" => {
                        is_write = true;
                        opts.write(true);
                    }
                    ":append" => {
                        is_write = true;
                        opts.append(true);
                    }
                    ":truncate" => {
                        is_write = true;
                        opts.write(true);
                        opts.truncate(true);
                    }
                    ":create" => {
                        is_write = true;
                        opts.write(true);
                        opts.create(true);
                    }
                    ":create-new" => {
                        is_write = true;
                        opts.write(true);
                        opts.create_new(true);
                    }
                    ":on-error-nil" => {
                        error_nil = true;
                    }
                    _ => {
                        let msg = format!("open: invalid directive, {}", sym);
                        return Err(io::Error::new(io::ErrorKind::Other, msg));
                    }
                };
            } else {
                let msg = format!("open: {} invalid", a);
                return Err(io::Error::new(io::ErrorKind::Other, msg));
            }
        }
        if is_read && is_write {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "open: only open file for read or write not both",
            ));
        }
        if !is_write {
            opts.read(true);
        }
        let file = match opts.open(file_name) {
            Ok(file) => file,
            Err(err) => {
                if error_nil {
                    return Ok(Expression::make_nil());
                } else {
                    return Err(err);
                }
            }
        };
        return if !is_write {
            let fd: i64 = file.as_raw_fd() as i64;
            let file_iter: CharIter = Box::new(
                Graphemes::from(BufReader::new(file))
                    .map(|s| {
                        if let Ok(s) = s {
                            Cow::Owned(s)
                        } else {
                            Cow::Borrowed("")
                        }
                    })
                    .peekable(),
            );
            Ok(Expression::alloc_data(ExpEnum::File(Rc::new(
                RefCell::new(FileState::Read(Some(file_iter), fd)),
            ))))
        } else {
            Ok(Expression::alloc_data(ExpEnum::File(Rc::new(
                RefCell::new(FileState::Write(RefCell::new(BufWriter::new(file)))),
            ))))
        };
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "open takes at least one form (a file name)",
    ))
}

fn builtin_close(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(exp) = args.next() {
        if args.next().is_none() {
            let exp = eval(environment, exp)?;
            return if let ExpEnum::File(file) = &exp.get().data {
                let closed = FileState::Closed;
                file.replace(closed);
                Ok(Expression::make_true())
            } else {
                Err(io::Error::new(
                    io::ErrorKind::Other,
                    "close requires a file",
                ))
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "close takes one form (file to close)",
    ))
}

fn builtin_flush(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(exp) = args.next() {
        if args.next().is_none() {
            let exp = eval(environment, exp)?;
            return if let ExpEnum::File(file) = &exp.get().data {
                match &mut *file.borrow_mut() {
                    FileState::Write(f) => {
                        f.borrow_mut().flush()?;
                        Ok(Expression::make_true())
                    }
                    FileState::Stdout => {
                        io::stdout().flush()?;
                        Ok(Expression::make_true())
                    }
                    _ => Err(io::Error::new(
                        io::ErrorKind::Other,
                        "flush requires a writeable file",
                    )),
                }
            } else {
                Err(io::Error::new(
                    io::ErrorKind::Other,
                    "flush requires a file",
                ))
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "flush takes one form (file to flush)",
    ))
}

fn builtin_read_line(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(exp) = args.next() {
        if args.next().is_none() {
            let exp = eval(environment, exp)?;
            return if let ExpEnum::File(file) = &exp.get().data {
                match &mut *file.borrow_mut() {
                    FileState::Read(f_iter, _) => {
                        let mut line = String::new();
                        if let Some(f_iter) = f_iter {
                            let mut out_ch = f_iter.next();
                            if out_ch.is_none() {
                                return Ok(Expression::make_nil());
                            }
                            while let Some(ch) = out_ch {
                                line.push_str(&ch);
                                if ch == "\n" {
                                    break;
                                }
                                out_ch = f_iter.next();
                            }
                        } else {
                            return Ok(Expression::make_nil());
                        }
                        Ok(Expression::alloc_data(ExpEnum::Atom(Atom::String(
                            line.into(),
                            None,
                        ))))
                    }
                    FileState::ReadBinary(f) => {
                        let mut line = String::new();
                        if 0 == f.read_line(&mut line)? {
                            Ok(Expression::make_nil())
                        } else {
                            Ok(Expression::alloc_data(ExpEnum::Atom(Atom::String(
                                line.into(),
                                None,
                            ))))
                        }
                    }
                    FileState::Stdin => {
                        let mut line = String::new();
                        if 0 == io::stdin().read_line(&mut line)? {
                            Ok(Expression::make_nil())
                        } else {
                            Ok(Expression::alloc_data(ExpEnum::Atom(Atom::String(
                                line.into(),
                                None,
                            ))))
                        }
                    }
                    _ => Err(io::Error::new(
                        io::ErrorKind::Other,
                        "read-line requires a file opened for reading",
                    )),
                }
            } else {
                Err(io::Error::new(
                    io::ErrorKind::Other,
                    "read-line takes a file",
                ))
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "read-line takes one form (file)",
    ))
}

pub fn read_prompt(
    environment: &mut Environment,
    prompt: &str,
    history: Option<&str>,
) -> io::Result<String> {
    let mut con = Context::new();
    apply_repl_settings(&mut con, &environment.repl_settings);
    con.set_word_divider(Box::new(get_liner_words));
    let mut home = match env::var("HOME") {
        Ok(val) => val,
        Err(_) => ".".to_string(),
    };
    if home.ends_with('/') {
        home = home[..home.len() - 1].to_string();
    }
    if let Some(history) = history {
        let history_file = if history.starts_with('/') || history.starts_with('.') {
            history.to_string()
        } else {
            format!("{}/.local/share/sl-sh/{}", home, history)
        };
        if let Err(err) = con.history.set_file_name_and_load_history(history_file) {
            eprintln!("WARNING: Unable to load read-line history: {}", err);
        }
    }
    //con.set_completer(Box::new(ShellCompleter::new(environment.clone())));
    match con.read_line(Prompt::from(prompt), None) {
        Ok(input) => {
            let input = input.trim();
            if history.is_some() {
                if let Err(err) = con.history.push(input) {
                    eprintln!("read-line: Error saving history: {}", err);
                }
            }
            Ok(input.into())
        }
        Err(err) => Err(io::Error::new(io::ErrorKind::Other, err)),
    }
}

fn builtin_read_prompt(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(exp) = args.next() {
        let h_str;
        let history_file = if let Some(h) = args.next() {
            let hist = eval(environment, h)?;
            let hist_d = hist.get();
            if let ExpEnum::Atom(Atom::String(s, _)) = &hist_d.data {
                h_str = s.to_string();
                Some(&h_str[..])
            } else {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "read-prompt: history file (if provided) must be a string.",
                ));
            }
        } else {
            None
        };
        if args.next().is_none() {
            let prompt = eval(environment, exp)?;
            let prompt_d = prompt.get();
            if let ExpEnum::Atom(Atom::String(s, _)) = &prompt_d.data {
                let input = read_prompt(environment, s, history_file)?;
                return Ok(Expression::alloc_data(ExpEnum::Atom(Atom::String(
                    input.into(),
                    None,
                ))));
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "read-prompt: requires a prompt string and option history file.",
    ))
}

fn builtin_read(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    fn read_stdin(environment: &mut Environment) -> io::Result<Expression> {
        let input = read_prompt(environment, "read> ", Some("read_history"))?;
        let input = unsafe { &*(input.as_ref() as *const str) };
        let chars = Box::new(
            UnicodeSegmentation::graphemes(&input[..], true)
                .map(|s| Cow::Borrowed(s))
                .peekable(),
        );
        match read_form(environment, chars) {
            Ok((ast, _)) => Ok(ast),
            Err((err, _)) => Err(io::Error::new(io::ErrorKind::Other, err.reason)),
        }
    }
    if let Some(exp) = args.next() {
        if args.next().is_none() {
            let exp = eval(environment, exp)?;
            let mut exp_d = exp.get_mut();
            return match &mut exp_d.data {
                ExpEnum::File(file) => match &mut *file.borrow_mut() {
                    FileState::Read(file_iter, _) => {
                        let iiter = file_iter.take().unwrap();
                        match read_form(environment, iiter) {
                            Ok((ast, i_iter)) => {
                                file_iter.replace(i_iter);
                                Ok(ast)
                            }
                            Err((err, i_iter)) => {
                                file_iter.replace(i_iter);
                                return Err(io::Error::new(io::ErrorKind::Other, err.reason));
                            }
                        }
                    }
                    FileState::Stdin => read_stdin(environment),
                    _ => Err(io::Error::new(
                        io::ErrorKind::Other,
                        "read: requires a character file opened for reading or string",
                    )),
                },
                ExpEnum::Atom(Atom::String(input, char_iter)) => {
                    if char_iter.is_none() {
                        // This unsafe should be fine as long as the iterator is invalidated (set to None)
                        // on ANY change to string.  See builtin_str_iter_start.
                        let nstr = unsafe { &*(input.as_ref() as *const str) };
                        *char_iter = Some(Box::new(
                            UnicodeSegmentation::graphemes(nstr, true)
                                .map(|s| Cow::Borrowed(s))
                                .peekable(),
                        ));
                    }
                    if char_iter.is_some() {
                        let chars = char_iter.take().unwrap();
                        match read_form(environment, chars) {
                            Ok((ast, ichars)) => {
                                char_iter.replace(ichars);
                                Ok(ast)
                            }
                            Err((err, _)) => {
                                return Err(io::Error::new(io::ErrorKind::Other, err.reason));
                            }
                        }
                    } else {
                        panic!("read: WTF, no char iter but just made one!");
                    }
                }
                _ => Err(io::Error::new(
                    io::ErrorKind::Other,
                    "read: requires a character file opened for reading or string",
                )),
            };
        }
    }
    // No args, ask for input
    read_stdin(environment)
}

fn builtin_read_all(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    fn do_read(environment: &mut Environment, input: &str) -> io::Result<Expression> {
        match read(environment, &input, None, false) {
            Ok(ast) => Ok(ast),
            Err(err) => Err(io::Error::new(io::ErrorKind::Other, err.reason)),
        }
    }
    if let Some(exp) = args.next() {
        if args.next().is_none() {
            let exp = eval(environment, exp)?;
            let mut exp_d = exp.get_mut();
            return match &mut exp_d.data {
                ExpEnum::File(file) => match &mut *file.borrow_mut() {
                    FileState::Read(file_iter, _) => {
                        if let Some(file_iter) = file_iter {
                            let input: String = file_iter.collect();
                            do_read(environment, &input)
                        } else {
                            Err(io::Error::new(
                                io::ErrorKind::Other,
                                "read-all: invalid read character iterator!",
                            ))
                        }
                    }
                    FileState::ReadBinary(file) => {
                        let mut input = String::new();
                        file.read_to_string(&mut input)?;
                        do_read(environment, &input)
                    }
                    FileState::Stdin => {
                        let input = read_prompt(environment, "read-all> ", Some("read_history"))?;
                        do_read(environment, &input)
                    }
                    _ => Err(io::Error::new(
                        io::ErrorKind::Other,
                        "read-all: requires a file opened for reading or string",
                    )),
                },
                ExpEnum::Atom(Atom::String(input, _char_iter)) => do_read(environment, input),
                _ => Err(io::Error::new(
                    io::ErrorKind::Other,
                    "read-all: requires a file opened for reading or string",
                )),
            };
        }
    }
    // No args, ask for input
    let input = read_prompt(environment, "read-all> ", Some("read_history"))?;
    do_read(environment, &input)
}

fn builtin_write_line(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(file) = args.next() {
        if let Some(line) = args.next() {
            if args.next().is_none() {
                let file = eval(environment, file)?;
                let line = eval(environment, line)?;
                return if let ExpEnum::File(file) = &file.get().data {
                    match &*file.borrow() {
                        FileState::Write(file) => {
                            writeln!(file.borrow_mut(), "{}", line.as_string(environment)?)?;
                            Ok(Expression::make_nil())
                        }
                        FileState::Stdout => {
                            println!("{}", line.as_string(environment)?);
                            Ok(Expression::make_nil())
                        }
                        FileState::Stderr => {
                            eprintln!("{}", line.as_string(environment)?);
                            Ok(Expression::make_nil())
                        }
                        _ => Err(io::Error::new(
                            io::ErrorKind::Other,
                            "write-line requires a file opened for writing",
                        )),
                    }
                } else {
                    Err(io::Error::new(
                        io::ErrorKind::Other,
                        "write-line requires a file opened for writing",
                    ))
                };
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "write-line takes two forms (file and line)",
    ))
}

fn builtin_write_string(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(file) = args.next() {
        if let Some(string) = args.next() {
            if args.next().is_none() {
                let file = eval(environment, file)?;
                let string = eval(environment, string)?;
                return if let ExpEnum::File(file) = &file.get().data {
                    match &*file.borrow() {
                        FileState::Write(file) => {
                            write!(file.borrow_mut(), "{}", string.as_string(environment)?)?;
                            Ok(Expression::make_nil())
                        }
                        FileState::Stdout => {
                            print!("{}", string.as_string(environment)?);
                            Ok(Expression::make_nil())
                        }
                        _ => Err(io::Error::new(
                            io::ErrorKind::Other,
                            "write-string requires a file opened for writing",
                        )),
                    }
                } else {
                    Err(io::Error::new(
                        io::ErrorKind::Other,
                        "write-string requires a file opened for writing",
                    ))
                };
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "write-string takes two forms (file and string)",
    ))
}

pub fn add_io_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, Reference, S>,
) {
    let root = interner.intern("root");
    data.insert(
        interner.intern("open"),
        Expression::make_function(
            builtin_open,
            "Usage: (open filename option*)

Open a file.

Options are:
    :read
    :write
    :append
    :truncate
    :create
    :create-new
    :on-error-nil

Section: file

Example:
(def 'test-open-f (open \"/tmp/slsh-tst-open.txt\" :create :truncate))
(write-line test-open-f \"Test Line One\")
(close test-open-f)
(test::assert-equal \"Test Line One\n\" (read-line (open \"/tmp/slsh-tst-open.txt\")))
",
            root,
        ),
    );
    data.insert(
        interner.intern("close"),
        Expression::make_function(
            builtin_close,
            "Usage: (close file)

Close a file.

Section: file

Example:
(def 'tst-file (open \"/tmp/slsh-tst-open.txt\" :create :truncate))
(write-line tst-file \"Test Line Two\")
(close tst-file)
(def 'tst-file (open \"/tmp/slsh-tst-open.txt\" :read))
(test::assert-equal \"Test Line Two\n\" (read-line tst-file))
(close tst-file)
",
            root,
        ),
    );
    data.insert(
        interner.intern("flush"),
        Expression::make_function(
            builtin_flush,
            "Usage: (flush file)

Flush a file.

Section: file

Example:
(def 'tst-file (open \"/tmp/slsh-tst-open.txt\" :create :truncate))
(write-line tst-file \"Test Line Three\")
(flush tst-file)
(def 'tst-file (open \"/tmp/slsh-tst-open.txt\" :read))
(test::assert-equal \"Test Line Three\n\" (read-line tst-file))
(close tst-file)
",
            root,
        ),
    );
    data.insert(
        interner.intern("read-line"),
        Expression::make_function(
            builtin_read_line,
            "Usage: (read-line file) -> string

Read a line from a file.

Section: file

Example:
(def 'tst-file (open \"/tmp/slsh-tst-open.txt\" :create :truncate))
(write-line tst-file \"Test Line Read Line One\")
(write-string tst-file \"Test Line Read Line Two\")
(flush tst-file)
(def 'tst-file (open \"/tmp/slsh-tst-open.txt\" :read))
(test::assert-equal \"Test Line Read Line One\n\" (read-line tst-file))
(test::assert-equal \"Test Line Read Line Two\" (read-line tst-file))
(close tst-file)
",
            root,
        ),
    );
    data.insert(
        interner.intern("read-prompt"),
        Expression::make_function(
            builtin_read_prompt,
            "Usage: (read-prompt string) -> string

Starts an interactive prompt (like the repl prompt) with the supplied prompt and
returns the input string.

Section: file

Example:
;(def 'input-string (read-prompt \"prompt> \"))
t
",
            root,
        ),
    );
    data.insert(
        interner.intern("read"),
        Expression::make_function(
            builtin_read,
            "Usage: (read file|string) -> list

Read a file or string and return the list representation.

Unlike most lisp readers this one will put loose symbols in a list (i.e. you
enter things at the repl without the enclosing parens).

Section: file

Example:
(def 'tst-file (open \"/tmp/slsh-tst-open.txt\" :create :truncate))
(write-line tst-file \"(1 2 3)(x y z)\")
;(write-string tst-file \"Test Line Read Line Two\")
(flush tst-file)
(def 'tst-file (open \"/tmp/slsh-tst-open.txt\" :read))
(test::assert-equal '(1 2 3) (read tst-file))
(test::assert-equal '(x y z) (read tst-file))
(close tst-file)
(test::assert-equal '(4 5 6) (read \"(4 5 6)\"))
(def 'test-str \"7 8 9\")
(test::assert-equal 7 (read test-str))
(test::assert-equal 8 (read test-str))
(test::assert-equal 9 (read test-str))
(test::assert-equal '(x y z) (read \"(x y z)\"))
",
            root,
        ),
    );
    data.insert(
        interner.intern("read-all"),
        Expression::make_function(
            builtin_read_all,
            "Usage: (read-all file|string) -> list

Read a file or string and return the list representation.  This reads the entire
file or string and will wrap in an outer vector if more then one form.

Unlike most lisp readers this one will put loose symbols in a list (i.e. you
enter things at the repl without the enclosing parens).

Section: file

Example:
(def 'tst-file (open \"/tmp/slsh-tst-open.txt\" :create :truncate))
(write-line tst-file \"(1 2 3)(x y z)\")
(flush tst-file)
(def 'tst-file (open \"/tmp/slsh-tst-open.txt\" :read))
(test::assert-equal '#((1 2 3)(x y z)) (read-all tst-file))
(close tst-file)
(test::assert-equal '(4 5 6) (read-all \"(4 5 6)\"))
(test::assert-equal '(7 8 9) (read-all \"7 8 9\"))
(test::assert-equal '(x y z) (read-all \"(x y z)\"))
",
            root,
        ),
    );
    data.insert(
        interner.intern("write-line"),
        Expression::make_function(
            builtin_write_line,
            "Usage: (write-line file string)

Write a line to a file.

Section: file

Example:
(def 'tst-file (open \"/tmp/slsh-tst-open.txt\" :create :truncate))
(write-line tst-file \"Test Line Write Line\")
(flush tst-file)
(def 'tst-file (open \"/tmp/slsh-tst-open.txt\" :read))
(test::assert-equal \"Test Line Write Line\n\" (read-line tst-file))
(close tst-file)
",
            root,
        ),
    );
    data.insert(
        interner.intern("write-string"),
        Expression::make_function(
            builtin_write_string,
            "Usage: (write-string file string)

Write a string to a file.

Section: file

Example:
(def 'tst-file (open \"/tmp/slsh-tst-open.txt\" :create :truncate))
(write-string tst-file \"Test Line Write String\")
(flush tst-file)
(def 'tst-file (open \"/tmp/slsh-tst-open.txt\" :read))
(test::assert-equal \"Test Line Write String\" (read-line tst-file))
(close tst-file)
",
            root,
        ),
    );
}
