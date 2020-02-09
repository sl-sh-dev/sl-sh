use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::OpenOptions;
use std::hash::BuildHasher;
use std::io::{self, BufRead, BufReader, BufWriter, Read, Write};
use std::rc::Rc;

use crate::environment::*;
use crate::eval::*;
use crate::reader::*;
use crate::types::*;

fn builtin_open(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(a) = args.next() {
        let a = eval(environment, a)?;
        if let Expression::Atom(Atom::Symbol(sym)) = &a {
            let ret = match &sym[..] {
                ":stdin" => Some(Expression::File(Rc::new(RefCell::new(FileState::Stdin)))),
                ":stdout" => Some(Expression::File(Rc::new(RefCell::new(FileState::Stdout)))),
                ":stderr" => Some(Expression::File(Rc::new(RefCell::new(FileState::Stderr)))),
                _ => None,
            };
            if let Some(ret) = ret {
                if args.next().is_some() {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "open: if first form is a symbol then other forms not valid",
                    ));
                }
                return Ok(ret);
            }
        }
        let file_name = match &a {
            Expression::Atom(Atom::String(name)) => name,
            _ => {
                return Err(io::Error::new(
                io::ErrorKind::Other,
                "open: first form must evaluate to a string (filename) or :stdin, :stdout, :stderr",
            ));
            }
        };
        let mut opts = OpenOptions::new();
        let mut is_read = false;
        let mut is_write = false;
        let mut error_nil = false;
        for a in args {
            let a = eval(environment, a)?;
            if let Expression::Atom(Atom::Symbol(sym)) = a {
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
                    return Ok(Expression::nil());
                } else {
                    return Err(err);
                }
            }
        };
        return if !is_write {
            Ok(Expression::File(Rc::new(RefCell::new(FileState::Read(
                RefCell::new(BufReader::new(file)),
            )))))
        } else {
            Ok(Expression::File(Rc::new(RefCell::new(FileState::Write(
                RefCell::new(BufWriter::new(file)),
            )))))
        };
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "open takes at least one form (a file name)",
    ))
}

fn builtin_close(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(exp) = args.next() {
        if args.next().is_none() {
            let exp = eval(environment, exp)?;
            return if let Expression::File(file) = exp {
                let closed = FileState::Closed;
                file.replace(closed);
                Ok(Expression::Atom(Atom::True))
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
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(exp) = args.next() {
        if args.next().is_none() {
            let exp = eval(environment, exp)?;
            return if let Expression::File(file) = &exp {
                match &mut *file.borrow_mut() {
                    FileState::Write(f) => {
                        f.borrow_mut().flush()?;
                        Ok(Expression::Atom(Atom::True))
                    }
                    FileState::Stdout => {
                        io::stdout().flush()?;
                        Ok(Expression::Atom(Atom::True))
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
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(exp) = args.next() {
        if args.next().is_none() {
            let exp = eval(environment, exp)?;
            return if let Expression::File(file) = &exp {
                match &*file.borrow() {
                    FileState::Read(f) => {
                        let mut line = String::new();
                        if 0 == f.borrow_mut().read_line(&mut line)? {
                            Ok(Expression::nil())
                        } else {
                            Ok(Expression::Atom(Atom::String(line)))
                        }
                    }
                    FileState::Stdin => {
                        let mut line = String::new();
                        if 0 == io::stdin().read_line(&mut line)? {
                            Ok(Expression::nil())
                        } else {
                            Ok(Expression::Atom(Atom::String(line)))
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

fn builtin_read(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    fn do_read(input: &str, mut add_parens: bool) -> io::Result<Expression> {
        add_parens = if add_parens {
            !(input.starts_with('(')
                || input.starts_with('\'')
                || input.starts_with('`')
                || input.starts_with('#'))
        } else {
            false
        };
        match read(&input, add_parens) {
            Ok(ast) => Ok(ast),
            Err(err) => Err(io::Error::new(io::ErrorKind::Other, err.reason)),
        }
    }
    let mut add_parens = false;
    let mut exp = None;
    for (i, arg) in args.enumerate() {
        if i > 1 {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "read: too many argument, file or string and optional :add-parens",
            ));
        }
        match arg {
            Expression::Atom(Atom::Symbol(s)) if s == ":add-parens" => add_parens = true,
            _ if exp.is_none() => exp = Some(arg),
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "read: too many argument, file or string and optional :add-parens",
                ))
            }
        }
    }
    if let Some(exp) = exp {
        let exp = eval(environment, exp)?;
        match &exp {
            Expression::File(file) => match &*file.borrow() {
                FileState::Read(file) => {
                    let mut input = String::new();
                    file.borrow_mut().read_to_string(&mut input)?;
                    do_read(&input, add_parens)
                }
                FileState::Stdin => {
                    let mut input = String::new();
                    io::stdin().read_to_string(&mut input)?;
                    do_read(&input, add_parens)
                }
                _ => Err(io::Error::new(
                    io::ErrorKind::Other,
                    "read: requires a file opened for reading or string",
                )),
            },
            Expression::Atom(Atom::String(input)) => do_read(input, add_parens),
            _ => Err(io::Error::new(
                io::ErrorKind::Other,
                "read: requires a file opened for reading or string",
            )),
        }
    } else {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "read: takes a file or string and optional :add-parens",
        ))
    }
}

fn builtin_write_line(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(file) = args.next() {
        if let Some(line) = args.next() {
            if args.next().is_none() {
                let file = eval(environment, file)?;
                let line = eval(environment, line)?;
                return if let Expression::File(file) = &file {
                    match &*file.borrow() {
                        FileState::Write(file) => {
                            writeln!(file.borrow_mut(), "{}", line.as_string(environment)?)?;
                            Ok(Expression::nil())
                        }
                        FileState::Stdout => {
                            println!("{}", line.as_string(environment)?);
                            Ok(Expression::nil())
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
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(file) = args.next() {
        if let Some(string) = args.next() {
            if args.next().is_none() {
                let file = eval(environment, file)?;
                let string = eval(environment, string)?;
                return if let Expression::File(file) = file {
                    match &*file.borrow() {
                        FileState::Write(file) => {
                            write!(file.borrow_mut(), "{}", string.as_string(environment)?)?;
                            Ok(Expression::nil())
                        }
                        FileState::Stdout => {
                            print!("{}", string.as_string(environment)?);
                            Ok(Expression::nil())
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

pub fn add_io_builtins<S: BuildHasher>(data: &mut HashMap<String, Rc<Reference>, S>) {
    data.insert(
        "open".to_string(),
        Rc::new(Expression::make_function(builtin_open, "Open a file.")),
    );
    data.insert(
        "close".to_string(),
        Rc::new(Expression::make_function(builtin_close, "Close a file.")),
    );
    data.insert(
        "flush".to_string(),
        Rc::new(Expression::make_function(builtin_flush, "Flush a file.")),
    );
    data.insert(
        "read-line".to_string(),
        Rc::new(Expression::make_function(
            builtin_read_line,
            "Read a line from a file.",
        )),
    );
    data.insert(
        "read".to_string(),
        Rc::new(Expression::make_function(
            builtin_read,
            "Read a file or string and return the list representation.",
        )),
    );
    data.insert(
        "write-line".to_string(),
        Rc::new(Expression::make_function(
            builtin_write_line,
            "Write a line to a file.",
        )),
    );
    data.insert(
        "write-string".to_string(),
        Rc::new(Expression::make_function(
            builtin_write_string,
            "Write a string to a file.",
        )),
    );
}
