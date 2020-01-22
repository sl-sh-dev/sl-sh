use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::OpenOptions;
use std::hash::BuildHasher;
use std::io::{self, BufRead, BufReader, BufWriter, Read, Write};
use std::rc::Rc;

use crate::builtins_util::*;
use crate::environment::*;
use crate::eval::*;
use crate::reader::*;
use crate::types::*;

fn builtin_open(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args = list_to_args(environment, args, false)?;
    if args.is_empty() {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "open takes at least one form (a file name)",
        ))
    } else {
        let arg_len = args.len();
        let mut args = args.iter();
        let a = args.next().unwrap();
        if let Expression::Atom(Atom::Symbol(sym)) = &a {
            let ret = match &sym[..] {
                ":stdin" => Some(Expression::File(FileState::Stdin)),
                ":stdout" => Some(Expression::File(FileState::Stdout)),
                ":stderr" => Some(Expression::File(FileState::Stderr)),
                _ => None,
            };
            if let Some(ret) = ret {
                if arg_len > 1 {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "open: if first form is a symbol then other forms not valid",
                    ));
                }
                return Ok(ret);
            }
        }
        let file_name = match eval(environment, &a)? {
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
                    return Ok(Expression::Atom(Atom::Nil));
                } else {
                    return Err(err);
                }
            }
        };
        if !is_write {
            Ok(Expression::File(FileState::Read(Rc::new(RefCell::new(
                BufReader::new(file),
            )))))
        } else {
            Ok(Expression::File(FileState::Write(Rc::new(RefCell::new(
                BufWriter::new(file),
            )))))
        }
    }
}

fn builtin_close(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let mut args = list_to_args(environment, args, true)?;
    if args.len() != 1 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "close takes one form (file to close)",
        ))
    } else {
        let exp = &args[0];
        if let Expression::File(FileState::Write(f)) = exp {
            // Flush in case there are more then one references to this file, at least the data is flushed.
            f.borrow_mut().get_ref().flush()?;
        }
        if let Expression::File(_) = exp {
            let mut closed = Expression::File(FileState::Closed);
            std::mem::swap(&mut args[0], &mut closed);
            return Ok(Expression::Atom(Atom::True));
        }
        Ok(Expression::Atom(Atom::Nil))
    }
}

fn builtin_flush(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() != 1 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "flush takes one form (file to flush)",
        ))
    } else {
        let exp = &args[0];
        if let Expression::File(FileState::Write(f)) = exp {
            f.borrow_mut().get_ref().flush()?;
            return Ok(Expression::Atom(Atom::True));
        }
        Ok(Expression::Atom(Atom::Nil))
    }
}

fn builtin_read_line(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() != 1 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "read-line takes one form (file)",
        ))
    } else {
        let exp = &args[0];
        if let Expression::File(FileState::Read(file)) = &exp {
            let mut line = String::new();
            if 0 == file.borrow_mut().read_line(&mut line)? {
                Ok(Expression::Atom(Atom::Nil))
            } else {
                Ok(Expression::Atom(Atom::String(line)))
            }
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "read-line requires a file opened for reading",
            ))
        }
    }
}

fn builtin_read(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
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
        if let Expression::File(FileState::Read(file)) = &exp {
            let mut fstr = String::new();
            file.borrow_mut().read_to_string(&mut fstr)?;
            match read(&fstr, add_parens) {
                Ok(ast) => Ok(ast),
                Err(err) => Err(io::Error::new(io::ErrorKind::Other, err.reason)),
            }
        } else if let Expression::Atom(Atom::String(string)) = &exp {
            match read(&string, add_parens) {
                Ok(ast) => Ok(ast),
                Err(err) => Err(io::Error::new(io::ErrorKind::Other, err.reason)),
            }
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "read: requires a file opened for reading or string",
            ))
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
    args: &[Expression],
) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() != 2 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "write-line takes two forms (file and line)",
        ))
    } else {
        let exp = &args[0];
        if let Expression::File(FileState::Write(file)) = &exp {
            writeln!(
                &mut file.borrow_mut(),
                "{}",
                &args[1].as_string(environment)?
            )?;
            Ok(Expression::Atom(Atom::Nil))
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "write-line requires a file opened for writing",
            ))
        }
    }
}

fn builtin_write_string(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() != 2 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "write-string takes two forms (file and string)",
        ))
    } else {
        let exp = &args[0];
        if let Expression::File(FileState::Write(file)) = &exp {
            write!(
                &mut file.borrow_mut(),
                "{}",
                &args[1].as_string(environment)?
            )?;
            Ok(Expression::Atom(Atom::Nil))
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "write-string requires a file opened for writing",
            ))
        }
    }
}

pub fn add_io_builtins<S: BuildHasher>(data: &mut HashMap<String, Rc<Expression>, S>) {
    data.insert("open".to_string(), Rc::new(Expression::Func(builtin_open)));
    data.insert(
        "close".to_string(),
        Rc::new(Expression::Func(builtin_close)),
    );
    data.insert(
        "flush".to_string(),
        Rc::new(Expression::Func(builtin_flush)),
    );
    data.insert(
        "read-line".to_string(),
        Rc::new(Expression::Func(builtin_read_line)),
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
        Rc::new(Expression::Func(builtin_write_line)),
    );
    data.insert(
        "write-string".to_string(),
        Rc::new(Expression::Func(builtin_write_string)),
    );
}
