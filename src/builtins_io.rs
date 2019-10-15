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

fn builtin_open(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.is_empty() {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "open takes at least one form (a file name)",
        ))
    } else {
        let mut args = args.iter();
        let file_name = if let Expression::Atom(Atom::String(name)) =
            eval(environment, &args.next().unwrap())?
        {
            name.clone()
        } else {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "open: first form must evaluate to a string (filename)",
            ));
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

fn is_open_file(exp: &Option<Rc<Expression>>) -> bool {
    if let Some(fexp) = exp {
        match &**fexp {
            Expression::File(FileState::Read(_)) => true,
            Expression::File(FileState::Write(_)) => true,
            _ => false,
        }
    } else {
        false
    }
}

fn builtin_close(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 1 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "close takes one form (file to close)",
        ))
    } else {
        if let Expression::Atom(Atom::Symbol(sym)) = &args[0] {
            let exp = get_expression(environment, &sym[..]);
            if is_open_file(&exp) {
                if let Some(fexp) = exp {
                    if let Expression::File(FileState::Write(f)) = &*fexp {
                        // Flush in case there are more then one references to this file, at least the data is flushed.
                        f.borrow_mut().get_ref().flush()?;
                    }
                }
                let exp_closed = Rc::new(Expression::File(FileState::Closed));
                set_expression_current(environment, sym.clone(), exp_closed);
                return Ok(Expression::Atom(Atom::True));
            }
        }
        Ok(Expression::Atom(Atom::Nil))
    }
}

fn builtin_flush(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 1 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "flush takes one form (file to flush)",
        ))
    } else {
        if let Expression::Atom(Atom::Symbol(sym)) = &args[0] {
            let exp = get_expression(environment, &sym[..]);
            if is_open_file(&exp) {
                if let Some(fexp) = exp {
                    if let Expression::File(FileState::Write(f)) = &*fexp {
                        f.borrow_mut().get_ref().flush()?;
                        return Ok(Expression::Atom(Atom::True));
                    }
                }
            }
        }
        Ok(Expression::Atom(Atom::Nil))
    }
}

fn builtin_read_line(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 1 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "read-line takes one form (file)",
        ))
    } else {
        let exp = eval(environment, &args[0])?;
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

fn builtin_read(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 1 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "read takes one form (file)",
        ))
    } else {
        let exp = eval(environment, &args[0])?;
        if let Expression::File(FileState::Read(file)) = &exp {
            let mut fstr = String::new();
            file.borrow_mut().read_to_string(&mut fstr)?;
            match read(&fstr, false) {
                Ok(ast) => Ok(ast),
                Err(err) => Err(io::Error::new(io::ErrorKind::Other, err.reason)),
            }
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "read requires a file opened for reading",
            ))
        }
    }
}

fn builtin_write_line(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    if args.len() != 2 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "write-line takes two forms (file and line)",
        ))
    } else {
        let exp = eval(environment, &args[0])?;
        if let Expression::File(FileState::Write(file)) = &exp {
            writeln!(
                &mut file.borrow_mut(),
                "{}",
                eval(environment, &args[1])?.to_string()
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
    if args.len() != 2 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "write-string takes two forms (file and line)",
        ))
    } else {
        let exp = eval(environment, &args[0])?;
        if let Expression::File(FileState::Write(file)) = &exp {
            write!(
                &mut file.borrow_mut(),
                "{}",
                eval(environment, &args[1])?.to_string()
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
    data.insert("read".to_string(), Rc::new(Expression::Func(builtin_read)));
    data.insert(
        "write-line".to_string(),
        Rc::new(Expression::Func(builtin_write_line)),
    );
    data.insert(
        "write-string".to_string(),
        Rc::new(Expression::Func(builtin_write_string)),
    );
}
