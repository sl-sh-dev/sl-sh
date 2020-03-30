use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::OpenOptions;
use std::hash::BuildHasher;
use std::io::{self, BufRead, BufReader, BufWriter, Read, Write};
use std::rc::Rc;

use crate::environment::*;
use crate::eval::*;
use crate::interner::*;
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
            Expression::Atom(Atom::String(name)) => name.to_string(),
            Expression::Atom(Atom::StringRef(name)) => (*name).to_string(),
            Expression::Atom(Atom::StringBuf(name)) => name.borrow().to_string(),
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
    fn do_read(environment: &mut Environment, input: &str) -> io::Result<Expression> {
        match read(environment, &input, None) {
            Ok(ast) => Ok(ast),
            Err(err) => Err(io::Error::new(io::ErrorKind::Other, err.reason)),
        }
    }
    if let Some(exp) = args.next() {
        if args.next().is_none() {
            let exp = eval(environment, exp)?;
            return match &exp {
                Expression::File(file) => match &*file.borrow() {
                    FileState::Read(file) => {
                        let mut input = String::new();
                        file.borrow_mut().read_to_string(&mut input)?;
                        do_read(environment, &input)
                    }
                    FileState::Stdin => {
                        let mut input = String::new();
                        io::stdin().read_to_string(&mut input)?;
                        do_read(environment, &input)
                    }
                    _ => Err(io::Error::new(
                        io::ErrorKind::Other,
                        "read: requires a file opened for reading or string",
                    )),
                },
                Expression::Atom(Atom::String(input)) => do_read(environment, input),
                Expression::Atom(Atom::StringRef(input)) => do_read(environment, input),
                Expression::Atom(Atom::StringBuf(input)) => do_read(environment, &input.borrow()),
                _ => Err(io::Error::new(
                    io::ErrorKind::Other,
                    "read: requires a file opened for reading or string",
                )),
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "read: takes a file or string",
    ))
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

pub fn add_io_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, Rc<Reference>, S>,
) {
    let root = interner.intern("root");
    data.insert(
        interner.intern("open"),
        Rc::new(Expression::make_function(
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

Example:
(write-line (open \"/tmp/slsh-tst-open.txt\" :create :truncate) \"Test Line One\")
(test::assert-equal \"Test Line One\n\" (read-line (open \"/tmp/slsh-tst-open.txt\")))
",
            root,
        )),
    );
    data.insert(
        interner.intern("close"),
        Rc::new(Expression::make_function(
            builtin_close,
            "Usage: (close file)

Close a file.

Example:
(def 'tst-file (open \"/tmp/slsh-tst-open.txt\" :create :truncate))
(write-line tst-file \"Test Line Two\")
(close tst-file)
(def 'tst-file (open \"/tmp/slsh-tst-open.txt\" :read))
(test::assert-equal \"Test Line Two\n\" (read-line tst-file))
(close tst-file)
",
            root,
        )),
    );
    data.insert(
        interner.intern("flush"),
        Rc::new(Expression::make_function(
            builtin_flush,
            "Usage: (flush file)

Flush a file.

Example:
(def 'tst-file (open \"/tmp/slsh-tst-open.txt\" :create :truncate))
(write-line tst-file \"Test Line Three\")
(flush tst-file)
(def 'tst-file (open \"/tmp/slsh-tst-open.txt\" :read))
(test::assert-equal \"Test Line Three\n\" (read-line tst-file))
(close tst-file)
",
            root,
        )),
    );
    data.insert(
        interner.intern("read-line"),
        Rc::new(Expression::make_function(
            builtin_read_line,
            "Usage: (read-line file) -> string

Read a line from a file.

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
        )),
    );
    data.insert(
        interner.intern("read"),
        Rc::new(Expression::make_function(
            builtin_read,
            "Usage: (read file|string) -> list

Read a file or string and return the list representation.

Unlike most lisp readers this one will put loose symbols in a list (i.e. you
enter things at the repl without the enclosing parens).

Example:
(def 'tst-file (open \"/tmp/slsh-tst-open.txt\" :create :truncate))
(write-line tst-file \"(1 2 3)\")
;(write-string tst-file \"Test Line Read Line Two\")
(flush tst-file)
(def 'tst-file (open \"/tmp/slsh-tst-open.txt\" :read))
(test::assert-equal '(1 2 3) (read tst-file))
(close tst-file)
(test::assert-equal '(4 5 6) (read \"(4 5 6)\"))
(test::assert-equal '(7 8 9) (read \"7 8 9\"))
(test::assert-equal '(x y z) (read \"(x y z)\"))
",
            root,
        )),
    );
    data.insert(
        interner.intern("write-line"),
        Rc::new(Expression::make_function(
            builtin_write_line,
            "Usage: (write-line file string)

Write a line to a file.

Example:
(def 'tst-file (open \"/tmp/slsh-tst-open.txt\" :create :truncate))
(write-line tst-file \"Test Line Write Line\")
(flush tst-file)
(def 'tst-file (open \"/tmp/slsh-tst-open.txt\" :read))
(test::assert-equal \"Test Line Write Line\n\" (read-line tst-file))
(close tst-file)
",
            root,
        )),
    );
    data.insert(
        interner.intern("write-string"),
        Rc::new(Expression::make_function(
            builtin_write_string,
            "Usage: (write-string file string)

Write a string to a file.

Example:
(def 'tst-file (open \"/tmp/slsh-tst-open.txt\" :create :truncate))
(write-string tst-file \"Test Line Write String\")
(flush tst-file)
(def 'tst-file (open \"/tmp/slsh-tst-open.txt\" :read))
(test::assert-equal \"Test Line Write String\" (read-line tst-file))
(close tst-file)
",
            root,
        )),
    );
}
