use std::collections::HashMap;
use std::hash::BuildHasher;
use std::io;
use std::rc::Rc;

use crate::builtins_util::*;
use crate::environment::*;
use crate::eval::*;
use crate::interner::*;
use crate::types::*;

fn builtin_type(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return Ok(Expression::Atom(Atom::StringRef(
                environment.interner.intern(&arg.display_type()),
            )));
        }
    }
    Err(io::Error::new(io::ErrorKind::Other, "type takes one form"))
}

fn builtin_is_nil(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if arg.is_nil() {
                Ok(Expression::Atom(Atom::True))
            } else {
                Ok(Expression::nil())
            };
        }
    }
    Err(io::Error::new(io::ErrorKind::Other, "nil? needs one form"))
}

fn builtin_is_true(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let Expression::Atom(Atom::True) = arg {
                Ok(Expression::Atom(Atom::True))
            } else {
                Ok(Expression::nil())
            };
        }
    }
    Err(io::Error::new(io::ErrorKind::Other, "true? needs one form"))
}

fn builtin_is_float(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let Expression::Atom(Atom::Float(_)) = arg {
                Ok(Expression::Atom(Atom::True))
            } else {
                Ok(Expression::nil())
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "float? needs one form",
    ))
}

fn builtin_is_int(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let Expression::Atom(Atom::Int(_)) = arg {
                Ok(Expression::Atom(Atom::True))
            } else {
                Ok(Expression::nil())
            };
        }
    }
    Err(io::Error::new(io::ErrorKind::Other, "int? needs one form"))
}

fn builtin_is_symbol(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let Expression::Atom(Atom::Symbol(_)) = arg {
                Ok(Expression::Atom(Atom::True))
            } else {
                Ok(Expression::nil())
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "symbol? needs one form",
    ))
}

fn builtin_is_string(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let Expression::Atom(Atom::String(_)) = arg {
                Ok(Expression::Atom(Atom::True))
            } else if let Expression::Atom(Atom::StringRef(_)) = arg {
                Ok(Expression::Atom(Atom::True))
            } else {
                Ok(Expression::nil())
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "string? needs one form",
    ))
}

fn builtin_is_string_buf(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let Expression::Atom(Atom::StringBuf(_)) = arg {
                Ok(Expression::Atom(Atom::True))
            } else {
                Ok(Expression::nil())
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "string-buf? needs one form",
    ))
}

fn builtin_is_char(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let Expression::Atom(Atom::Char(_)) = arg {
                Ok(Expression::Atom(Atom::True))
            } else {
                Ok(Expression::nil())
            };
        }
    }
    Err(io::Error::new(io::ErrorKind::Other, "char? needs one form"))
}

fn builtin_is_lambda(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let Expression::Atom(Atom::Lambda(_)) = arg {
                Ok(Expression::Atom(Atom::True))
            } else {
                Ok(Expression::nil())
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "lambda? needs one form",
    ))
}

fn builtin_is_macro(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let Expression::Atom(Atom::Macro(_)) = arg {
                Ok(Expression::Atom(Atom::True))
            } else {
                Ok(Expression::nil())
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "macro? needs one form",
    ))
}

fn builtin_is_vec(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let Expression::Vector(_, _) = arg {
                Ok(Expression::Atom(Atom::True))
            } else {
                Ok(Expression::nil())
            };
        }
    }
    Err(io::Error::new(io::ErrorKind::Other, "vec? needs one form"))
}

fn builtin_is_pair(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let Expression::Pair(p, _) = arg {
                if let Some((_, _)) = &*p.borrow() {
                    Ok(Expression::Atom(Atom::True))
                } else {
                    Ok(Expression::nil())
                }
            } else {
                Ok(Expression::nil())
            };
        }
    }
    Err(io::Error::new(io::ErrorKind::Other, "pair? needs one form"))
}

fn builtin_is_builtin(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            return match eval(environment, arg)? {
                Expression::Function(_) => Ok(Expression::Atom(Atom::True)),
                _ => Ok(Expression::nil()),
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "builtin? needs one form",
    ))
}

fn builtin_is_process(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let Expression::Process(_) = arg {
                Ok(Expression::Atom(Atom::True))
            } else {
                Ok(Expression::nil())
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "process? needs one form",
    ))
}

fn builtin_is_file(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let Expression::File(_) = arg {
                Ok(Expression::Atom(Atom::True))
            } else {
                Ok(Expression::nil())
            };
        }
    }
    Err(io::Error::new(io::ErrorKind::Other, "file? needs one form"))
}

fn builtin_is_hash(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let Expression::HashMap(_) = arg {
                Ok(Expression::Atom(Atom::True))
            } else {
                Ok(Expression::nil())
            };
        }
    }
    Err(io::Error::new(io::ErrorKind::Other, "hash? needs one form"))
}

fn builtin_is_list(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if arg.is_nil() || is_proper_list(&arg) {
                Ok(Expression::Atom(Atom::True))
            } else {
                Ok(Expression::nil())
            };
        }
    }
    Err(io::Error::new(io::ErrorKind::Other, "list? needs one form"))
}

pub fn add_type_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, Rc<Reference>, S>,
) {
    let root = interner.intern("root");
    data.insert(
        interner.intern("type"),
        Rc::new(Expression::make_function(
            builtin_type,
            "Usage: (type expression)

Return the type of the given expression as a string.

Types are:
    True
    Float
    Int
    Symbol
    String
    StringBuf
    Char
    Lambda
    Macro
    Process
    SpecialForm
    Function
    Vector
    Pair
    Nil
    HashMap
    File

Example:
(test::assert-equal \"True\" (type t))
(test::assert-equal \"Float\" (type 1.1))
(test::assert-equal \"Int\" (type 1))
(test::assert-equal \"Symbol\" (type 'symbol))
(def 'type-sym 'symbol)
(test::assert-equal \"Symbol\" (type type-sym))
(test::assert-equal \"String\" (type \"string\"))
(test::assert-equal \"StringBuf\" (type (str-buf \"buffer\")))
(test::assert-equal \"Char\" (type #\\a))
(test::assert-equal \"Lambda\" (type (fn () ())))
(test::assert-equal \"Macro\" (type (macro () ())))
(test::assert-equal \"Process\" (type (true)))
(test::assert-equal \"SpecialForm\" (type if))
(test::assert-equal \"Function\" (type type))
(test::assert-equal \"Vector\" (type '#(1 2 3)))
(def 'type-vec '#(4 5 6))
(test::assert-equal \"Vector\" (type type-vec))
(test::assert-equal \"Pair\" (type '(1 . 2)))
(test::assert-equal \"Pair\" (type '(1 2 3)))
(test::assert-equal \"Nil\" (type nil))
(test::assert-equal \"Nil\" (type '()))
(test::assert-equal \"HashMap\" (type (make-hash)))
(test::assert-equal \"File\" (type (open :stdin)))
",
            root,
        )),
    );
    data.insert(
        interner.intern("nil?"),
        Rc::new(Expression::make_function(
            builtin_is_nil,
            "Usage: (nil? expression)

True if the expression is nil, false otherwise.

Example:
(test::assert-true (nil? nil))
(test::assert-false (nil? t))
",
            root,
        )),
    );
    data.insert(
        interner.intern("true?"),
        Rc::new(Expression::make_function(
            builtin_is_true,
            "Usage: (true? expression)

True if the expression is true (true type NOT non-null), false otherwise.

Example:
(test::assert-true (true? t))
(test::assert-false (true? nil))
(test::assert-false (true? 1))
(test::assert-false (true? \"str\"))
",
            root,
        )),
    );
    data.insert(
        interner.intern("float?"),
        Rc::new(Expression::make_function(
            builtin_is_float,
            "Usage: (float? expression)

True if the expression is a float, false otherwise.

Example:
(test::assert-true (float? 1.5))
(test::assert-false (float? 1))
",
            root,
        )),
    );
    data.insert(
        interner.intern("int?"),
        Rc::new(Expression::make_function(
            builtin_is_int,
            "Usage: (int? expression)

True if the expression is an int, false otherwise.

Example:
(test::assert-true (int? 1))
(test::assert-false (int? 1.5))
",
            root,
        )),
    );
    data.insert(
        interner.intern("symbol?"),
        Rc::new(Expression::make_function(
            builtin_is_symbol,
            "Usage: (symbol? expression)

True if the expression is a symbol, false otherwise.

Example:
(test::assert-true (symbol? 'symbol))
(test::assert-false (symbol? 1))
",
            root,
        )),
    );
    data.insert(
        interner.intern("string?"),
        Rc::new(Expression::make_function(
            builtin_is_string,
            "Usage: (string? expression)

True if the expression is a string, false otherwise.

Example:
(test::assert-true (string? \"string\"))
(test::assert-false (string? 1))
",
            root,
        )),
    );
    data.insert(
        interner.intern("string-buf?"),
        Rc::new(Expression::make_function(
            builtin_is_string_buf,
            "Usage: (string-buf? expression)

True if the expression is a string buffer, false otherwise.

Example:
(test::assert-true (string-buf? (str-buf \"string\")))
(test::assert-false (string-buf? \"string\"))
(test::assert-false (string-buf? 1))
",
            root,
        )),
    );
    data.insert(
        interner.intern("char?"),
        Rc::new(Expression::make_function(
            builtin_is_char,
            "Usage: (char? expression)

True if the expression is a char, false otherwise.

Example:
(test::assert-true (char? #\\a))
(test::assert-false (char? 1))
(test::assert-false (char? \"a\"))
",
            root,
        )),
    );
    data.insert(
        interner.intern("lambda?"),
        Rc::new(Expression::make_function(
            builtin_is_lambda,
            "Usage: (lambda? expression)

True if the expression is a lambda, false otherwise.

Example:
(test::assert-true (lambda? (fn () ())))
(test::assert-true (lambda? copy-seq))
(test::assert-false (lambda? 1))
(test::assert-false (lambda? if))
",
            root,
        )),
    );
    data.insert(
        interner.intern("macro?"),
        Rc::new(Expression::make_function(
            builtin_is_macro,
            "Usage: (macro? expression)

True if the expression is a macro, false otherwise.

Example:
(test::assert-true (macro? (macro () ())))
(test::assert-true (macro? defn))
(test::assert-false (macro? 1))
(test::assert-false (macro? if))
",
            root,
        )),
    );
    data.insert(
        interner.intern("vec?"),
        Rc::new(Expression::make_function(
            builtin_is_vec,
            "Usage: (vec? expression)

True if the expression is a vector, false otherwise.

Example:
(test::assert-true (vec? '#(1 2 3)) \"reader macro\")
(test::assert-true (vec? (make-vec)) \"make-vec\") 
(test::assert-true (vec? (vec 1 2 3)) \"vec\") 
(test::assert-false (vec? 1))
(test::assert-false (vec? '(1 2 3)))
(test::assert-false (vec? (list)))
",
            root,
        )),
    );
    data.insert(
        interner.intern("pair?"),
        Rc::new(Expression::make_function(
            builtin_is_pair,
            "Usage: (pair? expression)

True if the expression is a pair, false otherwise.

Example:
(test::assert-true (pair? '(1 . 2)) \"reader macro\")
(test::assert-true (pair? (join 1 2)) \"join\") 
(test::assert-true (pair? '(1 2)))
(test::assert-false (pair? 1))
(test::assert-false (pair? '#(1 2 3)))
(test::assert-false (pair? (vec)))
",
            root,
        )),
    );
    data.insert(
        interner.intern("builtin?"),
        Rc::new(Expression::make_function(
            builtin_is_builtin,
            "Usage: (builtin? expression)

True if the expression is a builtin function or special form, false otherwise.

Example:
(test::assert-true (builtin? type))
(test::assert-true (builtin? if))
(test::assert-false (builtin? (fn () ())))
(test::assert-false (builtin? copy-seq))
(test::assert-false (builtin? 1))
",
            root,
        )),
    );
    data.insert(
        interner.intern("process?"),
        Rc::new(Expression::make_function(
            builtin_is_process,
            "Usage: (process? expression)

True if the expression is a process, false otherwise.

Example:
(test::assert-true (process? (true)))
(test::assert-false (process? (fn () ())))
(test::assert-false (process? copy-seq))
(test::assert-false (process? 1))
",
            root,
        )),
    );
    data.insert(
        interner.intern("file?"),
        Rc::new(Expression::make_function(
            builtin_is_file,
            "Usage: (file? expression)

True if the expression is a file, false otherwise.

Example:
(test::assert-true (file? (open :stdout)))
(test::assert-false (file? (fn () ())))
(test::assert-false (file? copy-seq))
(test::assert-false (file? 1))
",
            root,
        )),
    );
    data.insert(
        interner.intern("hash?"),
        Rc::new(Expression::make_function(
            builtin_is_hash,
            "Usage: (hash? expression)

True if the expression is a hash map, false otherwise.

Example:
(test::assert-true (hash? (make-hash)) \"make-vec\") 
(test::assert-false (hash? 1))
(test::assert-false (hash? '(1 2 3)))
(test::assert-false (hash? (list)))
(test::assert-false (hash? (vec)))
",
            root,
        )),
    );
    data.insert(
        interner.intern("list?"),
        Rc::new(Expression::make_function(
            builtin_is_list,
            "Usage: (list? expression)

True if the expression is a list, false otherwise.

Example:
(test::assert-true (list? '(1 2 3)) \"reader macro\")
(test::assert-true (list? (list 1 2 3)) \"list\") 
(test::assert-false (list? 1))
(test::assert-false (list? '#(1 2 3)))
(test::assert-false (list? (vec)))
(test::assert-false (list? '(1 . 2)))
",
            root,
        )),
    );
}
