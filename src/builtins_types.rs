use std::collections::HashMap;
use std::hash::BuildHasher;
use std::io;

use crate::builtins_util::*;
use crate::environment::*;
use crate::eval::*;
use crate::interner::*;
use crate::types::*;

fn builtin_type(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return Ok(Expression::alloc_data(ExpEnum::Atom(Atom::String(
                environment.interner.intern(&arg.display_type()).into(),
                None,
            ))));
        }
    }
    Err(io::Error::new(io::ErrorKind::Other, "type takes one form"))
}

fn builtin_is_nil(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if arg.is_nil() {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_nil())
            };
        }
    }
    Err(io::Error::new(io::ErrorKind::Other, "nil? needs one form"))
}

fn builtin_is_true(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let ExpEnum::Atom(Atom::True) = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_nil())
            };
        }
    }
    Err(io::Error::new(io::ErrorKind::Other, "true? needs one form"))
}

fn builtin_is_float(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let ExpEnum::Atom(Atom::Float(_)) = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_nil())
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
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let ExpEnum::Atom(Atom::Int(_)) = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_nil())
            };
        }
    }
    Err(io::Error::new(io::ErrorKind::Other, "int? needs one form"))
}

fn builtin_is_symbol(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let ExpEnum::Atom(Atom::Symbol(_)) = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_nil())
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
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let ExpEnum::Atom(Atom::String(_, _)) = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_nil())
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "string? needs one form",
    ))
}

fn builtin_is_char(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let ExpEnum::Atom(Atom::Char(_)) = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_nil())
            };
        }
    }
    Err(io::Error::new(io::ErrorKind::Other, "char? needs one form"))
}

fn builtin_is_lambda(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let ExpEnum::Atom(Atom::Lambda(_)) = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_nil())
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
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let ExpEnum::Atom(Atom::Macro(_)) = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_nil())
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
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let ExpEnum::Vector(_) = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_nil())
            };
        }
    }
    Err(io::Error::new(io::ErrorKind::Other, "vec? needs one form"))
}

fn builtin_is_pair(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let ExpEnum::Pair(_, _) = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_nil())
            };
        }
    }
    Err(io::Error::new(io::ErrorKind::Other, "pair? needs one form"))
}

fn builtin_is_builtin(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            return match eval(environment, arg)?.get().data {
                ExpEnum::Function(_) => Ok(Expression::make_true()),
                _ => Ok(Expression::make_nil()),
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
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let ExpEnum::Process(_) = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_nil())
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
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let ExpEnum::File(_) = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_nil())
            };
        }
    }
    Err(io::Error::new(io::ErrorKind::Other, "file? needs one form"))
}

fn builtin_is_hash(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let ExpEnum::HashMap(_) = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_nil())
            };
        }
    }
    Err(io::Error::new(io::ErrorKind::Other, "hash? needs one form"))
}

fn builtin_is_list(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if arg.is_nil() || is_proper_list(&arg) {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_nil())
            };
        }
    }
    Err(io::Error::new(io::ErrorKind::Other, "list? needs one form"))
}

pub fn add_type_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, Reference, S>,
) {
    let root = interner.intern("root");
    data.insert(
        interner.intern("type"),
        Expression::make_function(
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

Section: type

Example:
(test::assert-equal \"True\" (type t))
(test::assert-equal \"Float\" (type 1.1))
(test::assert-equal \"Int\" (type 1))
(test::assert-equal \"Symbol\" (type 'symbol))
(def 'type-sym 'symbol)
(test::assert-equal \"Symbol\" (type type-sym))
(test::assert-equal \"String\" (type \"string\"))
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
        ),
    );
    data.insert(
        interner.intern("nil?"),
        Expression::make_function(
            builtin_is_nil,
            "Usage: (nil? expression)

True if the expression is nil, false otherwise.

Section: type

Example:
(test::assert-true (nil? nil))
(test::assert-false (nil? t))
",
            root,
        ),
    );
    data.insert(
        interner.intern("true?"),
        Expression::make_function(
            builtin_is_true,
            "Usage: (true? expression)

True if the expression is true (true type NOT non-null), false otherwise.

Section: type

Example:
(test::assert-true (true? t))
(test::assert-false (true? nil))
(test::assert-false (true? 1))
(test::assert-false (true? \"str\"))
",
            root,
        ),
    );
    data.insert(
        interner.intern("float?"),
        Expression::make_function(
            builtin_is_float,
            "Usage: (float? expression)

True if the expression is a float, false otherwise.

Section: type

Example:
(test::assert-true (float? 1.5))
(test::assert-false (float? 1))
",
            root,
        ),
    );
    data.insert(
        interner.intern("int?"),
        Expression::make_function(
            builtin_is_int,
            "Usage: (int? expression)

True if the expression is an int, false otherwise.

Section: type

Example:
(test::assert-true (int? 1))
(test::assert-false (int? 1.5))
",
            root,
        ),
    );
    data.insert(
        interner.intern("symbol?"),
        Expression::make_function(
            builtin_is_symbol,
            "Usage: (symbol? expression)

True if the expression is a symbol, false otherwise.

Section: type

Example:
(test::assert-true (symbol? 'symbol))
(test::assert-false (symbol? 1))
",
            root,
        ),
    );
    data.insert(
        interner.intern("string?"),
        Expression::make_function(
            builtin_is_string,
            "Usage: (string? expression)

True if the expression is a string, false otherwise.

Section: type

Example:
(test::assert-true (string? \"string\"))
(test::assert-false (string? 1))
",
            root,
        ),
    );
    data.insert(
        interner.intern("char?"),
        Expression::make_function(
            builtin_is_char,
            "Usage: (char? expression)

True if the expression is a char, false otherwise.

Section: type

Example:
(test::assert-true (char? #\\a))
(test::assert-false (char? 1))
(test::assert-false (char? \"a\"))
",
            root,
        ),
    );
    data.insert(
        interner.intern("lambda?"),
        Expression::make_function(
            builtin_is_lambda,
            "Usage: (lambda? expression)

True if the expression is a lambda, false otherwise.

Section: type

Example:
(test::assert-true (lambda? (fn () ())))
(test::assert-true (lambda? caar))
(test::assert-false (lambda? 1))
(test::assert-false (lambda? if))
",
            root,
        ),
    );
    data.insert(
        interner.intern("macro?"),
        Expression::make_function(
            builtin_is_macro,
            "Usage: (macro? expression)

True if the expression is a macro, false otherwise.

Section: type

Example:
(test::assert-true (macro? (macro () ())))
(test::assert-true (macro? defn))
(test::assert-false (macro? 1))
(test::assert-false (macro? if))
",
            root,
        ),
    );
    data.insert(
        interner.intern("vec?"),
        Expression::make_function(
            builtin_is_vec,
            "Usage: (vec? expression)

True if the expression is a vector, false otherwise.

Section: type

Example:
(test::assert-true (vec? '#(1 2 3)) \"reader macro\")
(test::assert-true (vec? (make-vec)) \"make-vec\") 
(test::assert-true (vec? (vec 1 2 3)) \"vec\") 
(test::assert-false (vec? 1))
(test::assert-false (vec? '(1 2 3)))
(test::assert-false (vec? (list)))
",
            root,
        ),
    );
    data.insert(
        interner.intern("pair?"),
        Expression::make_function(
            builtin_is_pair,
            "Usage: (pair? expression)

True if the expression is a pair, false otherwise.

Section: type

Example:
(test::assert-true (pair? '(1 . 2)) \"reader macro\")
(test::assert-true (pair? (join 1 2)) \"join\") 
(test::assert-true (pair? '(1 2)))
(test::assert-false (pair? 1))
(test::assert-false (pair? '#(1 2 3)))
(test::assert-false (pair? (vec)))
",
            root,
        ),
    );
    data.insert(
        interner.intern("builtin?"),
        Expression::make_function(
            builtin_is_builtin,
            "Usage: (builtin? expression)

True if the expression is a builtin function or special form, false otherwise.

Section: type

Example:
(test::assert-true (builtin? type))
(test::assert-true (builtin? if))
(test::assert-false (builtin? (fn () ())))
(test::assert-false (builtin? caar))
(test::assert-false (builtin? 1))
",
            root,
        ),
    );
    data.insert(
        interner.intern("process?"),
        Expression::make_function(
            builtin_is_process,
            "Usage: (process? expression)

True if the expression is a process, false otherwise.

Section: type

Example:
(test::assert-true (process? (true)))
(test::assert-false (process? (fn () ())))
(test::assert-false (process? caar))
(test::assert-false (process? 1))
",
            root,
        ),
    );
    data.insert(
        interner.intern("file?"),
        Expression::make_function(
            builtin_is_file,
            "Usage: (file? expression)

True if the expression is a file, false otherwise.

Section: type

Example:
(test::assert-true (file? (open :stdout)))
(test::assert-false (file? (fn () ())))
(test::assert-false (file? caar))
(test::assert-false (file? 1))
",
            root,
        ),
    );
    data.insert(
        interner.intern("hash?"),
        Expression::make_function(
            builtin_is_hash,
            "Usage: (hash? expression)

True if the expression is a hash map, false otherwise.

Section: type

Example:
(test::assert-true (hash? (make-hash)) \"make-vec\") 
(test::assert-false (hash? 1))
(test::assert-false (hash? '(1 2 3)))
(test::assert-false (hash? (list)))
(test::assert-false (hash? (vec)))
",
            root,
        ),
    );
    data.insert(
        interner.intern("list?"),
        Expression::make_function(
            builtin_is_list,
            "Usage: (list? expression)

True if the expression is a list, false otherwise.

Section: type

Example:
(test::assert-true (list? '(1 2 3)) \"reader macro\")
(test::assert-true (list? (list 1 2 3)) \"list\") 
(test::assert-false (list? 1))
(test::assert-false (list? '#(1 2 3)))
(test::assert-false (list? (vec)))
(test::assert-false (list? '(1 . 2)))
",
            root,
        ),
    );
}
