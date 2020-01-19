use std::collections::HashMap;
use std::hash::BuildHasher;
use std::io;
use std::rc::Rc;

use crate::builtins_util::*;
use crate::environment::*;
use crate::eval::*;
use crate::types::*;

fn builtin_type(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return Ok(Expression::Atom(Atom::String(arg.display_type())));
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
            return if let Expression::Atom(Atom::Nil) = arg {
                Ok(Expression::Atom(Atom::True))
            } else {
                Ok(Expression::Atom(Atom::Nil))
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
                Ok(Expression::Atom(Atom::Nil))
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
                Ok(Expression::Atom(Atom::Nil))
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
                Ok(Expression::Atom(Atom::Nil))
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
                Ok(Expression::Atom(Atom::Nil))
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
            } else {
                Ok(Expression::Atom(Atom::Nil))
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
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let Expression::Atom(Atom::Char(_)) = arg {
                Ok(Expression::Atom(Atom::True))
            } else {
                Ok(Expression::Atom(Atom::Nil))
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
                Ok(Expression::Atom(Atom::Nil))
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
                Ok(Expression::Atom(Atom::Nil))
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
            return if let Expression::Vector(_) = arg {
                Ok(Expression::Atom(Atom::True))
            } else {
                Ok(Expression::Atom(Atom::Nil))
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
            return if let Expression::Pair(_, _) = arg {
                Ok(Expression::Atom(Atom::True))
            } else {
                Ok(Expression::Atom(Atom::Nil))
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
                Expression::Func(_) => Ok(Expression::Atom(Atom::True)),
                Expression::Function(_) => Ok(Expression::Atom(Atom::True)),
                _ => Ok(Expression::Atom(Atom::Nil)),
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
                Ok(Expression::Atom(Atom::Nil))
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
                Ok(Expression::Atom(Atom::Nil))
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
                Ok(Expression::Atom(Atom::Nil))
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
            return  if let Expression::Atom(Atom::Nil) = arg {
                Ok(Expression::Atom(Atom::True))
            } else if is_proper_list(&arg) {
                Ok(Expression::Atom(Atom::True))
            } else {
                Ok(Expression::Atom(Atom::Nil))
            };
        }
    }
    Err(io::Error::new(io::ErrorKind::Other, "list? needs one form"))
}

pub fn add_type_builtins<S: BuildHasher>(data: &mut HashMap<String, Rc<Expression>, S>) {
    data.insert(
        "type".to_string(),
        Rc::new(Expression::make_function(builtin_type, "")),
    );
    data.insert(
        "nil?".to_string(),
        Rc::new(Expression::make_function(builtin_is_nil, "")),
    );
    data.insert(
        "true?".to_string(),
        Rc::new(Expression::make_function(builtin_is_true, "")),
    );
    data.insert(
        "float?".to_string(),
        Rc::new(Expression::make_function(builtin_is_float, "")),
    );
    data.insert(
        "int?".to_string(),
        Rc::new(Expression::make_function(builtin_is_int, "")),
    );
    data.insert(
        "symbol?".to_string(),
        Rc::new(Expression::make_function(builtin_is_symbol, "")),
    );
    data.insert(
        "string?".to_string(),
        Rc::new(Expression::make_function(builtin_is_string, "")),
    );
    data.insert(
        "char?".to_string(),
        Rc::new(Expression::make_function(builtin_is_char, "")),
    );
    data.insert(
        "lambda?".to_string(),
        Rc::new(Expression::make_function(builtin_is_lambda, "")),
    );
    data.insert(
        "macro?".to_string(),
        Rc::new(Expression::make_function(builtin_is_macro, "")),
    );
    data.insert(
        "vec?".to_string(),
        Rc::new(Expression::make_function(builtin_is_vec, "")),
    );
    data.insert(
        "pair?".to_string(),
        Rc::new(Expression::make_function(builtin_is_pair, "")),
    );
    data.insert(
        "builtin?".to_string(),
        Rc::new(Expression::make_function(builtin_is_builtin, "")),
    );
    data.insert(
        "process?".to_string(),
        Rc::new(Expression::make_function(builtin_is_process, "")),
    );
    data.insert(
        "file?".to_string(),
        Rc::new(Expression::make_function(builtin_is_file, "")),
    );
    data.insert(
        "hash?".to_string(),
        Rc::new(Expression::make_function(builtin_is_hash, "")),
    );
    data.insert(
        "list?".to_string(),
        Rc::new(Expression::make_function(builtin_is_list, "")),
    );
}
