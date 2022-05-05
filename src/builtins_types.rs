use std::collections::HashMap;
use std::hash::BuildHasher;
use std::num::{ParseFloatError, ParseIntError};

use crate::builtins_util::*;
use crate::environment::*;
use crate::eval::*;
use crate::interner::*;
use crate::types::*;

fn builtin_type(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return Ok(Expression::alloc_data(ExpEnum::String(
                environment.interner.intern(&arg.display_type()).into(),
                None,
            )));
        }
    }
    Err(LispError::new("type takes one form"))
}

fn builtin_is_values(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return if let ExpEnum::Values(_) = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_false())
            };
        }
    }
    Err(LispError::new("values? needs one form"))
}

fn is_nil(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
    name: &str,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval_no_values(environment, arg)?;
            return if arg.is_nil() {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_false())
            };
        }
    }
    Err(LispError::new(format!("{} needs one form", name)))
}

fn builtin_is_nil(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    is_nil(environment, args, "nil?")
}

fn builtin_is_none(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    is_nil(environment, args, "none?")
}

fn builtin_is_some(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval_no_values(environment, arg)?;
            return if !arg.is_nil() {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_false())
            };
        }
    }
    Err(LispError::new("some? needs one form"))
}

fn builtin_is_true(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval_no_values(environment, arg)?;
            return if let ExpEnum::True = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_false())
            };
        }
    }
    Err(LispError::new("true? needs one form"))
}

fn builtin_is_false(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval_no_values(environment, arg)?;
            return if let ExpEnum::False = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_false())
            };
        }
    }
    Err(LispError::new("false? needs one form"))
}

fn builtin_is_boolean(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval_no_values(environment, arg)?;
            return match arg.get().data {
                ExpEnum::True => Ok(Expression::make_true()),
                ExpEnum::False => Ok(Expression::make_true()),
                _ => Ok(Expression::make_false()),
            };
        }
    }
    Err(LispError::new("boolean? needs one form"))
}

fn builtin_is_float(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval_no_values(environment, arg)?;
            return if let ExpEnum::Float(_) = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_false())
            };
        }
    }
    Err(LispError::new("float? needs one form"))
}

fn builtin_is_regex(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval_no_values(environment, arg)?;
            return if let ExpEnum::Regex(_) = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_false())
            };
        }
    }
    Err(LispError::new("regex? needs one form"))
}
fn builtin_is_int(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval_no_values(environment, arg)?;
            return if let ExpEnum::Int(_) = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_false())
            };
        }
    }
    Err(LispError::new("int? needs one form"))
}

fn builtin_is_symbol(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval_no_values(environment, arg)?;
            return if let ExpEnum::Symbol(_, _) = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_false())
            };
        }
    }
    Err(LispError::new("symbol? needs one form"))
}

fn builtin_is_string(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval_no_values(environment, arg)?;
            return if let ExpEnum::String(_, _) = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_false())
            };
        }
    }
    Err(LispError::new("string? needs one form"))
}

fn builtin_is_char(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval_no_values(environment, arg)?;
            return if let ExpEnum::Char(_) = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_false())
            };
        }
    }
    Err(LispError::new("char? needs one form"))
}

fn builtin_is_lambda(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval_no_values(environment, arg)?;
            return if let ExpEnum::Lambda(_) = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_false())
            };
        }
    }
    Err(LispError::new("lambda? needs one form"))
}

fn builtin_is_macro(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval_no_values(environment, arg)?;
            return if let ExpEnum::Macro(_) = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_false())
            };
        }
    }
    Err(LispError::new("macro? needs one form"))
}

fn builtin_is_vec(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval_no_values(environment, arg)?;
            return if let ExpEnum::Vector(_) = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_false())
            };
        }
    }
    Err(LispError::new("vec? needs one form"))
}

fn builtin_is_pair(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval_no_values(environment, arg)?;
            return if let ExpEnum::Pair(_, _) = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_false())
            };
        }
    }
    Err(LispError::new("pair? needs one form"))
}

fn builtin_is_builtin(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            return match eval_no_values(environment, arg)?.get().data {
                ExpEnum::Function(_) => Ok(Expression::make_true()),
                ExpEnum::DeclareDef => Ok(Expression::make_true()),
                ExpEnum::DeclareVar => Ok(Expression::make_true()),
                ExpEnum::DeclareFn => Ok(Expression::make_true()),
                ExpEnum::DeclareMacro => Ok(Expression::make_true()),
                ExpEnum::Quote => Ok(Expression::make_true()),
                ExpEnum::BackQuote => Ok(Expression::make_true()),
                _ => Ok(Expression::make_false()),
            };
        }
    }
    Err(LispError::new("builtin? needs one form"))
}

fn builtin_is_process(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval_no_values(environment, arg)?;
            return if let ExpEnum::Process(_) = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_false())
            };
        }
    }
    Err(LispError::new("process? needs one form"))
}

fn builtin_is_file(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval_no_values(environment, arg)?;
            return if let ExpEnum::File(_) = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_false())
            };
        }
    }
    Err(LispError::new("file? needs one form"))
}

fn builtin_is_hash(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval_no_values(environment, arg)?;
            return if let ExpEnum::HashMap(_) = arg.get().data {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_false())
            };
        }
    }
    Err(LispError::new("hash? needs one form"))
}

fn builtin_is_list(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval_no_values(environment, arg)?;
            return if arg.is_nil() || is_proper_list(&arg) {
                Ok(Expression::make_true())
            } else {
                Ok(Expression::make_false())
            };
        }
    }
    Err(LispError::new("list? needs one form"))
}

fn builtin_str_to_int(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            if let ExpEnum::String(istr, _) = &eval(environment, arg)?.get().data {
                let potential_int: Result<i64, ParseIntError> = istr.parse();
                return match potential_int {
                    Ok(v) => Ok(Expression::alloc_data(ExpEnum::Int(v))),
                    Err(_) => Err(LispError::new("str->int: string is not a valid integer")),
                };
            }
        }
    }
    Err(LispError::new("str->int: requires a string"))
}

fn builtin_str_to_float(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            if let ExpEnum::String(istr, _) = &eval(environment, arg)?.get().data {
                let potential_float: Result<f64, ParseFloatError> = istr.parse();
                return match potential_float {
                    Ok(v) => Ok(Expression::alloc_data(ExpEnum::Float(v))),
                    Err(_) => Err(LispError::new("str->float: string is not a valid float")),
                };
            }
        }
    }
    Err(LispError::new("str->float: requires a string"))
}

fn builtin_int_to_float(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            if let ExpEnum::Int(i) = &eval(environment, arg)?.get().data {
                return Ok(Expression::alloc_data(ExpEnum::Float(*i as f64)));
            }
        }
    }
    Err(LispError::new("int->float: requires an int"))
}

fn builtin_float_to_int(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            if let ExpEnum::Float(f) = &eval(environment, arg)?.get().data {
                return Ok(Expression::alloc_data(ExpEnum::Int(*f as i64)));
            }
        }
    }
    Err(LispError::new("float->int: requires a float"))
}

fn builtin_to_symbol(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let mut res = String::new();
    for a in args {
        res.push_str(&eval(environment, a)?.as_string(environment)?);
    }
    Ok(Expression::alloc_data(ExpEnum::Symbol(
        environment.interner.intern(&res),
        SymLoc::None,
    )))
}

fn builtin_symbol_to_str(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg0) = args.next() {
        if args.next().is_none() {
            let arg0 = eval(environment, arg0)?;
            return match &arg0.get().data {
                ExpEnum::Symbol(s, _) => {
                    Ok(Expression::alloc_data(ExpEnum::String((*s).into(), None)))
                }
                _ => Err(LispError::new(
                    "sym->str: can only convert a symbol to a string",
                )),
            };
        }
    }
    Err(LispError::new("sym->str: take one form (a symbol)"))
}

fn builtin_falsey(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let arg = param_eval(environment, args, "falsey?")?;
    params_done(args, "falsey?")?;
    if arg.is_falsy() {
        Ok(Expression::make_true())
    } else {
        Ok(Expression::make_false())
    }
}

pub fn add_type_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, (Expression, String), S>,
) {
    data.insert(
        interner.intern("type"),
        Expression::make_function(
            builtin_type,
            r#"Usage: (type expression)

Return the type of the given expression as a string.

Types are:
    True
    False
    Float
    Int
    Symbol
    String
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
(test::assert-equal "True" (type #t))
(test::assert-equal "False" (type #f))
(test::assert-equal "Float" (type 1.1))
(test::assert-equal "Int" (type 1))
(test::assert-equal "Symbol" (type 'symbol))
(def type-sym 'symbol)
(test::assert-equal "Symbol" (type type-sym))
(test::assert-equal "String" (type "string"))
(test::assert-equal "Char" (type #\a))
(test::assert-equal "Lambda" (type (fn () ())))
(test::assert-equal "Macro" (type (macro () ())))
(test::assert-equal "Process" (type (syscall 'true)))
(test::assert-equal "SpecialForm" (type if))
(test::assert-equal "Function" (type type))
(test::assert-equal "Vector" (type '#(1 2 3)))
(def type-vec '#(4 5 6))
(test::assert-equal "Vector" (type type-vec))
(test::assert-equal "Pair" (type '(1 . 2)))
(test::assert-equal "Pair" (type '(1 2 3)))
(test::assert-equal "Nil" (type nil))
(test::assert-equal "Nil" (type '()))
(test::assert-equal "HashMap" (type (make-hash)))
(test::assert-equal "File" (type (open :stdin)))
"#,
        ),
    );
    data.insert(
        interner.intern("values?"),
        Expression::make_function(
            builtin_is_values,
            r#"Usage: (values? expression)

True if the expression is multi values object, false otherwise.
NOTE: A values object will ALSO be the type of its first value.

Section: type

Example:
(test::assert-true (values? (values 1 "str" 5.5)))
(test::assert-false (values? '(1 2 3)))
(test::assert-false (values? '(1 . 3)))
(test::assert-false (values? 1))
(test::assert-true (int? (values 1 "str" 5.5)))
(test::assert-false (string? (values 1 "str" 5.5)))
(test::assert-false (float? (values 1 "str" 5.5)))
(def test-is-values (values 1 2 3 "string" 1.5))
(test::assert-true (values? test-is-values))
(test::assert-true (int? test-is-values))
(test::assert-false (string? test-is-values))
(test::assert-false (float? test-is-values))
"#,
        ),
    );
    data.insert(
        interner.intern("nil?"),
        Expression::make_function(
            builtin_is_nil,
            r#"Usage: (nil? expression)

True if the expression is nil, false otherwise.

Section: type

Example:
(test::assert-true (nil? nil))
(test::assert-false (nil? #t))
"#,
        ),
    );
    data.insert(
        interner.intern("none?"),
        Expression::make_function(
            builtin_is_none,
            r#"Usage: (none? expression)

True if the expression is nil (aka none/nothing), false otherwise.

Section: type

Example:
(test::assert-true (none? nil))
(test::assert-true (none? '()))
(test::assert-false (none? #t))
(test::assert-false (none? '(1)))
"#,
        ),
    );
    data.insert(
        interner.intern("some?"),
        Expression::make_function(
            builtin_is_some,
            r#"Usage: (some? expression)

True if the expression is NOT nil (aka something), false otherwise.
Note that anything other then nil (including false) is something.

Section: type

Example:
(test::assert-false (some? nil))
(test::assert-false (some? '()))
(test::assert-true (some? #t))
(test::assert-true (some? '(1)))
"#,
        ),
    );
    data.insert(
        interner.intern("true?"),
        Expression::make_function(
            builtin_is_true,
            r#"Usage: (true? expression)

True if the expression is true(#t) (true type NOT non-nil), false otherwise.

Section: type

Example:
(test::assert-true (true? #t))
(test::assert-false (true? #f))
(test::assert-false (true? nil))
(test::assert-false (true? 1))
(test::assert-false (true? "str"))
"#,
        ),
    );
    data.insert(
        interner.intern("false?"),
        Expression::make_function(
            builtin_is_false,
            r#"Usage: (false? expression)

True if the expression is false(#f) (false type NOT nil), false otherwise.

Section: type

Example:
(test::assert-true (false? #f))
(test::assert-false (false? nil))
(test::assert-false (false? nil))
(test::assert-false (false? 1))
(test::assert-false (false? "str"))
"#,
        ),
    );
    data.insert(
        interner.intern("boolean?"),
        Expression::make_function(
            builtin_is_boolean,
            r#"Usage: (boolean? expression)

True if the expression is true(#t) or false(#f), false otherwise.

Section: type

Example:
(test::assert-true (boolean? #f))
(test::assert-true (boolean? #t))
(test::assert-false (boolean? nil))
(test::assert-false (boolean? nil))
(test::assert-false (boolean? 1))
(test::assert-false (boolean? "str"))
"#,
        ),
    );
    data.insert(
        interner.intern("float?"),
        Expression::make_function(
            builtin_is_float,
            r#"Usage: (float? expression)

True if the expression is a float, false otherwise.

Section: type

Example:
(test::assert-true (float? 1.5))
(test::assert-false (float? 1))
"#,
        ),
    );
    data.insert(
        interner.intern("regex?"),
        Expression::make_function(
            builtin_is_regex,
            r#"Usage: (regex? expression)

True if the expression is a regex, false otherwise.

Section: type

Example:
(test::assert-true (regex? (make-regex "\d{2}-\d{2}-\d{4}")))
(test::assert-true (regex? #/[a-z]+/))
(test::assert-false (regex? 1.5))
"#,
        ),
    );
    data.insert(
        interner.intern("int?"),
        Expression::make_function(
            builtin_is_int,
            r#"Usage: (int? expression)

True if the expression is an int, false otherwise.

Section: type

Example:
(test::assert-true (int? 1))
(test::assert-false (int? 1.5))
"#,
        ),
    );
    data.insert(
        interner.intern("symbol?"),
        Expression::make_function(
            builtin_is_symbol,
            r#"Usage: (symbol? expression)

True if the expression is a symbol, false otherwise.

Section: type

Example:
(test::assert-true (symbol? 'symbol))
(test::assert-false (symbol? 1))
"#,
        ),
    );
    data.insert(
        interner.intern("string?"),
        Expression::make_function(
            builtin_is_string,
            r#"Usage: (string? expression)

True if the expression is a string, false otherwise.

Section: type

Example:
(test::assert-true (string? "string"))
(test::assert-false (string? 1))
"#,
        ),
    );
    data.insert(
        interner.intern("char?"),
        Expression::make_function(
            builtin_is_char,
            r#"Usage: (char? expression)

True if the expression is a char, false otherwise.

Section: type

Example:
(test::assert-true (char? #\a))
(test::assert-false (char? 1))
(test::assert-false (char? "a"))
"#,
        ),
    );
    data.insert(
        interner.intern("lambda?"),
        Expression::make_function(
            builtin_is_lambda,
            r#"Usage: (lambda? expression)

True if the expression is a lambda, false otherwise.

Section: type

Example:
(test::assert-true (lambda? (fn () ())))
(test::assert-true (lambda? caar))
(test::assert-false (lambda? 1))
(test::assert-false (lambda? if))
"#,
        ),
    );
    data.insert(
        interner.intern("macro?"),
        Expression::make_function(
            builtin_is_macro,
            r#"Usage: (macro? expression)

True if the expression is a macro, false otherwise.

Section: type

Example:
(test::assert-true (macro? (macro () ())))
(test::assert-true (macro? defn))
(test::assert-false (macro? 1))
(test::assert-false (macro? if))
"#,
        ),
    );
    data.insert(
        interner.intern("vec?"),
        Expression::make_function(
            builtin_is_vec,
            r#"Usage: (vec? expression)

True if the expression is a vector, false otherwise.

Section: type

Example:
(test::assert-true (vec? '#(1 2 3)) "reader macro")
(test::assert-true (vec? (make-vec)) "make-vec")
(test::assert-true (vec? (vec 1 2 3)) "vec")
(test::assert-false (vec? 1))
(test::assert-false (vec? '(1 2 3)))
(test::assert-false (vec? (list)))
"#,
        ),
    );
    data.insert(
        interner.intern("pair?"),
        Expression::make_function(
            builtin_is_pair,
            r#"Usage: (pair? expression)

True if the expression is a pair, false otherwise.

Section: type

Example:
(test::assert-true (pair? '(1 . 2)) "reader macro")
(test::assert-true (pair? (join 1 2)) "join")
(test::assert-true (pair? '(1 2)))
(test::assert-false (pair? 1))
(test::assert-false (pair? '#(1 2 3)))
(test::assert-false (pair? (vec)))
"#,
        ),
    );
    data.insert(
        interner.intern("builtin?"),
        Expression::make_function(
            builtin_is_builtin,
            r#"Usage: (builtin? expression)

True if the expression is a builtin function or special form, false otherwise.

Section: type

Example:
(test::assert-true (builtin? type))
(test::assert-true (builtin? if))
(test::assert-false (builtin? (fn () ())))
(test::assert-false (builtin? caar))
(test::assert-false (builtin? 1))
"#,
        ),
    );
    data.insert(
        interner.intern("process?"),
        Expression::make_function(
            builtin_is_process,
            r#"Usage: (process? expression)

True if the expression is a process, false otherwise.

Section: type

Example:
(test::assert-true (process? (syscall 'true)))
(test::assert-true (process? (fork ((fn () nil)))))
(test::assert-false (process? (fn () ())))
(test::assert-false (process? caar))
(test::assert-false (process? 1))
"#,
        ),
    );
    data.insert(
        interner.intern("file?"),
        Expression::make_function(
            builtin_is_file,
            r#"Usage: (file? expression)

True if the expression is a file, false otherwise.

Section: type

Example:
(test::assert-true (file? (open :stdout)))
(test::assert-false (file? (fn () ())))
(test::assert-false (file? caar))
(test::assert-false (file? 1))
"#,
        ),
    );
    data.insert(
        interner.intern("hash?"),
        Expression::make_function(
            builtin_is_hash,
            r#"Usage: (hash? expression)

True if the expression is a hash map, false otherwise.

Section: type

Example:
(test::assert-true (hash? (make-hash)) "make-vec")
(test::assert-false (hash? 1))
(test::assert-false (hash? '(1 2 3)))
(test::assert-false (hash? (list)))
(test::assert-false (hash? (vec)))
"#,
        ),
    );
    data.insert(
        interner.intern("list?"),
        Expression::make_function(
            builtin_is_list,
            r#"Usage: (list? expression)

True if the expression is a list, false otherwise.

Section: type

Example:
(test::assert-true (list? '(1 2 3)) "reader macro")
(test::assert-true (list? (list 1 2 3)) "list")
(test::assert-false (list? 1))
(test::assert-false (list? '#(1 2 3)))
(test::assert-false (list? (vec)))
(test::assert-false (list? '(1 . 2)))
"#,
        ),
    );
    data.insert(
        interner.intern("str->int"),
        Expression::make_function(
            builtin_str_to_int,
            r#"Usage: (str->int string) -> int

If string is a valid representation of an integer return that int.  Error if not.

Section: type

Example:
(test::assert-equal 0 (str->int "0"))
(test::assert-equal 101 (str->int "101"))
(test::assert-equal -101 (str->int "-101"))
(test::assert-error (str->int "not int"))
(test::assert-error (str->int "10.0"))
(test::assert-error (str->int "--10"))
"#,
        ),
    );
    data.insert(
        interner.intern("str->float"),
        Expression::make_function(
            builtin_str_to_float,
            r#"Usage: (str->float string) -> float

If string is a valid representation of a float return that float.  Error if not.

Section: type

Example:
(test::assert-equal 0 (str->float "0"))
(test::assert-equal 10.0 (str->float "10.0"))
(test::assert-equal 10.5 (str->float "10.5"))
(test::assert-equal 101 (str->float "101"))
(test::assert-equal -101.95 (str->float "-101.95"))
(test::assert-error (str->float "not int"))
(test::assert-error (str->float "--10"))
"#,
        ),
    );
    data.insert(
        interner.intern("int->float"),
        Expression::make_function(
            builtin_int_to_float,
            r#"Usage: (int->float int) -> float

Cast an int as a float.

Section: type

Example:
(test::assert-equal 0 (int->float 0))
(test::assert-equal 10 (int->float 10))
(test::assert-equal -101 (int->float -101))
(test::assert-error (int->float "not int"))
"#,
        ),
    );
    data.insert(
        interner.intern("float->int"),
        Expression::make_function(
            builtin_float_to_int,
            r#"Usage: (float->int float) -> int

Cast a float as an int.  Truncates.

Section: type

Example:
(test::assert-equal 0 (float->int 0.0))
(test::assert-equal 10 (float->int 10.0))
(test::assert-equal 10 (float->int 10.1))
(test::assert-equal 10 (float->int 10.5))
(test::assert-equal 10 (float->int 10.9))
(test::assert-equal -101 (float->int -101.99))
(test::assert-error (float->int "not int"))
"#,
        ),
    );
    data.insert(
        interner.intern("sym"),
        Expression::make_function(
            builtin_to_symbol,
            r#"Usage: (sym expression+) -> symbol

Takes one or more forms, converts them to strings, concatenates them and returns
a symbol with that name.

Section: type

Example:
(def test-to-symbol-sym nil)
(test::assert-true (symbol? (sym 55)))
(test::assert-true (symbol? (sym 55.0)))
(test::assert-true (symbol? (sym "to-symbol-test-new-symbol")))
(test::assert-true (symbol? (sym (str "to-symbol-test-new-symbol-buf"))))
(test::assert-true (symbol? (sym 'test-to-symbol-sym)))
(set! test-to-symbol-sym "testing-sym")
(test::assert-equal "testing-sym" (sym->str (sym test-to-symbol-sym)))
(test::assert-true (symbol? (sym (sym->str 'test-to-symbol-sym))))
"#,
        ),
    );
    data.insert(
        interner.intern("sym->str"),
        Expression::make_function(
            builtin_symbol_to_str,
            r#"Usage: (sym->str symbol) -> string

Convert a symbol to the string representation representation of it's name.

The string will be the symbol name as a string.

Section: type

Example:
(def test-sym->str-sym nil)
(test::assert-true (string? (sym->str 'test-sym->str-sym)))
(test::assert-equal "test-sym->str-sym" (sym->str 'test-sym->str-sym))
"#,
        ),
    );
    data.insert(
        interner.intern("falsey?"),
        Expression::make_function(
            builtin_falsey,
            r#"Usage: (falsey? under-test) -> bool

Returns true if the expression under-test evaluates to nil or false.

Section: type

Example:
(test::assert-true (falsey? nil))
(test::assert-true (falsey? #f))
(test::assert-false (falsey? #t))
(test::assert-false (falsey? "false"))
"#,
        ),
    );
}
