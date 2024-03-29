use sl_sh_proc_macros::sl_sh_fn;
use std::collections::HashMap;
use std::hash::BuildHasher;
use std::num::{ParseFloatError, ParseIntError};

use crate::builtins_util::*;
use crate::environment::*;
use crate::interner::*;
use crate::types::*;
use crate::{LispError, LispResult, VarArgs};

/// Usage: (type expression)
///
/// Return the type of the given expression as a string.
///
/// Types are:
///     True
///     False
///     Float
///     Int
///     Symbol
///     String
///     Char
///     Lambda
///     Macro
///     Process
///     SpecialForm
///     Function
///     Vector
///     Pair
///     Nil
///     HashMap
///     File
///
/// Section: type
///
/// Example:
/// (test::assert-equal "True" (type #t))
/// (test::assert-equal "False" (type #f))
/// (test::assert-equal "Float" (type 1.1))
/// (test::assert-equal "Int" (type 1))
/// (test::assert-equal "Symbol" (type 'symbol))
/// (def type-sym 'symbol)
/// (test::assert-equal "Symbol" (type type-sym))
/// (test::assert-equal "String" (type "string"))
/// (test::assert-equal "Char" (type #\a))
/// (test::assert-equal "Lambda" (type (fn () ())))
/// (test::assert-equal "Macro" (type (macro () ())))
/// (test::assert-equal "Process" (type (syscall 'true)))
/// (test::assert-equal "SpecialForm" (type if))
/// (test::assert-equal "Function" (type type))
/// (test::assert-equal "Vector" (type '#(1 2 3)))
/// (def type-vec '#(4 5 6))
/// (test::assert-equal "Vector" (type type-vec))
/// (test::assert-equal "Pair" (type '(1 . 2)))
/// (test::assert-equal "Pair" (type '(1 2 3)))
/// (test::assert-equal "Nil" (type nil))
/// (test::assert-equal "Nil" (type '()))
/// (test::assert-equal "HashMap" (type (make-hash)))
/// (test::assert-equal "File" (type (open :stdin)))
#[sl_sh_fn(fn_name = "type")]
fn to_type(exp: Expression) -> String {
    exp.display_type()
}

/// Usage: (values? expression)
///
/// True if the expression is multi values object, false otherwise.
/// NOTE: A values object will ALSO be the type of its first value.
///
/// Section: type
///
/// Example:
/// (test::assert-true (values? (values 1 "str" 5.5)))
/// (test::assert-false (values? '(1 2 3)))
/// (test::assert-false (values? '(1 . 3)))
/// (test::assert-false (values? 1))
/// (test::assert-true (int? (values 1 "str" 5.5)))
/// (test::assert-false (string? (values 1 "str" 5.5)))
/// (test::assert-false (float? (values 1 "str" 5.5)))
/// (def test-is-values (values 1 2 3 "string" 1.5))
/// (test::assert-true (values? test-is-values))
/// (test::assert-true (int? test-is-values))
/// (test::assert-false (string? test-is-values))
/// (test::assert-false (float? test-is-values))
#[sl_sh_fn(fn_name = "values?", eval_values = true)]
fn is_values(exp: Expression) -> bool {
    matches!(exp.get().data, ExpEnum::Values(_))
}

/// Usage: (nil? expression)
///
/// True if the expression is nil, false otherwise.
///
/// Section: type
///
/// Example:
/// (test::assert-true (nil? nil))
/// (test::assert-false (nil? #t))
#[sl_sh_fn(fn_name = "nil?")]
fn is_nil(exp: Expression) -> bool {
    exp.is_nil()
}

/// Usage: (none? expression)
///
/// True if the expression is nil (aka none/nothing), false otherwise.
///
/// Section: type
///
/// Example:
/// (test::assert-true (none? nil))
/// (test::assert-true (none? '()))
/// (test::assert-false (none? #t))
/// (test::assert-false (none? '(1)))
#[sl_sh_fn(fn_name = "none?")]
fn is_none(exp: Expression) -> bool {
    exp.is_nil()
}

/// Usage: (some? expression)
///
/// True if the expression is NOT nil (aka something), false otherwise.
/// Note that anything other then nil (including false) is something.
///
/// Section: type
///
/// Example:
/// (test::assert-false (some? nil))
/// (test::assert-false (some? '()))
/// (test::assert-true (some? #t))
/// (test::assert-true (some? '(1)))
#[sl_sh_fn(fn_name = "some?")]
fn is_some(exp: Expression) -> bool {
    !exp.is_nil()
}

/// Usage: (true? expression)
///
/// True if the expression is true(#t) (true type NOT non-nil), false otherwise.
///
/// Section: type
///
/// Example:
/// (test::assert-true (true? #t))
/// (test::assert-false (true? #f))
/// (test::assert-false (true? nil))
/// (test::assert-false (true? 1))
/// (test::assert-false (true? "str"))
#[sl_sh_fn(fn_name = "true?")]
fn is_true(exp: Expression) -> bool {
    matches!(exp.get().data, ExpEnum::True)
}

/// Usage: (false? expression)
///
/// True if the expression is false(#f) (false type NOT nil), false otherwise.
///
/// Section: type
///
/// Example:
/// (test::assert-true (false? #f))
/// (test::assert-false (false? nil))
/// (test::assert-false (false? nil))
/// (test::assert-false (false? 1))
/// (test::assert-false (false? "str"))
#[sl_sh_fn(fn_name = "false?")]
fn is_false(exp: Expression) -> bool {
    matches!(exp.get().data, ExpEnum::False)
}

/// Usage: (boolean? expression)
///
/// True if the expression is true(#t) or false(#f), false otherwise.
///
/// Section: type
///
/// Example:
/// (test::assert-true (boolean? #f))
/// (test::assert-true (boolean? #t))
/// (test::assert-false (boolean? nil))
/// (test::assert-false (boolean? nil))
/// (test::assert-false (boolean? 1))
/// (test::assert-false (boolean? "str"))
#[sl_sh_fn(fn_name = "boolean?")]
fn is_boolean(exp: Expression) -> bool {
    matches!(exp.get().data, ExpEnum::True | ExpEnum::False)
}

/// Usage: (float? expression)
///
/// True if the expression is a float, false otherwise.
///
/// Section: type
///
/// Example:
/// (test::assert-true (float? 1.5))
/// (test::assert-false (float? 1))
#[sl_sh_fn(fn_name = "float?")]
fn is_float(exp: Expression) -> bool {
    matches!(exp.get().data, ExpEnum::Float(_))
}

/// Usage: (regex? expression)
///
/// True if the expression is a regex, false otherwise.
///
/// Section: type
///
/// Example:
/// (test::assert-true (regex? (make-regex "\d{2}-\d{2}-\d{4}")))
/// (test::assert-true (regex? #/[a-z]+/))
/// (test::assert-false (regex? 1.5))
#[sl_sh_fn(fn_name = "regex?")]
fn is_regex(exp: Expression) -> bool {
    matches!(exp.get().data, ExpEnum::Regex(_))
}

/// Usage: (int? expression)
///
/// True if the expression is an int, false otherwise.
///
/// Section: type
///
/// Example:
/// (test::assert-true (int? 1))
/// (test::assert-false (int? 1.5))
#[sl_sh_fn(fn_name = "int?")]
fn is_int(exp: Expression) -> bool {
    matches!(exp.get().data, ExpEnum::Int(_))
}

/// Usage: (symbol? expression)
///
/// True if the expression is a symbol, false otherwise.
///
/// Section: type
///
/// Example:
/// (test::assert-true (symbol? 'symbol))
/// (test::assert-false (symbol? 1))
#[sl_sh_fn(fn_name = "symbol?")]
fn is_symbol(exp: Expression) -> bool {
    matches!(exp.get().data, ExpEnum::Symbol(_, _))
}

/// Usage: (string? expression)
///
/// True if the expression is a string, false otherwise.
///
/// Section: type
///
/// Example:
/// (test::assert-true (string? "string"))
/// (test::assert-false (string? 1))
#[sl_sh_fn(fn_name = "string?")]
fn is_string(exp: &Expression) -> bool {
    matches!(exp.get().data, ExpEnum::String(_, _))
}

/// Usage: (char? expression)
///
/// True if the expression is a char, false otherwise.
///
/// Section: type
///
/// Example:
/// (test::assert-true (char? #\a))
/// (test::assert-false (char? 1))
/// (test::assert-false (char? "a"))
#[sl_sh_fn(fn_name = "char?")]
fn is_char(exp: Expression) -> bool {
    matches!(exp.get().data, ExpEnum::Char(_))
}

/// Usage: (lambda? expression)
///
/// True if the expression is a lambda, false otherwise.
///
/// Section: type
///
/// Example:
/// (test::assert-true (lambda? (fn () ())))
/// (test::assert-true (lambda? caar))
/// (test::assert-false (lambda? 1))
/// (test::assert-false (lambda? if))
#[sl_sh_fn(fn_name = "lambda?")]
fn is_lambda(exp: Expression) -> bool {
    matches!(exp.get().data, ExpEnum::Lambda(_))
}

/// Usage: (macro? expression)
///
/// True if the expression is a macro, false otherwise.
///
/// Section: type
///
/// Example:
/// (test::assert-true (macro? (macro () ())))
/// (test::assert-true (macro? defn))
/// (test::assert-false (macro? 1))
/// (test::assert-false (macro? if))
#[sl_sh_fn(fn_name = "macro?")]
fn is_macro(exp: Expression) -> bool {
    matches!(exp.get().data, ExpEnum::Macro(_))
}

/// Usage: (vec? expression)
///
/// True if the expression is a vector, false otherwise.
///
/// Section: type
///
/// Example:
/// (test::assert-true (vec? '#(1 2 3)) "reader macro")
/// (test::assert-true (vec? (make-vec)) "make-vec")
/// (test::assert-true (vec? (vec 1 2 3)) "vec")
/// (test::assert-false (vec? 1))
/// (test::assert-false (vec? '(1 2 3)))
/// (test::assert-false (vec? (list)))
#[sl_sh_fn(fn_name = "vec?")]
fn is_vec(exp: Expression) -> bool {
    matches!(exp.get().data, ExpEnum::Vector(_))
}

/// Usage: (pair? expression)
///
/// True if the expression is a pair, false otherwise.
///
/// Section: type
///
/// Example:
/// (test::assert-true (pair? '(1 . 2)) "reader macro")
/// (test::assert-true (pair? (join 1 2)) "join")
/// (test::assert-true (pair? '(1 2)))
/// (test::assert-false (pair? 1))
/// (test::assert-false (pair? '#(1 2 3)))
/// (test::assert-false (pair? (vec)))
#[sl_sh_fn(fn_name = "pair?")]
fn is_pair(exp: Expression) -> bool {
    matches!(exp.get().data, ExpEnum::Pair(_, _))
}

/// Usage: (builtin? expression)
///
/// True if the expression is a builtin function or special form, false otherwise.
///
/// Section: type
///
/// Example:
/// (test::assert-true (builtin? type))
/// (test::assert-true (builtin? if))
/// (test::assert-false (builtin? (fn () ())))
/// (test::assert-false (builtin? caar))
/// (test::assert-false (builtin? 1))
#[sl_sh_fn(fn_name = "builtin?")]
fn is_builtin(exp: Expression) -> bool {
    matches!(
        exp.get().data,
        ExpEnum::Function(_)
            | ExpEnum::DeclareDef
            | ExpEnum::DeclareVar
            | ExpEnum::DeclareFn
            | ExpEnum::DeclareMacro
            | ExpEnum::Quote
            | ExpEnum::BackQuote
    )
}

/// Usage: (process? expression)
///
/// True if the expression is a process, false otherwise.
///
/// Section: type
///
/// Example:
/// (test::assert-true (process? (syscall 'true)))
/// (test::assert-true (process? (fork ((fn () nil)))))
/// (test::assert-false (process? (fn () ())))
/// (test::assert-false (process? caar))
/// (test::assert-false (process? 1))
#[sl_sh_fn(fn_name = "process?")]
fn is_process(exp: Expression) -> bool {
    matches!(exp.get().data, ExpEnum::Process(_))
}

/// "Usage: (file? expression)
///
/// True if the expression is a file, false otherwise.
///
/// Section: type
///
/// Example:
/// (test::assert-true (file? (open :stdout)))
/// (test::assert-false (file? (fn () ())))
/// (test::assert-false (file? caar))
/// (test::assert-false (file? 1))
#[sl_sh_fn(fn_name = "file?")]
fn is_file(exp: Expression) -> bool {
    matches!(exp.get().data, ExpEnum::File(_))
}

/// Usage: (hash? expression)
///
/// True if the expression is a hash map, false otherwise.
///
/// Section: type
///
/// Example:
/// (test::assert-true (hash? (make-hash)) "make-vec")
/// (test::assert-false (hash? 1))
/// (test::assert-false (hash? '(1 2 3)))
/// (test::assert-false (hash? (list)))
/// (test::assert-false (hash? (vec)))
#[sl_sh_fn(fn_name = "hash?")]
fn is_hash(exp: Expression) -> bool {
    matches!(exp.get().data, ExpEnum::HashMap(_))
}

/// Usage: (list? expression)
///
/// True if the expression is a list, false otherwise.
///
/// Section: type
///
/// Example:
/// (test::assert-true (list? '(1 2 3)) "reader macro")
/// (test::assert-true (list? (list 1 2 3)) "list")
/// (test::assert-false (list? 1))
/// (test::assert-false (list? '#(1 2 3)))
/// (test::assert-false (list? (vec)))
/// (test::assert-false (list? '(1 . 2)))
#[sl_sh_fn(fn_name = "list?")]
fn is_list(exp: Expression) -> bool {
    exp.is_nil() || is_proper_list(&exp)
}

/// Usage: (str->int string) -> int
///
/// If string is a valid representation of an integer return that int.  Error if not.
///
/// Section: type
///
/// Example:
/// (test::assert-equal 0 (str->int "0"))
/// (test::assert-equal 101 (str->int "101"))
/// (test::assert-equal -101 (str->int "-101"))
/// (test::assert-error (str->int "not int"))
/// (test::assert-error (str->int "10.0"))
/// (test::assert-error (str->int "--10"))
#[sl_sh_fn(fn_name = "str->int")]
fn str_to_int(istr: &str) -> LispResult<i64> {
    let potential_int: Result<i64, ParseIntError> = istr.parse();
    match potential_int {
        Ok(v) => Ok(v),
        Err(_) => Err(LispError::new("str->int: string is not a valid integer")),
    }
}

/// Usage: (str->float string) -> float
///
/// If string is a valid representation of a float return that float.  Error if not.
///
/// Section: type
///
/// Example:
/// (test::assert-equal 0 (str->float "0"))
/// (test::assert-equal 10.0 (str->float "10.0"))
/// (test::assert-equal 10.5 (str->float "10.5"))
/// (test::assert-equal 101 (str->float "101"))
/// (test::assert-equal -101.95 (str->float "-101.95"))
/// (test::assert-error (str->float "not int"))
/// (test::assert-error (str->float "--10"))
#[sl_sh_fn(fn_name = "str->float")]
fn str_to_float(istr: &str) -> LispResult<f64> {
    let potential_float: Result<f64, ParseFloatError> = istr.parse();
    match potential_float {
        Ok(v) => Ok(v),
        Err(_) => Err(LispError::new("str->float: string is not a valid float")),
    }
}

/// Usage: (int->float int) -> float
///
/// Cast an int as a float.
///
/// Section: type
///
/// Example:
/// (test::assert-equal 0 (int->float 0))
/// (int->float 10))
/// (test::assert-equal -101 (int->float -101))
/// (test::assert-error (int->float "not int"))
#[sl_sh_fn(fn_name = "int->float")]
fn int_to_float(int: i64) -> f64 {
    int as f64
}

/// Usage: (float->int float) -> int
///
/// Cast a float as an int. Narrows the float if necessary
/// to the max allowable int. If float is NaN 0 is returned.
///
/// Section: type
///
/// Example:
/// (test::assert-equal 0 (float->int 0.0))
/// (test::assert-equal 10 (float->int 10.0))
/// (test::assert-equal 10 (float->int 10.1))
/// (test::assert-equal 10 (float->int 10.5))
/// (test::assert-equal 10 (float->int 10.9))
/// (test::assert-equal -101 (float->int -101.99))
/// (test::assert-error (float->int "not int"))
#[sl_sh_fn(fn_name = "float->int")]
fn float_to_int(float: f64) -> i64 {
    match float {
        float if float.is_nan() => 0,
        float if float > i64::MAX as f64 || float == f64::INFINITY => i64::MAX,
        float if float < i64::MIN as f64 || float == f64::NEG_INFINITY => i64::MIN,
        float => float as i64,
    }
}

/// Usage: (sym expression+) -> symbol
///
/// Takes one or more forms, converts them to strings, concatenates them and returns
/// a symbol with that name.
///
/// Section: type
///
/// Example:
/// (def test-to-symbol-sym nil)
/// (test::assert-true (symbol? (sym 55)))
/// (test::assert-true (symbol? (sym 55.0)))
/// (test::assert-true (symbol? (sym "to-symbol-test-new-symbol")))
/// (test::assert-true (symbol? (sym (str "to-symbol-test-new-symbol-buf"))))
/// (test::assert-true (symbol? (sym 'test-to-symbol-sym)))
/// (set! test-to-symbol-sym "testing-sym")
/// (test::assert-equal "testing-sym" (sym->str (sym test-to-symbol-sym)))
/// (test::assert-true (symbol? (sym (sym->str 'test-to-symbol-sym))))
#[sl_sh_fn(fn_name = "sym", takes_env = true)]
fn sym(environment: &mut Environment, args: VarArgs<Expression>) -> LispResult<Expression> {
    let mut res = String::new();
    for a in args {
        res.push_str(&a.as_string(environment)?);
    }
    Ok(Expression::alloc_data(ExpEnum::Symbol(
        environment.interner.intern(&res),
        SymLoc::None,
    )))
}

/// Usage: (sym->str symbol) -> string
///
/// Convert a symbol to the string representation representation of it's name.
///
/// The string will be the symbol name as a string.
///
/// Section: type
///
/// Example:
/// (def test-sym->str-sym nil)
/// (test::assert-true (string? (sym->str 'test-sym->str-sym)))
/// (test::assert-equal "test-sym->str-sym" (sym->str 'test-sym->str-sym))
#[sl_sh_fn(fn_name = "sym->str")]
fn symbol_to_str(exp: &Expression) -> LispResult<String> {
    match exp.get().data {
        ExpEnum::Symbol(s, _) => Ok(s.to_string()),
        _ => Err(LispError::new(
            "sym->str: can only convert a symbol to a string",
        )),
    }
}

/// Usage: (falsey? under-test) -> bool
///
/// Returns true if the expression under-test evaluates to nil or false.
///
/// Section: type
///
/// Example:
/// (test::assert-true (falsey? nil))
/// (test::assert-true (falsey? #f))
/// (test::assert-false (falsey? #t))
/// (test::assert-false (falsey? "false"))
#[sl_sh_fn(fn_name = "falsey?")]
fn is_falsey(exp: Expression) -> bool {
    exp.is_falsey()
}

pub fn add_type_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, (Expression, String), S>,
) {
    intern_to_type(interner, data);
    intern_is_values(interner, data);
    intern_is_nil(interner, data);
    intern_is_none(interner, data);
    intern_is_some(interner, data);
    intern_is_true(interner, data);
    intern_is_false(interner, data);
    intern_is_boolean(interner, data);
    intern_is_float(interner, data);
    intern_is_regex(interner, data);
    intern_is_int(interner, data);
    intern_is_symbol(interner, data);
    intern_is_string(interner, data);
    intern_is_char(interner, data);
    intern_is_lambda(interner, data);
    intern_is_macro(interner, data);
    intern_is_vec(interner, data);
    intern_is_pair(interner, data);
    intern_is_builtin(interner, data);
    intern_is_process(interner, data);
    intern_is_file(interner, data);
    intern_is_hash(interner, data);
    intern_is_list(interner, data);
    intern_str_to_int(interner, data);
    intern_str_to_float(interner, data);
    intern_int_to_float(interner, data);
    intern_float_to_int(interner, data);
    intern_sym(interner, data);
    intern_symbol_to_str(interner, data);
    intern_is_falsey(interner, data);
}

#[cfg(test)]
mod test {
    use super::*;

    impl PartialEq<Self> for Expression {
        fn eq(&self, other: &Self) -> bool {
            match (&self.get().data, &other.get().data) {
                (ExpEnum::True, ExpEnum::True) => true,
                (ExpEnum::False, ExpEnum::False) => true,
                (ExpEnum::Nil, ExpEnum::Nil) => true,
                (ExpEnum::Float(lf), ExpEnum::Float(rt)) => lf == rt,
                (ExpEnum::Int(lf), ExpEnum::Int(rt)) => lf == rt,
                (ExpEnum::Char(lf), ExpEnum::Char(rt)) => lf == rt,
                (ExpEnum::CodePoint(lf), ExpEnum::CodePoint(rt)) => lf == rt,
                (_, _) => false,
            }
            //(ExpEnum::String(lf_s, lf_i), ExpEnum::String(rt_s, rt_i) => {}
            //(ExpEnum::Symbol(_, _), ExpEnum::Symbol(_, _)) => {}
            //(ExpEnum::Regex(_), ExpEnum::Regex(_)) => {}
            //ExpEnum::Symbol(_, _) => {}
            //ExpEnum::Lambda(_) => {}
            //ExpEnum::Macro(_) => {}
            //ExpEnum::Function(_) => {}
            //ExpEnum::LazyFn(_, _) => {}
            //ExpEnum::Vector(_) => {}
            //ExpEnum::Values(_) => {}
            //ExpEnum::Pair(_, _) => {}
            //ExpEnum::HashMap(_) => {}
            //ExpEnum::Process(_) => {}
            //ExpEnum::File(_) => {}
            //ExpEnum::Wrapper(_) => {}
            //ExpEnum::DeclareDef => {}
            //ExpEnum::DeclareVar => {}
            //ExpEnum::DeclareFn => {}
            //ExpEnum::DeclareMacro => {}
            //ExpEnum::Quote => {}
            //ExpEnum::BackQuote => {}
            //ExpEnum::Undefined => {}
        }
    }

    /// Usage: (tuple-vec-to-float int) -> float
    ///     Cast an int as a float.
    /// Section: type
    /// Example:
    ///     (test::assert-equal 34 (tuple-vec-to-float '#(#(7 1 9) #(4 5 8))))
    ///     (test::assert-equal 34 (tuple-vec-to-float (list (join 8 (join 5 4)) (join 9 (join 4 4)))))
    ///     (test::assert-equal 34 (tuple-vec-to-float '((8 5 4) (9 4 4)))
    ///     (test::assert-error (tuple-vec-to-float "not vec"))
    ///     (test::assert-error (tuple-vec-to-float 9))
    #[sl_sh_fn(fn_name = "tuple-vec-to-float")]
    fn tuple_vec_to_float(ints: Vec<(i64, i64, i64)>) -> f64 {
        let mut f = 0.0;
        for i in ints.iter() {
            f += i.0 as f64 + i.1 as f64 + i.2 as f64;
        }
        f
    }

    /// Usage: (tuple-to-float int) -> float
    ///     Cast an int as a float.
    /// Section: type
    /// Example:
    ///     (test::assert-equal 17 (tuple-to-float '#(8 9 0)))
    ///     (test::assert-equal 18 (tuple-to-float '(8 9 1)))
    ///     (test::assert-equal 17 (tuple-to-float (join 8 (join 5 4))))
    ///     (test::assert-error (tuple-to-float "not vec"))
    ///     (test::assert-error (tuple-to-float 8))
    #[sl_sh_fn(fn_name = "tuple-to-float")]
    fn tuple_to_float(i: (i64, i64, i64)) -> f64 {
        i.0 as f64 + i.1 as f64 + i.2 as f64
    }

    /// Usage: (int-pair-float int) -> float
    ///     Cast an int as a float.
    /// Section: type
    /// Example:
    ///     (test::assert-equal 17 (int-pair-float '#(8 9)))
    ///     (test::assert-equal 17 (int-pair-float '(8 9)))
    ///     (test::assert-equal 17 (int-pair-float (join 8 9)))
    ///     (test::assert-error (int-pair-float "not vec"))
    ///     (test::assert-error (int-pair-float 8))
    #[sl_sh_fn(fn_name = "int-pair-float")]
    fn int_pair_float(i: (i64, i64)) -> Option<f64> {
        Some(i.0 as f64 + i.1 as f64)
    }

    /// Usage: (intvec-to-float int) -> float
    ///     Cast an int as a float.
    /// Section: type
    /// Example:
    ///     (test::assert-equal 17 (intvec-to-float '#(8 9)))
    ///     (test::assert-equal 17 (intvec-to-float '(8 9)))
    ///     (test::assert-equal 17 (intvec-to-float (join 8 9)))
    ///     (test::assert-error (intvec-to-float "not vec"))
    ///     (test::assert-error (intvec-to-float "8))
    #[sl_sh_fn(fn_name = "intvec-to-float")]
    fn intvec_to_float(int: Vec<i64>) -> f64 {
        let mut f: f64 = 0.0;
        for i in int {
            f += i as f64
        }
        f
    }

    /// the above four functions generate entrypoints with the names
    /// `parse_<fn_name>` where <fn_name> is the name of the function
    /// with the sl_sh_fn  annotation. This tests that the generated functions
    /// take and receive the proper sl-sh objects.
    #[test]
    fn test_sl_sh_fn() {
        let int8 = Expression::alloc_data(ExpEnum::Int(8));
        let int9 = Expression::alloc_data(ExpEnum::Int(9));
        let f_result = Expression::alloc_data(ExpEnum::Float(17.0));
        let intvec = Expression::alloc_data(ExpEnum::Pair(int8.clone(), int9.clone()));
        let quote = Expression::alloc_data(ExpEnum::Quote);
        let intvec = Expression::alloc_data(ExpEnum::Vector(vec![quote, intvec]));
        let mut env = build_default_environment();

        let result =
            parse_intvec_to_float(&mut env, &mut vec![intvec.clone()].into_iter()).unwrap();
        assert_eq!(f_result, result);

        let result = parse_int_pair_float(&mut env, &mut vec![intvec.clone()].into_iter()).unwrap();
        assert_eq!(f_result, result);

        let int4 = Expression::alloc_data(ExpEnum::Int(4));
        let intvec = Expression::alloc_data(ExpEnum::Pair(
            int9.clone(),
            Expression::alloc_data(ExpEnum::Pair(int4.clone(), int4.clone())),
        ));
        let quote = Expression::alloc_data(ExpEnum::Quote);
        let intvec = Expression::alloc_data(ExpEnum::Vector(vec![quote, intvec]));

        let result = parse_tuple_to_float(&mut env, &mut vec![intvec.clone()].into_iter()).unwrap();
        assert_eq!(f_result, result);

        let intvec = Expression::alloc_data(ExpEnum::Vector(vec![
            int4.clone(),
            int9.clone(),
            int4.clone(),
        ]));
        let quote = Expression::alloc_data(ExpEnum::Quote);
        let intvec = Expression::alloc_data(ExpEnum::Vector(vec![intvec]));
        let intvec = Expression::alloc_data(ExpEnum::Vector(vec![quote, intvec]));
        let result =
            parse_tuple_vec_to_float(&mut env, &mut vec![intvec.clone()].into_iter()).unwrap();
        assert_eq!(f_result, result);
    }
}
