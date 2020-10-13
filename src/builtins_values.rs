use std::collections::HashMap;
use std::hash::BuildHasher;

use crate::builtins_util::*;
use crate::environment::*;
use crate::eval::*;
use crate::gc::Handle;
use crate::interner::*;
use crate::types::*;

fn builtin_values(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let mut vals: Vec<Handle> = Vec::new();
    for a in args {
        vals.push(eval(environment, a)?.handle_no_root());
    }
    Ok(Expression::alloc_data(ExpEnum::Values(vals)))
}

fn builtin_values_nth(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let idx = param_eval(environment, args, "values-nth")?;
    let vals = param_eval(environment, args, "values-nth")?;
    params_done(args, "values-nth")?;
    if let ExpEnum::Int(idx) = &idx.get().data {
        if let ExpEnum::Values(vals) = &vals.get().data {
            if *idx < 0 || *idx >= vals.len() as i64 {
                let msg = format!("values-nth: index {} out of range {}", idx, vals.len());
                return Err(LispError::new(msg));
            }
            return Ok(vals[*idx as usize].clone().into());
        }
    }
    Err(LispError::new(
        "values-nth: Requires an int and multi values object, see (doc 'values-nth) for usage.",
    ))
}

fn builtin_values_length(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let vals = param_eval(environment, args, "values-length")?;
    params_done(args, "values-length")?;
    if let ExpEnum::Values(vals) = &vals.get().data {
        return Ok(Expression::alloc_data(ExpEnum::Int(vals.len() as i64)));
    }
    Err(LispError::new(
        "values-length: Requires a muti values object, see (doc 'values-length) for usage.",
    ))
}

pub fn add_values_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, Reference, S>,
) {
    let root = interner.intern("root");
    data.insert(
        interner.intern("values"),
        Expression::make_function(
            builtin_values,
            "Usage: (values expression*)

Produces a multi values object.  Useful for returning more then one value from
a function when most of time you only care about the first (primary) item.  When
evaluting a muti values object it will evaluate as if it the first item only.

Section: core

Example:
(test::assert-true (values? (values 1 \"str\" 5.5)))
(test::assert-equal 1 (values-nth 0 (values 1 \"str\" 5.5)))
(test::assert-equal \"str\" (values-nth 1 (values 1 \"str\" 5.5)))
(test::assert-equal 5.5 (values-nth 2 (values 1 \"str\" 5.5)))
",
            root,
        ),
    );
    data.insert(
        interner.intern("values-nth"),
        Expression::make_function(
            builtin_values_nth,
            "Usage: (values-nth idx expression)

If expression is a values object then return the item at index idx.

Section: core

Example:
(test::assert-equal 1 (values-nth 0 (values 1 \"str\" 5.5)))
(test::assert-equal \"str\" (values-nth 1 (values 1 \"str\" 5.5)))
(test::assert-equal 5.5 (values-nth 2 (values 1 \"str\" 5.5)))
(def test-vals-nth (values 1 \"str\" 5.5))
(test::assert-equal 1 (values-nth 0 test-vals-nth))
(test::assert-equal \"str\" (values-nth 1 test-vals-nth))
(test::assert-equal 5.5 (values-nth 2 test-vals-nth))
",
            root,
        ),
    );
    data.insert(
        interner.intern("values-length"),
        Expression::make_function(
            builtin_values_length,
            "Usage: (values-length expression)

If expression is a values object then return it's length (number of values).

Section: core

Example:
(test::assert-equal 3 (values-length (values 1 \"str\" 5.5)))
(test::assert-equal 2 (values-length (values 1 \"str\")))
(test::assert-equal 1 (values-length (values \"str\")))
(test::assert-equal 0 (values-length (values)))
(test::assert-equal \"str\" (values-nth 1 (values 1 \"str\" 5.5)))
(test::assert-equal 5.5 (values-nth 2 (values 1 \"str\" 5.5)))
(def test-vals-len (values 1 \"str\" 5.5))
(test::assert-equal 3 (values-length test-vals-len))
",
            root,
        ),
    );
}
