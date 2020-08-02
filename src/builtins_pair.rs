use std::collections::HashMap;
use std::hash::BuildHasher;

use crate::environment::*;
use crate::eval::*;
use crate::gc::*;
use crate::interner::*;
use crate::types::*;

fn builtin_join(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg0) = args.next() {
        if let Some(arg1) = args.next() {
            if args.next().is_none() {
                let arg0: Handle = eval(environment, arg0)?.into();
                let arg1: Handle = eval(environment, arg1)?.into();
                return Ok(Expression::alloc_data(ExpEnum::Pair(
                    arg0.clone_no_root(),
                    arg1.clone_no_root(),
                )));
            }
        }
    }
    Err(LispError::new("join needs two forms"))
}

fn builtin_list(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let mut head: Option<Expression> = None;
    let mut last = head.clone();
    for a in args {
        let a: Handle = eval(environment, a)?.into();
        if let Some(inner_last) = last.clone() {
            if let ExpEnum::Pair(_, e2) = &inner_last.get().data {
                let e2: Expression = e2.into();
                e2.get_mut().data.replace(ExpEnum::Pair(
                    a.clone_no_root(),
                    Expression::make_nil_h().clone_no_root(),
                ));
                // XXX
                gc_mut().down_root(&e2);
                last = Some(e2.clone());
            }
        } else {
            let nil = Expression::make_nil_h().clone_no_root();
            last = Some(Expression::alloc_data(ExpEnum::Pair(
                a.clone_no_root(),
                nil,
            )));
        }
        if head.is_none() {
            head = last.clone();
        }
    }
    Ok(head.unwrap_or_else(Expression::make_nil))
}

fn builtin_car(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return match &arg.get().data {
                ExpEnum::Pair(e1, _) => Ok(e1.into()),
                ExpEnum::Nil => Ok(arg.clone()),
                _ => Err(LispError::new("car requires a pair")),
            };
        }
    }
    Err(LispError::new("car takes one form (pair)"))
}

fn builtin_cdr(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return match &arg.get().data {
                ExpEnum::Pair(_, e2) => Ok(e2.into()),
                ExpEnum::Nil => Ok(arg.clone()),
                _ => Err(LispError::new("cdr requires a pair")),
            };
        }
    }
    Err(LispError::new("cdr takes one form (pair)"))
}

// Destructive
fn builtin_xar(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(pair) = args.next() {
        if let Some(arg) = args.next() {
            if args.next().is_none() {
                let arg = eval(environment, arg)?;
                let pair = eval(environment, pair)?;
                let mut pair_d = pair.get_mut();
                let new_pair = match &pair_d.data {
                    ExpEnum::Pair(_e1, e2) => ExpEnum::Pair(arg.into(), e2.clone_no_root()),
                    ExpEnum::Nil => ExpEnum::Pair(arg.into(), Expression::make_nil_h()),
                    _ => {
                        return Err(LispError::new("xar! requires a pair for it's first form"));
                    }
                };
                pair_d.data.replace(new_pair);
                drop(pair_d);
                // XXX
                gc_mut().down_root(&pair);
                return Ok(pair.clone());
            }
        }
    }
    Err(LispError::new("xar! takes two forms (pair and expression)"))
}

// Destructive
fn builtin_xdr(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(pair) = args.next() {
        if let Some(arg) = args.next() {
            if args.next().is_none() {
                let arg = eval(environment, arg)?;
                let pair = eval(environment, pair)?;
                let mut pair_d = pair.get_mut();
                let new_pair = match &pair_d.data {
                    ExpEnum::Pair(e1, _e2) => ExpEnum::Pair(e1.clone(), arg.into()),
                    ExpEnum::Nil => ExpEnum::Pair(Expression::make_nil_h(), arg.into()),
                    _ => {
                        return Err(LispError::new("xdr! requires a pair for it's first form"));
                    }
                };
                pair_d.data.replace(new_pair);
                drop(pair_d);
                // XXX
                gc_mut().down_root(&pair);
                return Ok(pair.clone());
            }
        }
    }
    Err(LispError::new("xdr! takes two forms (pair and expression)"))
}

pub fn add_pair_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, Reference, S>,
) {
    let root = interner.intern("root");
    data.insert(
        interner.intern("join"),
        Expression::make_function(
            builtin_join,
            "Usage: (join car cdr)
 
Create a pair with the provided car and cdr.

Section: pair

Example:
(def 'tst-pair-one (join 1 2))
(test::assert-equal 1 (car tst-pair-one))
(test::assert-equal 2 (cdr tst-pair-one))
(test::assert-equal '(1 2 3) (join 1 (join 2 (join 3 nil))))
",
            root,
        ),
    );
    data.insert(
        interner.intern("list"),
        Expression::make_function(
            builtin_list,
            "Usage: (list item0 item1 .. itemN)

Create a proper list from pairs with items 0 - N.

Section: pair

Example:
(test::assert-equal '(1 2 3) (list 1 2 3))
",
            root,
        ),
    );
    data.insert(
        interner.intern("car"),
        Expression::make_function(
            builtin_car,
            "Usage: (car pair)

Return the car (first item) from a pair.  If used on a proper list this will be the first element.

Section: pair

Example:
(def 'tst-pairs-two (list 'x 'y 'z))
(test::assert-equal 'x (car tst-pairs-two))
(test::assert-equal 10 (car '(10)))
(test::assert-equal 9 (car '(9 11 13)))
",
            root,
        ),
    );
    data.insert(
        interner.intern("cdr"),
        Expression::make_function(builtin_cdr, "Usage: (cdr pair)

Return the cdr (second item) from a pair.  If used on a proper list this will be the list minus the first element.

Section: pair

Example:
(def 'tst-pairs-three (list 'x 'y 'z))
(test::assert-equal '(y z) (cdr tst-pairs-three))
(test::assert-equal nil (cdr '(10)))
(test::assert-equal '(13) (cdr '(9 13)))
(test::assert-equal '(11 13) (cdr '(9 11 13)))
", root),
    );
    data.insert(
        interner.intern("xar!"),
        Expression::make_function(
            builtin_xar,
            "Usage: (xar! pair expression)

Destructive form that replaces the car (first item) in a pair with a new expression.

If used on a proper list will replace the first item.  Can be used on nil to
create a pair (expression . nil).

Section: pair

Example:
(def 'tst-pairs-three (list 'x 'y 'z))
(test::assert-equal '(x y z) tst-pairs-three)
(test::assert-equal '(s y z) (xar! tst-pairs-three 's))
(test::assert-equal '(s y z) tst-pairs-three)
(def 'tst-pairs-four nil)
(test::assert-equal '() tst-pairs-four)
(test::assert-equal '(t) (xar! tst-pairs-four 't))
(test::assert-equal '(t) tst-pairs-four)
",
            root,
        ),
    );
    data.insert(
        interner.intern("xdr!"),
        Expression::make_function(
            builtin_xdr,
            "Usage: (xdr! pair expression)

Destructive form that replaces the cdr (second item) in a pair with a new expression.

If used on a proper list will replace everthing after the first item.
Can be used on nil to create a pair (nil . expression).

Section: pair

Example:
(def 'tst-pairs-five (list 'a 'b 'c))
(test::assert-equal '(a b c) tst-pairs-five)
(test::assert-equal '(a y z) (xdr! tst-pairs-five '(y z)))
(test::assert-equal '(a y z) tst-pairs-five)
(def 'tst-pairs-six nil)
(test::assert-equal '() tst-pairs-six)
(test::assert-equal '(nil . v) (xdr! tst-pairs-six 'v))
(test::assert-equal '(nil . v) tst-pairs-six)
",
            root,
        ),
    );
}
