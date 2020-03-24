use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::BuildHasher;
use std::io;
use std::rc::Rc;

use crate::environment::*;
use crate::eval::*;
use crate::types::*;

fn builtin_join(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg0) = args.next() {
        if let Some(arg1) = args.next() {
            if args.next().is_none() {
                let arg0 = eval(environment, arg0)?;
                let arg1 = eval(environment, arg1)?;
                return Ok(Expression::Pair(Rc::new(RefCell::new(Some((arg0, arg1))))));
            }
        }
    }
    Err(io::Error::new(io::ErrorKind::Other, "join needs two forms"))
}

fn builtin_list(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    let mut head = Expression::nil();
    let mut last = head.clone();
    for a in args {
        let a = eval(environment, a)?;
        let pair = Expression::Pair(Rc::new(RefCell::new(Some((a, Expression::nil())))));
        if let Expression::Pair(p) = last {
            let p = &mut *p.borrow_mut();
            if let Some(p) = p {
                p.1 = pair.clone();
            }
        }
        last = pair;
        if head.is_nil() {
            head = last.clone();
        }
    }
    Ok(head)
}

fn builtin_car(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return match &arg {
                Expression::Pair(p) => {
                    if let Some((e1, _e2)) = &*p.borrow() {
                        Ok(e1.clone())
                    } else {
                        // Nil
                        Ok(Expression::nil())
                    }
                }
                _ => Err(io::Error::new(io::ErrorKind::Other, "car requires a pair")),
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "car takes one form (pair)",
    ))
}

fn builtin_cdr(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return match &arg {
                Expression::Pair(p) => {
                    if let Some((_e1, e2)) = &*p.borrow() {
                        Ok(e2.clone())
                    } else {
                        Ok(Expression::nil())
                    }
                }
                _ => Err(io::Error::new(io::ErrorKind::Other, "cdr requires a pair")),
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "cdr takes one form (pair)",
    ))
}

// Destructive
fn builtin_xar(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(pair) = args.next() {
        if let Some(arg) = args.next() {
            if args.next().is_none() {
                let arg = eval(environment, arg)?;
                let pair = eval(environment, pair)?;
                if let Expression::Pair(pair) = &pair {
                    let mut new_pair = None;
                    {
                        let pair = &mut *pair.borrow_mut();
                        match pair {
                            None => {
                                new_pair = Some((arg, Expression::nil()));
                            }
                            Some(pair) => {
                                pair.0 = arg;
                            }
                        }
                    }
                    if new_pair.is_some() {
                        pair.replace(new_pair);
                    }
                } else {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "xar! requires a pair for it's first form",
                    ));
                }
                return Ok(pair);
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "xar! takes two forms (pair and expression)",
    ))
}

// Destructive
fn builtin_xdr(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(pair) = args.next() {
        if let Some(arg) = args.next() {
            if args.next().is_none() {
                let arg = eval(environment, arg)?;
                let pair = eval(environment, pair)?;
                if let Expression::Pair(pair) = &pair {
                    let mut new_pair = None;
                    {
                        let pair = &mut *pair.borrow_mut();
                        match pair {
                            None => {
                                new_pair = Some((Expression::nil(), arg));
                            }
                            Some(pair) => {
                                pair.1 = arg;
                            }
                        }
                    }
                    if new_pair.is_some() {
                        pair.replace(new_pair);
                    }
                } else {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "xdr! requires a pair for it's first form",
                    ));
                }
                return Ok(pair);
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "xdr! takes two forms (pair and expression)",
    ))
}

pub fn add_pair_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, Rc<Reference>, S>,
) {
    data.insert(
        interner.intern("join"),
        Rc::new(Expression::make_function(
            builtin_join,
            "Usage: (join car cdr)
 
Create a pair with the provided car and cdr.

Example:
(def 'tst-pair-one (join 1 2))
(test::assert-equal 1 (car tst-pair-one))
(test::assert-equal 2 (cdr tst-pair-one))
(test::assert-equal '(1 2 3) (join 1 (join 2 (join 3 nil))))
",
        )),
    );
    data.insert(
        interner.intern("list"),
        Rc::new(Expression::make_function(
            builtin_list,
            "Usage: (list item0 item1 .. itemN)

Create a proper list from pairs with items 0 - N.

Example:
(test::assert-equal '(1 2 3) (list 1 2 3))
",
        )),
    );
    data.insert(
        interner.intern("car"),
        Rc::new(Expression::make_function(
            builtin_car,
            "Usage: (car pair)

Return the car (first item) from a pair.  If used on a proper list this will be the first element.

Example:
(def 'tst-pairs-two (list 'x 'y 'z))
(test::assert-equal 'x (car tst-pairs-two))
(test::assert-equal 10 (car '(10)))
(test::assert-equal 9 (car '(9 11 13)))
",
        )),
    );
    data.insert(
        interner.intern("cdr"),
        Rc::new(Expression::make_function(builtin_cdr, "Usage: (cdr pair)

Return the cdr (second item) from a pair.  If used on a proper list this will be the list minus the first element.

Example:
(def 'tst-pairs-three (list 'x 'y 'z))
(test::assert-equal '(y z) (cdr tst-pairs-three))
(test::assert-equal nil (cdr '(10)))
(test::assert-equal '(13) (cdr '(9 13)))
(test::assert-equal '(11 13) (cdr '(9 11 13)))
")),
    );
    data.insert(
        interner.intern("xar!"),
        Rc::new(Expression::make_function(
            builtin_xar,
            "Usage: (xar! pair expression)

Destructive form thst replaces the car (first item) in a pair with a new expression.

If used on a proper list will replace the first item.  Can be used on nil to
create a pair (expression . nil).

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
        )),
    );
    data.insert(
        interner.intern("xdr!"),
        Rc::new(Expression::make_function(
            builtin_xdr,
            "Usage: (xdr! pair expression)

Destructive form that replaces the cdr (second item) in a pair with a new expression.

If used on a proper list will replace everthing after the first item.
Can be used on nil to create a pair (nil . expression).

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
        )),
    );
}
