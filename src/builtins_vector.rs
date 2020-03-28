use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::BuildHasher;
use std::io;
use std::iter::FromIterator;
use std::rc::Rc;

use crate::environment::*;
use crate::eval::*;
use crate::interner::*;
use crate::types::*;

fn builtin_vec(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    let mut new_args: Vec<Expression> = Vec::new();
    for a in args {
        new_args.push(eval(environment, a)?);
    }
    Ok(Expression::with_list(new_args))
}

fn builtin_make_vec(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    let list = if let Some(cap) = args.next() {
        let cap = eval(environment, cap)?;
        let cap = if let Expression::Atom(Atom::Int(c)) = cap {
            c
        } else {
            let msg = format!("make-vec first arg must be an integer, found {:?}", cap);
            return Err(io::Error::new(io::ErrorKind::Other, msg));
        };
        let mut list = Vec::with_capacity(cap as usize);
        if let Some(item) = args.next() {
            if args.next().is_some() {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "make-vec takes at most two forms",
                ));
            }
            let item = eval(environment, item)?;
            for _ in 0..cap {
                list.push(item.clone());
            }
        }
        list
    } else {
        return Ok(Expression::Vector(Rc::new(RefCell::new(Vec::new())), None));
    };
    Ok(Expression::with_list(list))
}

fn builtin_vec_slice(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    let (vec, start, end, has_end) = if let Some(vec) = args.next() {
        if let Some(start) = args.next() {
            let start = if let Expression::Atom(Atom::Int(i)) = eval(environment, start)? {
                i as usize
            } else {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "vec-slice second arg must be an integer",
                ));
            };
            if let Some(end) = args.next() {
                let end = if let Expression::Atom(Atom::Int(i)) = eval(environment, end)? {
                    i as usize
                } else {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "vec-slice third arg must be an integer",
                    ));
                };
                (eval(environment, vec)?, start, end, true)
            } else {
                (eval(environment, vec)?, start, 0, false)
            }
        } else {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "vec-slice takes two or three forms",
            ));
        }
    } else {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "vec-slice takes two or three forms",
        ));
    };
    match &vec {
        Expression::Vector(list, _) => {
            let list = list.borrow();
            if !list.is_empty() {
                let len = list.len();
                if start == len {
                    return Ok(Expression::nil());
                }
                if start > (len - 1) || end > len {
                    let msg = format!(
                        "vec-slice index out of range (start  {}, end {}, length {})",
                        start, end, len
                    );
                    return Err(io::Error::new(io::ErrorKind::Other, msg));
                }
                let slice = if has_end {
                    Vec::from_iter(list[start..end].iter().cloned())
                } else {
                    Vec::from_iter(list[start..].iter().cloned())
                };
                Ok(Expression::with_list(slice))
            } else {
                Ok(Expression::nil())
            }
        }
        _ => Err(io::Error::new(
            io::ErrorKind::Other,
            "vec-slice operates on a vector",
        )),
    }
}

fn builtin_vec_nth(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(idx) = args.next() {
        if let Some(list) = args.next() {
            if args.next().is_none() {
                if let Expression::Atom(Atom::Int(idx)) = eval(environment, &idx)? {
                    if let Expression::Vector(list, _) = eval(environment, &list)? {
                        let list = list.borrow();
                        if idx < 0 || idx >= list.len() as i64 {
                            return Err(io::Error::new(
                                io::ErrorKind::Other,
                                "vec-nth index out of range",
                            ));
                        }
                        return Ok(list.get(idx as usize).unwrap().clone());
                    }
                }
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "vec-nth takes two forms (int and vector)",
    ))
}

// Destructive
fn builtin_vec_setnth(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(idx) = args.next() {
        if let Some(new_element) = args.next() {
            if let Some(list) = args.next() {
                if args.next().is_none() {
                    let idx = if let Expression::Atom(Atom::Int(i)) = eval(environment, idx)? {
                        i
                    } else {
                        return Err(io::Error::new(
                            io::ErrorKind::Other,
                            "vec-setnth! first form must be an int",
                        ));
                    };
                    let new_element = eval(environment, new_element)?;
                    return match eval(environment, list)? {
                        Expression::Vector(list, _) => {
                            if idx < 0 || idx >= list.borrow().len() as i64 {
                                return Err(io::Error::new(
                                    io::ErrorKind::Other,
                                    "vec-setnth! index out of range",
                                ));
                            }
                            list.borrow_mut()[idx as usize] = new_element;
                            Ok(Expression::Vector(list, None))
                        }
                        _ => Err(io::Error::new(
                            io::ErrorKind::Other,
                            "vec-setnth! third form must be a vector",
                        )),
                    };
                }
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "vec-setnth! takes three forms (index, new element and vector)",
    ))
}

// Destructive
fn builtin_vec_push(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(list) = args.next() {
        if let Some(new_item) = args.next() {
            if args.next().is_none() {
                let new_item = eval(environment, new_item)?;
                return match eval(environment, list)? {
                    Expression::Vector(list, _) => {
                        list.borrow_mut().push(new_item);
                        Ok(Expression::Vector(list, None))
                    }
                    _ => Err(io::Error::new(
                        io::ErrorKind::Other,
                        "vec-push!'s first form must be a vector",
                    )),
                };
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "vec-push! takes two forms (vector and form)",
    ))
}

// Destructive
fn builtin_vec_pop(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(list) = args.next() {
        if args.next().is_none() {
            return match eval(environment, list)? {
                Expression::Vector(list, _) => {
                    if let Some(item) = list.borrow_mut().pop() {
                        Ok(item)
                    } else {
                        Ok(Expression::nil())
                    }
                }
                _ => Err(io::Error::new(
                    io::ErrorKind::Other,
                    "vec-pop!'s first form must be a vector",
                )),
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "vec-pop! takes a vector",
    ))
}

fn builtin_vec_is_empty(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(list) = args.next() {
        if args.next().is_none() {
            let list = eval(environment, list)?;
            return match list {
                Expression::Vector(list, _) => {
                    if list.borrow().is_empty() {
                        Ok(Expression::Atom(Atom::True))
                    } else {
                        Ok(Expression::nil())
                    }
                }
                _ => Err(io::Error::new(
                    io::ErrorKind::Other,
                    "vec-empty?'s first form must be a vector",
                )),
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "vec-empty? takes a vector",
    ))
}

// Destructive
fn builtin_vec_vclear(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(list) = args.next() {
        if args.next().is_none() {
            let list = eval(environment, list)?;
            return match list {
                Expression::Vector(list, _) => {
                    list.borrow_mut().clear();
                    Ok(Expression::nil())
                }
                _ => Err(io::Error::new(
                    io::ErrorKind::Other,
                    "vec-clear!'s first form must be a vector",
                )),
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "vec-clear! takes a vector",
    ))
}

// Destructive
fn builtin_vec_remove_nth(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(idx) = args.next() {
        if let Some(list) = args.next() {
            if args.next().is_none() {
                let idx = eval(environment, idx)?;
                let list = eval(environment, list)?;
                let idx = if let Expression::Atom(Atom::Int(i)) = idx {
                    i
                } else {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "vec-remove-nth! first form must be an int",
                    ));
                };
                return match list {
                    Expression::Vector(list, _) => {
                        if idx < 0 || idx >= list.borrow().len() as i64 {
                            return Err(io::Error::new(
                                io::ErrorKind::Other,
                                "vec-remove-nth! index out of range",
                            ));
                        }
                        list.borrow_mut().remove(idx as usize);
                        Ok(Expression::Vector(list, None))
                    }
                    _ => Err(io::Error::new(
                        io::ErrorKind::Other,
                        "vec-remove-nth! second form must be a vector",
                    )),
                };
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "vec-remove-nth! takes two forms (index and vector)",
    ))
}

// Destructive
fn builtin_vec_insert_nth(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(idx) = args.next() {
        if let Some(new_element) = args.next() {
            if let Some(list) = args.next() {
                if args.next().is_none() {
                    let idx = eval(environment, idx)?;
                    let new_element = eval(environment, new_element)?;
                    let list = eval(environment, list)?;
                    let idx = if let Expression::Atom(Atom::Int(i)) = idx {
                        i
                    } else {
                        return Err(io::Error::new(
                            io::ErrorKind::Other,
                            "vec-insert-nth! first form must be an int",
                        ));
                    };
                    return match list {
                        Expression::Vector(list, _) => {
                            if idx < 0 || idx > list.borrow().len() as i64 {
                                return Err(io::Error::new(
                                    io::ErrorKind::Other,
                                    "vec-insert-nth! index out of range",
                                ));
                            }
                            list.borrow_mut().insert(idx as usize, new_element);
                            Ok(Expression::Vector(list, None))
                        }
                        _ => Err(io::Error::new(
                            io::ErrorKind::Other,
                            "vec-insert-nth! third form must be a vector",
                        )),
                    };
                }
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "vec-insert-nth! takes three forms (index, new element and vector)",
    ))
}

pub fn add_vec_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, Rc<Reference>, S>,
) {
    let root = interner.intern("root");
    data.insert(
        interner.intern("vec"),
        Rc::new(Expression::make_function(
            builtin_vec,
            "Usage: (vec item1 item2 .. itemN)

Make a new vector with items.

Example:
(test::assert-false (vec))
(test::assert-equal '(1 2 3) (vec 1 2 3))
",
            root,
        )),
    );
    data.insert(
        interner.intern("make-vec"),
        Rc::new(Expression::make_function(
            builtin_make_vec,
            "Usage: (make-vec capacity default)

Make a new vector with capacity and default item(s).

Example:
(test::assert-false (make-vec))
(test::assert-equal '(x x x) (make-vec 3 'x))
(test::assert-equal '(nil nil nil nil nil) (make-vec 5 nil))
(test::assert-equal '() (make-vec 5))
",
            root,
        )),
    );
    data.insert(
        interner.intern("vec-slice"),
        Rc::new(Expression::make_function(
            builtin_vec_slice,
            "Usage: (vec-slice vector start end?)

Returns a slice of a vector (0 based indexes, end is exclusive).

Example:
(test::assert-equal '(5 6) (vec-slice '#(1 2 3 4 5 6) 4 6))
(test::assert-equal '(1 2 3) (vec-slice '#(1 2 3 4 5 6) 0 3))
(test::assert-equal '(3 4 5) (vec-slice '#(1 2 3 4 5 6) 2 5))
(test::assert-equal '(3 4 5 6) (vec-slice '#(1 2 3 4 5 6) 2))
",
            root,
        )),
    );
    data.insert(
        interner.intern("vec-nth"),
        Rc::new(Expression::make_function(
            builtin_vec_nth,
            "Usage: (vec-nth index vector)

Get the nth element (0 based) of a vector.

Example:
(test::assert-equal 5 (vec-nth 4 '#(1 2 3 4 5 6)))
(test::assert-equal 1 (vec-nth 0 '#(1 2 3 4 5 6)))
(test::assert-equal 3 (vec-nth 2 '#(1 2 3 4 5 6)))
(test::assert-equal 6 (vec-nth 5 '#(1 2 3 4 5 6)))
",
            root,
        )),
    );
    data.insert(
        interner.intern("vec-setnth!"),
        Rc::new(Expression::make_function(
            builtin_vec_setnth,
            "Usage: (vec-setnth! index value vector)

Set the nth index (0 based) of a vector to value.  This is destructive!

Example:
(def 'test-setnth-vec (vec 1 2 3))
(test::assert-equal '(1 5 3) (vec-setnth! 1 5 test-setnth-vec))
(test::assert-equal '(7 5 3) (vec-setnth! 0 7 test-setnth-vec))
(test::assert-equal '(7 5 9) (vec-setnth! 2 9 test-setnth-vec))
",
            root,
        )),
    );
    data.insert(
        interner.intern("vec-push!"),
        Rc::new(Expression::make_function(
            builtin_vec_push,
            "Usage: (vec-push! vector object)

Pushes the provided object onto the end of the vector.  This is destructive!

Example:
(def 'test-push-vec (vec))
(test::assert-equal '(1) (vec-push! test-push-vec 1))
(test::assert-equal '(1) test-push-vec)
(test::assert-equal '(1 2) (vec-push! test-push-vec 2))
(test::assert-equal '(1 2) test-push-vec)
(test::assert-equal '(1 2 3) (vec-push! test-push-vec 3))
(test::assert-equal '(1 2 3) test-push-vec)
",
            root,
        )),
    );
    data.insert(
        interner.intern("vec-pop!"),
        Rc::new(Expression::make_function(
            builtin_vec_pop,
            "Usage: (vec-pop! vector object)

Pops the last object off of the end of the vector.  This is destructive!

Example:
(def 'test-pop-vec (vec 1 2 3))
(test::assert-equal 3 (vec-pop! test-pop-vec))
(test::assert-equal '(1 2) test-pop-vec)
(test::assert-equal 2 (vec-pop! test-pop-vec))
(test::assert-equal '(1) test-pop-vec)
(test::assert-equal 1 (vec-pop! test-pop-vec))
(test::assert-equal '() test-pop-vec)
",
            root,
        )),
    );
    data.insert(
        interner.intern("vec-empty?"),
        Rc::new(Expression::make_function(
            builtin_vec_is_empty,
            "Usage: (vec-empty? vector)

True if the vector is empty.

Example:
(test::assert-true (vec-empty? '#()))
(test::assert-false (vec-empty? '#(1 2 3)))
",
            root,
        )),
    );
    data.insert(
        interner.intern("vec-clear!"),
        Rc::new(Expression::make_function(
            builtin_vec_vclear,
            "Usage: (vec-clear! vector)

Clears a vector.  This is destructive!

Example:
(def 'test-clear-vec (vec 1 2 3))
(test::assert-false (vec-empty? test-clear-vec))
(vec-clear! test-clear-vec)
(test::assert-true (vec-empty? test-clear-vec))
",
            root,
        )),
    );
    data.insert(
        interner.intern("vec-remove-nth!"),
        Rc::new(Expression::make_function(
            builtin_vec_remove_nth,
            "Usage: (vec-remove-nth! index vector)

Remove the element at index from vector.  This is destructive!

Example:
(def 'test-remove-nth-vec (vec 1 2 3))
(test::assert-equal '(1 2 3) test-remove-nth-vec)
(vec-remove-nth! 1 test-remove-nth-vec)
(test::assert-equal '(1 3) test-remove-nth-vec)
(vec-remove-nth! 1 test-remove-nth-vec)
(test::assert-equal '(1) test-remove-nth-vec)
(vec-remove-nth! 0 test-remove-nth-vec)
(test::assert-equal '() test-remove-nth-vec)
",
            root,
        )),
    );
    data.insert(
        interner.intern("vec-insert-nth!"),
        Rc::new(Expression::make_function(
            builtin_vec_insert_nth,
            "Usage: (vec-insert-nth! index new-element vector)

Inserts new-element at index and moves following elements right in vector.  This is destructive!

Example:
(def 'test-insert-nth-vec (vec 1 2 3))
(test::assert-equal '(1 2 3) test-insert-nth-vec)
(vec-insert-nth! 1 5 test-insert-nth-vec)
(test::assert-equal '(1 5 2 3) test-insert-nth-vec)
(vec-insert-nth! 2 6 test-insert-nth-vec)
(test::assert-equal '(1 5 6 2 3) test-insert-nth-vec)
(vec-insert-nth! 0 4 test-insert-nth-vec)
(test::assert-equal '(4 1 5 6 2 3) test-insert-nth-vec)
",
            root,
        )),
    );
}
