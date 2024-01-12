use std::collections::HashMap;
use std::hash::BuildHasher;

use crate::builtins_util::*;
use crate::environment::*;
use crate::eval::*;
use crate::interner::*;
use crate::types::*;

fn builtin_vec(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let mut new_args: Vec<Expression> = Vec::new();
    for a in args {
        new_args.push(eval(environment, a)?);
    }
    Ok(Expression::with_list(new_args))
}

fn builtin_make_vec(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let list = if let Some(cap) = args.next() {
        let cap = eval(environment, cap)?;
        let cap = if let ExpEnum::Int(c) = cap.get().data {
            c
        } else {
            let msg = format!("make-vec first arg must be an integer, found {:?}", cap);
            return Err(LispError::new(msg));
        };
        let mut list: Vec<Expression> = Vec::with_capacity(cap as usize);
        if let Some(item) = args.next() {
            if args.next().is_some() {
                return Err(LispError::new("make-vec takes at most two forms"));
            }
            let item = eval(environment, item)?;
            for _ in 0..cap {
                // Make a copy of each item instead if using the same item for each.
                list.push(item.duplicate());
            }
        }
        list
    } else {
        return Ok(Expression::alloc_data(ExpEnum::Vector(Vec::new())));
    };
    Ok(Expression::with_list(list))
}

fn builtin_vec_slice(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let (vec, start, end, has_end) = if let Some(vec) = args.next() {
        if let Some(start) = args.next() {
            let start = if let ExpEnum::Int(i) = eval(environment, start)?.get().data {
                if i < 0 {
                    return Err(LispError::new(
                        "vec-slice second arg (start) must be a positive integer",
                    ));
                }
                i as usize
            } else {
                return Err(LispError::new(
                    "vec-slice second arg (start) must be an integer",
                ));
            };
            if let Some(end) = args.next() {
                let end = if let ExpEnum::Int(i) = eval(environment, end)?.get().data {
                    if i < 0 {
                        return Err(LispError::new(
                            "vec-slice third arg (end) must be a positive integer",
                        ));
                    }
                    i as usize
                } else {
                    return Err(LispError::new(
                        "vec-slice third arg (end) must be an integer",
                    ));
                };
                (eval(environment, vec)?, start, end, true)
            } else {
                (eval(environment, vec)?, start, 0, false)
            }
        } else {
            return Err(LispError::new("vec-slice takes two or three forms"));
        }
    } else {
        return Err(LispError::new("vec-slice takes two or three forms"));
    };
    let vec_d = vec.get();
    match &vec_d.data {
        ExpEnum::Vector(list) => {
            if !list.is_empty() {
                let len = list.len();
                if start == len && end <= len {
                    //return Ok(Expression::make_nil());
                    return Ok(Expression::with_list(Vec::new()));
                }
                if start > (len - 1) || end > len {
                    let msg = format!(
                        "vec-slice index out of range (start  {}, end {}, length {})",
                        start, end, len
                    );
                    return Err(LispError::new(msg));
                }
                let slice = if has_end {
                    list[start..end].to_vec()
                } else {
                    list[start..].to_vec()
                };
                Ok(Expression::with_list(slice))
            } else {
                // XXX this probably is no right, empty list is better?
                Ok(Expression::make_nil())
            }
        }
        _ => Err(LispError::new("vec-slice operates on a vector")),
    }
}

fn builtin_vec_nth(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let vector = param_eval(environment, args, "vec-nth")?;
    let idx = param_eval(environment, args, "vec-nth")?;
    params_done(args, "vec-nth")?;
    let idx_d = idx.get();
    if let ExpEnum::Int(idx) = &idx_d.data {
        let vector_d = vector.get();
        if let ExpEnum::Vector(list) = &vector_d.data {
            if *idx < 0 || *idx >= list.len() as i64 {
                let msg = format!("vec-nth: index {} out of range {}", idx, list.len());
                Err(LispError::new(msg))
            } else {
                Ok(list[*idx as usize].clone())
            }
        } else {
            Err(LispError::new(format!(
                "vec-nth: first form must be a vector, got {} {}",
                vector.display_type(),
                vector
            )))
        }
    } else {
        Err(LispError::new("vec-nth: second form must be an int"))
    }
}

// Destructive
fn builtin_vec_set(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let vector = param_eval(environment, args, "vec-set!")?;
    let idx = param_eval(environment, args, "vec-set!")?;
    let obj = param_eval(environment, args, "vec-set!")?;
    params_done(args, "vec-set!")?;
    let idx = if let ExpEnum::Int(i) = idx.get().data {
        i
    } else {
        return Err(LispError::new("vec-set! second form must be an int"));
    };
    let mut vector_d = vector.get_mut();
    match &mut vector_d.data {
        ExpEnum::Vector(vec) => {
            if idx < 0 || idx >= vec.len() as i64 {
                return Err(LispError::new("vec-set! index out of range"));
            }
            vec[idx as usize] = obj;
            Ok(vector.clone())
        }
        _ => Err(LispError::new("vec-set! first form must be a vector")),
    }
}

// Destructive
fn builtin_vec_push(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(list) = args.next() {
        if let Some(new_item) = args.next() {
            if args.next().is_none() {
                let new_item = eval(environment, new_item)?;
                let vec = eval(environment, list)?;
                let mut vec_d = vec.get_mut();
                return match &mut vec_d.data {
                    ExpEnum::Vector(list) => {
                        list.push(new_item);
                        Ok(vec.clone())
                    }
                    _ => {
                        drop(vec_d);
                        Err(LispError::new(format!(
                            "vec-push!'s first form must be a vector, got {} {}",
                            vec.display_type(),
                            vec
                        )))
                    }
                };
            }
        }
    }
    Err(LispError::new(
        "vec-push! takes two forms (vector and form)",
    ))
}

// Destructive
fn builtin_vec_pop(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(list) = args.next() {
        if args.next().is_none() {
            return match &mut eval(environment, list)?.get_mut().data {
                ExpEnum::Vector(list) => {
                    if let Some(item) = list.pop() {
                        Ok(item)
                    } else {
                        Ok(Expression::make_nil())
                    }
                }
                _ => Err(LispError::new("vec-pop!'s first form must be a vector")),
            };
        }
    }
    Err(LispError::new("vec-pop! takes a vector"))
}

fn builtin_vec_is_empty(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(list) = args.next() {
        if args.next().is_none() {
            let list = eval(environment, list)?;
            return match &list.get().data {
                ExpEnum::Vector(list) => {
                    if list.is_empty() {
                        Ok(Expression::make_true())
                    } else {
                        Ok(Expression::make_nil())
                    }
                }
                _ => Err(LispError::new(format!(
                    "vec-empty?'s first form must be a vector, got {}",
                    list.display_type()
                ))),
            };
        }
    }
    Err(LispError::new("vec-empty? takes a vector"))
}

// Destructive
fn builtin_vec_vclear(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(list) = args.next() {
        if args.next().is_none() {
            let list = eval(environment, list)?;
            return match &mut list.get_mut().data {
                ExpEnum::Vector(list) => {
                    list.clear();
                    Ok(Expression::make_nil())
                }
                _ => Err(LispError::new("vec-clear!'s first form must be a vector")),
            };
        }
    }
    Err(LispError::new("vec-clear! takes a vector"))
}

// Destructive
fn builtin_vec_remove(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let vector = param_eval(environment, args, "vec-remove!")?;
    let idx = param_eval(environment, args, "vec-remove!")?;
    params_done(args, "vec-remove!")?;
    let idx = if let ExpEnum::Int(i) = idx.get().data {
        i
    } else {
        return Err(LispError::new("vec-remove! second form must be an int"));
    };
    let mut vector_d = vector.get_mut();
    match &mut vector_d.data {
        ExpEnum::Vector(inner_list) => {
            if idx < 0 || idx >= inner_list.len() as i64 {
                return Err(LispError::new("vec-remove! index out of range"));
            }
            inner_list.remove(idx as usize);
            Ok(vector.clone())
        }
        _ => Err(LispError::new("vec-remove! first form must be a vector")),
    }
}

// Destructive
fn builtin_vec_insert(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let vector = param_eval(environment, args, "vec-insert!")?;
    let idx = param_eval(environment, args, "vec-insert!")?;
    let obj = param_eval(environment, args, "vec-insert!")?;
    params_done(args, "vec-insert!")?;
    let idx_d = idx.get();
    if let ExpEnum::Int(idx) = idx_d.data {
        let mut vector_d = vector.get_mut();
        match &mut vector_d.data {
            ExpEnum::Vector(inner_list) => {
                if idx < 0 || idx > inner_list.len() as i64 {
                    Err(LispError::new("vec-insert!: index out of range"))
                } else {
                    inner_list.insert(idx as usize, obj);
                    Ok(vector.clone())
                }
            }
            _ => Err(LispError::new("vec-insert!: first form must be a vector")),
        }
    } else {
        Err(LispError::new("vec-insert!: second form must be an int"))
    }
}

pub fn add_vec_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, (Expression, String), S>,
) {
    data.insert(
        interner.intern("vec"),
        Expression::make_function(
            builtin_vec,
            "Usage: (vec item1 item2 .. itemN)

Make a new vector with items.

Section: vector

Example:
(test::assert-equal '() (vec))
(test::assert-equal '(1 2 3) (vec 1 2 3))
",
        ),
    );
    data.insert(
        interner.intern("make-vec"),
        Expression::make_function(
            builtin_make_vec,
            "Usage: (make-vec capacity default)

Make a new vector with capacity and default item(s).

Section: vector

Example:
(test::assert-equal '() (make-vec))
(test::assert-equal '(x x x) (make-vec 3 'x))
(test::assert-equal '(nil nil nil nil nil) (make-vec 5 nil))
(test::assert-equal '() (make-vec 5))
",
        ),
    );
    data.insert(
        interner.intern("vec-slice"),
        Expression::make_function(
            builtin_vec_slice,
            "Usage: (vec-slice vector start end?)

Returns a slice of a vector (0 based indexes, end is exclusive).

Section: vector

Example:
(test::assert-equal '(5 6) (vec-slice '#(1 2 3 4 5 6) 4 6))
(test::assert-equal '(1 2 3) (vec-slice '#(1 2 3 4 5 6) 0 3))
(test::assert-equal '(3 4 5) (vec-slice '#(1 2 3 4 5 6) 2 5))
(test::assert-equal '(3 4 5 6) (vec-slice '#(1 2 3 4 5 6) 2))
",
        ),
    );
    data.insert(
        interner.intern("vec-nth"),
        Expression::make_function(
            builtin_vec_nth,
            "Usage: (vec-nth vector index) -> object

Get the nth element (0 based) of a vector. If you need the equivalent operation
on a list use [nth](root::nth).

Section: vector

Example:
(test::assert-equal 5 (vec-nth '#(1 2 3 4 5 6) 4))
(test::assert-equal 1 (vec-nth '#(1 2 3 4 5 6) 0))
(test::assert-equal 3 (vec-nth '#(1 2 3 4 5 6) 2))
(test::assert-equal 6 (vec-nth '#(1 2 3 4 5 6) 5))
",
        ),
    );
    data.insert(
        interner.intern("vec-set!"),
        Expression::make_function(
            builtin_vec_set,
            "Usage: (vec-set! vector index value) -> vector

Set the nth index (0 based) of a vector to value. This is destructive! If you
need the equivalent operation on a list use [setnth!](root::setnth!).

Section: vector

Example:
(def test-setnth-vec (vec 1 2 3))
(test::assert-equal '(1 5 3) (vec-set! test-setnth-vec 1 5))
(test::assert-equal '(7 5 3) (vec-set! test-setnth-vec 0 7))
(test::assert-equal '(7 5 9) (vec-set! test-setnth-vec 2 9))
",
        ),
    );
    data.insert(
        interner.intern("vec-push!"),
        Expression::make_function(
            builtin_vec_push,
            "Usage: (vec-push! vector object) -> vector

Pushes the provided object onto the end of the vector.  This is destructive!

Section: vector

Example:
(def test-push-vec (vec))
(test::assert-equal '(1) (vec-push! test-push-vec 1))
(test::assert-equal '(1) test-push-vec)
(test::assert-equal '(1 2) (vec-push! test-push-vec 2))
(test::assert-equal '(1 2) test-push-vec)
(test::assert-equal '(1 2 3) (vec-push! test-push-vec 3))
(test::assert-equal '(1 2 3) test-push-vec)
",
        ),
    );
    data.insert(
        interner.intern("vec-pop!"),
        Expression::make_function(
            builtin_vec_pop,
            "Usage: (vec-pop! vector) -> object

Pops the last object off of the end of the vector.  This is destructive!

Section: vector

Example:
(def test-pop-vec (vec 1 2 3))
(test::assert-equal 3 (vec-pop! test-pop-vec))
(test::assert-equal '(1 2) test-pop-vec)
(test::assert-equal 2 (vec-pop! test-pop-vec))
(test::assert-equal '(1) test-pop-vec)
(test::assert-equal 1 (vec-pop! test-pop-vec))
(test::assert-equal '() test-pop-vec)
",
        ),
    );
    data.insert(
        interner.intern("vec-empty?"),
        Expression::make_function(
            builtin_vec_is_empty,
            "Usage: (vec-empty? vector)

True if the vector is empty.

Section: vector

Example:
(test::assert-true (vec-empty? '#()))
(test::assert-false (vec-empty? '#(1 2 3)))
",
        ),
    );
    data.insert(
        interner.intern("vec-clear!"),
        Expression::make_function(
            builtin_vec_vclear,
            "Usage: (vec-clear! vector)

Clears a vector.  This is destructive!

Section: vector

Example:
(def test-clear-vec (vec 1 2 3))
(test::assert-false (vec-empty? test-clear-vec))
(vec-clear! test-clear-vec)
(test::assert-true (vec-empty? test-clear-vec))
",
        ),
    );
    data.insert(
        interner.intern("vec-remove!"),
        Expression::make_function(
            builtin_vec_remove,
            "Usage: (vec-remove! vector index) -> vector

Remove the element at index from vector, shifting all elements after it to the left.
This is destructive!

Section: vector

Example:
(def test-remove-nth-vec (vec 1 2 3))
(test::assert-equal '(1 2 3) test-remove-nth-vec)
(vec-remove! test-remove-nth-vec 1)
(test::assert-equal '(1 3) test-remove-nth-vec)
(vec-remove! test-remove-nth-vec 1)
(test::assert-equal '(1) test-remove-nth-vec)
(vec-remove! test-remove-nth-vec 0)
(test::assert-equal '() test-remove-nth-vec)
",
        ),
    );
    data.insert(
        interner.intern("vec-insert!"),
        Expression::make_function(
            builtin_vec_insert,
            "Usage: (vec-insert! vector index new-element) -> vector

Inserts new-element at index and moves following elements right in vector.  This is destructive!

Section: vector

Example:
(def test-insert-nth-vec (vec 1 2 3))
(test::assert-equal '(1 2 3) test-insert-nth-vec)
(vec-insert! test-insert-nth-vec 1 5)
(test::assert-equal '(1 5 2 3) test-insert-nth-vec)
(vec-insert! test-insert-nth-vec 2 6)
(test::assert-equal '(1 5 6 2 3) test-insert-nth-vec)
(vec-insert! test-insert-nth-vec 0 4)
(test::assert-equal '(4 1 5 6 2 3) test-insert-nth-vec)
",
        ),
    );
}
