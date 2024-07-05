use slvm::{from_i56, CallFuncSig, Chunk, GVm, Interned, Value};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct SymbolsInt {
    pub syms: HashMap<Interned, usize>,
    count: usize,
}

impl SymbolsInt {
    pub fn add_sym(&mut self, sym: Interned) {
        self.syms.insert(sym, self.count);
        self.count += 1;
    }
}

// symbol name, idx/reg for scope, idx/reg for outer scope
type Captures = Rc<RefCell<Vec<(Interned, usize, usize)>>>;

#[derive(Clone, Debug)]
pub struct Symbols {
    pub data: Rc<RefCell<SymbolsInt>>,
    outer: Option<Rc<RefCell<Symbols>>>,
    pub captures: Captures,
}

impl Symbols {
    pub fn with_outer(outer: Option<Rc<RefCell<Symbols>>>) -> Symbols {
        let data = Rc::new(RefCell::new(SymbolsInt {
            syms: HashMap::new(),
            count: 1,
        }));
        Symbols {
            data,
            outer,
            captures: Rc::new(RefCell::new(Vec::new())),
        }
    }

    pub fn with_let(source: Rc<RefCell<Symbols>>) -> Symbols {
        let data = Rc::new(RefCell::new(SymbolsInt {
            syms: HashMap::new(),
            count: source.borrow().data.borrow().count,
        }));
        {
            let mut datad = data.borrow_mut();
            for (key, val) in source.borrow().data.borrow().syms.iter() {
                datad.syms.insert(*key, *val);
            }
        }
        Symbols {
            data,
            outer: source.borrow().outer.clone(),
            captures: source.borrow().captures.clone(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.data.borrow().syms.is_empty()
    }

    pub fn regs_count(&self) -> usize {
        self.data.borrow().count
    }

    pub fn contains_symbol(&self, key: Interned) -> bool {
        self.data.borrow().syms.contains_key(&key)
    }

    #[allow(clippy::assigning_clones)] // clippy warns about clone on copy for performance, but we need to do it this way to avoid a borrow checker error.
    pub fn can_capture(&self, key: Interned) -> bool {
        let mut loop_outer = self.outer.clone();
        while let Some(outer) = loop_outer {
            let outer = outer.borrow();
            if outer.contains_symbol(key) {
                return true;
            }
            loop_outer = outer.outer.clone();
        }
        false
    }

    pub fn get_capture_binding(&self, key: Interned) -> Option<usize> {
        for cap in &*self.captures.borrow() {
            if cap.0 == key {
                return Some(cap.2);
            }
        }
        None
    }

    pub fn get(&self, key: Interned) -> Option<usize> {
        self.data.borrow().syms.get(&key).copied()
    }

    pub fn clear(&mut self) {
        self.data.borrow_mut().syms.clear();
    }

    pub fn reserve_reg(&mut self) -> usize {
        let mut data = self.data.borrow_mut();
        let count = data.count;
        data.count += 1;
        count
    }

    pub fn insert(&mut self, key: Interned) -> usize {
        let mut data = self.data.borrow_mut();
        let count = data.count;
        data.syms.insert(key, count);
        data.count += 1;
        count
    }

    pub fn insert_reserved(&mut self, key: Interned, register: usize) {
        let mut data = self.data.borrow_mut();
        data.syms.insert(key, register);
    }

    pub fn insert_capture(&self, _vm: &mut SloshVm, key: Interned) -> Option<usize> {
        let data_d = self.data.borrow();
        if let Some(idx) = data_d.syms.get(&key) {
            Some(*idx)
        } else {
            if let Some(outer) = &self.outer {
                drop(data_d);
                // Also capture in outer lexical scope or bad things can happen.
                if let Some(outer_idx) = outer.borrow().insert_capture(_vm, key) {
                    let mut data = self.data.borrow_mut();
                    let count = data.count;
                    data.syms.insert(key, count);
                    data.count += 1;
                    self.captures.borrow_mut().push((key, count, outer_idx));
                    return Some(count);
                }
            }
            None
        }
    }

    pub fn len_captures(&self) -> usize {
        self.captures.borrow().len()
    }
}

pub struct Specials {
    pub def: Interned,
    pub set: Interned,
    pub do_: Interned,
    pub fn_: Interned,
    pub mac_: Interned,
    pub if_: Interned,
    pub add: Interned,
    pub sub: Interned,
    pub mul: Interned,
    pub div: Interned,
    pub inc: Interned,
    pub dec: Interned,
    pub list: Interned,
    pub list_append: Interned,
    pub cons: Interned,
    pub car: Interned,
    pub cdr: Interned,
    pub xar: Interned,
    pub xdr: Interned,
    pub make_hash: Interned,
    pub vec: Interned,
    pub make_vec: Interned,
    pub vec_pop: Interned,
    pub vec_push: Interned,
    pub quote: Interned,
    pub backquote: Interned,
    pub recur: Interned,
    pub this_fn: Interned,
    pub numeq: Interned,
    pub numlt: Interned,
    pub numlte: Interned,
    pub numgt: Interned,
    pub numgte: Interned,
    pub eq: Interned,
    pub equal: Interned,
    pub type_: Interned,
    pub not: Interned,
    pub and: Interned,
    pub or: Interned,
    pub err: Interned,
    pub len: Interned,
    pub clear: Interned,
    pub str_: Interned,
    pub let_: Interned,
    pub let_while: Interned,
    pub call_cc: Interned,
    pub defer: Interned,
    pub on_error: Interned,
    pub while_: Interned,
    pub doc_string: Interned,
    pub get: Interned,
    pub mk_err: Interned,
    pub is_err: Interned,
    pub is_ok: Interned,
    pub ret: Interned,
    pub ns: Interned,
    pub import: Interned,

    pub rest: Interned,
    pub optional: Interned,
    pub scratch: Interned,
}

impl Specials {
    pub fn new(vm: &mut SloshVm) -> Self {
        Self {
            def: add_special(vm, "def", r#"Usage: (def symbol doc_string? expression) -> expression

Adds an expression to the current namespace.  Return the expression that was defined.
Symbol is not evaluted.  Can take an option doc string (docstrings can only be
set on namespaced (global) symbols).

Section: core

Example:
(def test-do-one nil)
(def test-do-two nil)
(def test-do-three (do (set! test-do-one "One")(set! test-do-two "Two")"Three"))
(test::assert-equal "One" test-do-one)
(test::assert-equal "Two" test-do-two)
(test::assert-equal "Three" test-do-three)
(let (test-do-one nil)
    ; Add this to tthe let's scope (shadow the outer test-do-two).
    (test::assert-equal "Default" (def test-do-four "Default"))
    ; set the currently scoped value.
    (set! test-do-one "1111")
    (set! test-do-two "2222")
    (test::assert-equal "1111" test-do-one)
    (test::assert-equal "2222" test-do-two)
    (test::assert-equal "Default" test-do-four))
; Original outer scope not changed.
(test::assert-equal "One" test-do-one)
(test::assert-equal "Default" test-do-four)"#),
            set: add_special(vm, "set!", r#"Usage: (set! symbol expression) -> expression

Sets an existing expression in the current scope(s).  Return the expression that was set.
Symbol is not evalauted.

Set will set the first binding it finds starting in the current scope and then
trying enclosing scopes until exhausted.

Section: core

Example:
(def test-do-one nil)
(def test-do-two nil)
(def test-do-three (do (set! test-do-one "One")(set! test-do-two "Two")"Three"))
(test::assert-equal "One" test-do-one)
(test::assert-equal "Two" test-do-two)
(test::assert-equal "Three" test-do-three)
(let (test-do-one nil)
    ; set the currently scoped value.
    (test::assert-equal "1111" (set! test-do-one "1111"))
    (test::assert-equal "1111" test-do-one))
; Original outer scope not changed.
(test::assert-equal "One" test-do-one)"#),
            do_: add_special(vm, "do", r#"Usage: (do exp0 ... expN) -> expN

Evaluatate each form and return the last.

Section: core

Example:
(def test-do-one nil)
(def test-do-two nil)
(def test-do-three (do (set! test-do-one "One") (set! test-do-two "Two") "Three"))
(test::assert-equal "One" test-do-one)
(test::assert-equal "Two" test-do-two)
(test::assert-equal "Three" test-do-three)
"#),
            fn_: add_special(vm, "fn", "Usage: (fn (param*) expr*) -> exprN

Create a function (lambda).

Section: core

Example:
(def test-fn1 nil)
(def test-fn2 nil)
(def test-fn3 nil)
(def test-fn-empty ((fn () nil)))
(test::assert-false test-fn-empty)
((fn () (set! test-fn1 1)))
(test::assert-equal 1 test-fn1)
((fn () (set! test-fn1 10)(set! test-fn2 2)))
(test::assert-equal 10 test-fn1)
(test::assert-equal 2 test-fn2)
((fn () (set! test-fn1 11)(set! test-fn2 20)(set! test-fn3 3)))
(test::assert-equal 11 test-fn1)
(test::assert-equal 20 test-fn2)
(test::assert-equal 3 test-fn3)
((fn (x y z) (set! test-fn1 x)(set! test-fn2 y)(set! test-fn3 z)) 12 21 30)
(test::assert-equal 12 test-fn1)
(test::assert-equal 21 test-fn2)
(test::assert-equal 30 test-fn3)
(test::assert-equal 63 ((fn (x y z) (set! test-fn1 x)(set! test-fn2 y)(set! test-fn3 z)(+ x y z)) 12 21 30))"),
            mac_: add_special(vm, "macro", "Usage: (macro (args) `(apply + ,@args))

Define an anonymous macro.

Section: core

Example:
(def test-macro1 nil)
(def test-macro2 nil)
(def test-macro-empty (macro () nil))
(test::assert-false (test-macro-empty))
(def test-mac nil)
(def mac-var 2)
(let (mac-var 3)
  (set! test-mac (macro (x) (set! test-macro2 100) (test::assert-equal 3 mac-var) (* mac-var x))))
(set! test-macro1 (test-mac 10))
(test::assert-equal 30 test-macro1)
(test::assert-equal 100 test-macro2)"),
            if_: add_special(vm, "if", r#"Usage: (if p1 a1 p2 a2 ... pn an?) -> [evaled form result]

If conditional.  Will evaluate p1 and if true (i.e. not nil or false) then
return the evaluation of a1, if falsey(i.e. nil or false) evaluate p2 and so on.
On an odd number of arguments (an is missing) then evaluate and return pn.
Return false(#f) if no predicate is true.  This degenerates into the traditional
(if predicate then-form else-form).
NOTE: Both nil and false(#f) are 'falsey' for the purposes of if.

Section: conditional

Example:
(def test-if-one
    (if #t "ONE TRUE" "ONE FALSE"))
(def test-if-two
    (if nil "TWO TRUE" "TWO FALSE"))
(def test-if-three
    (if #f "THREE TRUE" "THREE FALSE"))
(test::assert-equal "ONE TRUE" test-if-one)
(test::assert-equal "TWO FALSE" test-if-two)
(test::assert-equal "THREE FALSE" test-if-three)

(def test-if-one2
    (if #t "ONE2 TRUE"))
(def test-if-two2
    (if nil "TWO2 TRUE"))
(def test-if-three2
    (if #f "THREE2 TRUE"))
(test::assert-equal "ONE2 TRUE" test-if-one2)
(test::assert-equal #f test-if-two2)
(test::assert-equal #f test-if-three2)

(def test-if-one2
    (if nil "ONE FALSE" #t "ONE TRUE" #t "ONE TRUE2"))
(def test-if-two2
    (if nil "TWO TRUE" #f "TWO FALSE" #t "TWO TRUE2"))
(def test-if-three2
    (if #f "THREE TRUE" nil "THREE FALSE" "THREE DEFAULT"))
(test::assert-equal "ONE TRUE" test-if-one2)
(test::assert-equal "TWO TRUE2" test-if-two2)
(test::assert-equal "THREE DEFAULT" test-if-three2)
(test::assert-equal nil (if nil))
(test::assert-equal #f (if nil #t nil #t nil #t))"#),
            add: add_special(vm, "+", r#"Usage: (+ number*)

Add a sequence of numbers.  (+) will return 0.

Section: math

Example:
(ns-import 'math)
(test::assert-equal 0 (+))
(test::assert-equal 5 (+ 5))
(test::assert-equal 10 (+ 5 5))
(test::assert-equal 6 (+ 1 5))
(test::assert-equal 6.5 (+ 1 5.5))
(test::assert-equal 7 (+ 1 2 4))
(test::assert-error (+ 1 2 4 "5"))"#),
            sub: add_special(vm, "-", r#"Usage: (- number+)

Subtract a sequence of numbers.  Requires at least one number (negate if only one number).

Section: math

Example:
(ns-import 'math)
(test::assert-error (- 5 "2"))
(test::assert-equal -5 (- 5))
(test::assert-equal -5.0 (- 5.0))
(test::assert-equal -4 (- 1 5))
(test::assert-equal -4.5 (- 1 5.5))
(test::assert-equal 4 (- 10 2 4))
(test::assert-equal 4.5 (- 10 2 3.5))
"#),
            mul: add_special(vm, "*", r#"Usage: (* number*)

Multiply a sequence of numbers.  (*) will return 1.

Section: math

Example:
(ns-import 'math)
(test::assert-equal 1 (*))
(test::assert-equal 5 (* 5))
(test::assert-equal 5 (* 1 5))
(test::assert-equal 5.0 (* 1.0 5))
(test::assert-equal 7.5 (* 1.5 5))
(test::assert-equal 7.5 (* 1.5 5.0))
(test::assert-equal 15 (* 3 5))
(test::assert-equal 8 (* 1 2 4))
(test::assert-equal 16 (* 2 2 4))
(test::assert-equal 16.0 (* 2 2.0 4))
(test::assert-equal 16.0 (* 2.0 2.0 4.0))
(test::assert-equal 54.9999999999999 (* 100 0.55))
(test::assert-error (* 1 2 4 "5"))
"#),
            div: add_special(vm, "/", r#"Usage: (/ number+)

Divide a sequence of numbers.  Requires at least two numbers.

Section: math
Example:
(ns-import 'math)
(test::assert-equal 5 (/ 50 10))
(test::assert-equal 5 (/ 50.0 10.0))
(test::assert-equal 0 (/ 1 5))
(test::assert-equal .2 (/ 1.0 5))
(test::assert-equal .2 (/ 1.0 5.0))
(test::assert-equal 5.5 (/ 5.5 1))
(test::assert-equal 2 (/ 16 2 4))
(test::assert-equal 5 (/ 100 2 5 2))
(test::assert-error (/))
(test::assert-error (/ 1))
(test::assert-error (/ 1 0))
(test::assert-error (/ 10 5 0))
(test::assert-error (/ 10 "5" 2))
"#),
            inc: add_special(vm, "inc!", r#"Usage: (inc! symbol [number]) -> new value

Increment the value in symbol by one or the optional number

Section: core

Example:
(def *inc-test* 1)
(test::assert-equal 2 (inc! *inc-test*))
(test::assert-equal 2 *inc-test*)
(test::assert-equal 5 (inc! *inc-test* 3))
(test::assert-equal 5 *inc-test*)
(let (inc-test 1)
  (test::assert-equal 2 (inc! inc-test))
  (test::assert-equal 2 inc-test)
  (test::assert-equal 5 (inc! inc-test 3))
  (test::assert-equal 5 inc-test))"#),
            dec: add_special(vm, "dec!", r#"Usage: (dec! symbol [number]) -> new value

Decrement the value in symbol by one or the optional number

Section: core

Example:
(def *dec-test* 5)
(test::assert-equal 4 (dec! *dec-test*))
(test::assert-equal 4 *dec-test*)
(test::assert-equal 1 (dec! *dec-test* 3))
(test::assert-equal 1 *dec-test*)
(let (dec-test 5)
  (test::assert-equal 4 (dec! dec-test))
  (test::assert-equal 4 dec-test)
  (test::assert-equal 1 (dec! dec-test 3))
  (test::assert-equal 1 dec-test))"#),
            list: add_special(vm, "list", "
Usage: (list item0 item1 .. itemN)

Create a proper list from pairs with items 0 - N.

Section: pair

Example:
(test::assert-equal '(1 2 3) (list 1 2 3))"),
            list_append: add_special(vm, "list-append", ""),
            cons: add_special(vm, "cons", ""),
            car: add_special(vm, "car", "Usage: (car pair)

Return the car (first item) from a pair.  If used on a proper list this will be the first element.

Section: pair

Example:
(def tst-pairs-two (list 'x 'y 'z))
(test::assert-equal 'x (car tst-pairs-two))
(test::assert-equal 10 (car '(10)))
(test::assert-equal 9 (car '(9 11 13)))"),
            cdr: add_special(vm, "cdr", "Usage: (cdr pair)

Return the cdr (second item) from a pair.  If used on a proper list this will be the list minus the first element.

Section: pair

Example:
(def tst-pairs-three (list 'x 'y 'z))
(test::assert-equal '(y z) (cdr tst-pairs-three))
(test::assert-equal nil (cdr '(10)))
(test::assert-equal '(13) (cdr '(9 13)))
(test::assert-equal '(11 13) (cdr '(9 11 13)))"),
            xar: add_special(
                vm,
                "xar!",
                "
Usage: (xar! pair expression)

Destructive form that replaces the car (first item) in a pair with a new expression.

If used on a proper list will replace the first item.  Can be used on nil to
create a pair (expression . nil).

Section: pair

Example:
(def tst-pairs-three (list 'x 'y 'z))
(test::assert-equal '(x y z) tst-pairs-three)
(test::assert-equal '(s y z) (xar! tst-pairs-three 's))
(test::assert-equal '(s y z) tst-pairs-three)
(def tst-pairs-four (list 't))
(test::assert-equal '(y) (xar! tst-pairs-four 'y))
(test::assert-equal '(y) tst-pairs-four)",
            ),
            xdr: add_special(
                vm,
                "xdr!",
                "Usage: (xdr! pair expression)

Destructive form that replaces the cdr (second item) in a pair with a new expression.

If used on a proper list will replace everthing after the first item.
Can be used on nil to create a pair (nil . expression).

Section: pair

Example:
(def tst-pairs-five (list 'a 'b 'c))
(test::assert-equal '(a b c) tst-pairs-five)
(test::assert-equal '(a y z) (xdr! tst-pairs-five '(y z)))
(test::assert-equal '(a y z) tst-pairs-five)
(def tst-pairs-six (list 'v))
(test::assert-equal (list 'v) tst-pairs-six)
(test::assert-equal '(v . v) (xdr! tst-pairs-six 'v))
(test::assert-equal '(v . v) tst-pairs-six)",
            ),
            make_hash: add_special(
                vm,
                "make-hash",
                "Usage: (make-hash associations?)

Make a new hash map.

If associations is provided (makes an empty map if not) then it is a list of
pairs (key . value) that populate the initial map.  Neither key nor value in the
associations will be evaluated.

Section: hashmap

",
            ),
            vec: add_special(
                vm,
                "vec",
                "Usage: (vec item1 item2 .. itemN)

Make a new vector with items.

Section: vector

Example:
(test::assert-equal [] (vec))
(test::assert-equal [1 2 3] (vec 1 2 3))
",
            ),
            make_vec: add_special(
                vm,
                "make-vec",
                "Usage: (make-vec capacity default)

Make a new vector with capacity and default item(s).

Section: vector

Example:
(test::assert-equal [] (make-vec))
(test::assert-equal ['x 'x 'x] (make-vec 3 'x))
(test::assert-equal [nil nil nil nil nil] (make-vec 5 nil))
(test::assert-equal [] (make-vec 5))
",
            ),
            vec_push: add_special(
                vm,
                "vec-push!",
                "Usage: (vec-push! vector object) -> vector

Pushes the provided object onto the end of the vector.  This is destructive!

Section: vector

Example:
(def test-push-vec (vec))
(test::assert-equal [1] (vec-push! test-push-vec 1))
(test::assert-equal [1] test-push-vec)
(test::assert-equal [1 2] (vec-push! test-push-vec 2))
(test::assert-equal [1 2] test-push-vec)
(test::assert-equal [1 2 3] (vec-push! test-push-vec 3))
(test::assert-equal [1 2 3] test-push-vec)
",
            ),
            vec_pop: add_special(
                vm,
                "vec-pop!",
                "Usage: (vec-pop! vector) -> object

Pops the last object off of the end of the vector.  This is destructive!

Section: vector

Example:
(def test-pop-vec (vec 1 2 3))
(test::assert-equal 3 (vec-pop! test-pop-vec))
(test::assert-equal [1 2] test-pop-vec)
(test::assert-equal 2 (vec-pop! test-pop-vec))
(test::assert-equal [1] test-pop-vec)
(test::assert-equal 1 (vec-pop! test-pop-vec))
(test::assert-equal [] test-pop-vec)
",
            ),
            quote: add_special(
                vm,
                "quote",
                "Usage: 'expression -> expression

Return expression without evaluation.
The reader macro 'expression will expand to (quote expression).

Section: core

Example:
(test::assert-equal (list 1 2 3) (quote (1 2 3)))
(test::assert-equal (list 1 2 3) '(1 2 3))
(test::assert-equal '(1 2 3) (quote (1 2 3)))",
            ),
            backquote: add_special(
                vm,
                "back-quote",
                "Usage: `expression -> expression

Return expression without evaluation.
Always use the ` reader macro or expansion will not work
(i.e. (back-quote expression) will not do , expansion).

Backquote (unlike quote) allows for symbol/form evaluation using , or ,@.

Section: core

Example:
(test::assert-equal (list 1 2 3) `(1 2 3))
(test::assert-equal `(1 2 3) '(1 2 3))
(def test-bquote-one 1)
(def test-bquote-list '(1 2 3))
(test::assert-equal (list 1 2 3) `(~test-bquote-one 2 3))
(test::assert-equal (list 1 2 3) `(~@test-bquote-list))
            ",
            ),
            recur: add_special(
                vm,
                "recur",
                "Usage: (recur &rest)

Recursively call the enclosing function with the given parameters.  Recur uses
tail call optimization and must be in the tail position or it is an error.  For
a named function it would be equivalent to a normal recursive call in a tail
position but it requires a tail position and does not need a name (a normal
recursive call would work in a non-tail position but could blow the stack if
it is to deep- unlike a recur or tail position recursive call).
NOTE: potential footgun, the let macro expands to a lambda (fn) and a recur used
inside the let would bind with the let not the enclosing lambda (this would
apply to any macro that also expands to a lamda- this is by design with the
loop macro but would be unexpected with let).

Section: core

Example:
(def tot 0)
(loop (idx) (3) (do
    (set! tot (+ tot 1))
    (if (> idx 1) (recur (- idx 1)))))
(test::assert-equal 3 tot)
(set! tot 0)
((fn (idx) (do
    (set! tot (+ tot 1))
    (if (> idx 1) (recur (- idx 1)))))5)
(test::assert-equal 5 tot)",
            ),
            this_fn: add_special(vm, "this-fn", ""),
            numeq: add_special(vm, "==", r#"Usage: (== val0 ... valN)

Equals.  Works for numeric types (int, float).

Section: conditional

Example:
(test::assert-false (== 1 2))
(test::assert-true (== 2 2))
(test::assert-true (== 2 2 2))
(test::assert-false (== 3 2 2))
(test::assert-false (== 3.0 2.0))
(test::assert-true (== 2.0 2.0))
(test::assert-true (== 2.0 2.0 2.0))
(test::assert-false (== 3.0 2.0 2.0))
(test::assert-false (== 2.1 2.0 3.0))
(test::assert-false (== 2 1))
(test::assert-false (== 3 2 1))
(test::assert-false (== 1.1 1.0))
(test::assert-true (== 1.1 1.1))
(test::assert-false (== 3 2 3))
"#),
            numlt: add_special(vm, "<", r#"Usage: (< val0 ... valN)

Less than.  Works for int, float or string.

Section: conditional

Example:
(test::assert-true (< 1 2))
(test::assert-true (< 1 2 3 4))
(test::assert-false (< 2 2))
(test::assert-false (< 2 2 2))
(test::assert-false (< 2 2 3))
(test::assert-true (< 1.0 2.0))
(test::assert-false (< 2.0 2.0))
(test::assert-false (< 2.0 2.0 2.0))
(test::assert-false (< 2.0 2.0 3.0))
(test::assert-false (< 2.1 2.0 3.0))
(test::assert-false (< 2 1))
(test::assert-false (< 3 2 3))
(test::assert-true (< 1.0 1.1 ))
(test::assert-true (< 1.0 1.01 ))
(test::assert-true (< 1.0 1.001 ))
(test::assert-true (< 1.0 1.0001 ))
(test::assert-true (< 1.0 1.00001 ))
(test::assert-true (< 1.0 1.000001 ))
(test::assert-true (< 1.0 1.0000001 ))
(test::assert-false (< 1.0 1.00000000000001 ))
"#),
            numlte: add_special(vm, "<=", r#"Usage: (<= val0 ... valN)

Less than or equal.  Works for int, float or string.

Section: conditional

Example:
(test::assert-true (<= 1 2))
(test::assert-true (<= 2 2))
(test::assert-true (<= 2 2 2))
(test::assert-true (<= 2 2 3))
(test::assert-true (<= 1.0 2.0))
(test::assert-true (<= 2.0 2.0))
(test::assert-true (<= 2.0 2.0 2.0))
(test::assert-true (<= 2.0 2.0 3.0))
(test::assert-false (<= 2.1 2.0 3.0))
(test::assert-false (<= 2 1))
(test::assert-false (<= 3 2 3))
(test::assert-true (<= 1.00000000000001 1.0000000000001 ))
(test::assert-true (<= 10.0000000000001 10.000000000001))
(test::assert-true (<= 100.000000000001 100.00000000001))
(test::assert-true (<= 1000.000000000001 1000.00000000001))
"#),
            numgt: add_special(vm, ">", r#"Usage: (> val0 ... valN)

Greater than.  Works for int, float or string.

Section: conditional

Example:
(test::assert-false (> 1 2))
(test::assert-false (> 2 2))
(test::assert-false (> 2 2 2))
(test::assert-false (> 3 2 2))
(test::assert-true (> 3.0 2.0))
(test::assert-false (> 2.0 2.0))
(test::assert-false (> 2.0 2.0 2.0))
(test::assert-false (> 3.0 2.0 2.0))
(test::assert-false (> 2.1 2.0 3.0))
(test::assert-true (> 2 1))
(test::assert-true (> 3 2 1))
(test::assert-true (> 1.1 1.0))
(test::assert-false (> 3 2 3))
(test::assert-true (> 1.001 1.0))
(test::assert-true (> 1.0000001 1.0))
(test::assert-false (> 1.00000000000001 1.0))
"#),
            numgte: add_special(vm, ">=", r#"Usage: (>= val0 ... valN)

Greater than or equal.  Works for int, float or string.

Section: conditional

Example:
(test::assert-false (>= 1 2))
(test::assert-true (>= 2 2))
(test::assert-true (>= 2 2 2))
(test::assert-true (>= 3 2 2))
(test::assert-true (>= 3.0 2.0))
(test::assert-true (>= 2.0 2.0))
(test::assert-true (>= 2.0 2.0 2.0))
(test::assert-true (>= 3.0 2.0 2.0))
(test::assert-false (>= 2.1 2.0 3.0))
(test::assert-true (>= 2 1))
(test::assert-true (>= 1.1 1.0))
(test::assert-false (>= 3 2 3))
(test::assert-true (>= 1.0000000000001 1.00000000000001))
(test::assert-true (>= 10.000000000001 10.0000000000001))
(test::assert-true (>= 100.00000000001 100.000000000001))
(test::assert-true (>= 1000.00000000001 1000.000000000001))
"#),
            eq: add_special(vm, "identical?", ""),
            equal: add_special(vm, "=", r#"Usage: (= val0 val1)

Test equality, works for most value types where it makes sense, not just primitives.

Section: core

Example:
(test::assert-false (= "aab" "aaa"))
(test::assert-true (= "aaa" "aaa"))
(test::assert-true (= "aaa" "aaa" "aaa"))
(test::assert-false (= "aaa" "aaaa" "aaa"))
(test::assert-false (= "ccc" "aab" "aaa"))
(test::assert-false (= "aaa" "aab"))
(test::assert-true (= (get-error (/ 1 0)) (get-error (/ 1 0))))
"#),
            type_: add_special(vm, "type", ""),
            not: add_special(vm, "not", "Usage: (not expression)

Return true(#t) if expression is nil, false(#f) otherwise.

Section: conditional

Example:
(test::assert-true (not nil))
(test::assert-false (not 10))
(test::assert-false (not #t))
(test::assert-false (not (+ 1 2 3)))"),
            and: add_special(vm, "and", r#"Usage: (and exp0 ... expN) -> [false(#f) or expN result]

Evaluates each form until one produces nil or false(#f), produces false(#f) if
any form is nil/#f or the result of the last expression.

The and form will stop evaluating when the first expression produces nil/#f.

Section: conditional

Example:
(test::assert-equal #f (and nil (err "and- can not happen")))
(test::assert-equal #f (and #f (err "and- can not happen")))
(test::assert-equal "and- done" (and #t "and- done"))
(test::assert-equal "and- done" (and #t #t "and- done"))
(test::assert-equal 6 (and #t #t (+ 1 2 3)))
(test::assert-equal 6 (and (/ 10 5) (* 5 2) (+ 1 2 3)))"#),
            or: add_special(vm, "or", r#"Usage: (or exp0 ... expN) -> [false(#f) or first non nil expression]

Evaluates each form until one produces a non-nil/non-false result, produces #f
if all expressions are 'falsey'.

The or form will stop evaluating when the first expression produces non-nil/false.

Section: conditional

Example:
(test::assert-true (or nil nil #t (err "and- can not happen")))
(test::assert-true (or #f nil #t (err "and- can not happen")))
(test::assert-true (or #f #f #t (err "and- can not happen")))
(test::assert-equal #f (or nil nil nil))
(test::assert-equal #f (or #f nil nil))
(test::assert-equal #f (or #f nil #f))
(test::assert-equal #f (or #f #f #f))
(test::assert-equal "or- done" (or nil "or- done"))
(test::assert-equal "or- done" (or nil nil "or- done"))
(test::assert-equal 6 (or nil nil (+ 1 2 3)))
(test::assert-equal 2 (or (/ 10 5) (* 5 2) (+ 1 2 3)))"#),
            len: add_special(
                vm,
                "len",
                r#"Usage: (len expression) -> int

Return length of supplied expression.  The length of an atom is 1.

Section: core

Example:
(test::assert-equal 0 (len nil))
(test::assert-equal 5 (len "12345"))
; Note the unicode symbol is only one char even though it is more then one byte.
(test::assert-equal 6 (len "12345Σ"))
(test::assert-equal 3 (len '(1 2 3)))
(test::assert-equal 3 (len [1 2 3]))
(test::assert-equal 3 (len (list 1 2 3)))
(test::assert-equal 3 (len (vec 1 2 3)))
(test::assert-equal 1 (len 100))
(test::assert-equal 1 (len 100.0))
(test::assert-equal 1 (len \tab))
"#,
            ),
            clear: add_special(
                vm,
                "clear!",
                "Usage: (clear! container)

Clears a container (vector, hash-map, string).  This is destructive!

Section: collection

Example:
(def test-clear-vec (vec 1 2 3))
(test::assert-false (empty? test-clear-vec))
(clear! test-clear-vec)
(test::assert-true (empty? test-clear-vec))
",
            ),
            str_: add_special(vm, "str", r#"Usage: (str arg0 ... argN) -> string

Make a new string with its arguments.

Arguments will be turned into strings.  If an argument is a process then the
output of the process will be captured and put into the string.

Section: string

Example:
(test::assert-equal "stringsome" (str "string" "some"))
(test::assert-equal "string" (str "string" ""))
(test::assert-equal "string 50" (str "string" " " 50))
"#),
            let_: add_special(vm, "let", r#"Usage: (let vals &rest let-body)

Takes list, vals, of form ((binding0 sexp0) (binding1 sexp1) ...) and evaluates
let-body with all values of binding bound to the result of the evaluation of
sexp.

Section: core

Example:
(def test-do-one "One1")
(def test-do-two "Two1")
(def test-do-three (let (test-do-one "One") (set! test-do-two "Two")(test::assert-equal "One" test-do-one)"Three"))
(test::assert-equal "One1" test-do-one)
(test::assert-equal "Two" test-do-two)
(test::assert-equal "Three" test-do-three)
((fn (idx) (let (v2 (+ idx 2) v3 (+ idx 3))
    (test::assert-equal (+ idx 2) v2)
    (test::assert-equal (+ idx 3) v3)
    (if (< idx 5) (recur (+ idx 1)))))0)
((fn (idx) (let (v2 (+ idx 2) v3 (+ idx 3))
    (test::assert-equal (+ idx 2) v2)
    (test::assert-equal (+ idx 3) v3)
    (if (< idx 5) (this-fn (+ idx 1)))))0)"#),
            let_while: add_special(vm, "let-while", r#"Usage: (let-while (initial-bindings) (loop bindings) condition & let-body)

Takes list of initial bindings (done once before loop) of form (binding0 sexp0, binding1 sexp1, ...),
and a list of loop bindings (done at the start of each iteration including the first) and evaluates
let-body with all values of binding bound to the result of the evaluation of
both bindings while condition is true.

Section: core

Example:
; both of these examples create a vector and iterate to print all the elements
; use traditional lisp structure
(def test-res [])
(let-while (l [1 2 3]) (done (empty? l), f (first l),  l (rest l)) (not done)
  (prn f)
  (vec-push! test-res f))
(let ([x y z] test-res)
  (test::assert-equal 1 x)
  (test::assert-equal 2 y)
  (test::assert-equal 3 z))
; same thing using destructuring
(def test-res [])
(let-while (l [1 2 3]) (done (empty? l), [% f & l] l) (not done)
  (prn f)
  (vec-push! test-res f))
(let ([x y z] test-res)
  (test::assert-equal 1 x)
  (test::assert-equal 2 y)
  (test::assert-equal 3 z))
"#),
            call_cc: add_special(vm, "call/cc", ""),
            defer: add_special(vm, "defer", ""),
            on_error: add_special(vm, "on-raised-error", r#"Usage: (on-raised-error (fn (error) ...))

Low level (consider this unstable) interface to the raised error machinery.
Useful for building higher level error handling (get-error for instance).
It takes either Nil or a callable with one parameter.  That parameter will be
the error that was raised.  The entire running "chunk" of code will be
displaced for the installed handler.  Probably best to use this with a
continuation or a function that ends in a continuation call otherwise it
may be difficult to reason about...

Will return the previously installed handler or Nil if one is not installed.
Calling with Nil will return the old handler and clear it (no handler
installed).

This special form will override breaking into the debugger when an error is
raised.

Section: core

Example:
(defmacro get-error-test (& body)
`(let (old-error (on-raised-error nil))
    (defer (on-raised-error old-error))
    (call/cc (fn (k) (on-raised-error (fn (err) (k (cons (car err)(cdr err)))))
                (cons :ok (do ~@body))))))

(test::assert-equal (cons :ok 6) (get-error-test (let (x 1, y 5) (+ x y))))
(test::assert-equal '(:test . "error") (get-error-test (let (x 1, y 5) (err :test "error")(+ x y))))
"#),
            while_: add_special(vm, "while", ""),
            doc_string: add_special(vm, "doc-string", ""),
            get: add_special(vm, "get", ""),
            err: add_special(vm, "err", r#"Usage: (err :keyword value)

Raises an error with keyword and value.  By default this will break into the
debugger like a runtime error (use get-error to avoid this).

Section: core

Example:
(let (error (get-error (err :test "Test error")))
    (test::assert-equal :test (car error))
    (test::assert-equal "Test error" (cdr error))
    (test::assert-true (err? error)))
"#),
            mk_err: add_special(vm, "mk-err", r#"Usage: (mk-err :keyword value)

Create an error object.  This does not raise the error but merly cretes it.
Can use car/cdr to extract the keyword and value.

Section: core

Example:
(let (error (mk-err :test "Test error"))
    (test::assert-equal :test (car error))
    (test::assert-equal "Test error" (cdr error))
    (test::assert-true (err? error)))
"#),
            is_err: add_special(vm, "err?", r#"Usage: (err? expression)

True if the expression is an error, false otherwise.

Section: type

Example:
(test::assert-true (err? (mk-err :arr "test")))
(test::assert-false (err? nil))
"#),
            is_ok: add_special(vm, "ok?", r#"Usage: (ok? expression)

True if the expression is NOT an error, false if it is an error.

Section: type

Example:
(test::assert-false (ok? (mk-err :arr "test")))
(test::assert-true (ok? nil))
"#),
            ret: add_special(vm, "return", ""),
            ns: add_special(vm, "ns", r#"Usage: (ns namespace)

Changes to namespace.  This will cause all globals defined to have namespace:: prepended.
This will also clear any existin imports.

Section: core

Example:
(ns testing)
(def x #t)
(test::assert-true x)
(ns nil)
(test::assert-true testing::x)
"#),
            import: add_special(vm, "import", r#"Usage: (import namespace [:as symbol])

Will import a namespace.  Without an as then all symbols in the namespace will become available in the current
namespace as if local.  With [:as symbol] then all namespace symbols become available with symbol:: prepended.

Section: core

Example:
(ns testing)
(def x #t)
(test::assert-true x)
(ns nil)
(test::assert-true testing::x)
(import testing)
(test::assert-true x)
(import testing :as t)
(test::assert-true t::x)
"#),

            rest: vm.intern_static("&"),
            optional: vm.intern_static("%"),
            scratch: vm.intern_static("[SCRATCH]"),
        }
    }
}

fn add_special(env: &mut SloshVm, name: &'static str, doc_string: &str) -> Interned {
    let i = env.intern_static(name);
    let val = Value::Special(i);
    let si = env.set_named_global(name, val);
    let key = env.intern("doc-string");
    let s = env.alloc_string(doc_string.to_string());
    env.set_global_property(si, key, s);
    i
}

pub struct CompileState {
    pub symbols: Rc<RefCell<Symbols>>,
    pub constants: HashMap<Value, usize>,
    pub lets: Option<HashMap<Interned, usize>>,
    pub chunk: Chunk,
    pub max_regs: usize,
    pub tail: bool,
    pub defers: usize,
    pub doc_string: Option<Value>,
}

impl Default for CompileState {
    fn default() -> Self {
        Self::new()
    }
}

impl CompileState {
    pub fn new() -> Self {
        CompileState {
            symbols: Rc::new(RefCell::new(Symbols::with_outer(None))),
            constants: HashMap::new(),
            lets: None,
            chunk: Chunk::new("no_file", 1),
            max_regs: 0,
            tail: false,
            defers: 0,
            doc_string: None,
        }
    }

    pub fn new_state(
        file_name: &'static str,
        first_line: u32,
        outer: Option<Rc<RefCell<Symbols>>>,
    ) -> Self {
        let symbols = Rc::new(RefCell::new(Symbols::with_outer(outer)));
        CompileState {
            symbols,
            constants: HashMap::new(),
            lets: None,
            chunk: Chunk::new(file_name, first_line),
            max_regs: 0,
            tail: false,
            defers: 0,
            doc_string: None,
        }
    }

    pub fn reserved_regs(&self) -> usize {
        self.symbols.borrow().regs_count()
    }

    pub fn get_symbol(&self, sym: Interned) -> Option<usize> {
        self.symbols.borrow().data.borrow().syms.get(&sym).copied()
    }

    pub fn add_constant(&mut self, exp: Value) -> usize {
        if let Some(i) = self.constants.get(&exp) {
            *i
        } else {
            let const_i = self.chunk.add_constant(exp);
            self.constants.insert(exp, const_i);
            const_i
        }
    }
}

/// Data for the current namespace
#[derive(Clone, Debug)]
pub struct Namespace {
    name: String,
    imports: Vec<(String, Option<String>)>,
}

impl Namespace {
    pub fn new_with_name(name: String) -> Self {
        Self {
            name,
            imports: vec![],
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

impl Default for Namespace {
    fn default() -> Self {
        Self {
            name: "".to_string(),
            imports: vec![],
        }
    }
}

pub struct CompileEnvironment {
    use_line: bool,
    line: u32,
    specials: Option<Specials>,
    global_map: HashMap<Interned, usize>,
    gensym_idx: usize,
    namespace: Namespace,
}

impl Default for CompileEnvironment {
    fn default() -> Self {
        Self::new()
    }
}

impl CompileEnvironment {
    pub fn new() -> Self {
        Self {
            use_line: true,
            line: 1,
            specials: None,
            global_map: HashMap::new(),
            gensym_idx: 0,
            namespace: Namespace {
                name: "".to_string(),
                imports: vec![],
            },
        }
    }

    pub fn next_gensym(&mut self) -> usize {
        let r = self.gensym_idx;
        self.gensym_idx += 1;
        r
    }

    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn set_namespace(&mut self, namespace: Namespace) {
        self.namespace = namespace;
    }

    pub fn add_ns_import(&mut self, ns: String, alias: Option<String>) {
        for (ns_name, ns_alias) in self.namespace.imports.iter_mut() {
            if ns_name == &ns {
                *ns_alias = alias;
                return;
            }
        }
        self.namespace.imports.push((ns, alias));
    }

    pub fn get_namespace(&self) -> &Namespace {
        &self.namespace
    }
}

pub type SloshVm = GVm<CompileEnvironment>;

pub trait SloshVmTrait {
    fn set_line_val(&mut self, state: &mut CompileState, val: Value);
    fn get_reserve_global(&mut self, symbol: Interned) -> u32;
    fn set_named_global(&mut self, string: &str, value: Value) -> u32;
    fn set_global_builtin(&mut self, string: &str, func: CallFuncSig<CompileEnvironment>) -> u32;
    fn dump_globals(&self);
    fn globals(&self) -> &HashMap<Interned, usize>;
    fn own_line(&self) -> Option<u32>;
    fn set_line_num(&mut self, line_num: u32);
    fn line_num(&self) -> u32;
    fn specials(&self) -> &Specials;
    fn global_intern_slot(&self, symbol: Interned) -> Option<u32>;
}

pub fn new_slosh_vm() -> SloshVm {
    let temp_env = CompileEnvironment::new();
    let mut vm = GVm::new_with_env(temp_env);
    let specials = Specials::new(&mut vm);
    vm.env_mut().specials = Some(specials);
    vm
}

impl SloshVmTrait for SloshVm {
    fn set_line_val(&mut self, state: &mut CompileState, val: Value) {
        if let (Some(Value::Int(dline)), Some(Value::StringConst(file_intern))) = (
            self.get_heap_property(val, "dbg-line"),
            self.get_heap_property(val, "dbg-file"),
        ) {
            let dline = from_i56(&dline) as u32;
            let file_name = self.get_interned(file_intern);
            if file_name == state.chunk.file_name && dline > self.env().line {
                self.env_mut().line = dline;
            }
        }
    }

    fn get_reserve_global(&mut self, symbol: Interned) -> u32 {
        if let Some(idx) = self.env().global_map.get(&symbol) {
            *idx as u32
        } else {
            let idx = self.reserve_global();
            self.env_mut().global_map.insert(symbol, idx as usize);
            idx
        }
    }

    fn set_named_global(&mut self, string: &str, value: Value) -> u32 {
        let sym = self.intern(string);
        let slot = self.get_reserve_global(sym);
        self.set_global(slot, value);
        slot
    }

    fn set_global_builtin(&mut self, string: &str, func: CallFuncSig<CompileEnvironment>) -> u32 {
        let f_val = self.add_builtin(func);
        self.set_named_global(string, f_val)
    }

    fn dump_globals(&self) {
        println!("GLOBALS:");
        let mut ordered_keys = Vec::with_capacity(self.env().global_map.len());
        ordered_keys.resize(self.env().global_map.len(), "");
        for (k, v) in self.env().global_map.iter() {
            ordered_keys[*v] = self.get_interned(*k);
        }
        for (i, k) in ordered_keys.iter().enumerate() {
            println!(
                "({:#010x})/{}: {}",
                i,
                *k,
                self.get_global(i as u32).display_value(self)
            );
        }
        println!();
    }

    fn globals(&self) -> &HashMap<Interned, usize> {
        &self.env().global_map
    }

    fn own_line(&self) -> Option<u32> {
        if self.env().use_line {
            Some(self.env().line)
        } else {
            None
        }
    }

    fn set_line_num(&mut self, line_num: u32) {
        if self.env().use_line {
            self.env_mut().line = line_num;
        }
    }

    fn line_num(&self) -> u32 {
        if self.env().use_line {
            self.env().line
        } else {
            0
        }
    }

    fn specials(&self) -> &Specials {
        self.env().specials.as_ref().expect("specials are missing!")
    }

    fn global_intern_slot(&self, symbol: Interned) -> Option<u32> {
        fn check_global(vm: &SloshVm, ns: &str, sym: &str) -> Option<u32> {
            let mut ns = ns.to_string();
            ns.push_str("::");
            ns.push_str(sym);
            if let Some(i) = vm.get_if_interned(&ns) {
                if let Some(global) = vm.env().global_map.get(&i).copied().map(|i| i as u32) {
                    return Some(global);
                }
            }
            None
        }
        fn is_alias(alias: &str, sym: &str) -> bool {
            let mut a_i = alias.chars();
            let mut s_i = sym.chars().peekable();
            let mut is_alias = true;
            while let (Some(ach), Some(sch)) = (a_i.next(), s_i.peek()) {
                if ach != *sch {
                    is_alias = false;
                    break;
                }
                s_i.next();
            }
            if is_alias {
                if let (Some(':'), Some(':')) = (s_i.next(), s_i.next()) {
                    return true;
                }
            }
            false
        }

        let sym = self.get_interned(symbol);
        if let Some(g) = check_global(self, &self.env().namespace.name, sym) {
            return Some(g);
        }
        for (import, alias) in &self.env().namespace.imports {
            if let Some(alias) = alias {
                if is_alias(alias, sym) {
                    let s = sym.replacen(alias, import, 1);
                    if let Some(i) = self.get_if_interned(&s) {
                        if let Some(global) =
                            self.env().global_map.get(&i).copied().map(|i| i as u32)
                        {
                            return Some(global);
                        }
                    }
                }
            } else if let Some(g) = check_global(self, import, sym) {
                return Some(g);
            }
        }
        self.env()
            .global_map
            .get(&symbol)
            .copied()
            .map(|i| i as u32)
    }
}
