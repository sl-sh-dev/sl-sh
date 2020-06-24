;;; These are forms that deal with "collections", i.e. lists or vectors primarily.
;;; They are simple forms that may do a test on input to determine the type.
;;; In general use iterators for much more powerful and general abstractions over
;;; collections but these can still be useful for dealing with list/vectors.

(defn collect-copy
"
Produces a copy of the provided list (copy has same type as the parameter).

Section: collection

Example:
(def 'test-colcl '(1 2 3))
(assert-true (list? test-colcl))
(def 'test-colcl2 (collect-copy test-colcl))
(assert-true (list? test-colcl2))
(assert-equal test-colcl test-colcl2)

(def 'test-colcv '#(1 2 3))
(assert-true (vec? test-colcv))
(def 'test-colcv2 (collect-copy test-colcv))
(assert-true (vec? test-colcv2))
(assert-equal test-colcv test-colcv2)
"
(seq)
    (if (vec? seq)
        (progn
            (def 'tseq (make-vec (length seq)))
            (for el seq (vec-push! tseq el))
            tseq)
        (if (list? seq)
            (progn
                (def 'tseq nil)
                (def 'tcell nil)
                (def 'head nil)
                (for el seq (progn
                    (if (null head)
                        (progn (set 'tseq (set 'head (join el nil))))
                        (progn (set 'tcell (join el nil)) (xdr! tseq tcell) (set 'tseq tcell)))))
                head)
            (err "Not a list or vector."))))

(defn nth
"
Produces the element at the provided index (0 based), error if index is out of bounds.
Because vectors support indexing and lists do not, this is a much faster
operation for a vector (uses [builtin](root::builtin?) [vec-nth](root::vec-nth)
on input of type vector). Supports negative indexing.

Section: collection

Example:
(defq mylist (list 0 1 2 3 4))
(defq myvec (vec 0 1 2 3 4))
(assert-equal 0 (root::nth -5 mylist))
(assert-equal 1 (root::nth 1 mylist))
(assert-equal 2 (root::nth 2 mylist))
(assert-equal 3 (root::nth -2 mylist))
(assert-equal 4 (root::nth 4 mylist))
(assert-equal 0 (root::nth -5 myvec))
(assert-equal 1 (root::nth 1 myvec))
(assert-equal 2 (root::nth 2 myvec))
(assert-equal 3 (root::nth -2 myvec))
(assert-equal 4 (root::nth 4 myvec))
"
    (idx obj) (progn
    (when (< idx 0)
        (if (> (* -1 idx) (length obj))
        (err "Absolute value of negative idx can't be greater than length of list.")
        (setq idx (+ idx (length obj)))))
    (if (vec? obj)
        (vec-nth idx obj)
        (if (list? obj)
        (if (= idx 0) (car obj) (recur (- idx 1) (cdr obj)))
            (err "Not a vector or list")))))

(defmacro for
"
bind is bound to the current element of in_list and is accesible
in body. body is evaluated a number of times equal to the the number of items
in in_list.


Section: collection
"
    (bind in_list body)
    `((fn (,bind)
        (if (> (length ,in_list) 0)
            (root::loop (plist) (,in_list) (progn
                (set ',bind (root::first plist))
                (,@body)
                (if (> (length plist) 1) (recur (root::rest plist)))))))nil))

(defmacro fori
"
idx-bind is an incrementing reference bound to current elemt in in_list and is
accesible in body. bind is bound to the current element of in_list as is also
accesible in body.  body is evaluated a number of times equal to the the number
of items in in_list.

Section: collection
"
    (idx_bind bind in_list body)
    `((fn () (progn
        (root::defq ,bind nil)(root::defq ,idx_bind nil)
        (if (> (length ,in_list) 0)
            (root::loop (plist idx) (,in_list 0) (progn
                (root::setq ,bind (root::first plist))
                (root::setq ,idx_bind idx)
                (,@body)
                (if (> (length plist) 1) (recur (root::rest plist) (+ idx 1))))))))))

(defn first
"
Produces the first element of the provided list or vector.  Nil if the
list/vector is nil/empty.  Note this is like car that works for lists and
vectors.

Section: collection

Example:
(assert-equal 1 (first '(1 2 3)))
(assert-equal 1 (first '#(1 2 3)))
(assert-equal nil (first '()))
(assert-equal nil (first nil))
(assert-equal nil (first '#()))
"
    (obj)
    (if (vec? obj)
        (if (vec-empty? obj) nil (vec-nth 0 obj))
        (if (list? obj)
            (car obj)
            (err "Not a vector or list"))))

(defn rest
"
Produces the provided list or vector minus the first element.  Nil if the
list/vector is nil/empty or one element.  Note this is like cdr that works for
lists and vectors.  This calls vec-slice to create a new vector when called with
a vector (i.e. is much more efficient with lists).

Section: collection

Example:
(assert-equal '(2 3) (rest '(1 2 3)))
(assert-equal '(2 3) (rest '#(1 2 3)))
(assert-equal nil (rest '(1)))
(assert-equal nil (rest '#(1)))
(assert-equal nil (rest '()))
(assert-equal nil (rest nil))
(assert-equal nil (rest '#()))
"
    (obj)
    (if (vec? obj)
        (vec-slice obj 1)
        (if (list? obj)
            (cdr obj)
            (err "Not a vector or list"))))

;; Shorthands for strings of car/cdr calls.
(defn caar
"Shorthand for car/cdr calls (a is car, d is cdr)

Section: collection

Example:
(assert-equal 1 (caar '((1) 2 3)))"
  (lst) (car (car lst)))

(defn cadr
"Shorthand for car/cdr calls (a is car, d is cdr)

Section: collection

Example:
(assert-equal 2 (cadr '(1 2 3)))"
  (lst) (car (cdr lst)))

(defn cdar
"Shorthand for car/cdr calls (a is car, d is cdr)

Section: collection

Example:
(assert-equal '(4 5) (cdar '((1 4 5) 2 3)))"
  (lst) (cdr (car lst)))

(defn cddr
"Shorthand for car/cdr calls (a is car, d is cdr)

Section: collection

Example:
(assert-equal '(3) (cddr '((1 4 5) 2 3)))"
  (lst) (cdr (cdr lst)))

(defn caaar
"Shorthand for car/cdr calls (a is car, d is cdr)

Section: collection

Example:
(assert-equal 1 (caaar '(((1 4) 5) (6 3) 2)))"
  (lst) (car (car (car lst))))

(defn caadr
"Shorthand for car/cdr calls (a is car, d is cdr)

Section: collection

Example:
(assert-equal 6 (caadr '((1 4 5) (6 3) 2)))"
  (lst) (car (car (cdr lst))))

(defn cadar
"Shorthand for car/cdr calls (a is car, d is cdr)

Section: collection

Example:
(assert-equal 4 (cadar '((1 4 5) (6 3) 2)))"
  (lst) (car (cdr (car lst))))

(defn caddr
"Shorthand for car/cdr calls (a is car, d is cdr)

Section: collection

Example:
(assert-equal 6 (caddr '((1 4 5) 2 6)))"
  (lst) (car (cdr (cdr lst))))

(defn cdaar
"Shorthand for car/cdr calls (a is car, d is cdr)

Section: collection

Example:
(assert-equal '(7 8) (cdaar '(((1 7 8) 4 5) 2 (6 3))))"
  (lst) (cdr (car (car lst))))

(defn cdadr
"Shorthand for car/cdr calls (a is car, d is cdr)

Section: collection

Example:
(assert-equal '(9) (cdadr '(((1 7 8) 4 5) (2 9) (6 3))))"
  (lst) (cdr (car (cdr lst))))

(defn cddar
"Shorthand for car/cdr calls (a is car, d is cdr)

Section: collection

Example:
(assert-equal '(5) (cddar '(((1 7 8) 4 5) 2 (6 3))))"
  (lst) (cdr (cdr (car lst))))

(defn cdddr
"Shorthand for car/cdr calls (a is car, d is cdr)

Section: collection

Example:
(assert-equal '(3 9) (cdddr '(((1 7 8) 4 5) 2 6 3 9)))"
  (lst) (cdr (cdr (cdr lst))))

(defn cadddr
"Shorthand for car/cdr calls (a is car, d is cdr)

Section: collection

Example:
(assert-equal 6 (cadddr '((1 7 8) (4 5) 2 6 (3 9))))"
  (lst) (car (cdr (cdr (cdr lst)))))

