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
        (do
            (def 'tseq (make-vec (length seq)))
            (iterator::for el in seq (vec-push! tseq el))
            tseq)
        (if (list? seq)
            (do
                (def 'tseq nil)
                (def 'tcell nil)
                (def 'head nil)
                (iterator::for el in seq (do
                    (if (null head)
                        (do (set 'tseq (set 'head (join el nil))))
                        (do (set 'tcell (join el nil)) (xdr! tseq tcell) (set 'tseq tcell)))))
                head)
            (err "Not a list or vector."))))

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
    (if (vec? obj) (if (vec-empty? obj) nil (vec-nth 0 obj))
        (list? obj) (car obj)
        (err "Not a vector or list")))

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
    (if (vec? obj) (vec-slice obj 1)
        (list? obj) (cdr obj)
        (err "Not a vector or list")))

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

