;; Shorthands for strings of car/cdr calls.
(defn caar
"Shorthand for car/cdr calls (a is car, d is cdr)

Section: pair-ext

Example:
(assert-equal 1 (caar '((1) 2 3)))"
  (lst) (car (car lst)))

(defn cadr
"Shorthand for car/cdr calls (a is car, d is cdr)

Section: pair-ext

Example:
(assert-equal 2 (cadr '(1 2 3)))"
  (lst) (car (cdr lst)))

(defn cdar
"Shorthand for car/cdr calls (a is car, d is cdr)

Section: pair-ext

Example:
(assert-equal '(4 5) (cdar '((1 4 5) 2 3)))"
  (lst) (cdr (car lst)))

(defn cddr
"Shorthand for car/cdr calls (a is car, d is cdr)

Section: pair-ext

Example:
(assert-equal '(3) (cddr '((1 4 5) 2 3)))"
  (lst) (cdr (cdr lst)))

(defn caaar
"Shorthand for car/cdr calls (a is car, d is cdr)

Section: pair-ext

Example:
(assert-equal 1 (caaar '(((1 4) 5) (6 3) 2)))"
  (lst) (car (car (car lst))))

(defn caadr
"Shorthand for car/cdr calls (a is car, d is cdr)

Section: pair-ext

Example:
(assert-equal 6 (caadr '((1 4 5) (6 3) 2)))"
  (lst) (car (car (cdr lst))))

(defn cadar
"Shorthand for car/cdr calls (a is car, d is cdr)

Section: pair-ext

Example:
(assert-equal 4 (cadar '((1 4 5) (6 3) 2)))"
  (lst) (car (cdr (car lst))))

(defn caddr
"Shorthand for car/cdr calls (a is car, d is cdr)

Section: pair-ext

Example:
(assert-equal 6 (caddr '((1 4 5) 2 6)))"
  (lst) (car (cdr (cdr lst))))

(defn cdaar
"Shorthand for car/cdr calls (a is car, d is cdr)

Section: pair-ext

Example:
(assert-equal '(7 8) (cdaar '(((1 7 8) 4 5) 2 (6 3))))"
  (lst) (cdr (car (car lst))))

(defn cdadr
"Shorthand for car/cdr calls (a is car, d is cdr)

Section: pair-ext

Example:
(assert-equal '(9) (cdadr '(((1 7 8) 4 5) (2 9) (6 3))))"
  (lst) (cdr (car (cdr lst))))

(defn cddar
"Shorthand for car/cdr calls (a is car, d is cdr)

Section: pair-ext

Example:
(assert-equal '(5) (cddar '(((1 7 8) 4 5) 2 (6 3))))"
  (lst) (cdr (cdr (car lst))))

(defn cdddr
"Shorthand for car/cdr calls (a is car, d is cdr)

Section: pair-ext

Example:
(assert-equal '(3 9) (cdddr '(((1 7 8) 4 5) 2 6 3 9)))"
  (lst) (cdr (cdr (cdr lst))))

(defn cadddr
"Shorthand for car/cdr calls (a is car, d is cdr)

Section: pair-ext

Example:
(assert-equal 6 (cadddr '((1 7 8) (4 5) 2 6 (3 9))))"
  (lst) (car (cdr (cdr (cdr lst)))))

