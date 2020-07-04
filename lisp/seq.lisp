;;; Forms that work with sequences (list or vectors).
;(if (ns-exists? 'core) (ns-enter 'core) (ns-create 'core))

(defn seq?
"Usage: (seq? expression) -> t/nil

True if expression is a sequence, nil otherwise.

Section: sequence

Example:
(test::assert-true (seq? '(1 2 3)))
(test::assert-true (seq? '#(1 2 3)))
(test::assert-true (seq? '()))
(test::assert-true (seq? '#()))
(test::assert-false (seq? \"aaa\"))
(test::assert-false (seq? 1))
"
    (obj)
    (or (vec? obj)(list? obj)))

(defn empty-seq?
"Usage: (empty-seq? obj)

`empty-seq?` returns true if a list or vector is empty and false
otherwise. If a non list or non vector is passed in it returns false.

Section: sequence
"
	(obj)
	(if (vec? obj)
		(vec-empty? obj)
		(if (list? obj)
			(not obj)
			nil)))

(defn non-empty-seq?
"Usage: (non-empty-seq? obj)

`non-empty-seq?` returns true if a list or vector is non-empty and false
otherwise. If a non list or non vector is passed in it returns false.

Section: sequence
"
	(obj)
	(if (vec? obj)
		(not (vec-empty? obj))
		(if (list? obj)
			(not (not obj))
			nil)))

(defn last
"
Produces the last element in the list.  Nil if the list is empty.

Section: sequence
"
    (obj)
    (if (vec? obj)
        (vec-nth (- (length obj) 1) obj)
        (if (list? obj)
            (if (null (cdr obj))
                (car obj)
                (recur (cdr obj)))
            (err "Not a vector or list"))))

(defn butlast
"
Produces the provided list minus the last element.  Nil if the list is empty or one element.

Section: sequence
"
    (obj)
    (if (vec? obj)
        (vec-slice obj 0 (- (length obj) 1))
        (if (list? obj) (progn
            (defq new-link (join nil nil))
            (if (null (cdr obj))
                (setq new-link nil)
                (setq new-link (join (car obj) (butlast (cdr obj)))))
            new-link)
            (err "Not a vector or list"))))

(defn setnth!
"
Sets idx item in the vector or list to obj, produces nil or errors on invalid input.
This is destructive!  Because vectors support indexing and lists do not, this is
a much faster operation for a vector (uses [builtin](root::builtin?) [vec-setnth!](root::vec-setnth!)
on input of type vector).

Section: sequence
Example:
(defq vctr (vec 0 1 2 3))
(defq vec-copy (collect-copy vctr))
(setnth! 0 -5 vctr)
(setnth! 1 -400000 vctr)
(setnth! 2 -402202 vctr)
(setnth! 3 -30000 vctr)
(assert-not-equal vec-copy vctr)
(setnth! 0 -4 vctr)
(setnth! 1 -3 vctr)
(setnth! 2 -2 vctr)
(setnth! 3 -1 vctr)
(assert-equal (list -4 -3 -2 -1) vctr)

(defq lst (list 0 1 2 3))
(defq list-copy (collect-copy lst))
(setnth! 0 -4 lst)
(setnth! 1 -3 lst)
(setnth! 2 -2 lst)
(setnth! 3 -1 lst)
(assert-not-equal list-copy lst)
(setnth! 0 -4 lst)
(setnth! 1 -3 lst)
(setnth! 2 -2 lst)
(setnth! 3 -1 lst)
(assert-equal (list -4 -3 -2 -1) lst)
"
    (idx obj l)
    (if (vec? l)
        (progn (vec-setnth! idx obj l) nil)
        (if (list? l)
            (if (= idx 0) (progn (xar! l obj) nil) (recur (- idx 1) obj (cdr l)))
            (err "Not a vector or list"))))

(defn in?
"
Takes a [seq?](#core::seq?) that is not an [empty-seq?](#core::empty-seq?) and returns true if the second argument is is in list, false otherwise.

Section: sequence

Example:
(let ((vowels-list (list 'a 'e 'i 'o 'u)))
    (assert-true (in? vowels-list 'u))
    (assert-false (in? vowels-list 'c))
    (assert-true (in? (list (list)) (list)))
    (assert-false (in? 8 18)))
"
  (seq-to-search item-to-match)
    (when (and (seq? seq-to-search ) (not (empty-seq? seq-to-search)))
        (if (= item-to-match (first seq-to-search)) #t (recur (rest seq-to-search) item-to-match))))

(defn qsort
"Usage: (qsort sequence comp-lambda?) -> [sorted vector]

Sort a sequence using the quick sort algorithm.  Returns a vector of the sorted sequence.

The comp-lambda argument is optional, if provided it should be a lambda or
builtin that takes two arguments and return t or nil (it is the compare
function for the sort).  Defaults to < if not provided.

Section: sequence

Example:
(test::assert-equal '(1 2 3) (qsort '(2 3 1)))
(test::assert-equal '(1 2 3) (qsort '#(2 3 1)))
(test::assert-equal '(3 2 1) (qsort '(2 3 1) >))
(test::assert-equal '(3 2 1) (qsort '#(2 3 1) (fn (a b) (< b a))))
(test::assert-equal '(\"aaa\" \"aab\" \"aba\" \"baa\" \"bab\" \"ccc\")
    (qsort '(\"aaa\" \"aab\" \"aba\" \"baa\" \"bab\" \"ccc\")))
(test::assert-equal '(\"aaa\" \"aab\" \"aba\" \"baa\" \"bab\" \"ccc\")
    (qsort '(\"ccc\" \"bab\" \"baa\" \"aba\" \"aab\" \"aaa\")))
(test::assert-equal '(\"aaa\" \"aab\" \"aba\" \"baa\" \"bab\" \"ccc\")
    (qsort '(\"aba\" \"bab\" \"aab\" \"ccc\" \"baa\" \"aaa\")))
(test::assert-equal '(\"ccc\" \"bab\" \"baa\" \"aba\" \"aab\" \"aaa\")
    (qsort '(\"aba\" \"bab\" \"aab\" \"ccc\" \"baa\" \"aaa\") >))
(test::assert-equal '(\"ccc\" \"bab\" \"baa\" \"aba\" \"aab\" \"aaa\")
    (qsort '(\"aba\" \"bab\" \"aab\" \"ccc\" \"baa\" \"aaa\") (fn (a b) (> a b))))
(test::assert-equal '() (qsort '()))
(test::assert-equal '() (qsort '#()))
(test::assert-equal '#() (qsort '()))
(test::assert-equal '#() (qsort '#()))
"
    (lst &rest comp) (progn
    (defn quick-inner (comp-fn sorted to-sort) (progn
        (if (> (length to-sort) 0) (progn
            (def 'lst (vec-pop! to-sort))
            (if (not (seq? lst))
                (progn
                    (vec-push! sorted lst)
                    (recur comp-fn sorted to-sort))
                (if (<= (length lst) 1)
                    (progn
                        (if (= (length lst) 1)
                            (vec-push! sorted (vec-pop! lst)))
                        (recur comp-fn sorted to-sort))
                    (progn
                        (def 'pivot (first lst))
                        (def 'less (vec))
                        (def 'greater (vec))
                        (col-for i in (rest lst)
                            (if (comp-fn i pivot) (vec-push! less i) (vec-push! greater i)))
                        (vec-push! to-sort greater)
                        (vec-push! to-sort pivot)
                        (vec-push! to-sort less)
                        (recur comp-fn sorted to-sort)))))
            sorted)))

    (if (> (length comp) 1) (err "qsort takes one option compare lambda"))
    (def 'comp-fn (if (= (length comp) 1) (first comp) <))
    (if (not (or (lambda? comp-fn)(builtin? comp-fn))) (err "compare must be a callable"))
    (def 'sorted (vec))
    (def 'to-sort (vec))
    (vec-push! to-sort lst)
    (quick-inner comp-fn sorted to-sort)
    sorted))

;(ns-export '(seq? non-empty-seq? empty-seq? last butlast setnth! in? qsort))

