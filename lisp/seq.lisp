;;; These are forms that deal with "sequences"/"collections", i.e. lists or vectors primarily.
;;; They are simple forms that may do a test on input to determine the type.
;;; In general use iterators for much more powerful and general abstractions over
;;; collections but these can still be useful for dealing with list/vectors.

(defn seq?
"Usage: (seq? expression) -> t/nil

True if expression is a list or vector, nil otherwise.

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
"Usage: (empty-seq? obj) -> t/nil

`empty-seq?` returns true if a list or vector is empty and false/nil
otherwise. If a non list or non vector is passed in it returns nil.

Section: sequence

Example:
(test::assert-false (empty-seq? '(1 2 3)))
(test::assert-false (empty-seq? '#(1 2 3)))
(test::assert-true (empty-seq? '()))
(test::assert-true (empty-seq? '#()))
(test::assert-false (empty-seq? \"aaa\"))
(test::assert-false (empty-seq? 1))
"
    (obj)
    (if (vec? obj) (vec-empty? obj)
        (list? obj) (not obj)
        nil))

(defn non-empty-seq?
"Usage: (non-empty-seq? obj) -> t/nil

`non-empty-seq?` returns true if a list or vector is not empty and false/nil
otherwise. If a non list or non vector is passed in it returns nil.

Section: sequence

Example:
(test::assert-true (non-empty-seq? '(1 2 3)))
(test::assert-true (non-empty-seq? '#(1 2 3)))
(test::assert-false (non-empty-seq? '()))
(test::assert-false (non-empty-seq? '#()))
(test::assert-false (non-empty-seq? \"aaa\"))
(test::assert-false (non-empty-seq? 1))
"
    (obj)
    (if (vec? obj) (not (vec-empty? obj))
        (list? obj) (not (not obj))
        nil))

(defn last
"
Produces the last element in a list or vector.  Nil if the list/vector is empty.

Section: sequence

Example:
(assert-equal 3 (last '(1 2 3)))
(assert-equal 3 (last '#(1 2 3)))
(assert-equal nil (last '()))
(assert-equal nil (last nil))
(assert-equal nil (last '#()))
"
    (obj)

    (var last-list (fn (obj)
        (if (null (cdr obj)) (car obj)
            (recur (cdr obj)))))

    (if (vec? obj) (if (> (length obj) 0) (vec-nth obj (- (length obj) 1)) nil)
        (list? obj) (last-list obj)
        (err "Not a vector or list")))

(defn butlast
"
Produces the provided list minus the last element.  Nil if the list is empty or one element.

Section: sequence

Example:
(assert-equal '(1 2) (butlast '(1 2 3)))
(assert-equal '(1 2) (butlast '#(1 2 3)))
(assert-equal nil (butlast '(1)))
(assert-equal nil (butlast '#(1)))
(assert-equal nil (butlast '()))
(assert-equal nil (butlast nil))
(assert-equal nil (butlast '#()))
"
    (obj)
    (if (vec? obj) (if (> (length obj) 0) (vec-slice obj 0 (- (length obj) 1)) nil)
        (list? obj) (do
            (var new-link (join nil nil))
            (if (null (cdr obj))
                (set! new-link nil)
                (set! new-link (join (car obj) (butlast (cdr obj)))))
            new-link)
        (err "Not a vector or list")))

(defn setnth!
"
Sets idx item in the vector or list to obj, produces nil or errors on invalid input.
This is destructive!  Because vectors support indexing and lists do not, this is
a much faster operation for a vector (uses [builtin](root::builtin?) [vec-set!](root::vec-set!)
on input of type vector).  Return the list or vector that was modified.

Section: sequence
Example:
(def vctr (vec 0 1 2 3))
(def vec-copy (collect-copy vctr))
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
(assert-equal '(1 5 3) (setnth! 1 5 '#(1 2 3)) \" Vector check\")
(assert-error (setnth! 0 1 '#()))
(assert-error (setnth! 0 1 (vec)))

(def lst (list 0 1 2 3))
(def list-copy (collect-copy lst))
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
(assert-equal '(1 5 3) (setnth! 1 5 '(1 2 3)) \" List check\")
(assert-error (setnth! 0 1 '()))
(assert-error (setnth! 0 1 (list)))
"
    (idx obj sequence)

    (var setnth-list (fn (idx obj l) (if (= idx 0) (do (xar! l obj) nil) (recur (- idx 1) obj (cdr l)))))

    (if (empty-seq? sequence) (err "setnth!: Not a sequence or empty!"))
    (if (vec? sequence) (vec-set! sequence idx obj)
        (list? sequence) (do (setnth-list idx obj sequence) sequence)
        (err "setnth!: Not a vector or list")))

(defn in?
"
Takes a [seq?](#root::seq?) that is not an [empty-seq?](#root::empty-seq?) and
returns true if the second argument is is in list, false otherwise.

Section: sequence

Example:
(let ((vowels-list (list 'a 'e 'i 'o 'u)))
    (assert-true (in? vowels-list 'u))
    (assert-false (in? vowels-list 'c))
    (assert-true (in? (list (list)) (list)))
    (assert-false (in? 8 18)))
"
  (seq-to-search item-to-match)
    (when (or (seq? seq-to-search)(iterator::iter? seq-to-search)) (do
        (var seq-iter (iterator::iter seq-to-search))
        (var inner-in (fn (seq-iter item-to-match)
            (if (iterator::empty? seq-iter) nil
                (if (= item-to-match (iterator::next! seq-iter)) t
                    (recur seq-iter item-to-match)))))
        (inner-in seq-iter item-to-match))))

(defn collect-copy
"
Produces a copy of the provided list (copy has same type as the parameter).

Section: sequence

Example:
(def test-colcl '(1 2 3))
(assert-true (list? test-colcl))
(def test-colcl2 (collect-copy test-colcl))
(assert-true (list? test-colcl2))
(assert-equal test-colcl test-colcl2)

(def test-colcv '#(1 2 3))
(assert-true (vec? test-colcv))
(def test-colcv2 (collect-copy test-colcv))
(assert-true (vec? test-colcv2))
(assert-equal test-colcv test-colcv2)
"
(seq)
    (if (vec? seq)
        (do
            (var tseq (make-vec (length seq)))
            (iterator::for el in seq (vec-push! tseq el))
            tseq)
        (if (list? seq)
            (do
                (var tseq nil)
                (var tcell nil)
                (var head nil)
                (iterator::for el in seq (do
                    (if (null head)
                        (do (set! tseq (set! head (join el nil))))
                        (do (set! tcell (join el nil)) (xdr! tseq tcell) (set! tseq tcell)))))
                head)
            (err "Not a list or vector."))))

(defn first
"
Produces the first element of the provided list or vector.  Nil if the
list/vector is nil/empty.  Note this is like car that works for lists and
vectors.

Section: sequence

Example:
(assert-equal 1 (first '(1 2 3)))
(assert-equal 1 (first '#(1 2 3)))
(assert-equal nil (first '()))
(assert-equal nil (first nil))
(assert-equal nil (first '#()))
"
    (obj)
    (if (vec? obj) (if (vec-empty? obj) nil (vec-nth obj 0))
        (list? obj) (car obj)
        (err "Not a vector or list")))

(defn rest
"
Produces the provided list or vector minus the first element.  Nil if the
list/vector is nil/empty or one element.  Note this is like cdr that works for
lists and vectors.  This calls vec-slice to create a new vector when called with
a vector (i.e. is much more efficient with lists).

Section: sequence

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
    (lst &rest comp)
    (var quick-inner (fn (comp-fn sorted to-sort)
        (if (> (length to-sort) 0) (do
            (var lst (vec-pop! to-sort))
            (if (not (seq? lst))
                (do
                    (vec-push! sorted lst)
                    (recur comp-fn sorted to-sort))
                (if (<= (length lst) 1)
                    (do
                        (if (= (length lst) 1)
                            (vec-push! sorted (vec-pop! lst)))
                        (recur comp-fn sorted to-sort))
                    (do
                        (var pivot (first lst))
                        (var less (vec))
                        (var greater (vec))
                        (iterator::for i in (rest lst)
                            (if (comp-fn i pivot) (vec-push! less i) (vec-push! greater i)))
                        (vec-push! to-sort greater)
                        (vec-push! to-sort pivot)
                        (vec-push! to-sort less)
                        (recur comp-fn sorted to-sort)))))
            sorted)))

    (if (> (length comp) 1) (err "qsort takes one option compare lambda"))
    (var comp-fn (if (= (length comp) 1) (first comp) <))
    (if (not (or (lambda? comp-fn)(builtin? comp-fn))) (err "compare must be a callable"))
    (var sorted (vec))
    (var to-sort (vec))
    (vec-push! to-sort lst)
    (quick-inner comp-fn sorted to-sort)
    sorted)

