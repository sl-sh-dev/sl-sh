;;; Forms that work with sequences (list or vectors).

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

(defn first
"
Produces the first element of the provided list.  Nil if the list is empty.

Section: sequence
"
    (obj)
    (if (vec? obj)
        (if (vec-empty? obj) nil (vec-nth 0 obj))
        (if (list? obj)
            (car obj)
            (err "Not a vector or list"))))

(defn rest
"
Produces the provided list minus the first element.  Nil if the list is empty or one element.

Section: sequence
"
    (obj)
    (if (vec? obj)
        (vec-slice obj 1)
        (if (list? obj)
            (cdr obj)
            (err "Not a vector or list"))))

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
(defq vec-copy (copy-seq vctr))
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
(defq list-copy (copy-seq lst))
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

(defn nth
"
Produces the element at the provided index (0 based), error if index is out of bounds.
Because vectors support indexing and lists do not, this is a much faster
operation for a vector (uses [builtin](root::builtin?) [vec-nth](root::vec-nth)
on input of type vector). Supports negative indexing.

Section: sequence

Example:
(defq mylist (list 0 1 2 3 4))
(defq myvec (vec 0 1 2 3 4))
(assert-equal 0 (nth -5 mylist))
(assert-equal 1 (nth 1 mylist))
(assert-equal 2 (nth 2 mylist))
(assert-equal 3 (nth -2 mylist))
(assert-equal 4 (nth 4 mylist))
(assert-equal 0 (nth -5 myvec))
(assert-equal 1 (nth 1 myvec))
(assert-equal 2 (nth 2 myvec))
(assert-equal 3 (nth -2 myvec))
(assert-equal 4 (nth 4 myvec))
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

(def 'append nil)
(def 'append!
"
Modifies the first list by appending the other lists onto it.

Section: sequence
"
nil)
(def 'fn-append! nil)
(def 'map nil)
(let ((tseq))
    (defn copy-els (to l) (progn
        (def 'tcell nil)
        (for el l
            (if (null to)
                (progn (set 'tseq (set 'to (join el nil))))
                (progn (set 'tcell (join el nil)) (xdr! tseq tcell) (set 'tseq tcell))))
        to))

    (defn last-cell (obj)
        (if (list? obj)
            (if (null (cdr obj))
                obj
                (recur (cdr obj)))
            (err "Not a list")))

    (setfn append
"
Produces a new list (type will be same as first parameter) by appending the other lists onto the first.

Section: sequence
"
        (l1 &rest others) (progn
        (def 'ret nil)
        (if (vec? l1)
            (progn
                (set 'ret (make-vec))
                (for el l1 (vec-push! ret el))
                (for l others
                    (if (seq? l)
                        (for el l (vec-push! ret el))
                        (vec-push! ret l))))
            (if (list? l1)
                (progn
                    (set 'ret (copy-els ret l1))
                    (for l others
                        (if (seq? l)
                            (set 'ret (copy-els ret l))
                            (progn
                                (def 'tcell (join l nil))
                                (if (not (null tseq))
                                    (xdr! tseq tcell))
                                (set 'tseq tcell)
                                (if (null ret) (set 'ret tseq))
                                ))))
                (err "append: First element not a list or vector.")))
        (set 'tseq nil)
        ret))

    (setfn fn-append! (ret &rest others) (progn
        (def 'tret ret)
        (if (vec? ret)
            (progn
                (for l others
                    (if (seq? l)
                        (for el l (vec-push! ret el))
                        (vec-push! ret l))))
            (if (list? ret)
                (progn
                    (set 'tseq (last-cell tret))
                    (for l others
                        (if (seq? l)
                            (set 'tret (copy-els tret l))
                            (progn
                                (if (null tseq)
                                    (xar! tseq l)
                                    (progn
                                        (def 'tcell (join l nil))
                                        (xdr! tseq tcell)
                                        (set 'tseq tcell)))
                                (if (null tret) (set 'tret tseq))
                                )))
                    (if (and (null ret) (not (null tret)))
                        (progn (xar! ret (car tret))(xdr! ret (cdr tret)))))
                (err "append!: First element not a list or vector.")))
        (set 'tseq nil)
        ret))

    ; If you have more then one reference to the same nil instance then only
    ; the reference passed to append! will change (ie symbols pointing to nil
    ; are unique even if one is set from the other).
    ; If using an actual sequence with two or more symbols pointing to it then
    ; all will be updated.
    (setmacro append!
"
Modifies the first list by appending the other lists onto it.

Section: sequence
"
			  (ret &rest others)
        `(if (and (symbol? (quote ,ret)) (null ,ret))
            (set (quote ,ret) (core::fn-append! ,ret ,@others))
            (core::fn-append! ,ret ,@others)))

    (defn map-into (fun items new-items) (progn
        (def 'tcell nil)
        (for i items
            (progn
                (if (null new-items)
                    (progn (set 'tseq (set 'new-items (join (fun i) nil))))
                    (progn (set 'tcell (join (fun i) nil)) (xdr! tseq tcell) (set 'tseq tcell)))))
        new-items))

    (setfn map
"
Returns a new list made by applying the lambda to each item in the provided list.

Section: sequence
"
        (fun items)
        (if (vec? items)
            (progn
                (defq new-items (make-vec (length items)))
                (for i items (vec-push! new-items (fun i)))
                new-items)
            (if (list? items)
                (progn
                    (defq new-items nil)
                    (set 'new-items (map-into(fun items new-items)))
                    (set 'tseq nil)
                    new-items)
                (if (null items)
                    nil
                    (err "Not a list or vector"))))))

(defn map!
"
Modifies a list by applying the lambda to each item in the list.

Section: sequence
"
    (fun items) (progn
    (fori i it items
        (setnth! i (fun it) items))
    items))

(defn reverse
"
Returns a new list made by reversing the elements of the provided list.

Section: sequence
"
    (items) (progn
    (if (vec? items)
        (progn
            (defn irev (items new-items num)
                (if (>= num 0) (progn (vec-push! new-items (nth num items))(recur items new-items (- num 1)))))
            (defq new-items (make-vec (length items)))
            (irev items new-items (- (length items) 1))
            new-items)
        (if (list? items)
            (progn
                (def 'titems (copy-seq items))
                (reverse! titems))
            (if (null items)
                nil
                (err "Not a list or vector."))))))

(defn reverse!
"
Modifies a list by reversing it's elements.

Section: sequence
"
(items) (progn

    (defn irev (items first last)
        (if (> last first) (progn
            (defq ftemp (nth first items))
            (setnth! first (nth last items) items)
            (setnth! last ftemp items)
            (recur items (+ first 1) (- last 1)))))

    (irev items 0 (- (length items) 1))
    items))

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
                        (for i (rest lst)
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

(defn filter
"
filter is used to strip a collection, coll, of all values that do not
pass the condition defined by the function, pred. Filter returns a new
collection.

Section: sequence

Example:
(assert-equal '(2 4) (filter (fn (x) (= (% x 2) 0)) (list 1 2 3 4 5)))
"
	(pred coll)
		(progn
			(defq filtering-fcn
				(fn (pred filtered-coll coll-to-filter)
					(progn (defq fst (first coll-to-filter))
					(if (empty-seq? coll-to-filter)
						filtered-coll
						(recur
							pred
							(if (pred fst) (append filtered-coll fst) filtered-coll)
							(rest coll-to-filter))))))
			(filtering-fcn pred (list) coll)))

(defn reduce
"
reduce is used to amalgamate a provided collection, coll, and an intitial value,
init-val, according to the reducing function, reducing-fcn, provided. The
reducing-fcn should be a function of two arguments. In the first iteration of
reduce, the init-val will be used as the first argument to the reducing-fcn and
(first coll) will be used as the second argument. For all subsequent iterations,
The result from the previous application of the reducing-fcn will be used as the
first argument to the reducing-fcn and the second argument will be the next item
in the collection when the collection is empty reduce will return the
amalgamated result.

Section: sequence

Example:

(assert-true (= 15 (reduce + 0 (list 1 2 3 4 5))))
(assert-false (= 15 (reduce + 1 (list 1 2 3 4 5))))
(assert-true (= \"one hoopy frood\" (reduce str \"\" (list \"one \" \"hoopy \" \"frood\"))))
"
	(reducing-fcn init-val coll)
		(if (empty-seq? coll)
				init-val
				(recur reducing-fcn (reducing-fcn init-val (first coll)) (rest coll))))

(defn reduce-times
"Apply wrapping-fcn to value number of times. Function is recursive. Recursive
binding for value is previous application of wrapping function to value.

Section: sequence

Example:

(assert-equal (list (list 3)) (reduce-times 3 list 2))
(assert-equal 5 (reduce-times (reduce-times 5 list 5) first 5))
"
	(value wrapping-fcn times)
	(if (<= times 0)
		value
		(recur (wrapping-fcn value) wrapping-fcn (- times 1))))

(defn slice
"Returns a slice of a seq (0 based indexes, end is exclusive and optional).

Section: sequence

Example:
(assert-equal '(5 6) (slice '#(1 2 3 4 5 6) 4 6))
(assert-equal '(5 6) (slice '(1 2 3 4 5 6) 4 6))
(assert-equal '(1 2 3) (slice '#(1 2 3 4 5 6) 0 3))
(assert-equal '(1 2 3) (slice '(1 2 3 4 5 6) 0 3))
(assert-equal '(3 4 5) (slice '#(1 2 3 4 5 6) 2 5))
(assert-equal '(3 4 5) (slice '(1 2 3 4 5 6) 2 5))
(assert-equal '(3 4 5 6) (slice '#(1 2 3 4 5 6) 2))
(assert-equal '(3 4 5 6) (slice '(1 2 3 4 5 6) 2))
(assert-equal nil (slice '#(1 2 3 4 5 6) 6))
(assert-equal nil (slice '(1 2 3 4 5 6) 6))"
	(lst start &rest end?) (block slice
	(if (= 0 (length end?))
		(if (vec? lst)
			(return-from slice (vec-slice lst start))
			(if (list? lst)
				(if (= (length lst) start)
					(return-from slice nil)
					(if (> start (length lst))
						(err (str "slice index out of range (start " start ", end 0, length " (length lst)))
						(return-from slice (reduce-times lst rest start))))
				(err "lst, must be a vector or list"))))
	(when (and (not (> (length end?) 1)) (not (int? (first end?))) (> (first end?) 0))
		(err "Function accepts two or three arguments. Third argument must be a postive integer."))
	(defq end (first end?))
	(if (vec? lst)
		(vec-slice lst start end)
		(if (list? lst)
			(loop (accrual iter) ((list) start)
				(if (>= iter end)
					accrual
					(progn
						(defq val (nth iter lst))
						(recur (append accrual (list val)) (+ 1 iter)))))
			(err "lst, must be a vector or list")))))

(ns-export '(seq? non-empty-seq? empty-seq? first rest last butlast setnth! nth append append! map map! reverse reverse! in? qsort filter reduce reduce-times slice))

