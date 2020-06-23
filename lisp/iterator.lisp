(if (ns-exists? 'iterator) (ns-enter 'iterator) (ns-create 'iterator))

(ns-import 'struct)

(deftrait iterator
"Usage: (defstruct iter (:fn next! (self)...)(:fn empty? (self)...)(:impl iterator))

Trait that provides iterator methods.
Requires a struct to define methods next! and empty?

Example:
(ns-import 'struct)
(ns-import 'iterator)
(defstruct test-iter
  ; fields
  (current 0)
  ; methods
  (:fn next! (self) (progn (def 'val current)(set 'current (+ 1 current)) val))
  (:fn empty? (self) (>= current 3))
  (:impl iterator))
(def 'tmap (test-iter))
(assert-false (tmap :empty?))
(assert-equal 0 (tmap :next!))
(assert-equal 1 (tmap :next!))
(assert-equal 2 (tmap :next!))
(assert-true (tmap :empty?))
"
  ;(:fn next! (s) ((iter s) :next!))
  ;(:fn empty? (s) ((iter s) :empty?))
  (:fn collect
"Collect all the values into a list.  This will consume the iterator and
produce a new list.

Example:
(def 'collect-test ((iter '#(1 2 3)) :collect))
(assert-true (list? collect-test))
(assert-equal '(1 2 3) collect-test)
"
    (self) (progn
    (def 'tseq nil)
    (def 'tcell nil)
    (def 'head nil)
    (sfor v self (progn
        (if (null head)
            (progn (set 'tseq (set 'head (join v nil))))
            (progn (set 'tcell (join v nil)) (xdr! tseq tcell) (set 'tseq tcell)))))
    head))
  (:fn collect-vec
"Collect all the values into a vector.  This will consume the iterator and
produce a new list.

Example:
(def 'collect-vec-test ((iter '(1 2 3)) :collect-vec))
(assert-true (vec? collect-vec-test))
(assert-equal '#(1 2 3) collect-vec-test)
"
    (self) (progn
    (def 'tseq (vec))
    (sfor v self (vec-push! tseq v))
    tseq))

  (:fn map
"Apply the provided function to each element of the iterator.  Map is lazy.

Example:
(def 'tmap ((test-iter) :map (fn (x) (+ 1 x))))
(assert-false (tmap :empty?))
(assert-equal 1 (tmap :next!))
(assert-equal 2 (tmap :next!))
(assert-equal 3 (tmap :next!))
(assert-true (tmap :empty?))
"
       (self map-fn) ((map-iter) :init map-fn self))
  (:fn filter
"Apply the provided predicate to the iterator producing only elements that are true.  Filter is lazy.

Example:
(def 'tmap ((test-iter) :filter (fn (x) (not (= x 1)))))
(assert-false (tmap :empty?))
(assert-equal 0 (tmap :next!))
(assert-equal 2 (tmap :next!))
(assert-true (tmap :empty?))
"
       (self predicate) ((filter-iter) :init predicate self))
  (:fn nth
"Consume the iterator until the nth element and return it (0 based).
Note that repeated called to nth will return new data since it consumes the iterator.

Example:
(def 'tmap ((list-iter) :init '(0 1 2 3 4)))
(assert-false (tmap :empty?))
(assert-equal 0 (tmap :nth 0))
(assert-equal 1 (tmap :nth 0))
(assert-equal 4 (tmap :nth 2))
(assert-true (tmap :empty?))
"
    (self idx) (progn
    (def 'ret nil)
    (def 'i 0)
    (loop (plist) (self) (if (not (plist :empty?))
                                        (if (= i idx)
                                          (set 'ret (plist :next!))
                                          (progn (set 'i (+ i 1))(plist :next!)(recur plist)))))
      ret)))

(defstruct list-iter
"Iterator that wraps a lisp.

Example:
(def 'test-list-iter ((list-iter) :init '(1 2 3)))
(assert-false (test-list-iter :empty?))
(assert-equal 1 (test-list-iter :next!))
(assert-equal 2 (test-list-iter :next!))
(assert-equal 3 (test-list-iter :next!))
(assert-true (test-list-iter :empty?))
"
  ; fields
  (data "Some data" nil)
  ; methods
  (:fn next! (self) (progn (def 'val (car data))(set 'data (cdr data)) val))
  (:fn empty? (self) (if data nil t))
  (:fn init (self l) (progn (if (list? l) (set 'data l) (err "list-iter requires a list")) self))
  (:impl iterator))

(defstruct vec-iter
"Iterator that wraps a vector.

Example:
(def 'test-vec-iter ((vec-iter) :init '#(1 2 3) 0))
(assert-false (test-vec-iter :empty?))
(assert-equal 1 (test-vec-iter :next!))
(assert-equal 2 (test-vec-iter :next!))
(assert-equal 3 (test-vec-iter :next!))
(assert-true (test-vec-iter :empty?))
"
  ; fields
  (data nil)
  (start 0)
  ; methods
  (:fn next! (self) (progn (def 'val (vec-nth start data))(set 'start (+ 1 start)) val))
  (:fn empty? (self) (>= start (length data)))
  (:fn nth (self idx) (progn (set 'start (+ start idx))(self :next!)))
  (:fn init (self v s) (progn (if (vec? v)
                                (progn (set 'data v) (set 'start s))
                                (err "seq-vec requires a vector")) self))
  (:impl iterator))

(defstruct range-iter
  ; fields
  (start 0)
  (end 0)
  (current 0)
  ; methods
  (:fn next! (self) (progn (def 'val current)(set 'current (+ 1 current)) val))
  (:fn empty? (self) (>= current end))
  (:fn init (self in-start in-end in-current) (progn
                                    (set 'start in-start)
                                    (set 'end in-end)
                                    (set 'current in-current)
                                    self))
  (:fn count (self total) (progn
                                    (set 'start 0)
                                    (set 'end total)
                                    (set 'current 0)
                                    self))
  (:impl iterator))

(defn iter? (thing)
  (and (meta-tag? thing :method:next!)
       (meta-tag? thing :method:empty?)))

(defn iter (thing)
  (if (iter? thing)
        thing
      (list? thing)
        ((list-iter) :init thing)
      (vec? thing)
        ((vec-iter) :init thing 0)
      (err "iter: requires a list or vector or existing iterator")))

(defstruct map-iter 
  ; fields
  (data nil)
  (map-fn nil)
  ; methods
  (:fn next! (self) (map-fn (data :next!)))
  (:fn empty? (self) (data :empty?))
  (:fn init (self mfn d) (progn (set 'data (iter d))
                                (set 'map-fn mfn)
                                self))
  (:impl iterator))

(defstruct filter-iter
  ; fields
  (data nil)
  (predicate nil)
  (next nil)
  (is-empty nil)
  ; methods
  (:fn next! (self) (progn (def 'val next)(self :advance-data!) val))
  (:fn empty? (self) is-empty)
  (:fn advance-data! (self) (progn 
         (loop (plist) (data) (if (not (plist :empty?))
             (progn
               (set 'next (plist :next!))
               (if (not (predicate next))
                 (recur plist)))
             (set 'is-empty t)))))
  (:fn init (self pred data-in) (progn
          (set 'data (iter data-in))
          (set 'predicate pred)
          (set 'is-empty (data :empty?))
          (self :advance-data!)
          self))
  (:impl iterator))

(defn next! (s) ((iter s) :next!))
(defn empty?
"Is an iterator empty (no more items)?  Will call iter on input first.

Example:
(assert-true (iterator::empty? nil))
(assert-true (iterator::empty? '#()))
(assert-false (iterator::empty? '#(1)))
"
    (s) ((iter s) :empty?))
(defn range (&rest i) (progn
    (def 'si (iter i))
    (if (= (length i) 1)
        ((range-iter) :count (si :next!))
        (if (= (length i) 2)
            (progn (def 'first (si :next!)) ((range-iter) :init first (si :next!) first)))
            (err "range: requires one or two integers"))))

(defn collect
"Collect all the values into a list.  This will consume the iterator and
produce a new list.  Will call iter on input to turn a collection into an iterator.

Example:
(def 'collect-test (iterator::collect '#(1 2 3)))
(assert-true (list? collect-test))
(assert-equal '(1 2 3) collect-test)
"
    (s)
    (if (list? s) s ((iter s) :collect)))
(defn collect-vec
"Collect all the values into a vector.  This will consume the iterator and
produce a new list.  Will call iter on input to turn a collection into an iterator.

Example:
(def 'collect-vec-test (iterator::collect-vec '(1 2 3)))
(assert-true (vec? collect-vec-test))
(assert-equal '#(1 2 3) collect-vec-test)
"
    (s)
    (if (vec? s) s ((iter s) :collect-vec)))

(defn smap (map-fn items) ((iter items) :map map-fn))
(defn sfilter (predicate items) ((iter items) :filter predicate))
(defn snth (idx coll) ((iter coll) :nth idx))

(defmacro sfor
"
bind is bound to the current element of in_list and is accesible
in body. body is evaluated a number of times equal to the the number of items
in in_list.


Section: shell
"
	(bind in_list body) (progn
	`(loop (plist) ((iterator::iter ,in_list)) (if (not (plist :empty?)) (progn
		(def ',bind (plist :next!))
		(,@body)
		(recur plist))))))

(ns-export '(
    iterator
    vec-iter
    list-iter
    iter?
    iter
    map-iter
    filter-iter
    range-iter
    next!
    empty?
    range
    collect
    collect-vec
    smap
    sfilter
    snth
    sfor))
