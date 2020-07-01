(if (ns-exists? 'iterator) (ns-enter 'iterator) (ns-create 'iterator))

(ns-import 'struct)

(deftrait iterator
"Usage: (defstruct iter (:fn next! (self)...)(:fn empty? (self)...)(:impl iterator))

Trait that provides iterator methods.
Requires a struct to define methods next! and empty?

Section: iterator

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
    (for v in self (progn
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
    (for v in self (vec-push! tseq v))
    tseq))

  (:fn collect-str
"Collect all the values into a string.  This will consume the iterator and
produce a new string.

Example:
(def 'collect-str-test (((iter \"λabc σ\") :map (fn (ch) (char-upper ch))) :collect-str))
(assert-true (string? collect-str-test))
(assert-equal \"ΛABC Σ\" collect-str-test)
"
    (self) (progn
    (def 'tseq "")
    (for v in self (str-push! tseq v))
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

  (:fn nth!
"Consume the iterator until the nth! element and return it (0 based).
Note that repeated called to nth! will return new data since it consumes the iterator.

Example:
(def 'tmap ((list-iter) :init '(0 1 2 3 4)))
(assert-false (tmap :empty?))
(assert-equal 0 (tmap :nth! 0))
(assert-equal 1 (tmap :nth! 0))
(assert-equal 4 (tmap :nth! 2))
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

(deftrait double-ended-iterator
"Usage: (defstruct iter (:fn next! (self)...)(:fn next-back! (self)...)(:fn empty? (self)...)(:impl iterator double-ended-iterator))

Trait that makes an iterator double ended (can get items from front and back.
Requires a struct to define methods next-back! and implement iterator.
Note that next! and next-back! can not cross, the iterator is empty when they meet.

Section: iterator

Example:
(ns-import 'struct)
(ns-import 'iterator)
(defstruct test-double-iter
  ; fields
  (current 0)
  (current-end 2)
  ; methods
  (:fn next! (self) (progn (def 'val current)(set 'current (+ 1 current)) val))
  (:fn next-back! (self) (progn (def 'val current-end)(set 'current-end (- current-end 1)) val))
  (:fn empty? (self) (> current current-end))
  (:impl iterator double-ended-iterator))
(def 'tmap (test-double-iter))
(assert-false (tmap :empty?))
(assert-equal 0 (tmap :next!))
(assert-equal 1 (tmap :next!))
(assert-equal 2 (tmap :next!))
(assert-true (tmap :empty?))
(def 'tmap (test-double-iter))
(assert-false (tmap :empty?))
(assert-equal 2 (tmap :next-back!))
(assert-equal 1 (tmap :next-back!))
(assert-equal 0 (tmap :next-back!))
(assert-true (tmap :empty?))
(def 'tmap (test-double-iter))
(assert-false (tmap :empty?))
(assert-equal 0 (tmap :next!))
(assert-equal 2 (tmap :next-back!))
(assert-equal 1 (tmap :next-back!))
(assert-true (tmap :empty?))
"
  ;(:fn next-back! (s) ((iter s) :next!))
  (:fn nth-back!
"Consume the iterator until the nth element from the end and return it (0 based).
Note that repeated called to nth-back! will return new data since it consumes the iterator.

Example:
(def 'tmap ((vec-iter) :init '#(0 1 2 3 4) 0))
(assert-false (tmap :empty?))
(assert-equal 4 (tmap :nth-back! 0))
(assert-equal 3 (tmap :nth-back! 0))
(assert-equal 0 (tmap :nth-back! 2))
(assert-true (tmap :empty?))
"
    (self idx) (progn
    (def 'ret nil)
    (def 'i 0)
    (loop (plist) (self) (if (not (plist :empty?))
                                        (if (= i idx)
                                          (set 'ret (plist :next-back!))
                                          (progn (set 'i (+ i 1))(plist :next-back!)(recur plist)))))
      ret))

  (:fn reverse
"Produce an iterator the is the reverse of self.

Example:
(def 'tmap ((test-double-iter) :reverse))
(assert-false (tmap :empty?))
(assert-equal 2 (tmap :next!))
(assert-equal 1 (tmap :next!))
(assert-equal 0 (tmap :next!))
(assert-true (tmap :empty?))
"
       (self) ((reverse-iter) :init self)))

(defstruct list-iter
"Iterator that wraps a list.

Section: iterator

Example:
(def 'test-list-iter ((list-iter) :init '(1 2 3)))
(assert-false (test-list-iter :empty?))
(assert-equal 1 (test-list-iter :next!))
(assert-equal 2 (test-list-iter :next!))
(assert-equal 3 (test-list-iter :next!))
(assert-true (test-list-iter :empty?))
"
  ; fields
  (data nil)
  ; methods
  (:fn next! (self) (progn (def 'val (car data))(set 'data (cdr data)) val))
  (:fn empty? (self) (if data nil t))
  (:fn init (self l) (progn (if (list? l) (set 'data l) (err "list-iter requires a list")) self))
  (:impl iterator))

(defstruct vec-iter
"Iterator that wraps a vector.

Section: iterator

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
  (end 0)
  ; methods
  (:fn next! (self) (progn (def 'val (vec-nth start data))(set 'start (+ 1 start)) val))
  (:fn next-back! (self) (progn (def 'val (vec-nth end data))(set 'end (- end 1)) val))
  (:fn empty? (self) (> start end))
  (:fn nth! (self idx) (progn (set 'start (+ start idx))(self :next!)))
  (:fn nth-back! (self idx) (progn (set 'end (- end idx))(self :next-back!)))
  (:fn init (self v s) (progn (if (vec? v)
                                (progn (set 'data v) (set 'start s) (set 'end (- (length v) 1)))
                                (err "seq-vec requires a vector")) self))
  (:impl iterator double-ended-iterator))

(defstruct string-iter
"Iterator that wraps a string.

Section: iterator

Example:
(def 'test-string-iter ((string-iter) :init \"123\"))
(assert-false (test-string-iter :empty?))
(assert-equal #\1 (test-string-iter :next!))
(assert-equal #\2 (test-string-iter :next!))
(assert-equal #\3 (test-string-iter :next!))
(assert-true (test-string-iter :empty?))
"
  ; fields
  (data nil)
  ; methods
  (:fn next! (self) (if data (str-iter-next! data) nil))
  (:fn empty? (self) (if data (str-iter-empty? data) t))
  (:fn init (self l) (progn (if (string? l)
                       (set 'data (if (str-iter-empty? l) (str-iter-start l) (str-iter-start (str l))))
                       (err "string-iter requires a string")) self))
  (:impl iterator))

(defstruct map-iter 
"Iterator that applies a lambda to each element of another iterator- is lazy.

Section: iterator

Example:
(def 'test-map-iter (((iterator::list-iter) :init '(1 2 3)) :map (fn (x) (* x 2))))
(assert-false (test-map-iter :empty?))
(assert-equal 2 (test-map-iter :next!))
(assert-equal 4 (test-map-iter :next!))
(assert-equal 6 (test-map-iter :next!))
(assert-true (test-map-iter :empty?))
"
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
"Iterator that applies a lambda to each element to determine if is returned- is lazy.

Section: iterator

Example:
(def 'test-iter (((iterator::list-iter) :init '(1 2 3)) :filter (fn (x) (not (= x 2)))))
(assert-false (test-iter :empty?))
(assert-equal 1 (test-iter :next!))
(assert-equal 3 (test-iter :next!))
(assert-true (test-iter :empty?))
"
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

(defstruct reverse-iter
"Iterator that reverses another iterators direction.  Requires a double ended iterator.

Section: iterator

Example:
(def 'test-iter (((vec-iter) :init '#(1 2 3) 0) :reverse))
(assert-false (test-iter :empty?))
(assert-equal 3 (test-iter :next!))
(assert-equal 2 (test-iter :next!))
(assert-equal 1 (test-iter :next!))
(assert-true (test-iter :empty?))
"
  ; fields
  (wrapped nil)
  ; methods
  (:fn next! (self) (wrapped :next-back!))
  (:fn next-back! (self) (wrapped :next!))
  (:fn empty? (self) (wrapped :empty?))
  (:fn init (self in-wrapped) (progn
                                    (set 'wrapped in-wrapped)
                                    self))
  (:impl iterator double-ended-iterator))

(defstruct range-iter
"Iterator that generates numbers within a range.

Section: iterator

Example:
(def 'test-iter ((range-iter) :init 3 6))
(assert-false (test-iter :empty?))
(assert-equal 3 (test-iter :next!))
(assert-equal 4 (test-iter :next!))
(assert-equal 5 (test-iter :next!))
(assert-equal 6 (test-iter :next!))
(assert-true (test-iter :empty?))
(def 'test-iter ((range-iter) :init 3 6))
(assert-false (test-iter :empty?))
(assert-equal 6 (test-iter :next-back!))
(assert-equal 5 (test-iter :next-back!))
(assert-equal 4 (test-iter :next-back!))
(assert-equal 3 (test-iter :next-back!))
(assert-true (test-iter :empty?))
"
  ; fields
  (start 0)
  (end 0)
  ; methods
  (:fn next! (self) (progn (def 'val start)(set 'start (+ start 1)) val))
  (:fn next-back! (self) (progn (def 'val end)(set 'end (- end 1)) val))
  (:fn empty? (self) (> start end))
  (:fn init (self in-start in-end) (progn
                                    (set 'start in-start)
                                    (set 'end in-end)
                                    self))
  (:fn count (self total) (progn
                                    (set 'start 0)
                                    (set 'end (- total 1))
                                    self))
  (:impl iterator double-ended-iterator))

(defn iter?
"Return true if thing is an iterator, nil otherwise.

Section: iterator

Example:
(assert-true (iterator::iter? (iterator::iter '(1 2 3))))
(assert-false (iterator::iter? '(1 2 3)))
"
  (thing)
  (and (meta-tag? thing :method:next!)
       (meta-tag? thing :method:empty?)))

(defn iter
"Return thing as an iterator if possible (if it is an iterator just return thing).

Section: iterator

Example:
(assert-true (iterator::iter? (iterator::iter '(1 2 3))))
(assert-true (iterator::iter? (iterator::iter '#(1 2 3))))
(assert-true (iterator::iter? (iterator::iter \"abc\")))
(assert-true (iterator::iter? (iterator::iter (iterator::iter '(1 2 3)))))
"
  (thing)
  (if (iter? thing)
        thing
      (list? thing)
        ((list-iter) :init thing)
      (vec? thing)
        ((vec-iter) :init thing 0)
      (string? thing)
        ((string-iter) :init thing)
      (err "iter: requires a list, vector, string or existing iterator")))

(defn next!
"Calls iter on s and returns the next item.

Section: iterator

Example:
(assert-equal 1 (iterator::next! '(1 2 3)))
(assert-equal 1 (iterator::next! '#(1 2 3)))
(def 'next-test (iterator::iter '(4 5 6)))
(assert-equal 4 (iterator::next! next-test))
(assert-equal 5 (iterator::next! next-test))
(assert-equal 6 (iterator::next! next-test))
(assert-true (next-test :empty?))
"
  (s) ((iter s) :next!))

(defn empty?
"Is an iterator empty (no more items)?  Will call iter on input first.

Section: iterator

Example:
(assert-true (iterator::empty? nil))
(assert-true (iterator::empty? '#()))
(assert-false (iterator::empty? '#(1)))
"
    (s) ((iter s) :empty?))

(defn range
"Create an iterator that generates numbers within a range.
Can be called with one int (n) to produce 0..(n-1) or with two ints (m, n) to
produce m..n.

Section: iterator

Example:
(def 'test-iter (iterator::range 3 6))
(assert-false (test-iter :empty?))
(assert-equal 3 (test-iter :next!))
(assert-equal 4 (test-iter :next!))
(assert-equal 5 (test-iter :next!))
(assert-equal 6 (test-iter :next!))
(assert-true (test-iter :empty?))
(def 'test-iter (iterator::range 3))
(assert-false (test-iter :empty?))
(assert-equal 0 (test-iter :next!))
(assert-equal 1 (test-iter :next!))
(assert-equal 2 (test-iter :next!))
(assert-true (test-iter :empty?))
"
    (&rest i) (progn
    (def 'si (iter i))
    (if (= (length i) 1)
          ((range-iter) :count (si :next!))
        (= (length i) 2)
          (progn (def 'first (si :next!)) ((range-iter) :init first (si :next!)))
        (err "range: requires one or two integers"))))

(defn collect
"Collect all the values into a list.  This will consume the iterator and
produce a new list.  Will call iter on input to turn a collection into an iterator.

Section: iterator

Example:
(def 'collect-test (iterator::collect '#(1 2 3)))
(assert-true (list? collect-test))
(assert-equal '(1 2 3) collect-test)
"
    (s)
    (if (list? s) s ((iter s) :collect)))

(defn collect-vec
"Collect all the values into a vector.  This will consume the iterator and
produce a new vector.  Will call iter on input to turn a collection into an iterator.

Section: iterator

Example:
(def 'collect-vec-test (iterator::collect-vec '(1 2 3)))
(assert-true (vec? collect-vec-test))
(assert-equal '#(1 2 3) collect-vec-test)
"
    (s)
    (if (vec? s) s ((iter s) :collect-vec)))

(defn collect-str
"Collect all the values into a string.  This will consume the iterator and
produce a new string.  Will call iter on input to turn a collection into an iterator.

Section: iterator

Example:
(def 'collect-str-test (iterator::collect-str (iterator::map (fn (ch) (char-upper ch)) \"λabc σ\")))
(assert-true (string? collect-str-test))
(assert-equal \"ΛABC Σ\" collect-str-test)
"
    (s)
    (if (string? s) s ((iter s) :collect-str)))

(defn map
"Returns a map-iter around items (will call iter on items).
Apply the provided function to each element of the iterator.  Map is lazy.

Section: iterator

Example:
(def 'tmap (iterator::map (fn (x) (+ 1 x)) '(0 1 2)))
(assert-false (tmap :empty?))
(assert-equal 1 (tmap :next!))
(assert-equal 2 (tmap :next!))
(assert-equal 3 (tmap :next!))
(assert-true (tmap :empty?))
"
    (map-fn items) ((iter items) :map map-fn))

(defn filter
"Returns a filter-iter around items (will call iter on items).
Iterator that applies a lambda to each element to determine if is returned- is lazy.

Section: iterator

Example:
(def 'test-iter (iterator::filter (fn (x) (not (= x 2))) '(1 2 3)))
(assert-false (test-iter :empty?))
(assert-equal 1 (test-iter :next!))
(assert-equal 3 (test-iter :next!))
(assert-true (test-iter :empty?))
"
    (predicate items) ((iter items) :filter predicate))

(defn reverse
"Produce an iterator the is the reverse of items.  Will call iter on items and 
requires a double ended iterator.

Example:
(def 'tmap (reverse ((vec-iter) :init '#(0 1 2) 0)))
(assert-false (tmap :empty?))
(assert-equal 2 (tmap :next!))
(assert-equal 1 (tmap :next!))
(assert-equal 0 (tmap :next!))
(assert-true (tmap :empty?))
"
       (items) ((iter items) :reverse))

(defn nth!
"Consume the iterator until the idx (nth) element and return it (0 based).
Note that repeated called to nth will return new data since it consumes the iterator.

Example:
(def 'tmap ((list-iter) :init '(0 1 2 3 4)))
(assert-false (tmap :empty?))
(assert-equal 0 (nth! 0 tmap))
(assert-equal 1 (nth! 0 tmap))
(assert-equal 4 (nth! 2 tmap))
(assert-true (tmap :empty?))
"
    (idx coll) ((iter coll) :nth! idx))

(defmacro for
"
Loops over each element in an iterator.  Will call iter on the input object.
bind is bound to the current element of items and is accesible
in body. body is evaluated a number of times equal to the the number of items
in in_list.

Section: iterator

Example:
(def 'i 0)
(iterator::for x in (iterator::range 11) (set 'i (+ 1 i)))
(assert-equal 11 i)
"
	(bind in items body) (progn
	(if (not (= in 'in)) (err "Invalid for: (for [i] in [iterator] (body))"))
	`(loop (plist) ((iterator::iter ,items)) (if (not (plist :empty?)) (progn
		(def ',bind (plist :next!))
		(,@body)
		(recur plist))))))

(ns-export '(
    iterator
    double-ended-iterator
    vec-iter
    list-iter
    iter?
    iter
    map-iter
    filter-iter
    reverse-iter
    range-iter
    next!
    empty?
    range
    collect
    collect-vec
    collect-str
    map
    filter
    reverse
    nth!
    for))
