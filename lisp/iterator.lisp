(ns-push 'iterator)

; Due to bootstrapping order can not use ns-import yet (needs iterators...).
(def defstruct struct::defstruct)
(def deftrait struct::deftrait)

(defmacro for
"
Loops over each element in an iterator.  Will call iter on the input object.
bind is bound to the current element of items and is accesible
in body. body is evaluated a number of times equal to the the number of items
in in_list.

Section: iterator

Example:
(def i 0)
(iterator::for x in (iterator::range 11) (set! i (+ 1 i)))
(assert-equal 11 i)
"
    (bind in items body) (do
    (if (not (= in 'in)) (err "Invalid for: (for [i] in [iterator] (body))"))
    (var plist (gensym))
    `(loop (,plist) ((iterator::iter ,items))
         (if (not (,plist :empty?)) (do
             (var ,bind (,plist :next!))
             (,@body)
             (recur ,plist))))))

(defmacro for-i
"
Loops over each element in an iterator.  Will call iter on the input object.
idx-bind is bound to an incrementing number starting with 0.
bind is bound to the current element of items and is accesible
in body. body is evaluated a number of times equal to the the number of items
in in_list.

Section: iterator

Example:
(def i 0)
(def i-tot 0)
(for-i idx x in '(1 2 3 4 5 6 7 8 9 10 11) (do (set! i-tot (+ idx i-tot))(set! i (+ 1 i))))
(assert-equal 11 i)
(assert-equal 55 i-tot)
"
    (idx-bind bind in items body) (do
    (if (not (= in 'in)) (err "Invalid for: (for [i] in [iterator] (body))"))
    (var loop-idx (gensym))
    (var plist (gensym))
    `(loop (,plist ,loop-idx) ((iterator::iter ,items) 0)
         (if (not (,plist :empty?)) (do
             (var ,bind (,plist :next!))
             (var ,idx-bind ,loop-idx)
             (,@body)
             (recur ,plist (+ ,loop-idx 1)))))))

(deftrait iterator
"Usage: (defstruct iter (:fn next! (self)...)(:fn empty? (self)...)(:impl iterator::iterator))

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
  (:fn next! (self) (do (def val current)(set! current (+ 1 current)) val))
  (:fn empty? (self) (>= current 3))
  (:impl iterator::iterator))
(def tmap (test-iter))
(assert-false (tmap :empty?))
(assert-equal 0 (tmap :next!))
(assert-equal 1 (tmap :next!))
(assert-equal 2 (tmap :next!))
(assert-true (tmap :empty?))
"
  (:fn collect
"Collect all the values into a list.  This will consume the iterator and
produce a new list.

Example:
(def collect-test ((iter '#(1 2 3)) :collect))
(assert-true (list? collect-test))
(assert-equal '(1 2 3) collect-test)
"
    (self) (do
    (var tseq nil)
    (var tcell nil)
    (var head nil)
    (for v in self (do
        (if (null head)
            (do (set! tseq (set! head (join v nil))))
            (do (set! tcell (join v nil)) (xdr! tseq tcell) (set! tseq tcell)))))
    head))

  (:fn collect-vec
"Collect all the values into a vector.  This will consume the iterator and
produce a new list.

Example:
(def collect-vec-test ((iter '(1 2 3)) :collect-vec))
(assert-true (vec? collect-vec-test))
(assert-equal '#(1 2 3) collect-vec-test)
"
    (self) (do
    (var tseq (vec))
    (for v in self (vec-push! tseq v))
    tseq))

  (:fn collect-str
"Collect all the values into a string.  This will consume the iterator and
produce a new string.

Example:
(def collect-str-test (((iter \"λabc σ\") :map (fn (ch) (char-upper ch))) :collect-str))
(assert-true (string? collect-str-test))
(assert-equal \"ΛABC Σ\" collect-str-test)
"
    (self) (do
    (var tseq (str))
    (for v in self (str-push! tseq v))
    tseq))

  (:fn map
"Apply the provided function to each element of the iterator.  Map is lazy.

Example:
(def tmap ((test-iter) :map (fn (x) (+ 1 x))))
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
(def tmap ((test-iter) :filter (fn (x) (not (= x 1)))))
(assert-false (tmap :empty?))
(assert-equal 0 (tmap :next!))
(assert-equal 2 (tmap :next!))
(assert-true (tmap :empty?))
"
       (self predicate) ((filter-iter) :init predicate self))

  (:fn slice
       (self start &rest end)
           (if (= (length end) 1) ((slice-iter) :init self start (vec-nth end 0))
               (> (length end) 1) (err "iter :slice Wrong number of arge (start end?)")
               ((slice-iter) :init self start)))

  (:fn count
"Consume the iterator and return the number of items.

Example:
(def tmap (test-iter))
(assert-false (tmap :empty?))
(assert-equal 3 (tmap :count))
(assert-true (tmap :empty?))
"
       (self) (do (var count 0) (iterator::for _ in self (set! count (+ count 1))) count))

  (:fn nth!
"Consume the iterator until the nth! element and return it (0 based).
Note that repeated called to nth! will return new data since it consumes the iterator.

Example:
(def tmap ((list-iter) :init '(0 1 2 3 4)))
(assert-false (tmap :empty?))
(assert-equal 0 (tmap :nth! 0))
(assert-equal 1 (tmap :nth! 0))
(assert-equal 4 (tmap :nth! 2))
(assert-true (tmap :empty?))
"
    (self idx) (do
    (var ret nil)
    (var i 0)
    (loop (plist) (self) (if (not (plist :empty?))
                                        (if (= i idx)
                                          (set! ret (plist :next!))
                                          (do (set! i (+ i 1))(plist :next!)(recur plist)))))
      ret))

  (:fn double-ended?
"Return t if this iterator is double ended, nil otherwise.

Example:
(ns-import 'struct)
(ns-import 'iterator)
(defstruct test-double-iter
  ; fields
  (current 0)
  (current-end 2)
  ; methods
  (:fn next! (self) (do (def val current)(set! current (+ 1 current)) val))
  (:fn next-back! (self) (do (def val current-end)(set! current-end (- current-end 1)) val))
  (:fn empty? (self) (> current current-end))
  (:impl iterator::iterator iterator::double-ended-iterator))
(assert-false ((test-iter) :double-ended?))
(assert-true ((test-double-iter) :double-ended?))
"
    (self) (meta-tag? self :trait-double-ended-iterator)))

(deftrait double-ended-iterator
"Usage: (defstruct iter (:fn next! (self)...)(:fn next-back! (self)...)(:fn empty? (self)...)(:impl iterator::iterator iterator::double-ended-iterator))

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
  (:fn next! (self) (do (var val current)(set! current (+ 1 current)) val))
  (:fn next-back! (self) (do (var val current-end)(set! current-end (- current-end 1)) val))
  (:fn empty? (self) (> current current-end))
  (:impl iterator::iterator iterator::double-ended-iterator))
(def tmap (test-double-iter))
(assert-false (tmap :empty?))
(assert-equal 0 (tmap :next!))
(assert-equal 1 (tmap :next!))
(assert-equal 2 (tmap :next!))
(assert-true (tmap :empty?))
(def tmap (test-double-iter))
(assert-false (tmap :empty?))
(assert-equal 2 (tmap :next-back!))
(assert-equal 1 (tmap :next-back!))
(assert-equal 0 (tmap :next-back!))
(assert-true (tmap :empty?))
(def tmap (test-double-iter))
(assert-false (tmap :empty?))
(assert-equal 0 (tmap :next!))
(assert-equal 2 (tmap :next-back!))
(assert-equal 1 (tmap :next-back!))
(assert-true (tmap :empty?))
"
  (:fn nth-back!
"Consume the iterator until the nth element from the end and return it (0 based).
Note that repeated called to nth-back! will return new data since it consumes the iterator.

Example:
(def tmap ((vec-iter) :init '#(0 1 2 3 4) 0))
(assert-false (tmap :empty?))
(assert-equal 4 (tmap :nth-back! 0))
(assert-equal 3 (tmap :nth-back! 0))
(assert-equal 0 (tmap :nth-back! 2))
(assert-true (tmap :empty?))
"
    (self idx) (do
    (var ret nil)
    (var i 0)
    (loop (plist) (self) (if (not (plist :empty?))
                                        (if (= i idx)
                                          (set! ret (plist :next-back!))
                                          (do (set! i (+ i 1))(plist :next-back!)(recur plist)))))
      ret))

  (:fn reverse
"Produce an iterator that is the reverse of self.

Example:
(def tmap ((test-double-iter) :reverse))
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
(def test-list-iter ((list-iter) :init '(1 2 3)))
(assert-false (test-list-iter :empty?))
(assert-equal 1 (test-list-iter :next!))
(assert-equal 2 (test-list-iter :next!))
(assert-equal 3 (test-list-iter :next!))
(assert-true (test-list-iter :empty?))
(def test-list-iter ((list-iter) :init '(1 2 3)))
(assert-false (test-list-iter :empty?))
(assert-equal 1 (test-list-iter :next!))
(assert-equal 3 (test-list-iter :next-back!))
(assert-equal 2 (test-list-iter :next!))
(assert-true (test-list-iter :empty?))
(def test-list-iter ((list-iter) :init '(1 2 3)))
(assert-false (test-list-iter :empty?))
(assert-equal 3 (test-list-iter :next-back!))
(assert-equal 2 (test-list-iter :next-back!))
(assert-equal 1 (test-list-iter :next-back!))
(assert-true (test-list-iter :empty?))
"
  ; fields
  (data nil)
  (rev-data nil)
  (elements nil)
  ; methods
  (:fn next! (self) (do
    (var val (car data))
    (set! data (cdr data))
    (if elements (set! elements (- elements 1)))
    val))
  (:fn next-back! (self) (do
    (if (not rev-data) (do (set! elements (length data))(set! rev-data (self :make-rev))))
    (var val (car rev-data))
    (set! rev-data (cdr rev-data))
    (if elements (set! elements (- elements 1)))
    val))
  (:fn empty? (self) (if (or (null data)(and (not (null elements))(<= elements 0))) t nil))
  (:fn init (self l) (do (if (list? l) (set! data l) (err "list-iter requires a list")) self))
  (:fn make-rev (self) (do
    (var node nil)
    ((fn (data) (if data (do (set! node (join (car data) node)) (recur (cdr data)))))data)
    node))
  (:impl iterator::iterator iterator::double-ended-iterator))

(defstruct vec-iter
"Iterator that wraps a vector.

Section: iterator

Example:
(def test-vec-iter ((vec-iter) :init '#(1 2 3) 0))
(assert-false (test-vec-iter :empty?))
(assert-equal 1 (test-vec-iter :next!))
(assert-equal 2 (test-vec-iter :next!))
(assert-equal 3 (test-vec-iter :next!))
(assert-true (test-vec-iter :empty?))
(def test-vec-iter ((vec-iter) :init (vec) 0))
(assert-true (test-vec-iter :empty?))
"
  ; fields
  (data nil)
  (start 0)
  (end 0)
  ; methods
  (:fn next! (self) (do (var val (vec-nth data start))(set! start (+ 1 start)) val))
  (:fn next-back! (self) (do (var val (vec-nth data end))(set! end (- end 1)) val))
  (:fn empty? (self) (> start end))
  (:fn nth! (self idx) (do (set! start (+ start idx))(self :next!)))
  (:fn nth-back! (self idx) (do (set! end (- end idx))(self :next-back!)))
  (:fn init (self v s) (do (if (vec? v)
                                (do (set! data v) (set! start s) (set! end (- (length v) 1)))
                                (err (str "vec-iter requires a vector, got " (type v) ", " v))) self))
  (:impl iterator::iterator iterator::double-ended-iterator))

(defstruct string-iter
"Iterator that wraps a string.

Section: iterator

Example:
(def test-string-iter ((string-iter) :init \"123\"))
(assert-false (test-string-iter :empty?))
(assert-equal #\1 (test-string-iter :next!))
(assert-equal #\2 (test-string-iter :next!))
(assert-equal #\3 (test-string-iter :next!))
(assert-true (test-string-iter :empty?))
"
  ; fields
  (data nil)
  ; methods
  (:fn next! (self) (str-iter-next! data))
  (:fn empty? (self) (not (str-iter-peek data)))
  (:fn init (self l) (do (if (string? l)
                       ;(set! data (if (str-iter-empty? l) (str-iter-start l) l))
                       (set! data (str-iter-start l))
                       (err "string-iter requires a string")) self))
  (:impl iterator::iterator))

(defstruct file-iter
"Iterator that wraps a file.  Each call to next! returns the next line (with 
trailing newline.

Section: iterator

Example:
(def tst-file (open \"/tmp/file-iter-test.txt\" :create :truncate))
(write-line tst-file \"line 1\")
(write-line tst-file \"line 2\")
(write-line tst-file \"line 3\")
(write-string tst-file \"line 4\")
(close tst-file)
(def test-iter ((iterator::file-iter) :init (open \"/tmp/file-iter-test.txt\")))
(assert-false (test-iter :empty?))
(assert-equal \"line 1\\n\" (test-iter :next!))
(assert-equal \"line 2\\n\" (test-iter :next!))
(assert-equal \"line 3\\n\" (test-iter :next!))
(assert-equal \"line 4\" (test-iter :next!))
(assert-true (test-iter :empty?))
"
  ; fields
  (file nil)
  (next-line nil)
  ; methods
  (:fn next! (self) (do (var val next-line) (set! next-line (read-line file)) val))
  (:fn empty? (self) (not next-line))
  (:fn init (self f) (if (file? f)
                       (do (set! file f) (set! next-line (read-line file))self)
                       (err "file-iter requires a file")))
  (:impl iterator::iterator))

(defstruct map-iter 
"Iterator that applies a lambda to each element of another iterator- is lazy.

Section: iterator

Example:
(def test-map-iter (((iterator::list-iter) :init '(1 2 3)) :map (fn (x) (* x 2))))
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
  (:fn next-back! (self) (map-fn (data :next-back!)))
  (:fn empty? (self) (data :empty?))
  (:fn init (self mfn d) (do (set! data (iter d))
                                (set! map-fn mfn)
                                self))
  (:fn double-ended? (self) (data :double-ended?))

  (:impl iterator::iterator iterator::double-ended-iterator))

(defstruct append-iter 
"Iterator that appends multiple iterators.  Append iter will consume
the iterators it is appending.  If non-list items are passed they are wrapped in
a singleton iterator (i.e. will work with loose object).

Section: iterator

Example:
(def test-iter ((iterator::append-iter) :init '(0 1 2) '#(3 4 5) '(6 7 8 9)))
(def test-slice-iter (test-iter :slice 3 7))
(assert-false (test-slice-iter :empty?))
(assert-equal 3 (test-slice-iter :next!))
(assert-equal 4 (test-slice-iter :next!))
(assert-equal 5 (test-slice-iter :next!))
(assert-equal 6 (test-slice-iter :next!))
(assert-true (test-slice-iter :empty?))
(def test-iter ((iterator::append-iter) :init '(0 1 2) nil '#(3 4 5) nil '(6 7 8 9)))
(def test-slice-iter (test-iter :slice 0 4))
(assert-false (test-slice-iter :empty?))
(assert-equal 0 (test-slice-iter :next!))
(assert-equal 1 (test-slice-iter :next!))
(assert-equal 2 (test-slice-iter :next!))
(assert-equal 3 (test-slice-iter :next!))
(assert-true (test-slice-iter :empty?))
(def test-iter ((iterator::append-iter) :init '(0 1 2) '#(3 4 5) '(6 7 8 9)))
(def test-slice-iter (test-iter :slice 7))
(assert-false (test-slice-iter :empty?))
(assert-equal 7 (test-slice-iter :next!))
(assert-equal 8 (test-slice-iter :next!))
(assert-equal 9 (test-slice-iter :next!))
(assert-true (test-slice-iter :empty?))
(def test-iter ((iterator::append-iter) :init '(0 1 2) '#(3 4 5) '(6 7 8 9)))
(assert-false (test-iter :empty?))
(assert-equal 10 (test-iter :count))
(assert-true (test-iter :empty?))
(def test-iter ((iterator::append-iter) :init '(0 1 2) '() '#(3 4 5) nil '(6 7 8 9)))
(assert-false (test-iter :empty?))
(assert-equal 10 (test-iter :count))
(assert-true (test-iter :empty?))
(def test-iter ((iterator::append-iter) :init nil '(0 1 2) (vec) '#(3 4 5) '(6 7 8 9) nil))
(assert-false (test-iter :empty?))
(assert-equal 10 (test-iter :count))
(assert-true (test-iter :empty?))
"
  ; fields
  (iters nil)
  ; methods
  (:fn next! (self)
      (if (self :empty?) nil ((car iters) :next!)))
  (:fn empty? (self) (do
      (loop () ()
          (if (and (not (null iters))((car iters) :empty?)) (do (set! iters (cdr iters))(recur))))
      (null iters)))
  (:fn init (self &rest rest-iters) (do
    (var tcell nil)
    (var tseq nil)
    ((fn (rest-iters)
        (if (non-empty-seq? rest-iters)
          (do
            (var v (first rest-iters))
            (if (and (not (null v))(or (and (iter? v)(not (v :empty?)))(not (iter? v)))) (do
                (set! tcell (join (iterator::iter-or-single v) nil))
                (if (null tseq) (set! iters tcell) (xdr! tseq tcell))
                (set! tseq tcell)))
            (recur (rest rest-iters)))))rest-iters)
    self))
  (:impl iterator::iterator))

(defstruct slice-iter
"Iterator that provides a slice of the underlying iter.  Slice iter will consume
the iterator it is slicing.

Section: iterator

Example:
(def test-slice-iter (((iterator::list-iter) :init '(0 1 2 3 4 5 6 7 8 9)) :slice 3 6))
(assert-false (test-slice-iter :empty?))
(assert-equal 3 (test-slice-iter :next!))
(assert-equal 4 (test-slice-iter :next!))
(assert-equal 5 (test-slice-iter :next!))
(assert-true (test-slice-iter :empty?))
(def test-slice-iter (((iterator::list-iter) :init '(0 1 2 3 4 5 6 7 8 9)) :slice 0 3))
(assert-false (test-slice-iter :empty?))
(assert-equal 0 (test-slice-iter :next!))
(assert-equal 1 (test-slice-iter :next!))
(assert-equal 2 (test-slice-iter :next!))
(assert-true (test-slice-iter :empty?))
(def test-slice-iter (((iterator::list-iter) :init '(0 1 2 3 4 5 6 7 8 9)) :slice 7))
(assert-false (test-slice-iter :empty?))
(assert-equal 7 (test-slice-iter :next!))
(assert-equal 8 (test-slice-iter :next!))
(assert-equal 9 (test-slice-iter :next!))
(assert-true (test-slice-iter :empty?))
"
  ; fields
  (data nil)
  (start 0)
  (total nil)
  (count 0)
  ; methods
  (:fn next! (self) (do (set! count (+ count 1)) (if (or (null total)(<= count total)) (data :next!) nil)))
  (:fn empty? (self) (or (data :empty?)(and (not (null total))(>= count total))))
  (:fn init (self d s &rest e) (do
                                 (set! data (iter d))
                                 (for _ in (range s) (data :next!))
                                 (set! start s)
                                 (if (= (length e) 1) (set! total (- (vec-nth e 0) start))
                                     (> (length e) 1) (err "slice-iter :init Wrong number of arge (iter start end?)"))
                                 self))
  (:impl iterator::iterator))

(defstruct filter-iter
"Iterator that applies a lambda to each element to determine if is returned- is lazy.

Section: iterator

Example:
(def test-iter (((iterator::list-iter) :init '(1 2 3)) :filter (fn (x) (not (= x 2)))))
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
  (:fn next! (self) (do (var val next)(self :advance-data!) val))
  (:fn empty? (self) is-empty)
  (:fn advance-data! (self) (do 
         (loop (plist) (data) (if (not (plist :empty?))
             (do
               (set! next (plist :next!))
               (if (not (predicate next))
                 (recur plist)))
             (set! is-empty t)))))
  (:fn init (self pred data-in) (do
          (set! data (iter data-in))
          (set! predicate pred)
          (set! is-empty (data :empty?))
          (self :advance-data!)
          self))
  (:impl iterator::iterator))

(defstruct reverse-iter
"Iterator that reverses another iterators direction.  Requires a double ended iterator.

Section: iterator

Example:
(def test-iter (((vec-iter) :init '#(1 2 3) 0) :reverse))
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
  (:fn init (self in-wrapped) (do
                                    (set! wrapped in-wrapped)
                                    self))
  (:impl iterator::iterator iterator::double-ended-iterator))

(defstruct range-iter
"Iterator that generates numbers within a range.

Section: iterator

Example:
(def test-iter ((range-iter) :init 3 6))
(assert-false (test-iter :empty?))
(assert-equal 3 (test-iter :next!))
(assert-equal 4 (test-iter :next!))
(assert-equal 5 (test-iter :next!))
(assert-equal 6 (test-iter :next!))
(assert-true (test-iter :empty?))
(def test-iter ((range-iter) :init 3 6))
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
  (:fn next! (self) (do (var val start)(set! start (+ start 1)) val))
  (:fn next-back! (self) (do (var val end)(set! end (- end 1)) val))
  (:fn empty? (self) (> start end))
  (:fn init (self in-start in-end) (do
                                    (set! start in-start)
                                    (set! end in-end)
                                    self))
  (:fn count (self total) (do
                                    (set! start 0)
                                    (set! end (- total 1))
                                    self))
  (:impl iterator::iterator iterator::double-ended-iterator))

(defstruct single-iter
"Iterator that wraps and returns a single object.

Section: iterator

Example:
(def test-iter ((iterator::single-iter) :init 3))
(assert-false (test-iter :empty?))
(assert-equal 3 (test-iter :next!))
(assert-true (test-iter :empty?))
(def test-iter ((iterator::single-iter) :init \"iter\"))
(assert-false (test-iter :empty?))
(assert-equal \"iter\" (test-iter :next!))
(assert-true (test-iter :empty?))
(def test-iter ((iterator::single-iter) :init 3))
(assert-false (test-iter :empty?))
(assert-equal 3 (test-iter :next-back!))
(assert-true (test-iter :empty?))
(def test-iter ((iterator::single-iter) :init \"iter\"))
(assert-false (test-iter :empty?))
(assert-equal \"iter\" (test-iter :next-back!))
(assert-true (test-iter :empty?))
"
  ; fields
  (value nil)
  (done nil)
  ; methods
  (:fn next! (self) (do (var ret (if done nil value))(set! done t)ret))
  (:fn next-back! (self) (do (var ret (if done nil value))(set! done t)ret))
  (:fn empty? (self) done)
  (:fn init (self v) (do (set! value v)self))
  (:impl iterator::iterator iterator::double-ended-iterator))

(defn iter?
"Return true if thing is an iterator, nil otherwise.

Section: iterator

Example:
(assert-true (iterator::iter? (iterator::iter '(1 2 3))))
(assert-false (iterator::iter? '(1 2 3)))
"
  (thing)
  (meta-tag? thing :trait-iterator))

(defn double-ended-iter?
"Return true if thing is an iterator and double ended, nil otherwise.

Section: iterator

Example:
(struct::defstruct test-iter
  ; fields
  (current 0)
  ; methods
  (:fn next! (self) (do (def val current)(set! current (+ 1 current)) val))
  (:fn empty? (self) (>= current 3))
  (:impl iterator::iterator))
(assert-true (iterator::double-ended-iter? (iterator::iter '(1 2 3))))
(assert-false (iterator::double-ended-iter? '(1 2 3)))
(assert-false (iterator::double-ended-iter? (test-iter)))
"
  (thing)
  (if (and (iter? thing)(thing :double-ended?)) t nil))

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
      (file? thing)
        ((file-iter) :init thing)
      (err "iter: requires a list, vector, string, file or existing iterator")))

(defn iter-or-single
"Return thing as an iterator if possible (if it is an iterator just return thing).
If not possible then wrap thing in a single iter and return that.

Section: iterator

Example:
(assert-true (iterator::iter? (iterator::iter-or-single '(1 2 3))))
(assert-true (iterator::iter? (iterator::iter-or-single '#(1 2 3))))
(assert-true (iterator::iter? (iterator::iter-or-single \"abc\")))
(assert-true (iterator::iter? (iterator::iter-or-single (iterator::iter '(1 2 3)))))
(assert-true (iterator::iter? (iterator::iter-or-single 1)))
(assert-true (iterator::iter? (iterator::iter-or-single #\A)))
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
      (file? thing)
        ((file-iter) :init thing)
      ((single-iter) :init thing)))

(defn next!
"Calls iter on s and returns the next item.

Section: iterator

Example:
(assert-equal 1 (iterator::next! '(1 2 3)))
(assert-equal 1 (iterator::next! '#(1 2 3)))
(def next-test (iterator::iter '(4 5 6)))
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
(def test-iter (iterator::range 3 6))
(assert-false (test-iter :empty?))
(assert-equal 3 (test-iter :next!))
(assert-equal 4 (test-iter :next!))
(assert-equal 5 (test-iter :next!))
(assert-equal 6 (test-iter :next!))
(assert-true (test-iter :empty?))
(def test-iter (iterator::range 3))
(assert-false (test-iter :empty?))
(assert-equal 0 (test-iter :next!))
(assert-equal 1 (test-iter :next!))
(assert-equal 2 (test-iter :next!))
(assert-true (test-iter :empty?))
"
    (&rest i) (do
    (var si (iter i))
    (if (= (length i) 1)
          ((range-iter) :count (si :next!))
        (= (length i) 2)
          (do (var first (si :next!)) ((range-iter) :init first (si :next!)))
        (err "range: requires one or two integers"))))

(defn collect
"Collect all the values into a list.  This will consume the iterator and
produce a new list.  Will call iter on input to turn a collection into an iterator.

Section: iterator

Example:
(def collect-test (iterator::collect '#(1 2 3)))
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
(def collect-vec-test (iterator::collect-vec '(1 2 3)))
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
(def collect-str-test (iterator::collect-str (iterator::map (fn (ch) (char-upper ch)) \"λabc σ\")))
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
(def tmap (iterator::map (fn (x) (+ 1 x)) '(0 1 2)))
(assert-false (tmap :empty?))
(assert-equal 1 (tmap :next!))
(assert-equal 2 (tmap :next!))
(assert-equal 3 (tmap :next!))
(assert-true (tmap :empty?))
(def tmap (iterator::reverse (iterator::map (fn (x) (+ 1 x)) '(0 1 2))))
(assert-false (tmap :empty?))
(assert-equal 3 (tmap :next!))
(assert-equal 2 (tmap :next!))
(assert-equal 1 (tmap :next!))
(assert-true (tmap :empty?))
"
    (map-fn items) ((iter items) :map map-fn))

(defn slice
"Provides a slice of iterator.  Will call iter on items.  Slice iter will consume
the iterator it is slicing.

Section: iterator

Example:
(def test-slice-iter (slice '(0 1 2 3 4 5 6 7 8 9) 3 6))
(assert-false (test-slice-iter :empty?))
(assert-equal 3 (test-slice-iter :next!))
(assert-equal 4 (test-slice-iter :next!))
(assert-equal 5 (test-slice-iter :next!))
(assert-true (test-slice-iter :empty?))
(def test-slice-iter (slice '(0 1 2 3 4 5 6 7 8 9) 0 3))
(assert-false (test-slice-iter :empty?))
(assert-equal 0 (test-slice-iter :next!))
(assert-equal 1 (test-slice-iter :next!))
(assert-equal 2 (test-slice-iter :next!))
(assert-true (test-slice-iter :empty?))
(def test-slice-iter (slice '(0 1 2 3 4 5 6 7 8 9) 7))
(assert-false (test-slice-iter :empty?))
(assert-equal 7 (test-slice-iter :next!))
(assert-equal 8 (test-slice-iter :next!))
(assert-equal 9 (test-slice-iter :next!))
(assert-true (test-slice-iter :empty?))
"
    (items start &rest end)
    (if (= (length end) 1) ((iter items) :slice start (vec-nth end 0))
      (> (length end) 1) (err "slice: Wrong number of args (items start end?).")
      ((iter items) :slice start)))

(defn filter
"Returns a filter-iter around items (will call iter on items).
Iterator that applies a lambda to each element to determine if is returned- is lazy.

Section: iterator

Example:
(def test-iter (iterator::filter (fn (x) (not (= x 2))) '(1 2 3)))
(assert-false (test-iter :empty?))
(assert-equal 1 (test-iter :next!))
(assert-equal 3 (test-iter :next!))
(assert-true (test-iter :empty?))
"
    (predicate items) ((iter items) :filter predicate))

(defn reverse
"Produce an iterator the is the reverse of items.  Will call iter on items and 
requires a double ended iterator.

Section: iterator

Example:
(def tmap (reverse ((vec-iter) :init '#(0 1 2) 0)))
(assert-false (tmap :empty?))
(assert-equal 2 (tmap :next!))
(assert-equal 1 (tmap :next!))
(assert-equal 0 (tmap :next!))
(assert-true (tmap :empty?))
(assert-error (reverse \"string\"))
"
    (items)
    (var i (iter items))
    (if (double-ended-iter? i) (i :reverse)
      (err "Not a double ended iterator or collection, can not reverse")))

(defn nth
"Consume the iterator until the idx (nth) element and return it (0 based).
Note that repeated called to nth will return new data since it consumes the iterator.

Section: iterator

Example:
(def tmap ((list-iter) :init '(0 1 2 3 4)))
(assert-false (tmap :empty?))
(assert-equal 0 (nth 0 tmap))
(assert-equal 1 (nth 0 tmap))
(assert-equal 4 (nth 2 tmap))
(assert-true (tmap :empty?))
"
    (idx coll) ((iter coll) :nth! idx))

(defn reduce
"
reduce is used to amalgamate a provided iterator, coll, and an intitial value,
according to the reducing function provided. The iter function will be called
on coll to make sure it is an iterator. The reducing-fcn should be a function
of two arguments. In the first iteration of reduce, the init-val will be used as
the first argument to the reducing-fcn and (next! coll) will be used as the
second argument. For all subsequent iterations, The result from the previous
application of the reducing-fcn will be used as the first argument to the
reducing-fcn and the second argument will be the next item in the collection
when the collection is empty reduce will return the amalgamated result.

Section: iterator

Example:

(assert-true (= 15 (reduce + 0 (list 1 2 3 4 5))))
(assert-false (= 15 (reduce + 1 (list 1 2 3 4 5))))
(assert-true (= \"one hoopy frood\" (reduce str \"\" (list \"one \" \"hoopy \" \"frood\"))))
"
    (reducing-fcn init-val coll) (let
    ; Only call iter on coll once.
    ((inner-reduce (fn (reducing-fcn init-val coll)
        (if (coll :empty?)
                init-val
                (recur reducing-fcn (reducing-fcn init-val (coll :next!)) coll)))))
    (inner-reduce reducing-fcn init-val (iter coll))))

(defn reduce-times
"Apply wrapping-fcn to value number of times. Function is recursive. Recursive
binding for value is previous application of wrapping function to value.

Section: iterator

Example:

(assert-equal (list (list 3)) (reduce-times 3 list 2))
(assert-equal 5 (reduce-times (reduce-times 5 list 5) first 5))
"
    (value wrapping-fcn times)
    (if (<= times 0)
        value
        (recur (wrapping-fcn value) wrapping-fcn (- times 1))))

(defn append
"Combine the provided items into a single iterator (calls iter on each parameter).
If non-list items are passed they are wrapped in a singleton iterator (i.e. will
work with loose object).  Note that nil is an empty list not a \"loose item\".

Section: iterator

Example:
(def test-iter (append '(0 1 2) '#(3 4 5) '(6 7 8 9)))
(assert-false (test-iter :empty?))
(assert-equal 10 (test-iter :count))
(assert-true (test-iter :empty?))
(def test-iter (append '(0 1 2) 3 4 5 '(6 7 8 9)))
(assert-false (test-iter :empty?))
(assert-equal 10 (test-iter :count))
(assert-true (test-iter :empty?))
(def test-iter (append 0 1 2 '(3 4)))
(assert-false (test-iter :empty?))
(assert-equal 0 (test-iter :next!))
(assert-equal 1 (test-iter :next!))
(assert-equal 2 (test-iter :next!))
(assert-equal 3 (test-iter :next!))
(assert-equal 4 (test-iter :next!))
(assert-true (test-iter :empty?))
(def test-iter (append 0 1 2 nil))
(assert-false (test-iter :empty?))
(assert-equal 0 (test-iter :next!))
(assert-equal 1 (test-iter :next!))
(assert-equal 2 (test-iter :next!))
(assert-true (test-iter :empty?))
(def test-iter (append 0 1 2 '(nil)))
(assert-false (test-iter :empty?))
(assert-equal 0 (test-iter :next!))
(assert-equal 1 (test-iter :next!))
(assert-equal 2 (test-iter :next!))
(assert-equal nil (test-iter :next!))
(assert-true (test-iter :empty?))
"
    (first-iter &rest rest-iters) (apply (append-iter) :init first-iter rest-iters))

(defn append-to!
"Combine the provided items after the first (first must be a vector or list)
into a single iterator.  These values are added the first argument destructively.
If non-list items are passed they are wrapped in a singleton iterator (i.e. will
work with loose object).  Note that nil is an empty list not a \"loose item\".

Section: iterator

Example:
(def test-iter '(0 1 2))
(append-to! test-iter '#(3 4 5) '(6 7 8 9))
(set! test-iter (iter test-iter))
(def test-slice-iter (test-iter :slice 3 7))
(assert-false (test-slice-iter :empty?))
(assert-equal 3 (test-slice-iter :next!))
(assert-equal 4 (test-slice-iter :next!))
(assert-equal 5 (test-slice-iter :next!))
(assert-equal 6 (test-slice-iter :next!))
(assert-true (test-slice-iter :empty?))
(def test-iter '(0 1 2))
(append-to! test-iter '#(3 4 5) '(6 7 8 9))
(set! test-iter (iter test-iter))
(def test-slice-iter (test-iter :slice 0 4))
(assert-false (test-slice-iter :empty?))
(assert-equal 0 (test-slice-iter :next!))
(assert-equal 1 (test-slice-iter :next!))
(assert-equal 2 (test-slice-iter :next!))
(assert-equal 3 (test-slice-iter :next!))
(assert-true (test-slice-iter :empty?))
(def test-iter '(0 1 2))
(append-to! test-iter '#(3 4 5) '(6 7 8 9))
(set! test-iter (iter test-iter))
(def test-slice-iter (test-iter :slice 7))
(assert-false (test-slice-iter :empty?))
(assert-equal 7 (test-slice-iter :next!))
(assert-equal 8 (test-slice-iter :next!))
(assert-equal 9 (test-slice-iter :next!))
(assert-true (test-slice-iter :empty?))
(def test-iter '(0 1 2))
(append-to! test-iter '#(3 4 5) '(6 7 8 9))
(set! test-iter (iter test-iter))
(assert-false (test-iter :empty?))
(assert-equal 10 (test-iter :count))
(assert-true (test-iter :empty?))
(def test-iter nil)
(append-to! test-iter nil '(0 1 2) nil '#(3 4 5) '(6 7 8 9) nil)
(set! test-iter (iter test-iter))
(assert-false (test-iter :empty?))
(assert-equal 10 (test-iter :count))
(assert-true (test-iter :empty?))
(def test-iter (vec 0))
(append-to! test-iter nil '#(1) '(2 3) nil)
(set! test-iter (iter test-iter))
(assert-false (test-iter :empty?))
(assert-equal 0 (test-iter :next!))
(assert-equal 1 (test-iter :next!))
(assert-equal 2 (test-iter :next!))
(assert-equal 3 (test-iter :next!))
(assert-true (test-iter :empty?))
"
    (ret &rest others) (do

    (var last-cell (fn (obj)
        (if (null (cdr obj))
            obj
            (recur (cdr obj)))))

    (set! others (apply append others))
    (if (vec? ret) (for l in others (vec-push! ret l))
        (list? ret) (do
                (var tseq (last-cell ret))
                (for l in others
                        (do
                            (if (null tseq)
                                (xar! tseq l)
                                (do
                                    (var tcell (join l nil))
                                    (xdr! tseq tcell)
                                    (set! tseq tcell))))))
        (err "append-to!: First element not a list or vector."))
    ret))

;; TODO broken iter
;; TODO scnd error when (apply str (collect (zip (repeat " " 3) (list 1 2 3))))
;; you do not feed it an iterator!
(defstruct broken-iter
"
Section: iterator
"
;; fields
(flip-flop #t)
;; methods
(:fn nextt (self) "broken")
(:fn emptyyy (self) nil)
(:fn init (self) self)
(:impl iterator::iterator iterator::double-ended-iterator))


(defstruct zip-iter
"create iterator that interleaves two iterators together. Resultant iter
is same length as fst

Section: iterator
"
;; fields
(sncd nil)
(fst nil)
(flip-flop #t)
;; methods
(:fn next! (self) (do
                   (set! flip-flop (not flip-flop))
                   (if flip-flop (fst :next!) (sncd :next!))))
(:fn empty? (self) (if flip-flop
                     (fst :empty?)
                     (sncd :empty?)))
(:fn init (self in-fst in-scndd)
     (do
       (set! fst in-fst)
       (set! sncd in-scndd)
       self))
(:impl iterator::iterator iterator::double-ended-iterator))

(defn zip
"interleaves two iterators together, like a zipper. Resultant iter
is same length as fst

Section: iterator
Example:

(test::assert-equal (list 1 2 3 4) (collect (zip (iter (list 1 3)) (iter (list 2 4)))))
(test::assert-equal (list 1 2 3 4) (collect (zip (iter (list 1 3)) (iter (list 2 4 5)))))
"
    (fst scnd)
      ((zip-iter) :init fst scnd))

(defstruct repeat-iter
"iterator that returns provided repeat with specified length.

Section: iterator
"
;; fields
(to-repeat nil)
(len 0)
(current 0)
;;methods
(:fn empty? (self) (>= current len))
(:fn next! (self) (do
    (set! current (+ current 1))
    to-repeat))
(:fn init (self in-to-repeat in-len)
    (do
        (set! to-repeat in-to-repeat)
        (set! len in-len)
        self))
(:impl iterator::iterator))

(defn repeat
"repeat target n times

Section: iterator

Example:
(test::assert-equal (list #\m #\m #\m #\m) (collect (repeat #\m 4)))

"
(target n)
((repeat-iter) :init target n))

(defstruct take-iter
"
take itertor

Section: iterator
"
;; fields
(provided-iter nil)
(len 0)
(current 0)
;;methods
(:fn empty? (self) (or (>= current len) (provided-iter :empty?)))
(:fn next! (self) (do
    (set! current (+ current 1))
    (provided-iter :next!)))
(:fn init (self in-to-repeat in-len)
    (do
        (set! provided-iter in-to-repeat)
        (set! len in-len)
        self))
(:impl iterator::iterator))

(defn take
"return iterator with first n items of provided-iter. Returned iterator is the
same as provided-iter if n is greater than the length of provided-iter.

Section: iterator

Example:
(test::assert-equal (list #\m #\m) (collect (take (repeat #\m 4) 2)))
"
  (provided-iter n)
  ((take-iter) :init provided-iter n))

(ns-export '(
    iterator
    double-ended-iterator
    vec-iter
    string-iter
    file-iter
    list-iter
    iter?
    double-ended-iter?
    iter
    zip-iter
    zip
    repeat-iter
    repeat
    take-iter
    take
    map-iter
    append-iter
    slice-iter
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
    slice
    filter
    reverse
    nth
    reduce
    reduce-times
    append
    append-to!
    for
    for-i))

(ns-pop)

