(if (ns-exists? 'iterator) (ns-enter 'iterator) (ns-create 'iterator))

(ns-import 'struct)

(deftrait iterator "Usage: (defstruct iter (:fn next! (self)...)(:fn empty? (self)...)(:impl iterator))\n\nTrait that provides iterator methods.\nRequires a struct to define methods next! and empty?\nExample:\n;some example\n"
  ;(:fn next! (s) ((iter s) :next!))
  ;(:fn empty? (s) ((iter s) :empty?))
  (:fn collect "collect all the values into a list
Example:
; ((iter '#(1 2 3)) :collect)" (self) (progn
    (def 'tseq nil)
    (def 'tcell nil)
    (def 'head nil)
    (sfor v self (progn
        (if (null head)
            (progn (set 'tseq (set 'head (join v nil))))
            (progn (set 'tcell (join v nil)) (xdr! tseq tcell) (set 'tseq tcell)))))
    head))
  (:fn collect-vec "collect all the values into a vector" (self) (progn
    (def 'tseq (vec))
    (sfor v self (vec-push! tseq v))
    tseq))

  (:fn map (self map-fn) ((map-iter) :init map-fn self))
  (:fn filter (self predicate) ((filter-iter) :init predicate self))
  (:fn nth (self idx) (progn
    (def 'ret nil)
    (def 'i 0)
    (loop (plist) (self) (if (not (plist :empty?))
                                        (if (= i idx)
                                          (set 'ret (plist :next!))
                                          (progn (set 'i (+ i 1))(plist :next!)(recur plist)))))
      ret)))

(defstruct list-iter
  ; fields
  (data "Some data" nil)
  ; methods
  (:fn next! (self) (progn (def 'val (car data))(set 'data (cdr data)) val))
  (:fn empty? (self) (if data nil t))
  (:fn init (self l) (progn (if (list? l) (set 'data l) (err "list-iter requires a list")) self))
  (:impl iterator))

(defstruct vec-iter
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
          (self :advance-data!)
          self))
  (:impl iterator))

(defn next! (s) ((iter s) :next!))
(defn empty? (s) ((iter s) :empty?))
(defn range (&rest i) (progn
    (def 'si (iter i))
    (if (= (length i) 1)
        ((range-iter) :count (si :next!))
        (if (= (length i) 2)
            (progn (def 'first (si :next!)) ((range-iter) :init first (si :next!) first)))
            (err "range: requires one or two integers"))))

(defn collect (s)
    (if (list? s) s ((iter s) :collect)))
(defn collect-vec (s)
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
