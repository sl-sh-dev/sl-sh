
; macro to build a "struct"
(defmacro defstruct (name fields methods)
    ((fn (params bindings meth-list) (progn
        (def 'accessors (make-hash))
        (def 'setters (make-hash))
        (core::fori idx field fields
            (if (= 1 (length field))
                    (progn
                      (vec-insert-nth! idx (core::nth 0 field) params)
                      (vec-insert-nth! idx nil bindings))
                (= 2 (length field))
                    (progn
                      (vec-insert-nth! idx (core::nth 0 field) params)
                      (vec-insert-nth! idx (core::nth 1 field) bindings))
                (= 3 (length field))
                    (progn
                      (def 'param (core::nth 0 field))
                      (def 'binding (core::nth 1 field))
                      (def 'perm (core::nth 2 field))
                      (vec-insert-nth! idx param params)
                      (vec-insert-nth! idx binding bindings)
                      (if (= perm :rw) (progn
                              (hash-set! accessors param t)
                              (hash-set! setters param t))
                          (= perm :ro)
                              (hash-set! accessors param t)
                          (= perm :wo)
                              (hash-set! setters param t)
                          (err "defstruct: invalid field access key (valid are :rw, :ro and :wo)")))
                (err "ERROR: invalid bindings on defstruct")))
        (def 'tags (vec))
        (vec-push! tags :struct)
        (vec-push! tags (to-symbol (str ":struct-" name)))
        (def 'dispatch-map (make-hash))
        (core::for p params (progn
            (if (hash-haskey accessors p) (progn
                (def 'tsym (to-symbol (str ":" p)))
                (hash-set! dispatch-map tsym `(fn (_) ,p))))
            (vec-push! tags (to-symbol (str ":accessor:" p)))
            (if (hash-haskey setters p) (progn
                (def 'tsym (to-symbol (str ":set-" p)))
                (hash-set! dispatch-map tsym `(fn (_ &rest args) (apply set ',p args)))))
            (vec-push! tags (to-symbol (str ":setter:" p)))))
        (core::for method methods (progn
            (def 'tsym (to-symbol (str ":" (first method))))
            (def 'm-params (first (rest method)))
            (def 'm-body (first (rest (rest method))))
            (hash-set! dispatch-map tsym `(fn ,m-params ,m-body))
            (vec-push! tags (to-symbol (str ":method:" (first method))))))
        (hash-set! dispatch-map :type `(fn (_) (sym->str ',name)))
        (def 'make-sym (to-symbol (str "make-" name)))
        `(def ',make-sym (fn () ((fn (dispatch-map ,@params) (progn
            (def 'self (fn (msg &rest args) (apply (eval (hash-get dispatch-map msg (err (str ("Invalid message (" msg ") to struct: " ,name))))) this-fn args)))
            (meta-add-tags self ',tags)
            (undef 'self))),dispatch-map ,@bindings))) )) ; bindings for params
     (make-vec (length fields)) ; params
     (make-vec (length fields)) ; bindings
     (make-vec (length methods)))) ; meth-list


(defstruct seq-list
  ; fields
  ((data nil))
  ; methods
  ((first (self) (car data))
   (rest (self) ((make-seq-list) :init (cdr data)))
   (rest! (self) (progn (set 'data (cdr data)) self))
   (empty? (self) (not data))
   (init (self l) (progn (if (list? l) (set 'data l) (err "seq-list requires a list")) self)))
  )

(defstruct seq-vec
  ; fields
  ((data nil)
   (start 0))
  ; methods
  ((first (self) (vec-nth data start))
   (rest (self) ((make-seq-vec) :init data(+ 1 start)))
   (rest! (self) (progn (set 'start (+ 1 start)) self))
   (empty? (self) (= start (length data)))
   (init (self v s) (progn (if (vec? v)
                             (progn (set 'data v) (set 'start s))
                             (err "seq-vec requires a vector")) self))))

(defstruct seq-range
  ; fields
  ((start 0)
   (end 0)
   (current 0))
  ; methods
  ((first (self) current)
   (rest (self) ((make-seq-range) :init start end (+ 1 current)))
   (rest! (self) (progn (set 'current (+ 1 current)) self))
   (empty? (self) (>= current end))
   (init (self in-start in-end in-current) (progn
                                    (set 'start in-start)
                                    (set 'end in-end)
                                    (set 'current in-current)
                                    self))
   (count (self total) (progn
                                    (set 'start 0)
                                    (set 'end total)
                                    (set 'current 0)
                                    self)))
  )

(defn sseq? (thing)
  (and (meta-tag? thing :method:first)
       (meta-tag? thing :method:rest)
       (meta-tag? thing :method:rest!)
       (meta-tag? thing :method:empty?)))

(defn seq (thing)
  (if (list? thing)
    ((make-seq-list) :init thing)
    (if (vec? thing)
      ((make-seq-vec) :init thing 0)
      (if (sseq? thing)
        thing
        (err "seq requires a list or vector or existing sequence!")))))

(defstruct seq-map 
  ; fields
  ((data nil)
   (map-fn nil))
  ; methods
  ((first (self) (map-fn (data :first)))
   (rest (self) ((make-seq-map) :init map-fn (data :rest)))
   (rest! (self) (progn (data :rest!) self))
   (empty? (self) (data :empty?))
   (init (self mfn d) (progn (set 'data (seq d))
                             (set 'map-fn mfn)
                             self))))

(defstruct seq-filter 
  ; fields
  ((data nil)
   (predicate nil))
  ; methods
  ((first (self) (data :first))
   (rest (self) ((make-seq-filter) :init predicate (data :rest)))
   (rest! (self) (progn (data :rest!) (self :advance-data) self))
   (empty? (self) (data :empty?))
   (advance-data (self) (progn 
       (core::loop (plist) (data) (if (or (plist :empty?)(predicate (sfirst plist)))
                                    (set 'data plist)
                                    (recur (data :rest))))))
   (init (self pred data-in) (progn
       (set 'data (seq data-in))
       (set 'predicate pred)
       (self :advance-data)
       self))))

(defn sfirst (s) ((seq s) :first))
(defn srest (s) ((seq s) :rest))
(defn sempty? (s) ((seq s) :empty?))
(defn range (&rest i) (progn
    (def 'si (seq i))
    (if (= (length i) 1)
        ((make-seq-range) :count (sfirst si))
        (if (= (length i) 2)
            ((make-seq-range) :init (sfirst si)(sfirst (srest si))(sfirst si))
            (err "range takes one or two integers")))))

(defn collect (s) (progn
    (def 'tseq nil)
    (def 'tcell nil)
    (def 'head nil)
    (sfor v (seq s) (progn
        (if (null head)
            (progn (set 'tseq (set 'head (join v nil))))
            (progn (set 'tcell (join v nil)) (xdr! tseq tcell) (set 'tseq tcell)))))
    head))
(defn collect-vec (s) (progn
    (def 'tseq (vec))
    (sfor v (seq s) (vec-push! tseq v))
    tseq))

(defn smap (map-fn items) ((make-seq-map) :init map-fn items))
(defn sfilter (predicate items) ((make-seq-filter) :init predicate items))
(defn snth (idx coll) (progn
    (def 'ret nil)
    (def 'i 0)
    (core::loop (plist) ((seq coll)) (if (not (sempty? plist))
                                       (if (= i idx)
                                         (set 'ret (sfirst plist))
                                         (progn (set 'i (+ i 1))(recur (srest plist))))))
      ret))

(defmacro sfor
"
bind is bound to the current element of in_list and is accesible
in body. body is evaluated a number of times equal to the the number of items
in in_list.


Section: shell
"
	(bind in_list body) (progn
	`(core::loop (plist) ((seq ,in_list)) (if (not (plist :empty?)) (progn
		(def ',bind (plist :first))
		(,@body)
		(recur (plist :rest)))))))

(defmacro sfor!
"
bind is bound to the current element of in_list and is accesible
in body. body is evaluated a number of times equal to the the number of items
in in_list.


Section: shell
"
	(bind in_list body) (progn
	`(core::loop (plist) ((seq ,in_list)) (if (not (plist :empty?)) (progn
		(def ',bind (plist :first))
		(,@body)
		(recur (plist :rest!)))))))

