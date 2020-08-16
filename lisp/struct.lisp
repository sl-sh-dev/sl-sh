(if (ns-exists? 'struct) (ns-enter 'struct) (ns-create 'struct))

(defn method (field dispatch-map tags doc doc-exp)
    (def 'tsym (to-symbol (str ":" (car field))))
    (def 'second (cadr field))
    (if (string? second) (do
        (def 'doc-split (str-splitn 2 "Example:" second))
        (str-push! doc "method: " tsym "\n\t" (vec-nth 0 doc-split) "\n")
        (if (= 2 (length doc-split))
          (str-push! doc-exp "; " tsym " Example\n" (vec-nth 1 doc-split) "\n"))
        (def 'm-params (caddr field))
        (def 'm-body (cadddr field)))
      (do
        (str-push! doc "method: " tsym "\n")
        (def 'm-params (cadr field))
        (def 'm-body (caddr field))))
    (hash-set! dispatch-map tsym `(fn ,m-params ,m-body))
    (vec-push! tags (to-symbol (str ":method" tsym))))

; macro to build a "trait" that can be implemented by a struct
(defmacro deftrait
"Define a trait.  Traits define methods that are added to structures (and usually require one or
more methods in the struct.  Use them to provide common functionality shared by different structures.
Use (:fn name doc-str? body) to add a method.

Section: struct

Example:
(struct::deftrait test-trait
  ; Require what-a and what-d in the structures that implement this trait.
  (:fn aaa (self) (self :what-a))
  (:fn ddd (self) (self :what-d)))
(struct::defstruct test-struct
  ; fields
  (a \"a private attribute\" nil)
  (b \"a read/write attribute\" \"bee\" :rw)
  (c \"a read only attribute\" \"see\" :ro)
  (d \"a write only attribute\" \"dee\" :wo)
  ; methods
  (:fn what-d (self) d)
  (:fn what-a (self) a)
  (:impl test-trait))

(def 'ts (test-struct))
(assert-equal nil (ts :aaa))
(assert-equal \"dee\" (ts :ddd))
(ts :set-d \"something else\")
(assert-equal \"something else\" (ts :ddd))
(assert-equal \"bee\" (ts :b))
(assert-equal \"see\" (ts :c))
(ts :set-b \"queen\")
(assert-equal \"queen\" (ts :b))
"
    (name &rest fields)
    ((fn ()
        (def 'tags (vec))
        (vec-push! tags (to-symbol (str ":trait-" name)))
        (def 'dispatch-map (make-hash))
        (def 'fields-len (length fields))
        (def 'doc "")
        (def 'doc-exp "")
        (def 'idx-start 0)
        (if (and (> fields-len 1)(string? (vec-nth 0 fields)))
            (do
              (def 'doc-split (str-splitn 2 "Section:" (vec-nth 0 fields)))
              (set 'doc (vec-nth 0 doc-split))
              (set 'idx-start 1)
              (if (= 2 (length doc-split)) (set 'doc-exp (vec-nth 1 doc-split)))))
        (if (not (str-contains "Example:" doc-exp)) (str-push! doc-exp "\nExample:\n"))

        ((fn (idx)
            (if (< idx fields-len) (do
                (def 'field (vec-nth idx fields))
                (if (= (car field) :fn)  (struct::method (cdr field) dispatch-map tags doc doc-exp)
                    (err "Traits only have :fn fields!"))
                (recur (+ idx 1))
            )))idx-start)
        (def 'keys (hash-keys dispatch-map))
        (def 'keys-len (length keys))
        (def 'tags-len (length tags))
        (def 'doc-final "")
        (if (not (str-contains "Usage:" doc)) (str-push! doc-final "Usage: (defstruct ... (:impl " name "))\n\n"))
        (str-push! doc-final doc (if (> (length doc-exp) 0) (str "\nSection:" doc-exp)""))
        `(def ',name ,doc-final (fn (target-dispatch-map target-tags)
            ((fn (idx)
                (if (< idx ,tags-len) (do
                    (vec-push! target-tags (vec-nth idx ',tags))
                    (recur (+ idx 1)))))0)
            ((fn (idx)
                (if (< idx ,keys-len) (do
                    (def 'key (vec-nth idx ',keys))
                    (if (not (hash-haskey target-dispatch-map key)) (hash-set! target-dispatch-map key (hash-get ,dispatch-map key)))
                    (recur (+ idx 1)) )))0) )) )))

; macro to build a "struct"
(defmacro defstruct
"Define a structure.  This produces a lambda with name that will create an instance.
Each 'field' will add an attribute, method or trait.
Use (attr-name doc-str? default? [:rw | :ro | :wo]?) if a final access modifier is not provided then it is private.
  NOTE: for attributes, if the default value is a string then doc-str is not optional (but can be empty).
Use (:fn name doc-str? body) to add a method.
Use (:impl trait) to add a trait.

Section: struct

Example:
(struct::defstruct test-struct
  ; fields
  (a \"a private attribute\" nil)
  (b \"a read/write attribute\" \"bee\" :rw)
  (c \"a read only attribute\" \"see\" :ro)
  (d \"a write only attribute\" \"dee\" :wo)
  ; methods
  (:fn what-d (self) d)
  (:fn what-a (self) a))

(def 'ts (test-struct))
(assert-equal nil (ts :what-a))
(assert-equal \"dee\" (ts :what-d))
(ts :set-d \"something else\")
(assert-equal \"something else\" (ts :what-d))
(assert-equal \"bee\" (ts :b))
(assert-equal \"see\" (ts :c))
(ts :set-b \"queen\")
(assert-equal \"queen\" (ts :b))
"
    (name &rest fields)
    ((fn (params bindings)
        (def 'tags (vec))
        (vec-push! tags :struct)
        (vec-push! tags (to-symbol (str ":struct-" name)))
        (def 'dispatch-map (make-hash))
        (def 'fields-len (length fields))
        (def 'doc "")
        (def 'doc-exp "")
        (def 'idx-start 0)
        (if (and (> fields-len 1)(string? (vec-nth 0 fields)))
            (do
              (def 'doc-split (str-splitn 2 "Section:" (vec-nth 0 fields)))
              (set 'doc (vec-nth 0 doc-split))
              (set 'idx-start 1)
              (if (= 2 (length doc-split)) (def 'doc-exp (vec-nth 1 doc-split))(def 'doc-exp ""))))
        (if (not (str-contains "Example:" doc-exp)) (str-push! doc-exp "\nExample:\n"))

        (defn attrib (field doc doc-exp)
            (def 'second (cadr field))
            (def 'fdoc "")
            (if (string? second)
                (do
                  (def 'doc-split (str-splitn 2 "Example:" second))
                  (set 'fdoc (str "\n\t" (vec-nth 0 doc-split)))
                  (if (= 2 (length doc-split)) (str-push! doc-exp (vec-nth 1 doc-split)))
                  (xdr! field (cddr field))))
            (if (= 1 (length field))
                    (do
                      (str-push! doc "attribute: " (car field) " private" fdoc "\n")
                      (vec-push! params (car field))
                      (vec-push! bindings nil))
                (= 2 (length field))
                    (do
                      (str-push! doc "attribute: " (car field) " private" fdoc "\n")
                      (vec-push! params (car field))
                      (vec-push! bindings (cadr field)))
                (= 3 (length field))
                    (do
                      (def 'param (car field))
                      (def 'binding (cadr field))
                      (def 'perm (caddr field))
                      (vec-push! params param)
                      (vec-push! bindings binding)
                      (if (= perm :rw) (do
                              (str-push! doc "attribute: " (car field) " read/write" fdoc "\n")
                              (def 'tsym (to-symbol (str ":" param)))
                              (hash-set! dispatch-map tsym `(fn (_) ,param))
                              (vec-push! tags (to-symbol (str ":accessor:" param)))
                              (def 'tsym (to-symbol (str ":set-" param)))
                              (hash-set! dispatch-map tsym `(fn (_ &rest args) (apply set ',param args)))
                              (vec-push! tags (to-symbol (str ":setter:" param))))
                          (= perm :ro) (do
                              (str-push! doc "attribute: " (car field) " read" fdoc "\n")
                              (def 'tsym (to-symbol (str ":" param)))
                              (hash-set! dispatch-map tsym `(fn (_) ,param))
                              (vec-push! tags (to-symbol (str ":accessor:" param))))
                          (= perm :wo) (do
                              (str-push! doc "attribute: " (car field) " write" fdoc "\n")
                              (def 'tsym (to-symbol (str ":set-" param)))
                              (hash-set! dispatch-map tsym `(fn (_ &rest args) (apply set ',param args)))
                              (vec-push! tags (to-symbol (str ":setter:" param))))
                          (err "defstruct: invalid field access key (valid are :rw, :ro and :wo)")))
                (err "ERROR: invalid attribute bindings on defstruct")))

        (defn impl (field doc)
            (if (not (not field)) (do (str-push! doc "impl " (car field) "\n")(apply (car field) dispatch-map tags nil) (recur (cdr field) doc))))

        ((fn (idx)
            (if (< idx fields-len) (do
                (def 'field (vec-nth idx fields))
                (if (= (car field) :fn)  (struct::method (cdr field) dispatch-map tags doc doc-exp)
                    (= (car field) :impl) nil ; do impls last (struct methods take precident).
                    (attrib field doc doc-exp))
                (recur (+ idx 1))
            )))idx-start)

        ((fn (idx)
            (if (< idx fields-len) (do
                (def 'field (vec-nth idx fields))
                (if (= (car field) :impl) (impl (cdr field) doc))
                (recur (+ idx 1))
            )))idx-start)

        (hash-set! dispatch-map :type `(fn (_) (symbol-name ',name)))
        (def 'doc-final "")
        (str-push! doc-final doc (if (> (length doc-exp) 0) (str "\nSection:" doc-exp)""))
        `(def ',name ,doc-final (fn () ((fn (dispatch-map ,@params)
            (def 'self (fn (msg &rest args) (apply (eval-in-scope (hash-get dispatch-map msg (err (str "Invalid message (" msg ") to struct: " ',name)))) this-fn args)))
            ;(def 'self (fn (msg &rest args) (apply (hash-get dispatch-map msg (err (str "Invalid message (" msg ") to struct: " ',name))) this-fn args)))
            (meta-add-tags self ',tags)
            (undef 'self)),dispatch-map ,@bindings))) ) ; bindings for params
     (make-vec (length fields)) ; params
     (make-vec (length fields)))) ; bindings

; Due to the bootstrap order of std lib files can not use the ns-export macro at this point.
(def '*ns-exports* (vec 'deftrait 'defstruct))

