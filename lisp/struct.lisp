(ns-push 'struct)

(defn method (field prefix methods dispatcher tags doc doc-exp)
    (var tsym (sym ":" (car field)))
    (var msym (sym prefix (car field)))
    (var second (cadr field))
    (var m-params nil)
    (var m-body nil)
    (if (string? second)
      (do
        (var doc-split (str-splitn 2 "Example:" second))
        (str-push! doc "method: " tsym "\n\t" (vec-nth doc-split 0) "\n")
        (if (= 2 (length doc-split))
          (str-push! doc-exp "; " tsym " Example\n" (vec-nth doc-split 1) "\n"))
        (set! m-params (caddr field))
        (set! m-body (cadddr field)))
      (do
        (str-push! doc "method: " tsym "\n")
        (set! m-params (cadr field))
        (set! m-body (caddr field))))
    (vec-push! methods `(var ,msym (fn ,m-params ,m-body)))
    (vec-push! dispatcher `(= msg ,tsym))
    (vec-push! dispatcher `(apply ,msym this-fn args))
    (vec-push! tags (sym ":method" tsym)))

; macro to build a "trait" that can be implemented by a struct
(defmacro deftrait
"Define a trait.  Traits define methods that are added to structures (and usually require one or
more methods in the struct.  Use them to provide common functionality shared by different structures.
Use (:fn name doc-str? body) to add a method.

Section: struct

Example:
(ns-push 'test-deftrait)

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
  (:impl test-deftrait::test-trait))

(def ts (test-struct))
(test::assert-equal nil (ts :aaa))
(test::assert-equal \"dee\" (ts :ddd))
(ts :set-d \"something else\")
(test::assert-equal \"something else\" (ts :ddd))
(test::assert-equal \"bee\" (ts :b))
(test::assert-equal \"see\" (ts :c))
(ts :set-b \"queen\")
(test::assert-equal \"queen\" (ts :b))
(ns-pop)
"
    (name &rest fields)
    ((fn ()
        (var tags (vec))
        (vec-push! tags (sym ":trait-" name))
        (var dispatch-map (make-hash))
        (var methods (vec))
        (var dispatcher (vec))
        (var fields-len (length fields))
        (var doc (str))
        (var doc-exp (str))
        (var idx-start 0)
        (if (and (> fields-len 1)(string? (vec-nth fields 0)))
            (do
              (var doc-split (str-splitn 2 "Section:" (vec-nth fields 0)))
              (set! doc (vec-nth doc-split 0))
              (set! idx-start 1)
              (if (= 2 (length doc-split)) (set! doc-exp (vec-nth doc-split 1)))))
        (if (not (str-contains "Example:" doc-exp)) (str-push! doc-exp "\nExample:\n"))

        ((fn (idx)
            (if (< idx fields-len) (do
                (var field (vec-nth fields idx))
                (if (= (car field) :fn) 
                    (method (cdr field) (str name "^") methods dispatcher tags doc doc-exp)
                    (err "Traits only have :fn fields!"))
                (recur (+ idx 1))
            )))idx-start)
        (var keys (hash-keys dispatch-map))
        (var keys-len (length keys))
        (var tags-len (length tags))
        (var methods-len (length methods))
        (var dispatcher-len (length dispatcher))
        (var doc-final (str))
        (if (not (str-contains "Usage:" doc)) (str-push! doc-final "Usage: (deftrait ... (:impl " name "))\n\n"))
        (str-push! doc-final doc (if (> (length doc-exp) 0) (str "\nSection:" doc-exp)""))
        `(def ,name ,doc-final (fn (target-dispatch-map target-methods target-dispatcher target-tags)
            ((fn (idx)
                (if (< idx ,tags-len) (do
                    (vec-push! target-tags (vec-nth ',tags idx))
                    (recur (+ idx 1)))))0)
            ((fn (idx)
                (if (< idx ,methods-len) (do
                    (vec-push! target-methods (vec-nth ',methods idx))
                    (recur (+ idx 1)))))0)
            ((fn (idx)
                (if (< idx ,dispatcher-len) (do
                    (vec-push! target-dispatcher (vec-nth ',dispatcher idx))
                    (recur (+ idx 1)))))0)
            ((fn (idx)
                (if (< idx ,keys-len) (do
                    (var key (vec-nth ',keys idx))
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

(def ts (test-struct))
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
       (let ((tags (vec))
             (dispatch-map (make-hash))
             (methods (vec))
             (dispatcher (vec))
             (fields-len (length fields))
             (doc (str))
             (doc-exp (str))
             (idx-start 0)
             (attrib)
             (impl))
         (vec-push! tags :struct)
         (vec-push! tags (sym ":struct-" name))
         (if (and (> fields-len 1)(string? (vec-nth fields 0)))
             (let ((doc-split (str-splitn 2 "Section:" (vec-nth fields 0))))
               (set! doc (vec-nth doc-split 0))
               (set! idx-start 1)
               (if (= 2 (length doc-split)) (set! doc-exp (vec-nth doc-split 1))(set! doc-exp (str)))))
         (if (not (str-contains "Example:" doc-exp)) (str-push! doc-exp "\nExample:\n"))

         (set! attrib
               (fn (field doc doc-exp)
                   (let ((second (cadr field))
                         (fdoc (str)))
                     (if (string? second)
                         (let ((doc-split (str-splitn 2 "Example:" second)))
                           (set! fdoc (str "\n\t" (vec-nth doc-split 0)))
                           (if (= 2 (length doc-split)) (str-push! doc-exp (vec-nth doc-split 1)))
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
                         (let ((param (car field))
                               (binding (cadr field))
                               (perm (caddr field))
                               (tsym nil)
                               (msym nil))
                           (vec-push! params param)
                           (vec-push! bindings binding)
                           (if (= perm :rw)
                               (do
                                (str-push! doc "attribute: " (car field) " read/write" fdoc "\n")
                                (set! tsym (sym ":" param))
                                 (set! msym (sym name "^acc^" param))
                                 (hash-set! dispatch-map tsym `(fn (_) ,param))
                                 (vec-push! methods `(var ,msym (fn (_) ,param)))
                                 (vec-push! tags (sym ":accessor:" param))
                                 (vec-push! dispatcher `(= msg ,tsym))
                                 (vec-push! dispatcher `(apply ,msym this-fn args))
                                 (set! tsym (sym ":set-" param))
                                 (set! msym (sym name "^set^" param))
                                 (hash-set! dispatch-map tsym `(fn (_ arg) (set! ,param arg)))
                                 (vec-push! methods `(var ,msym (fn (_ arg) (set! ,param arg))))
                                 (vec-push! dispatcher `(= msg ,tsym))
                                 (vec-push! dispatcher `(apply ,msym this-fn args))
                                 (vec-push! tags (sym ":setter:" param)))
                               (= perm :ro)
                               (do
                                (str-push! doc "attribute: " (car field) " read" fdoc "\n")
                                (set! tsym (sym ":" param))
                                 (set! msym (sym name "^acc^" param))
                                 (hash-set! dispatch-map tsym `(fn (_) ,param))
                                 (vec-push! methods `(var ,msym (fn (_) ,param)))
                                 (vec-push! dispatcher `(= msg ,tsym))
                                 (vec-push! dispatcher `(apply ,msym this-fn args))
                                 (vec-push! tags (sym ":accessor:" param)))
                               (= perm :wo)
                               (do
                                (str-push! doc "attribute: " (car field) " write" fdoc "\n")
                                (set! tsym (sym ":set-" param))
                                 (set! msym (sym name "^set^" param))
                                 (hash-set! dispatch-map tsym `(fn (_ arg) (set! ,param arg)))
                                 (vec-push! methods `(var ,msym (fn (_ arg) (set! ,param arg))))
                                 (vec-push! dispatcher `(= msg ,tsym))
                                 (vec-push! dispatcher `(apply ,msym this-fn args))
                                 (vec-push! tags (sym ":setter:" param)))
                               (err "defstruct: invalid field access key (valid are :rw, :ro and :wo)")))
                         (err "ERROR: invalid attribute bindings on defstruct")))))

               (set! impl
                     (fn (field doc)
                         (if (not (not field))
                             (do
                              (str-push! doc "impl " (car field) "\n")
                              (apply (car field) dispatch-map methods dispatcher tags nil)
                               (recur (cdr field) doc)))))

               ((fn (idx)
                    (if (< idx fields-len)
                        (do (let ((field (vec-nth fields idx)))
                          ;(println "XXXX field " fields ", "idx", " field)
                          (if (= (car field) :fn) (method (cdr field) (str name "^") methods dispatcher tags doc doc-exp)
                              (= (car field) :impl) nil ; do impls last (struct methods take precident).
                              (attrib field doc doc-exp)))
                          (recur (+ idx 1)))))idx-start)

               ((fn (idx)
                    (if (< idx fields-len)
                        (do (let ((field (vec-nth fields idx)))
                          (if (= (car field) :impl) (impl (cdr field) doc)))
                          (recur (+ idx 1)))))idx-start)

               (hash-set! dispatch-map :type `(fn (_) (sym->str ',name)))
               (let ((msym (sym name "^type"))
                     (doc-final (str)))
                 (vec-push! methods `(var ,msym (fn (_) (sym->str ',name))))
                 (vec-push! dispatcher `(= msg :type))
                 (vec-push! dispatcher `(apply ,msym this-fn args))
                 (str-push! doc-final doc (if (> (length doc-exp) 0) (str "\nSection:" doc-exp)""))
                 `(def ,name ,doc-final (fn () ((fn (,@params)
                                                    ,@methods
                                                    (var self
                                                         (fn (msg &rest args)
                                                             (if ,@dispatcher (err (str "Invalid message (" msg ") to struct: " ',name)))))
                                                    (meta-add-tags self ',tags) self) ,@bindings))))))
         (make-vec (length fields)) ; params
         (make-vec (length fields)))) ; bindings

; Due to the bootstrap order of std lib files can not use the ns-export macro at this point.
(def *ns-exports* (vec 'deftrait 'defstruct))

(ns-pop)
