;;; These are core forms that are installed in the root namespace so they
;;; are always available.

; XXX TODO- use a gensym in shell-reader so we can more consistently detect these.
(def maybe-docstring?
"Usage: (maybe-docstring? form)

True if form might be a docstring, nil otherwise.

Section: core

Example:
(test::assert-true (maybe-docstring? \"string\"))
(test::assert-true (maybe-docstring? '(str 1 2 3)))
(test::assert-false (maybe-docstring? '(1 2 3)))
"
    (fn (test-form)
                          (or (string? test-form)
                              (and (list? test-form)
                                   (symbol? (car test-form))
                                   (= (str (car test-form)) "str"))
                              (and (vec? test-form)
                                   (> (length test-form) 0)
                                   (symbol? (vec-nth test-form 0))
                                   (= (str (vec-nth test-form 0) "str"))))))
(def defmacro
  "Usage: (defmacro name doc_string? argument_list body)

Create a macro and bind it to a symbol in the current scope.

Section: core

Example:
(defmacro test-mac (x) (let ((y (+ (ref (ref x)) 1))) `(set! ,x ,y)))
(def test-mac-x 2)
(test-mac test-mac-x)
(test::assert-equal 3 test-mac-x)
(defmacro test-mac (x) `(set! ,x 15))
(test-mac test-mac-x)
(test::assert-equal 15 test-mac-x)
"
  (macro (name &rest args)
         ((fn ()
              ((fn (ars body doc-str)
                (if (< (length args) 2) (err "defmacro: Wrong number of args."))
                (if (maybe-docstring? (vec-nth args 0))
                    (do
                     (set! doc-str (vec-nth args 0))
                     (set! ars (vec-nth args 1))
                      (set! body (vec-slice args 2))
                      `(do (def ,name ,doc-str (macro ,ars ,@body)) nil))
                    (do
                     (set! ars (vec-nth args 0))
                     (set! body (vec-slice args 1))
                      `(def ,name (macro ,ars ,@body)))))nil nil nil)))))

(defmacro ns-export
"Export a symbol or list of symbols to be imported into other namespaces.

Section: namespace"
    (symbol_or_sequence) `(do
    (if (not (def? *ns-exports*)) (def *ns-exports* (vec)))
    (if (symbol? ,symbol_or_sequence)
        (vec-push! *ns-exports* (quote ,symbol_or_sequence))
        (if (or (list? ,symbol_or_sequence) (vec? ,symbol_or_sequence))
            (iterator::for symbol in ,symbol_or_sequence (vec-push! *ns-exports* symbol))
            (err "ns-export takes a symbol or sequence.")))))

(defmacro ns-import
"Import any symbols exported from namespace into the current namespace.

Section: namespace"
    (namespace)
    `((fn ()
        (def import (vec def 1 2))
        (iterator::for symbol in (ref (sym ,namespace "::*ns-exports*"))
                       (do
                         (vec-set! import 1 (sym "ns::" symbol))
                         (vec-set! import 2 (sym ,namespace "::" symbol))
                         (eval import))))))

(defmacro defn
"
Define a named function in the current namespace.

Section: core

Example:
(defn defn-test (x y) (+ x y))
(test::assert-equal 5 (defn-test 2 3))
(defn defn-test (x y) (set! x (* x 2))(+ x y))
(test::assert-equal 7 (defn-test 2 3))
(defn defn-test (x y))
(test::assert-false (defn-test 2 3))
(defn defn-test (x y) #t)
(test::assert-true (defn-test 2 3))
"
    (name &rest args)
    ((fn ()
         (if (< (length args) 1) (err (str "defn: Wrong number of args creating " name)))
         ((fn (ars body doc-str)
         (if (maybe-docstring? (vec-nth args 0))
           (do
             (if (< (length args) 2) (err "defn: Wrong number of args."))
             (set! doc-str (vec-nth args 0))
             (set! ars (vec-nth args 1))
             (set! body (if (> (length args) 2) (vec-slice args 2) (vec nil)))
             `(def ,name ,doc-str (fn ,ars (block ,name ,@body))))
           (do
             (set! ars (vec-nth args 0))
             (set! body (if (> (length args) 1) (vec-slice args 1) (vec nil)))
             `(def ,name (fn ,ars (block ,name ,@body))))))nil nil nil))))

; Due to the way namespaces interact with lambdas this needs to be a symbol in
; root.
(def ^ns-stack-xyz^ (list))

(defmacro ns-push
"Usage: (ns-push 'namespace)

Pushes the current namespace on a stack for ns-pop and enters or creates namespace.

Section: namespace

Example:
(def test-ns-push *ns*)
(ns-push 'ns-pop-test-namespace)
; *ns* will not be right...
(test::assert-equal \"ns-pop-test-namespace\" *active-ns*)
(ns-push 'ns-pop-test-namespace2)
(test::assert-equal \"ns-pop-test-namespace2\" *active-ns*)
(ns-pop)
(test::assert-equal \"ns-pop-test-namespace\" *active-ns*)
(ns-pop)
(test::assert-equal test-ns-push *ns*)
"
    (namespace)
    `(do
        (set! ^ns-stack-xyz^ (join (if (def? *active-ns*) *active-ns* 'root) ^ns-stack-xyz^))
        (if (ns-exists? ,namespace) (ns-enter ,namespace) (ns-create ,namespace))))

(defmacro ns-pop
"Usage: (ns-pop)

Returns to the previous namespace saved in the last ns-push.

Section: namespace

Example:
(def test-ns-pop *ns*)
(ns-push 'ns-pop-test-namespace)
(test::assert-equal \"ns-pop-test-namespace\" *active-ns*)
(ns-pop)
(test::assert-equal test-ns-pop *ns*)
"
    ()
    `(do
        (ns-enter (car ^ns-stack-xyz^))
        (set! ^ns-stack-xyz^ (cdr ^ns-stack-xyz^))))

(defmacro let
  "Takes list, vals, of form ((binding0 sexp0) (binding1 sexp1) ...) and evaluates
let-body with all values of binding bound to the result of the evaluation of
sexp.

Section: core

Example:
(def test-do-one \"One1\")
(def test-do-two \"Two1\")
(def test-do-three (let ((test-do-one \"One\")) (set! test-do-two \"Two\")(test::assert-equal \"One\" test-do-one)\"Three\"))
(test::assert-equal \"One1\" test-do-one)
(test::assert-equal \"Two\" test-do-two)
(test::assert-equal \"Three\" test-do-three)
((fn (idx) (let ((v2 (+ idx 2))(v3 (+ idx 3)))
    (test::assert-equal (+ idx 2) v2)
    (test::assert-equal (+ idx 3) v3)
    (if (< idx 5) (recur (+ idx 1)))))0)
((fn (idx) (let ((v2 (+ idx 2))(v3 (+ idx 3)))
    (test::assert-equal (+ idx 2) v2)
    (test::assert-equal (+ idx 3) v3)
    (if (< idx 5) (this-fn (+ idx 1)))))0)
"
  (vals &rest let-body)
  ((fn (vars binds)
       ((fn (el plist)
            (if (not (null el))
                (do
                 (if (= 1 (length el))
                     (do
                      (vec-push! vars (car el))
                      (vec-push! binds (list)))
                     (= 2 (length el))
                     (do
                      (vec-push! vars (car el))
                      (vec-push! binds (car (cdr el))))
                     (err "ERROR: invalid bindings on let"))
                 (recur (car plist)(cdr plist)))))(car vals)(cdr vals))
       `((fn :no-recur ,vars ,@let-body) ,@binds))(make-vec (length vals))(make-vec (length vals))))

(defmacro loop
  "
Binds bindings to parameters in body. Use recur with desired bindings for
subsequent iteration.
Within the loop the lambda 'break' will end the loop, break can take an option
argument that is what the loop produces (nil if no argument).

Section: core

Example:
(def tot 0)
(loop (idx) (3) (do
    (set! tot (+ tot 1))
    (if (> idx 1) (recur (- idx 1)))))
(assert-equal 3 tot)
(def tot 0)
(loop (idx) (0)
    (set! tot (+ tot 1))
    (if (= idx 2) (break))
    (recur (+ idx 1)))
(assert-equal 3 tot)
(assert-equal 11 (loop (idx) (0)
    (if (= idx 2) (break 11))
    (recur (+ idx 1))))
(assert-false (loop (idx) (0)
    (if (= idx 2) (break))
    (recur (+ idx 1))))
(assert-error (loop (idx) (0)
    (if (= idx 2) (break 1 3))
    (recur (+ idx 1))))
"
  (params bindings &rest body)
  (let ((loop-block (gensym))
        (result-name (gensym)))
    `(let ((break (fn (&rest ret)
                      (if (= (length ret) 0)(return-from ,loop-block nil)
                          (= (length ret) 1)(return-from ,loop-block (vec-nth ret 0))
                          (err "break: requires 0 or 1 argument"))))
           (,result-name))
       (block ,loop-block
         (set! ,result-name ((fn ,params ,@body) ,@bindings))
         ,result-name))))

(defmacro dotimes
"
Evaluate body a number of times equal to times' numerical value.

Section: core

Example:
(def i 0)
(dotimes 11 (set! i (+ 1 i)))
(assert-equal 11 i)
"
    (times body)
    ((fn (idx-name)
    `(if (> ,times 0)
        (loop (,idx-name) (,times) (do
            (,@body)
            (if (> ,idx-name 1) (recur (- ,idx-name 1)))))))(gensym)))

(defmacro dotimes-i
"
Evaluate body a number of times equal to times' numnrical value. Includes an
incrementing reference binding, idx-bind, accesible in body.

Section: core

Example:
(def i 0)
(def i-tot 0)
(dotimes-i idx 11 (do (set! i-tot (+ idx i-tot))(set! i (+ 1 i))))
(assert-equal 11 i)
(assert-equal 55 i-tot)
"
    (idx-bind times body)
    ((fn (stop-name)
    `(if (> ,times 0)
        (loop (,idx-bind ,stop-name) (0 (- ,times 1)) (do
            (,@body)
            (if (< ,idx-bind ,stop-name) (recur (+ ,idx-bind 1) ,stop-name))))))(gensym)))

(defmacro match
  "Usage: (match condition (value form*)*) -> result

Evaluate condition and look for matching value in each branch of type
(value form*). Form(s) will be wrapped in an implicit do. Use nil to take
action if no match (encouraged!).

Section: conditional

Example:

(defn select-option (a)
    (match a (1 \"opt-one\")
             (2 (set! b 5) \"opt-two\")
             (3 (str \"opt\" \"-three\"))))
(defn select-option-def (a)
    (match a (1 \"opt-one\")
             (2 \"opt-two\")
             (3 (str \"opt\" \"-three\"))
             (nil \"default\")))
(def b 0)
(assert-equal b 0)
(assert-equal \"opt-one\" (select-option 1))
(assert-equal \"opt-two\" (select-option 2))
(assert-equal b 5)
(assert-equal \"opt-three\" (select-option 3))
(assert-equal #f (select-option 4))
(assert-equal \"opt-one\" (select-option-def 1))
(assert-equal \"opt-two\" (select-option-def 2))
(assert-equal \"opt-three\" (select-option-def 3))
(assert-equal \"default\" (select-option-def 4))
"
  (condition &rest branches)
  ((fn ()
       (let ((out_list (list))
             (make-action (fn (action)
                              (if (seq? action)
                                  `(do ,@action)
                                  `action)))
             (make-cond (fn (condition val action others)
                            (if (null val) (make-action action)
                                (if (empty-seq? others) `((= ,condition ,val) ,(make-action action))
                                    `((= ,condition ,val) ,(make-action action) ,@(make-cond condition (first (first others)) (rest (first others)) (rest others)))))))
             (cond-name condition))
         `(if ,@(make-cond cond-name (first (first branches)) (rest (first branches)) (rest branches)))))))

(defmacro cond
  "Usage: (cond ((test form*)*) -> result

Evaluate each test in order.  If it is true then evaluate the form(s) in an
implicit do and return the result.  Stop evaluting at the first true test.
Return nil if no conditions are true.

Section: conditional

Example:

(defn select-option (a)
    (cond ((= a 1) \"opt-one\")
          ((= a 2) (set! b 5) \"opt-two\")
          ((= a 3) (str \"opt\" \"-three\"))))
(defn select-option-def (a)
    (cond ((= a 1) \"opt-one\")
          ((= a 2) \"opt-two\")
          ((= a 3) (str \"opt\" \"-three\"))
          (#t \"default\")))
(def b 0)
(assert-equal \"opt-one\" (select-option 1))
(assert-equal b 0)
(assert-equal \"opt-two\" (select-option 2))
(assert-equal b 5)
(assert-equal \"opt-three\" (select-option 3))
(assert-equal nil (select-option 4))
(assert-equal \"opt-one\" (select-option-def 1))
(assert-equal \"opt-two\" (select-option-def 2))
(assert-equal \"opt-three\" (select-option-def 3))
(assert-equal \"default\" (select-option-def 4))
"
  (&rest branches)
  ((fn ()
       (let ((out_list (list))
             (make-action (fn (action)
                              (if (seq? action)
                                  `(do ,@action)
                                  `action)))
             (make-cond (fn (condition action others)
                            (if (empty-seq? others)
                                `(,condition ,(make-action action) nil)
                                `(,condition ,(make-action action) ,@(make-cond (first (first others)) (rest (first others)) (rest others)))))))
         `(if ,@(make-cond (first (first branches)) (rest (first branches)) (rest branches)))))))

(defmacro func?
"
True if the expression is a [builtin?](#root::builtin?), a [lambda?](#root::lambda?), or a [macro?](#root::macro?)

Section: type

Example:
(def func?-test 1)
(test::assert-false (func? func?-test))
(test::assert-true (func? car))
(test::assert-true (func? first))
(test::assert-true (func? let))
"
    (to-test) `(or (builtin? ,to-test) (lambda? ,to-test) (macro? ,to-test)))

(defmacro when
"Usage: (when provided-condition if-true)

when is a convenience function used to check a form, provided-condition,
and run some form, if-true, if provided-condition evaluates to true.

Section: conditional

Example:

(assert-true (when #t #t))
(assert-false (when #t nil))
(assert-false (when nil nil))
"
    (provided-condition if-true)
    `(if ,provided-condition ,if-true))

; Reader macro for #.
(defn reader-macro-dot
"Reader macro for #.(...).  Do not call directly.

Section: core

Example:
(def dot-test (read \"(1 2 #.(* 3 10) #.(str \"o\" \"ne\"))))
(test::assert-equal '(1 2 30 \"one\"))
"
      (stream ch) (eval (read stream)))

(defn nsubstitute!
"Replaces all instances of old-item in lst with new-item. If last argument
passed in is keyword :first only the first instance of old-item will be
replaced.

Section: core

Example:

(let ((lst (list 1 2 3 4 5)))
    (test::assert-equal (list 1 2 10 4 5) (nsubstitute! 10 3 lst))
    (test::assert-equal (list 1 2 10 4 5) lst))"
    (new-item old-item lst &rest mods)
    (let ((early-return (in? mods :first)))
      (loop (idx items) (0 lst)
        (if (empty-seq? items)
            lst
            (do
              (when (= (first items) old-item)
                  (do
                    (setnth! idx new-item lst)
                    (when early-return (return-from nsubstitute! lst))))
              (recur (+ 1 idx) (rest items))))))
    lst)

(defmacro substitute
"Replaces all instances of old-item in copy of lst with new-item.  If last
argument passed in is keyword :first only the first instance of old-item will be
replaced.

Section: core

Example:

(let ((lst (list 1 2 3 4 5))
      (lst2 (list 1 2 3 3 3 4 5)))
    (test::assert-equal (list 1 2 10 4 5) (substitute 10 3 lst))
    (test::assert-equal lst lst)
    (test::assert-equal (list 1 2 4 4 4 4 5) (substitute 4 3 lst2))
    (test::assert-equal (list 1 2 4 3 3 4 5) (substitute 4 3 lst2 :first)))"
     (new-item old-item lst &rest mods)
     `(nsubstitute! ,new-item ,old-item (collect-copy ,lst) ,@mods))

(defmacro dyn
"Usage: (dyn key value expression) -> result_of_expression

Creates a dynamic binding for key, assigns value to it and evals expression under it.
Note that if key must be a symbol and is not evaluted.

The binding is gone once the dyn form ends. This is basically a set! on the
binding in an unwind protect to reset it when done.  When used on a global will
set the first binding found and reset it when done.
Calls to dyn can be nested and previous dynamic values will
be restored as interior dyn's exit.

Section: core

Example:
(defn test-dyn-fn () (print \"Print dyn out\"))
(dyn *stdout* (open \"/tmp/sl-sh.dyn.test\" :create :truncate) (test-dyn-fn))
(test::assert-equal \"Print dyn out\" (read-line (open \"/tmp/sl-sh.dyn.test\" :read)))
"
; XXX this could be better and match the original builtin dyn closer but this version
; is good enough for now (redirectiong stdout/err, etc).  Also if it sets a root
; binding and then the name is used for the current namespace it might 'restore'
; the wrong binding so look into that.
  (key value expression)
  (let ((old-val (gensym)))
    `(let ((,old-val ,key))
       (unwind-protect
            (do (set! ,key ,value) ,expression)
         (set! ,key ,old-val)))))

(defn identity
"Identity function.

Section: core

Example:

(assert-equal 0 (identity 0))"
      (x) x)

(defmacro len0?
"Is the length of thing 0?

Section: core

Example:
(assert-true (len0? nil))
(assert-true (len0? '()))
(assert-true (len0? (vec)))
(assert-true (len0? (str)))
(assert-true (len0? \"\"))
(assert-false (len0? '(1)))
(assert-false (len0? (vec 1 2)))
(assert-false (len0? \"string\"))
"
    (thing) `(= (length ,thing) 0))

(defmacro len>0?
"Is the length of thing greater than 0?

Section: core

Example:
(assert-false (len>0? nil))
(assert-false (len>0? '()))
(assert-false (len>0? (vec)))
(assert-false (len>0? (str)))
(assert-false (len>0? \"\"))
(assert-true (len>0? '(1)))
(assert-true (len>0? (vec 1 2)))
(assert-true (len>0? \"string\"))
"
    (thing) `(> (length ,thing) 0))

(defmacro inc!
"Usage: (inc! symbol [number]) -> new value

Increment the value in symbol by one or the optional number

Section: core

Example:
(def *inc-test* 1)
(test::assert-equal 2 (inc! *inc-test*))
(test::assert-equal 2 *inc-test*)
(test::assert-equal 5 (inc! *inc-test* 3))
(test::assert-error (inc! *inc-test* \"xxx\"))
(test::assert-equal 5 *inc-test*)
(def *inc-test* \"xxx\")
(test::assert-error (inc! *inc-test*))
(let ((inc-test 1))
  (test::assert-equal 2 (inc! inc-test))
  (test::assert-equal 2 inc-test)
  (test::assert-equal 5 (inc! inc-test 3))
  (test::assert-equal 5 inc-test))
"
    (s &rest args)
  (if (len0? args) `(set! ,s (+ ,s 1))
      (= (length args) 1) `(set! ,s (+ ,s ,(vec-nth args 0)))
      (err "inc!: requires a symbol and optional number")))

(defmacro dec!
"Usage: (dec! symbol [number]) -> new value

Decrement the value in symbol by one or the optional number

Section: core

Example:
(def *dec-test* 5)
(test::assert-equal 4 (dec! *dec-test*))
(test::assert-equal 4 *dec-test*)
(test::assert-error (dec! *dec-test* \"xxx\"))
(test::assert-equal 1 (dec! *dec-test* 3))
(test::assert-equal 1 *dec-test*)
(def *dec-test* \"xxx\")
(test::assert-error (dec! *dec-test*))
(let ((dec-test 5))
  (test::assert-equal 4 (dec! dec-test))
  (test::assert-equal 4 dec-test)
  (test::assert-equal 1 (dec! dec-test 3))
  (test::assert-equal 1 dec-test))
"
    (s &rest args)
  (if (len0? args) `(set! ,s (- ,s 1))
      (= (length args) 1) `(set! ,s (- ,s ,(vec-nth args 0)))
      (err "dec!: requires a symbol and optional number")))

(load "collection.lisp")
