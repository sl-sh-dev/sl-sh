;;; These are core forms that are installed in the root namespace so they
;;; are always available.

(def 'internal-macro
"
Template for macros the define macros, can pass an 'op' like def or set.
Intended for use by other core macros.

Section: core
"
    (macro (op name &rest args)
           ((fn ()
                (if (< (length args) 2) (err "defmacro: Wrong number of args."))
                (if (string? (vec-nth 0 args))
                  (do
                    (def 'doc-str (vec-nth 0 args))
                    (def 'ars (vec-nth 1 args))
                    (def 'body (vec-slice args 2))
                    `(do (,op ',name ,doc-str (macro ,ars ,@body)) nil))
                  (do
                    (def 'ars (vec-nth 0 args))
                    (def 'body (vec-slice args 1))
                    `(do (,op ',name (macro ,ars ,@body)) nil)))))))

(def 'defmacro
"Usage: (defmacro name doc_string? argument_list body)

Create a macro and bind it to a symbol in the current scope.

Section: core
"
    (macro (name &rest args) `(internal-macro def ,name ,@args)))

(def 'setmacro
"Usage: (setmacro name doc_string? argument_list body)

Set a macro to an existing symbol.

Section: core
"
    (macro (name &rest args) `(internal-macro set ,name ,@args)))

(defmacro ns-export
"Export a symbol or list of symbols to be imported into other namespaces.

Section: namespace"
    (symbol_or_sequence) `(do
    (if (not (def? '*ns-exports*)) (def '*ns-exports* (vec)))
    (if (symbol? ,symbol_or_sequence)
        (vec-push! *ns-exports* (quote ,symbol_or_sequence))
        (if (or (list? ,symbol_or_sequence) (vec? ,symbol_or_sequence))
            (iterator::for sym in ,symbol_or_sequence (vec-push! *ns-exports* sym))
            (err "ns-export takes a symbol or sequence.")))))

(defmacro ns-import
"Import any symbols exported from namespace into the current namespace.

Section: namespace"
    (namespace)
    `((fn ()
        (def 'import (vec defq 1 2))
        (iterator::for sym in (eval (to-symbol (str ,namespace "::*ns-exports*")))
                       (do
                         (vec-setnth! 1 (to-symbol (str "ns::" sym)) import)
                         (vec-setnth! 2 (to-symbol (str ,namespace "::" sym)) import)
                         (eval import))))))

(defmacro setq
"Usage: (setq sym doc-string? expression) -> expression

Set an expession to a quoted symbol (ie set 'sym bind)

Section: root

Example:
(def 'test-do-one nil)
(def 'test-do-two nil)
(def 'test-do-three (do (setq test-do-one \"One\")(setq test-do-two \"Two\")\"Three\"))
(test::assert-error (setq xyryx-dont-exist \"something\"))
(test::assert-equal \"One\" test-do-one)
(test::assert-equal \"Two\" test-do-two)
(test::assert-equal \"Three\" test-do-three)
(let ((test-do-one nil))
    ; set the currently scoped value.
    (test::assert-equal \"1111\" (setq test-do-one \"1111\"))
    (test::assert-equal \"1111\" test-do-one))
; Original outer scope not changed.
(test::assert-equal \"One\" test-do-one)
"
    (sym &rest args)
    `(set ',sym ,@args))

(defmacro defq
"Usage: (defq sym doc-string? expression) -> expression

Binds an expession to a quoted symbol (ie def 'sym bind)

Section: root

Example:
(defq test-do-one \"One\")
(defq test-do-two \"Two\")
(defq test-do-three \"Three\")
(test::assert-equal \"One\" test-do-one)
(test::assert-equal \"Two\" test-do-two)
(test::assert-equal \"Three\" test-do-three)
(let ((test-do-one nil))
    ; Add this to the let's scope (shadow the outer test-do-two).
    (test::assert-equal \"Default\" (defq test-do-two \"Default\"))
    ; set the currently scoped value.
    (set 'test-do-one \"1111\")
    (set 'test-do-two \"2222\")
    (test::assert-equal \"1111\" test-do-one)
    (test::assert-equal \"2222\" test-do-two))
; Original outer scope not changed.
(test::assert-equal \"One\" test-do-one)
"
    (sym &rest args)
    `(def ',sym ,@args))

(defmacro internal-fn
"
Template for macros the define functions, can pass an 'op' like def or set.
Intended for use by other core macros.

Section: core

Example:
; tested in defn and setfn.
t
"
    (op name &rest args)
    ((fn ()
         (if (< (length args) 1) (err "defn: Wrong number of args."))
         (if (string? (vec-nth 0 args))
           (do
             (if (< (length args) 2) (err "defn: Wrong number of args."))
             (def 'doc-str (vec-nth 0 args))
             (def 'ars (vec-nth 1 args))
             (def 'body (if (> (length args) 2) (vec-slice args 2) (vec nil)))
             `(,op ',name ,doc-str (fn ,ars (block ,name ,@body))))
           (do
             (def 'ars (vec-nth 0 args)) 
             (def 'body (if (> (length args) 1) (vec-slice args 1) (vec nil)))
             `(,op ',name (fn ,ars (block ,name ,@body))))))))

(defmacro defn
"
Define a named function in the current scope.

Section: core

Example:
(defn defn-test (x y) (+ x y))
(test::assert-equal 5 (defn-test 2 3))
(defn defn-test (x y) (set 'x (* x 2))(+ x y))
(test::assert-equal 7 (defn-test 2 3))
(defn defn-test (x y))
(test::assert-false (defn-test 2 3))
(defn defn-test (x y) t)
(test::assert-true (defn-test 2 3))
"
    (name &rest args) `(internal-fn def ,name ,@args)) 

(defmacro setfn
"
Binds name to function body in current namespace.

Section: core

Example:
(def 'defn-test nil)
(setfn defn-test (x y) (+ x y))
(test::assert-equal 5 (defn-test 2 3))
(setfn defn-test (x y) (set 'x (* x 2))(+ x y))
(test::assert-equal 7 (defn-test 2 3))
(setfn defn-test (x y))
(test::assert-false (defn-test 2 3))
(setfn defn-test (x y) t)
(test::assert-true (defn-test 2 3))
"
    (name &rest args) `(internal-fn set ,name ,@args)) 

(defn ns-pop
"Usage: (ns-pop)

Returns to the previous namespace.

Section: namespace

Example:
(ns-create 'ns-pop-test-namespace)
(test::assert-equal \"ns-pop-test-namespace\" *ns*)
(ns-pop)
(test::assert-not-equal \"ns-pop-test-namespace\" *ns*)
"
    () (ns-enter *last-ns*))

(defmacro loop
"
Binds bindings to parameters in body. Use recur with desired bindings for
subsequent iteration.

Section: root

Example:
(def 'tot 0)
(loop (idx) (3) (do
    (set 'tot (+ tot 1))
    (if (> idx 1) (recur (- idx 1)))))
(assert-equal 3 tot)
"
    (params bindings body)
        `((fn ,params ,body) ,@bindings))

(defmacro dotimes
"
Evaluate body a number of times equal to times' numerical value.

Section: root

Example:
(def 'i 0)
(dotimes 11 (set 'i (+ 1 i)))
(assert-equal 11 i)
"
    (times body)
    ((fn (idx-name)
    `(if (> ,times 0)
        (root::loop (,idx-name) (,times) (do
            (,@body)
            (if (> ,idx-name 1) (recur (- ,idx-name 1)))))))(gensym)))

(defmacro dotimes-i
"
Evaluate body a number of times equal to times' numnrical value. Includes an
incrementing reference binding, idx-bind, accesible in body.

Section: root

Example:
(def 'i 0)
(def 'i-tot 0)
(dotimes-i idx 11 (do (set 'i-tot (+ idx i-tot))(set 'i (+ 1 i))))
(assert-equal 11 i)
(assert-equal 55 i-tot)
"
    (idx-bind times body)
    ((fn (stop-name)
    `(if (> ,times 0)
        (root::loop (,idx-bind ,stop-name) (0 (- ,times 1)) (do
            (,@body)
            (if (< ,idx-bind ,stop-name) (recur (+ ,idx-bind 1) ,stop-name))))))(gensym)))

(defmacro match
"Usage: (match condition (value form*)*) -> result

Evaluate condition and look for matching value in each branch of type
(value form*). Form(s) will be wrapped in an implicit do. Use nil to take
action if no match (encouraged!).

Section: core

Example:

(defn select-option (a)
    (match a (1 \"opt-one\")
             (2 (set 'b 5) \"opt-two\")
             (3 (str \"opt\" \"-three\"))))
(defn select-option-def (a)
    (match a (1 \"opt-one\")
             (2 \"opt-two\")
             (3 (str \"opt\" \"-three\"))
             (nil \"default\")))
(def 'b 0)
(assert-equal b 0)
(assert-equal \"opt-one\" (select-option 1))
(assert-equal \"opt-two\" (select-option 2))
(assert-equal b 5)
(assert-equal \"opt-three\" (select-option 3))
(assert-equal nil (select-option 4))
(assert-equal \"opt-one\" (select-option-def 1))
(assert-equal \"opt-two\" (select-option-def 2))
(assert-equal \"opt-three\" (select-option-def 3))
(assert-equal \"default\" (select-option-def 4))
"
    (condition &rest branches)
    ((fn ()
        (def 'out_list (list))
        (def 'make-action (fn (action)
            (if (seq? action)
                `(do ,@action)
                `action)))
        (def 'make-cond (fn (condition val action others)
            (if (null val) (make-action action)
                (if (empty-seq? others) `((= ,condition ,val) ,(make-action action))
                    `((= ,condition ,val) ,(make-action action) ,@(make-cond condition (root::first (root::first others)) (root::rest (root::first others)) (root::rest others)))))))
        (def 'cond-name condition)
        `(if ,@(make-cond cond-name (root::first (root::first branches)) (root::rest (root::first branches)) (root::rest branches))))))

(defmacro cond
"Usage: (cond ((test form*)*) -> result

Evaluate each test in order.  If it is true then evaluate the form(s) in an
implicit do and return the result.  Stop evaluting at the first true test.
Return nil if no conditions are true.

Section: core

Example:

(defn select-option (a)
    (cond ((= a 1) \"opt-one\")
          ((= a 2) (set 'b 5) \"opt-two\")
          ((= a 3) (str \"opt\" \"-three\"))))
(defn select-option-def (a)
    (cond ((= a 1) \"opt-one\")
          ((= a 2) \"opt-two\")
          ((= a 3) (str \"opt\" \"-three\"))
          (t \"default\")))
(def 'b 0)
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
        (def 'out_list (list))
        (def 'make-action (fn (action)
            (if (seq? action)
                `(do ,@action)
                `action)))
        (def 'make-cond (fn (condition action others)
            (if (empty-seq? others)
                `(,condition ,(make-action action) nil)
                `(,condition ,(make-action action) ,@(make-cond (root::first (root::first others)) (root::rest (root::first others)) (root::rest others))))))
        `(if ,@(make-cond (root::first (root::first branches)) (root::rest (root::first branches)) (root::rest branches))))))

(defmacro let
"
Takes list, vals, of form ((binding0 sexp0) (binding1 sexp1) ...) and evaluates
let-body with all values of binding bound to the result of the evaluation of
sexp.

Section: core

Example:
(def 'test-do-one \"One1\")
(def 'test-do-two \"Two1\")
(def 'test-do-three (let ((test-do-one \"One\")) (set 'test-do-two \"Two\")(test::assert-equal \"One\" test-do-one)\"Three\"))
(test::assert-equal \"One1\" test-do-one)
(test::assert-equal \"Two\" test-do-two)
(test::assert-equal \"Three\" test-do-three)
"
    (vals &rest let-body)
    (lex
      (def 'vars (make-vec (length vals)))
        (iterator::for-i idx el in vals
            (if (= 1 (length el))
                (vec-push! vars `(def ',(iterator::nth 0 el) nil))
                (if (= 2 (length el))
                  (vec-push! vars `(def ',(iterator::nth 0 el) ,(iterator::nth 1 el)))
                  (err "ERROR: invalid bindings on let"))))
        `(lex ,@vars ,@let-body)))

(defmacro func?
"
True if the expression is a [builtin?](#root::builtin?), a [lambda?](#root::lambda?), or a [macro?](#root::macro?)

Section: type

Example:
(def 'func?-test 1)
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

(defmacro ->
"inserts result of previous expression as second argument to current expression.
First argument is not evaluated.

Section: shell

Example:

(assert-equal
    (str \"I go at the beginning.I'll be stuck in the middle.I'll be at the end.\")
    (-> \"I go at the beginning.\"
        (str \"I'll be stuck in the middle.\")
        (str \"I'll be at the end.\")))"
(&rest args)
    `(if (< (length (quote ,args)) 2)
        (err "-> (thush operator) requires at least two arguments")
        (do
            (defq fst (first (quote ,args)))
            (loop (curr-form forms) (fst (rest (quote ,args)))
                (if (empty-seq? forms)
                    curr-form
                    (do
                    (defq sexp nil)
                    (defq fcn (first forms))
                    (if (seq? fcn)
                        (setq sexp (collect (iterator::append (list (first fcn)) curr-form (rest fcn))))
                        (setq sexp (list fcn curr-form)))
                    (recur (eval sexp) (rest forms))))))))

(defmacro ->>
"inserts result of previous expression as last argument to current expression.
First argument is not evaluated.

Section: shell

Example:

(assert-equal
    (str \"I'll be at the beginning.I'll be more in the middle.I go at the end.\")
    (->> \"I go at the end.\"
        (str \"I'll be more in the middle.\")
        (str \"I'll be at the beginning.\")))"
(&rest args)
    `(if (< (length (quote ,args)) 2)
        (err "->> (thush operator) requires at least two arguments")
        (do
            (defq fst (first (quote ,args)))
            (loop (curr-form forms) (fst (rest (quote ,args)))
                (if (empty-seq? forms)
                    curr-form
                    (do
                      (defq sexp nil)
                      (defq fcn (first forms))
                      (if (seq? fcn)
                        (setq sexp (collect (iterator::append fcn curr-form)))
                        (setq sexp (list fcn curr-form)))
                      (recur (eval sexp) (rest forms))))))))

; Reader macro for #.
(defn reader-macro-dot
"Reader macro for #.(...).  Do not call directly.

Section: root
"
      (stream ch) (eval (read stream)))

(load "collection.lisp")
