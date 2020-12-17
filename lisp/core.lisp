;;; These are core forms that are installed in the root namespace so they
;;; are always available.

(def defmacro
"Usage: (defmacro name doc_string? argument_list body)

Create a macro and bind it to a symbol in the current scope.

Section: core

Example:
(defmacro test-mac (x) (var y (+ (ref (ref x)) 1)) `(set! ,x ,y))
(def test-mac-x 2)
(test-mac test-mac-x)
(test::assert-equal 3 test-mac-x)
(defmacro test-mac (x) `(set! ,x 15))
(test-mac test-mac-x)
(test::assert-equal 15 test-mac-x)
"
    (macro (name &rest args) 
           ((fn ()
                (var ars nil)
                (var body nil)
                (if (< (length args) 2) (err "defmacro: Wrong number of args."))
                (if (string? (vec-nth args 0))
                  (do
                    (var doc-str (vec-nth args 0))
                    (set! ars (vec-nth args 1))
                    (set! body (vec-slice args 2))
                    `(do (def ,name ,doc-str (macro ,ars ,@body)) nil))
                  (do
                    (set! ars (vec-nth args 0))
                    (set! body (vec-slice args 1))
                    `(def ,name (macro ,ars ,@body))))))))

(defmacro lex 
            "Usage: (lex exp0 ... expN) -> expN

Evaluatate each form and return the last like do but it creates a new lexical scope around the call.
This is basically like wrapping in a fn call but without the fn call or like a let
without the initial bindings (you can use var to bind symbols in the new scope instead).

Section: core

Example:
(def test-do-one \"One1\")
(def test-do-two \"Two1\")
(def test-do-three (lex (var test-do-one \"One\")(set! test-do-two \"Two\")(test::assert-equal \"One\" test-do-one)\"Three\"))
(test::assert-equal \"One1\" test-do-one)
(test::assert-equal \"Two\" test-do-two)
(test::assert-equal \"Three\" test-do-three)
"
  (&rest lex-body) `((fn () ,@lex-body)))

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

(defmacro internal-fn
"
Template for macros the define functions, can pass an 'op' like def or set.
Intended for use by other core macros.

Section: core

Example:
; tested in defn and varfn.
t
"
    (op name &rest args)
    ((fn ()
         (if (< (length args) 1) (err "defn: Wrong number of args."))
         (var ars nil)
         (var body nil)
         (if (string? (vec-nth args 0))
           (do
             (if (< (length args) 2) (err "defn: Wrong number of args."))
             (var doc-str (vec-nth args 0))
             (set! ars (vec-nth args 1))
             (set! body (if (> (length args) 2) (vec-slice args 2) (vec nil)))
             `(,op ,name ,doc-str (fn ,ars (block ,name ,@body))))
           (do
             (set! ars (vec-nth args 0)) 
             (set! body (if (> (length args) 1) (vec-slice args 1) (vec nil)))
             `(,op ,name (fn ,ars (block ,name ,@body))))))))

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
(defn defn-test (x y) t)
(test::assert-true (defn-test 2 3))
"
    (name &rest args) `(internal-fn def ,name ,@args)) 

(defmacro varfn
"
Binds name to function body in current lexical scope (not namespace- like var).

Section: core

Example:
(lex
  (varfn varfn-test (x y) (+ x y))
  (test::assert-equal 5 (varfn-test 2 3))
  (varfn varfn-test2 (x y) (set! x (* x 2))(+ x y))
  (test::assert-equal 7 (varfn-test2 2 3))
  (test::assert-true (def? varfn-test))
  (test::assert-true (def? varfn-test2)))
(test::assert-false (def? varfn-test))
(test::assert-false (def? varfn-test2))
"
    (name &rest args) `(internal-fn var ,name ,@args)) 

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
        (set! ^ns-stack-xyz^ (cdr ^ns-stack-xyz^))));)

(defmacro loop
"
Binds bindings to parameters in body. Use recur with desired bindings for
subsequent iteration.

Section: core

Example:
(def tot 0)
(loop (idx) (3) (do
    (set! tot (+ tot 1))
    (if (> idx 1) (recur (- idx 1)))))
(assert-equal 3 tot)
"
    (params bindings body)
        `((fn ,params ,body) ,@bindings))

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
        (root::loop (,idx-name) (,times) (do
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
        (root::loop (,idx-bind ,stop-name) (0 (- ,times 1)) (do
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
(assert-equal nil (select-option 4))
(assert-equal \"opt-one\" (select-option-def 1))
(assert-equal \"opt-two\" (select-option-def 2))
(assert-equal \"opt-three\" (select-option-def 3))
(assert-equal \"default\" (select-option-def 4))
"
    (condition &rest branches)
    ((fn ()
        (var out_list (list))
        (var make-action (fn (action)
            (if (seq? action)
                `(do ,@action)
                `action)))
        (var make-cond (fn (condition val action others)
            (if (null val) (make-action action)
                (if (empty-seq? others) `((= ,condition ,val) ,(make-action action))
                    `((= ,condition ,val) ,(make-action action) ,@(make-cond condition (root::first (root::first others)) (root::rest (root::first others)) (root::rest others)))))))
        (var cond-name condition)
        `(if ,@(make-cond cond-name (root::first (root::first branches)) (root::rest (root::first branches)) (root::rest branches))))))

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
          (t \"default\")))
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
        (var out_list (list))
        (var make-action (fn (action)
            (if (seq? action)
                `(do ,@action)
                `action)))
        (var make-cond (fn (condition action others)
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
(def test-do-one \"One1\")
(def test-do-two \"Two1\")
(def test-do-three (let ((test-do-one \"One\")) (set! test-do-two \"Two\")(test::assert-equal \"One\" test-do-one)\"Three\"))
(test::assert-equal \"One1\" test-do-one)
(test::assert-equal \"Two\" test-do-two)
(test::assert-equal \"Three\" test-do-three)
"
    (vals &rest let-body)
    (var vars (make-vec (length vals)))
    (var binds (make-vec (length vals)))
      ((fn (plist)
        (if (> (length plist) 0)
          (do
            (var el (car plist))
            (if (= 1 (length el))
              (do
                (vec-push! vars (car el))
                (vec-push! binds nil))
              (if (= 2 (length el))
                (do
                  (vec-push! vars (car el))
                  (vec-push! binds (car (cdr el))))
                (err "ERROR: invalid bindings on let")))
            (recur (cdr plist)))))vals)
      `((fn ,vars ,@let-body) ,@binds))

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
    (if (< (length args) 2)
        (err "-> (thush operator) requires at least two arguments")
        (do
            (var fst (first args))
            (loop (curr-form forms) (fst (rest args))
                (if (empty-seq? forms)
                    curr-form
                    (do
                        (var sexp nil)
                        (var fcn (first forms))
                        (if (seq? fcn)
                            (set! sexp `(,(first fcn) ,curr-form ,@(rest fcn)))
                            (set! sexp (list fcn curr-form)))
                        (recur sexp (rest forms))))))))

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
    (if (< (length args) 2)
        (err "->> (thush operator) requires at least two arguments")
        (do
            (var fst (first args))
            (loop (curr-form forms) (fst (rest args))
                (if (empty-seq? forms)
                    curr-form
                    (do
                      (var sexp nil)
                      (var fcn (first forms))
                      (if (seq? fcn)
                        (set! sexp `(,@fcn ,curr-form))
                        (set! sexp (list fcn curr-form)))
                      (recur sexp (rest forms))))))))

; Reader macro for #.
(defn reader-macro-dot
"Reader macro for #.(...).  Do not call directly.

Section: core

Example:
(def dot-test (read \"(1 2 #.(* 3 10) #.(str \"o\" \"ne\"))))
(test::assert-equal '(1 2 30 \"one\"))
"
      (stream ch) (eval (read stream)))

(load "collection.lisp")

