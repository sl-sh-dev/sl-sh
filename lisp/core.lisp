(if (ns-exists? 'core) (ns-enter 'core) (ns-create 'core))

(if (def? '*ns-exports*) (vec-clear! *ns-exports*))

(def 'defmacro
"Usage: (defmacro name doc_string? argument_list body)

Create a macro and bind it to a symbol in the current scope.

Section: core
"
	(macro (name &rest args) ((fn ()
	(if (= (length args) 2)
		(progn
			(def 'ars (vec-nth 0 args))
			(def 'body (vec-nth 1 args))
			`(progn (def ',name (macro ,ars ,body)) nil))
		(if (= (length args) 3)
			(progn
				(def 'doc-str (vec-nth 0 args))
				(def 'ars (vec-nth 1 args))
				(def 'body (vec-nth 2 args))
				`(progn (def ',name ,doc-str (macro ,ars ,body)) nil))
			(err "defmacro: Wrong number of args.")))))))

(def 'setmacro
"Usage: (setmacro name doc_string? argument_list body)

Set a macro to an existing symbol.

Section: core
"
	(macro (name &rest args) ((fn ()
	(if (= (length args) 2)
		(progn
			(def 'ars (vec-nth 0 args))
			(def 'body (vec-nth 1 args))
			`(progn (set ',name (macro ,ars ,body)) nil))
		(if (= (length args) 3)
			(progn
				(def 'doc-str (vec-nth 0 args))
				(def 'ars (vec-nth 1 args))
				(def 'body (vec-nth 2 args))
				`(progn (set ',name ,doc-str (macro ,ars ,body)) nil))
			(err "setmacro: Wrong number of args.")))))))

(defmacro ns-export
"Export a symbol or list of symbols to be imported into other namespaces.

Section: namespace"
	(symbol_or_sequence) `(progn
	(if (not (def? '*ns-exports*)) (def '*ns-exports* (vec)))
	(if (symbol? ,symbol_or_sequence)
		(vec-push! *ns-exports* (quote ,symbol_or_sequence))
		(if (or (list? ,symbol_or_sequence) (vec? ,symbol_or_sequence))
			(core::for sym ,symbol_or_sequence (vec-push! *ns-exports* sym))
			(err "ns-export takes a symbol or sequence.")))))

(defmacro ns-import
"Import any symbols exported from namespace into the current namespace.

Section: namespace"
	(namespace)
	`((fn () (progn
		(def 'import (make-vec 2 1))
		(core::for sym (eval (to-symbol (str ,namespace "::*ns-exports*")))
		(progn
			(vec-setnth! 0 (to-symbol (str "ns::" sym)) import)
			(vec-setnth! 1 (eval (to-symbol (str ,namespace "::" sym))) import)
			(apply def import)))))))

(defmacro setq
"Usage: (setq sym doc-string? expression) -> expression

Set an expession to a quoted symbol (ie set 'sym bind)

Section: core
"
	(sym &rest args)
	`(set ',sym ,@args))

(defmacro defq
"Usage: (defq sym doc-string? expression) -> expression

Binds an expession to a quoted symbol (ie def 'sym bind)

Section: core"
	(sym &rest args)
	`(def ',sym ,@args))

(defmacro defn
"
Define a named function in the current namespace.

Section: core
"
	(name &rest args) (core::let ()
	(if (= (length args) 2)
		(progn
			(def 'ars (vec-nth 0 args))
			(def 'body (vec-nth 1 args))
			`(def ',name (fn ,ars (block ,name ,body))))
		(if (= (length args) 3)
			(progn
				(def 'doc-str (vec-nth 0 args))
				(def 'ars (vec-nth 1 args))
				(def 'body (vec-nth 2 args))
				`(def ',name ,doc-str (fn ,ars (block ,name ,body))))
			(err "defn: Wrong number of args.")))))

(defmacro setfn
"
Binds name to function body in current namespace.

Section: core
"
	(name &rest args) (core::let ()
	(if (= (length args) 2)
		(progn
			(def 'ars (vec-nth 0 args))
			(def 'body (vec-nth 1 args))
			`(set ',name (fn ,ars ,body)))
		(if (= (length args) 3)
			(progn
				(def 'doc-str (vec-nth 0 args))
				(def 'ars (vec-nth 1 args))
				(def 'body (vec-nth 2 args))
				`(set ',name ,doc-str (fn ,ars ,body)))
			(err "setfn: Wrong number of args.")))))

(defmacro loop
"
Binds bindings to parameters in body. Use recur with desired bindings for
subsequent iteration.

Section: sequence
"
	(params bindings body)
		`((fn ,params ,body) ,@bindings))

(defmacro dotimes
"
Evaluate body a number of times equal to times' numerical value.

Section: shell
"
	(times body)
	(core::let ((idx-name (gensym)))
	`(if (> ,times 0)
		(core::loop (,idx-name) (,times) (progn
			(,@body)
			(if (> ,idx-name 1) (recur (- ,idx-name 1))))))))

(defmacro dotimesi
"
Evaluate body a number of times equal to times' numnrical value. Includes an
incrementing reference binding, idx-bind, accesible in body.

Section: shell
"
	(idx-bind times body)
	(core::let ((stop-name (gensym)))
	`(if (> ,times 0)
		(core::loop (,idx-bind ,stop-name) (0 (- ,times 1)) (progn
			(,@body)
			(if (< ,idx-bind ,stop-name) (recur (+ ,idx-bind 1) ,stop-name)))))))

(defmacro for
"
bind is bound to the current element of in_list and is accesible
in body. body is evaluated a number of times equal to the the number of items
in in_list.


Section: shell
"
	(bind in_list body)
	`(core::let ((,bind))
		(if (> (length ,in_list) 0)
			(core::loop (plist) (,in_list) (progn
				(core::setq ,bind (core::first plist))
				(,@body)
				(if (> (length plist) 1) (recur (core::rest plist))))))))

(defmacro fori
"
idx-bind is an incrementing reference bound to current elemt in in_list and is
accesible in body. bind is bound to the current element of in_list as is also
accesible in body.  body is evaluated a number of times equal to the the number
of items in in_list.

Section: shell
"
	(idx_bind bind in_list body)
	`((fn () (progn
		(core::defq ,bind nil)(core::defq ,idx_bind nil)
		(if (> (length ,in_list) 0)
			(core::loop (plist idx) (,in_list 0) (progn
				(core::setq ,bind (core::first plist))
				(core::setq ,idx_bind idx)
				(,@body)
				(if (> (length plist) 1) (recur (core::rest plist) (+ idx 1))))))))))

(defmacro match
"Usage: (match condition (value form*)*) -> result

Evaluate condition and look for matching value in each branch of type
(value form*). Form(s) will be wrapped in an implicit progn. Use nil to take
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
	(core::let ((cond-name) (out_list (list)) (make-cond) (make-action))
		(core::setq make-action (fn (action)
			(if (seq? action)
				`(progn ,@action)
				`action)))
		(core::setq make-cond (fn (condition val action others)
			(if (null val) (make-action action)
				(if (empty-seq? others) `(if (= ,condition ,val) ,(make-action action))
					`(if (= ,condition ,val) ,(make-action action) ,(make-cond condition (core::first (core::first others)) (core::rest (core::first others)) (core::rest others)))))))
		(core::setq cond-name condition)
		(make-cond cond-name (core::first (core::first branches)) (core::rest (core::first branches)) (core::rest branches))))

(defmacro cond
"Usage: (cond ((test form*)*) -> result

Evaluate each test in order.  If it is true then evaluate the form(s) in an
implicit progn and return the result.  Stop evaluting at the first true test.
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
	(core::let ((out_list (list)) (make-cond) (make-action))
		(core::setq make-action (fn (action)
			(if (seq? action)
				`(progn ,@action)
				`action)))
		(core::setq make-cond (fn (condition action others)
			(if (empty-seq? others)
				`(if ,condition ,(make-action action) nil)
				`(if ,condition ,(make-action action) ,(make-cond (core::first (core::first others)) (core::rest (core::first others)) (core::rest others))))))
		(make-cond (core::first (core::first branches)) (core::rest (core::first branches)) (core::rest branches))))

(defmacro let
"
Takes list, vals, of form ((binding0 sexp0) (binding1 sexp1) ...) and evaluates
let_body with all values of binding bound to the result of the evaluation of
sexp.

Section: core
"
	(vals &rest let_body)
	((fn (params bindings) (progn
		(core::fori idx el vals
			(if (= 1 (length el))
				(progn (vec-insert-nth! idx (core::nth 0 el) params) (vec-insert-nth! idx nil bindings))
				(if (= 2 (length el))
					(progn (vec-insert-nth! idx (core::nth 0 el) params) (vec-insert-nth! idx (core::nth 1 el) bindings))
					(err "ERROR: invalid bindings on let"))))
		`((fn ,params (progn ,@let_body)) ,@bindings))) (make-vec (length vals)) (make-vec (length vals))))

(defn copy-seq
"
Produces a copy of the provided list (copy has same type as the parameter).

Section: sequence

"
(seq)
    (if (vec? seq)
        (progn
            (def 'tseq (make-vec (length seq)))
            (for el seq (vec-push! tseq el))
            tseq)
        (if (list? seq)
            (progn
                (def 'tseq nil)
                (def 'tcell nil)
                (def 'head nil)
                (for el seq (progn
                    (if (null head)
                        (progn (set 'tseq (set 'head (join el nil))))
                        (progn (set 'tcell (join el nil)) (xdr! tseq tcell) (set 'tseq tcell)))))
                head)
            (err "Not a list or vector."))))

(defmacro func?
"
True if the expression is a [builtin?](#root::builtin?), a [lambda?](#root::lambda?), or a [macro?](#root::macro?)

Section: type
"
	(to-test) `(progn
	(or (builtin? ,to-test) (lambda? ,to-test) (macro? ,to-test))))

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
		(progn
			(defq fst (first (quote ,args)))
			(loop (curr-form forms) (fst (rest (quote ,args)))
				(if (empty-seq? forms)
					curr-form
					(progn
					(defq sexp nil)
					(defq fcn (first forms))
					(if (seq? fcn)
						(setq sexp (append (list (first fcn)) curr-form (rest fcn)))
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
		(progn
			(defq fst (first (quote ,args)))
			(loop (curr-form forms) (fst (rest (quote ,args)))
				(if (empty-seq? forms)
					curr-form
					(progn
					(defq sexp nil)
					(defq fcn (first forms))
					(if (seq? fcn)
						(setq sexp (append fcn curr-form))
						(setq sexp (list fcn curr-form)))
					(recur (eval sexp) (rest forms))))))))

(load "seq.lisp")

(ns-export '(defmacro setmacro ns-export ns-import setq defq defn setfn loop dotimes dotimesi for fori match cond let copy-seq when func? ->> ->))
