(if (ns-exists? 'core) (ns-enter 'core) (ns-create 'core))

(if (def? '*ns-exports*) (vec-clear! *ns-exports*))

(def 'defmacro
"Usage: (defmacro name doc_string? argument_list body)

Create a macro and bind it to a symbol in the current scope,"
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

Set a macro to an existing symbol."
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
"Export a symbol or list of symbols to be imported into other namespaces."
	(symbol_or_sequence) `(progn
	(if (not (def? '*ns-exports*)) (def '*ns-exports* (vec)))
	(if (symbol? ,symbol_or_sequence)
		(vec-push! *ns-exports* (quote ,symbol_or_sequence))
		(if (or (list? ,symbol_or_sequence) (vec? ,symbol_or_sequence))
			(core::for sym ,symbol_or_sequence (vec-push! *ns-exports* sym))
			(err "ns-export takes a symbol or sequence.")))))

(defmacro ns-import
"Import any symbols exported from namespace into the current namespace."
	(namespace)
	`((fn () (progn
		(def 'import (make-vec 2 1))
		(core::for sym (eval (to-symbol (str ,namespace "::*ns-exports*")))
		(progn
			(vec-setnth! 0 '(to-symbol (str "ns::" sym)) import)
			(vec-setnth! 1 (to-symbol (str ,namespace "::" sym)) import)
			(apply def import)))))))

(defmacro setq
"Usage: (setq sym doc-string? expression) -> expression

Set an expession to a quoted symbol (ie set 'sym bind)"
	(sym &rest args)
	`(apply set ',sym ',args))

(defmacro defq
"Usage: (defq sym doc-string? expression) -> expression

Binds an expession to a quoted symbol (ie def 'sym bind)"
	(sym &rest args)
	`(apply def ',sym ',args))

(defmacro defn (name &rest args) (core::let ()
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

(defmacro setfn (name &rest args) (core::let ()
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

(defmacro loop (params bindings body)
		`((fn ,params ,body) ,@bindings))

(defmacro dotimes (times body)
	(core::let ((idx-name (gensym)))
	`(if (> ,times 0)
		(core::loop (idx-name) (,times) (progn
			(,@body)
			(if (> idx-name 1) (recur (- idx-name 1))))))))

(defmacro dotimesi (idx-bind times body)
	(core::let ((stop-name (gensym)))
	`(if (> ,times 0)
		(core::loop (,idx-bind stop-name) (0 (- ,times 1)) (progn
			(,@body)
			(if (< ,idx-bind stop-name) (recur (+ ,idx-bind 1) stop-name)))))))

(defmacro for (bind in_list body)
	`(core::let ((,bind))
		(if (> (length ,in_list) 0)
			(core::loop (plist) (,in_list) (progn
				(core::setq ,bind (core::first plist))
				(,@body)
				(if (> (length plist) 1) (recur (core::rest plist))))))))

(defmacro fori (idx_bind bind in_list body)
	`((fn () (progn
		(core::defq ,bind nil)(core::defq ,idx_bind nil)
		(if (> (length ,in_list) 0)
			(core::loop (plist idx) (,in_list 0) (progn
				(core::setq ,bind (core::first plist))
				(core::setq ,idx_bind idx)
				(,@body)
				(if (> (length plist) 1) (recur (core::rest plist) (+ idx 1))))))))))

(defmacro match (condition &rest branches)
	(core::let ((cond-name) (out_list (list)) (make-cond))
		(core::setq make-cond (fn (condition val action others)
			(if (null val) action
				(if (null others) `(if (= ,condition ,val) ,action)
					`(if (= ,condition ,val) ,action ,(make-cond condition (core::first (core::first others)) (core::nth 1 (core::first others)) (core::rest others)))))))
		(core::setq cond-name condition)
		(make-cond cond-name (core::first (core::first branches)) (core::nth 1 (core::first branches)) (core::rest branches))))

(defmacro let (vals &rest let_body)
	((fn (params bindings) (progn
		(core::fori idx el vals
			(if (= 1 (length el))
				(progn (vec-insert-nth! idx (core::nth 0 el) params) (vec-insert-nth! idx nil bindings))
				(if (= 2 (length el))
					(progn (vec-insert-nth! idx (core::nth 0 el) params) (vec-insert-nth! idx (core::nth 1 el) bindings))
					(err "ERROR: invalid bindings on let"))))
		`((fn ,params (progn ,@let_body)) ,@bindings))) (make-vec (length vals)) (make-vec (length vals))))

(defn copy-seq (seq)
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

(defn reduce
"Usage: (reduce reducing-fcn init-val coll)

`reduce` is used to amalgamate a provided collection, `coll`, and an intitial value,
`init-val`, according to the reducing function, reducing-fcn, provided. The
reducing-fcn should be a function of two arguments. In the first iteration of
reduce, the `init-val` will be used as the first argument to the reducing-fcn and
`(first coll)` will be used as the second argument. For all subsequent iterations,
The result from the previous application of the reducing-fcn will be used as the
first argument to the reducing-fcn and the second argument will be the next item
in the collection when the collection is empty reduce will return the
amalgamated result.
"
	(reducing-fcn init-val coll)
		(if (not (first coll))
				init-val
				(recur reducing-fcn (reducing-fcn init-val (first coll)) (rest coll))))

(defmacro when
"Usage: (when provided-condition if-true)

`when` is a convenience function used to check a form, `provided-condition`,
and run some form, `if-true`, if the provided as the first argument evaluates
to true.
"
	(provided-condition if-true)
	`(if ,provided-condition ,if-true))

(defn filter
"Usage: (reduce pred coll)

`filter` is used to strip a collection, `coll`, of all values that do not
pass the condition defined by the function, `pred`. Filter returns a new
collection.

Example:
(assert-equal '(2 4) (filter (fn (x) (= (% x 2) 0)) (list 1 2 3 4 5)))
"
	(pred coll)
		(progn
			(defq filtering-fcn
				(fn (pred filtered-coll coll-to-filter)
					(progn (defq fst (first coll-to-filter))
					(if (empty-seq? coll-to-filter)
						filtered-coll
						(recur
							pred
							(if (pred fst) (append filtered-coll fst) filtered-coll)
							(rest coll-to-filter))))))
			(filtering-fcn pred (list) coll)))

(defmacro func? (to-test) `(progn
	(or (builtin? ,to-test) (lambda? ,to-test) (macro? ,to-test))))

(load "seq.lisp")

(ns-export '(defmacro setmacro ns-export ns-import setq defq defn setfn loop dotimes dotimesi for fori match let copy-seq reduce when filter func?))
