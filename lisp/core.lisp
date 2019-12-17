(ns-create 'core)

(if (def? '*ns-exports*) (vec-clear! *ns-exports*))

(def 'defmacro (macro (name args body)
	`(progn (def (quote ,name) (macro ,args ,body)) nil)))

(defmacro ns-export (symbol) `(progn
    (if (not (def? '*ns-exports*)) (defq *ns-exports* (vec)))
    (if (symbol? ,symbol)
        (vec-push! *ns-exports* (quote ,symbol))
        (if (or (list? ,symbol) (vec? ,symbol))
            (for sym ,symbol (vec-push! *ns-exports* sym))
            (err "ns-export takes a symbol or sequence.")))))

(defmacro ns-import (namespace)
    `(core::for sym (eval (to-symbol (str ,namespace "::*ns-exports*"))) (def (to-symbol (str "ns::" sym)) (eval (to-symbol (str ,namespace "::" sym))))))

(defmacro setq (sym bind)
	`(set (quote ,sym) ,bind))

(defmacro defq (sym bind)
	`(def (quote ,sym) ,bind))

(defmacro defn (name args body)
	`(defq ,name (fn ,args ,body)))

(defmacro setfn (name args body)
	`(setq ,name (fn ,args ,body)))

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

(load "seq.lisp")

(ns-export '(defmacro ns-export ns-import setq defq defn setfn loop dotimes dotimesi for fori match let copy-seq))

