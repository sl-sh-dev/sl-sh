#!/usr/bin/env sl-sh

(load "docstrings-to-md.lisp")

(ns-push 'mkdocs)
(ns-import 'iterator)
(ns-import 'shell)
(ns-import 'struct)
(ns-import 'test)

(defn filter-user-undocable-forms (sym-list)
	(filter (fn (x)
		(and (not (= x 'custom-lisp-config))
			(not (= x '*repl-settings*))
			(not (= x '__completion_hook))
			(not (= x '__line_handler))
			(not (= x '__exec_hook))
				(not (= x '__prompt))))
			sym-list))

(defn filter-undocable-forms (sym-list)
	(filter (fn (x)
		(and
			(not (= x 'filter-undocable-forms))
			(not (= x '*std-lib-syms-hash*))
			(not (= x '*ns*))
			(not (= x '*euid*))
			(not (= x '*uid*))
			(not (= x '*repl-settings*))
			(not (= x '*ns-exports*))
			(not (= x '^ns-stack-xyz^))
			(not (= x 'tok-slsh-form-color))
			(not (= x 'tok-slsh-fcn-color))
			(not (= x 'tok-default-color))
			(not (= x 'tok-sys-command-color))
			(not (= x 'tok-sys-alias-color))
			(not (= x 'tok-string-color))
			(not (= x 'tok-invalid-color))
			(not (= x '*last-status*))
			(not (= x '*repl-settings*))
			(not (= x '*last-command*))
			(not (= x '*run-script*))
			(not (= x '*active-ns*))
			(not (= x 'internal-fn))
			(not (= x 'internal-macro))
			(not (= x 'get-doc-list-for))
			(not (= x 'make-md-file))
			(not (= x '__completion_hook))
			(not (= x '__line_handler))
			(not (= x '__exec_hook))
			(not (= x '__prompt))
			(not (= x 'list-of-all-slsh-syms))
			(not (= x 'filter-user-undocable-forms))
			(not (= x 'filter-undocable-forms))
			(not (= x 'args))))
		sym-list))

(defn list-of-all-slsh-syms ()
	(var sym-list (ns-symbols 'root))
	(for a-ns in (filter (fn (x) (and
						(not (= x "docmd"))
						(not (= x "mkpost"))
						(not (= x "moddocs"))
						(not (= x "docstruct"))
						(not (= x "docify"))
						(not (= x "root"))
						;;(not (= x "test"))
						(not (= x "user")
                             ))) (ns-list)) (do
		(append-to! sym-list (eval (sym (str a-ns "::*ns-exports*"))))))
	(filter-undocable-forms (qsort sym-list)))

(defn get-doc-list-for
	(target-doc-form)
	(match target-doc-form
			(:single (append '() (last (list-of-all-slsh-syms))))
			(:lang (list-of-all-slsh-syms))))

(defn make-md-file
"Generate slsh standard library documentation md page."
(index-file target-doc-form)
	(docmd::make-md-file
		index-file
		(collect-vec (map (fn (x) (sym x))
			  (get-doc-list-for target-doc-form)))))

;;(when (> (length args) 0)
;;;;  (println (get-doc-list-for (vec-nth args 1))))

(ns-auto-export 'mkdocs)
(ns-pop)
