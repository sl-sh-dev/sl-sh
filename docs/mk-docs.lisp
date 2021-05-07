#!/usr/bin/env sl-sh

(load "docstrings-to-md.lisp")

(ns-push 'mkdocs)
(ns-import 'iterator)
(ns-import 'shell)
(ns-import 'struct)
(ns-import 'test)
(ns-import 'docmd)

;; TODO
;;  - some undocable forms are global variables that
;;  should actually have docs, and be explained to the user,
;;  because they are useful
;;  - global symbols arne't showing up...
;;  - all the relative links are broken..?
(defn filter-undocable-forms (sym-list)
	(filter (fn (x)
		(and
			(not (= x 'filter-undocable-forms))
			(not (= x '*ns*))
			(not (= x '*euid*))
			(not (= x '*uid*))
			(not (= x '*ns-exports*))
			(not (= x '^ns-stack-xyz^))
			(not (= x 'tok-slsh-form-color))
			(not (= x 'tok-slsh-fcn-color))
			(not (= x 'tok-default-color))
			(not (= x 'tok-sys-command-color))
			(not (= x 'tok-sys-alias-color))
			(not (= x 'tok-string-color))
			(not (= x 'tok-invalid-color))
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
			(not (= x 'args))))
		sym-list))

(def *default-namespaces* (list "mkpost" "mkdocs" "docmd" "docstruct" "stats" "struct" "math" "iterator" "root" "test"))

(defn list-of-all-slsh-syms ()
    (let* ((sym-list (collect (filter-undocable-forms (qsort (hash-keys *std-lib-syms-hash*))))))
    sym-list))

(defn get-doc-list-for
    (target-doc-form)
    (match target-doc-form
            (:single (append '() (last (list-of-all-slsh-syms))))
            (:lang (list-of-all-slsh-syms))
            (nil (err "target doc keyword invalid! must be one of :single, for
                       a preview of one random doc, or :lang, to generate doc
                       page for entire sl-sh std lib."))))

(defn make-md-file
"Generate slsh standard library documentation md page."
(index-file syms-or-doc-form)
	(if (symbol? syms-or-doc-form)
      (let ((syms (collect-vec (map (fn (x) (sym x))
			(get-doc-list-for syms-or-doc-form)))))
		(gen-std-lib-md-file
		index-file
		syms))
     ;;TODO this or requires too much esoteric knowledge to be a good callee
     ;; need a common way to ask if something is useable on the std lib's 
     ;; seq/iter stuffz.
      (if (or (seq? syms-or-doc-form) (iter? syms-or-doc-form))
        (let ((syms (collect-vec (map (fn (x) (sym x))
               syms-or-doc-form))))
          (gen-std-lib-md-file index-file (qsort syms)))
        (err "second argument ot make-md-file must be keyword :single or :lang
        or a sequence of sl-sh symbols"))))

(ns-auto-export 'mkdocs)
(ns-pop)
