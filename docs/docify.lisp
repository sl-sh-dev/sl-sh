#!/bin/sl-sh

(load "docstrings-to-md.lisp")
(core::ns-import 'core)
(ns-import 'shell)

(error-stack-on)

(when (not (= 2 (length args)))
	(err "Need required arguments, a path for the markdown file output, and
		either --lang XOR --user. --lang to indicate doc generation with sl-sh
		forms (all forms not in user namespace) or --user to indicate doc
		generation of user forms. Future work will beed to be done to
		disambiguate sl-sh forms and user created forms when users themselves
		start making many different namespaces."))

(defq target-doc-forms nil)
(defq index-file nil)

(for arg args (progn
	(if (or (= arg ":lang") (= arg ":user") (= arg ":single"))
		(setq target-doc-forms (to-symbol arg))
		(setq index-file arg))))

(defn filter-undocable-forms (sym-list)
	(filter (fn (x)
		(and
			(not (= x '*ns*))
			(not (= x 'tok-slsh-form-color))
			(not (= x 'tok-slsh-fcn-color))
			(not (= x 'tok-default-color))
			(not (= x 'tok-sys-command-color))
			(not (= x 'tok-sys-alias-color))
			(not (= x 'tok-string-color))
			(not (= x 'tok-invalid-color))
			;;TODO should not exclude last-status OR export
			(not (= x '*last-status*))
			(not (= x '*repl-settings*))
			(not (= x 'export))
			(not (= x 'args))))
		sym-list))

(defn list-of-user-slsh-syms ()
	(filter-undocable-forms (qsort (ns-symbols "user"))))

(defn list-of-all-slsh-syms () (progn
	(defq sym-list (ns-symbols 'root))
	(for a-ns (filter (fn (x) (and
						(not (= x "docmd"))
						(not (= x "docparse"))
						(not (= x "root"))
						(not (= x "user")))) (ns-list)) (progn
		(append! sym-list (eval (to-symbol (str a-ns "::*ns-exports*"))))))
	(filter-undocable-forms (qsort sym-list))))

(defq docstrings-list nil)
(match target-doc-forms
	(:single (setq docstrings-list (append '() (last (list-of-all-slsh-syms)))))
	(:user (setq docstrings-list (list-of-user-slsh-syms)))
	(:lang (setq docstrings-list (list-of-all-slsh-syms))))

(docmd::make-md-file index-file docstrings-list)

;; TODO deployment polish
;;	-	deploying a dir to gh-pages https://gist.github.com/cobyism/4730490
;;	-	make pre push hook: https://victorafanasev.info/tech/deploy-jekyll-build-to-github-pages-using-git-pre-push-hook, should check return code and fail if there are uncategorized forms!
