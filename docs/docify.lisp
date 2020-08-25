#!/usr/bin/env sl-sh

(load "mk-docs.lisp")
(ns-import 'iterator)
(ns-import 'shell)

(error-stack-on)

(when (not (= 2 (length args)))
	(err "Need required arguments, a path for the markdown file output, and one
		of :lang, :user, or :single. :lang to indicate doc generation with sl-sh
		forms (all forms not in user namespace), :user to indicate doc
		generation of user forms, or :single to generate a page for a random
		function for debugging purposes. Future work will need to be done to
		disambiguate sl-sh forms and user created forms when users themselves
		start making many different namespaces."))

(defq kwd-list (list :user :lang :single))

(defq index-file (first args))
(defq target-doc-form (do
	(defq kwd (to-symbol (first (rest args))))
	(if (in? kwd-list kwd)
		kwd
		(err (str "Second argument must be one of " kwd-list " was, " kwd ", type: " (type kwd))))))

(for arg in args (do
	(if (or (= arg ":lang") (= arg ":user") (= arg ":single"))
		(setq target-doc-form (to-symbol arg))
		(setq index-file arg))))

(if (mkdocs::make-md-file index-file target-doc-form) (exit 0) (exit 1))
