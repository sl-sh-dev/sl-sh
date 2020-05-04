#!/bin/sl-sh

(core::ns-import 'core)
(ns-import 'shell)
(load "parse-docstrings.lisp")
(ns-import 'docparse)

(error-stack-on)

(when (not (= 1 (length args)))
	(err "Need required argument, a path to write the markdown file."))

(defq header "---
layout: default
title: Sl-sh form documentation
---

# Sl-sh form documentation

;;TODO need anchor links / table of contents to all forms here
")

(defq user-input (first args))
(defq index-file (progn
	(defq new-file (open user-input :create :truncate))
	(write-string new-file header)
	(close new-file)
	user-input))

(defn make-str-md-row (&rest args) (progn
	(defq row (list " | "))
	(for a args (progn
		(append! row (list a " | "))))
	row))

(defn make-doc-md-row (doc-list) (progn
	(defq row (list " | "))
	(for a doc-list (progn
		(append! row (list a " | "))))
	row))

(defn join-md-rows (&rest args) (progn
	(defq table (list))
	(for row args
		(append! table (list row)))
	table))

(defn human-readable-name (key)
	;; TODO why does this need to be stringified?
	(match (str key)
		("char" "Char forms")
		("conditional" "Conditional forms")
		("core" "Core forms")
		("file" "File forms")
		("hashmap" "Hashmap forms")
		("math" "Math forms")
		("namespace" "Namespace forms")
		("pair" "Pair forms")
		("shell" "Shell forms")
		("string" "String forms")
		("type" "Type forms")
		("vector" "Vector forms")
		(":uncategorized" "Uncategorized forms")
		(nil "Unknown forms")))

(defn write-md-table (key table file-name) (progn
	(defq file (open file-name :append))
	(defq name (human-readable-name key))
	(write-line file (str "## " (if (not name) "Unknown forms" name)))
	(write-line file "")
	(write-line file "")
	(for row table (progn
		(defq line (apply str row))
		(write-line file line)))
	(write-line file "")
	(close file)
	file-name))

(defn format-first-line-as-code (text-slice delim) (progn
	(defq arr (str-split delim text-slice))
	;; if first char in str is delim, 0th elem is "" when we don't need to
	;; bracket with backticks
	(defq trim-arr (if (= "" (first arr)) (rest arr) arr))
	(str-cat-list delim (append (list (str "``" (first trim-arr) "``")) (rest trim-arr)))))

(defn doc-str-to-md-row (doc-map) (progn
	(defq row (list))
	(defq doc-namespace (hash-get doc-map :namespace))
	(defq doc-type (hash-get doc-map :type))
	(defq doc-usage (hash-get doc-map :usage))
	(defq doc-example (hash-get doc-map :example))
	(append! row
		(progn
		(defq form (hash-get doc-map :form))
		(str "<a id=\"" doc-namespace "__" form  "\" class=\"anchor\" aria-hidden=\"true\" href=\"#sl-sh-form-documentation\">`" form "`</a>")))
	(append! row doc-type)
	(append! row doc-namespace)
	(append! row (format-first-line-as-code doc-usage "<br>"))
	(when (not (= doc-example ""))
	  (append! row (str "``" doc-example "``")))
	;;(append! row (hash-get doc-map :example))
	row))

(defn make-doc-map-md-row (doc-str)
	(make-doc-md-row (doc-str-to-md-row doc-str)))

;; TODO might want user docs under certain circumstances. or only user, might
;; want to the list of syms md files are generated for very flexible
(defn list-of-all-slsh-syms () (progn
	(defq sym-list (ns-symbols 'root))
	(for a-ns (filter (fn (x) (and (not (= x "root")) (not (= x "user")))) (ns-list)) (progn
		(append! sym-list (eval (to-symbol (str a-ns "::*ns-exports*"))))))
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
			(not (= x 'export))
			(not (= x 'args))))
		(qsort sym-list))))

(defq docstrings-map (parse-docstrings-for-syms (list-of-all-slsh-syms)))
;;(defq docstrings-map (parse-docstrings-for-syms (append '() (first (list-of-all-slsh-syms)))))

(defmacro maketables (key docstrings)
	(eval `(write-md-table ,key
			(reduce
				(macro (existing new-item) (append ,existing ((make-doc-map-md-row ,new-item))))
				(join-md-rows
					(make-str-md-row  "form" "type" "namespace" "usage" "example")
					(make-str-md-row  "----" "----" "----" "----" "----"))
					docstrings)
			index-file)))

(for key (qsort (hash-keys docstrings-map)) (progn
	(defq docstrings (hash-get docstrings-map key))
	;;TODO this should be a macro that evals
	;; literally all this list and quote stuff is what macros are for...
	(defq make-tables (list 'write-md-table 'key
			(reduce
				(macro (existing new-item) `(append ,existing (list (list 'make-doc-map-md-row ,new-item))))
				(list 'join-md-rows
					(list 'make-str-md-row  "form" "type" "namespace" "usage" "example")
					(list 'make-str-md-row  "----" "----" "----" "----" "----"))
					docstrings)
			index-file))
	(eval make-tables)))

;;TODO formatting issues
;; - too many <br>s in certain sections;
;; - why does my docstrings-map have " " as a key?
;;		(println "length of parsing: " (hash-keys docstrings-map))
;;		(println "length of parsing: " (hash-get docstrings-map " "))
;; - tables have extra columns, miiight go away when we switch from tables?
;; - make pre push hook: https://victorafanasev.info/tech/deploy-jekyll-build-to-github-pages-using-git-pre-push-hook
