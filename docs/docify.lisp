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

# Sl-sh

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

(defn create-anchor (id)
	(str "<a id=\"" id "\" class=\"anchor\" aria-hidden=\"true\" href=\"#sl-sh-form-documentation\"></a>"))

(defn write-heading (heading file-name) (progn
	(defq file (open file-name :append))
	(write-line file "")
	(write-line file (str "## " heading))
	(write-line file "")
	(close file)
	file-name))

(defn get-anchor-link-for-doc (doc-map) (progn
	(defq doc-form (hash-get doc-map :form))
	(defq doc-namespace (hash-get doc-map :namespace))
	(str doc-form "-" doc-namespace)))

(defn table-of-contents (key docstrings file-name) (progn
	(defq file (open file-name :append))
	(defq name (human-readable-name key))
	(write-line file (str "### " (create-anchor (str name "-meta" )) "[" name "](#" name "-body)"))
	(write-line file "")
	(write-line file "")
	(for doc-map docstrings (progn
		(defq form (hash-get doc-map :form))
		(defq doc-namespace (hash-get doc-map :namespace))
		(write-string file (str "[``" form "``](#" (get-anchor-link-for-doc doc-map)  "), "))))
	(write-line file "")
	(close file)
	file-name))

(defn doc-structure (file-name) (progn
	(defq file (open file-name :append))
	(write-line file "")
	(write-line file "")
	(write-line file "| <b>form</b> | <b>type</b> |")
	(write-line file "| <b>namespace</b> | <b>usage</b> |")
	(write-line file "")
	(write-line file "```")
	(write-line file "example code if exists")
	(write-line file "```")
	(close file)
	file-name))

(defn format-first-line-as-code (text-slice delim) (progn
	(defq arr (str-split delim text-slice))
	;; if first char in str is delim, 0th elem is "" when we don't need to
	;; bracket with backticks
	(defq trim-arr (if (= "" (first arr)) (rest arr) arr))
	(str-cat-list delim (append (list (str "``" (first trim-arr) "``")) (rest trim-arr)))))

(defn sanitize-for-md-row (to-santiize)
		(str-replace to-santiize "|" "\|"))

(defn write-md-table (key docstrings file-name) (progn
	(defq file (open file-name :append))
	(defq name (human-readable-name key))
	(write-line file (str "### " (create-anchor (str name "-body" )) "[" name "](#" name "-meta)"))
	(for doc-map docstrings (progn
		(defq doc-form (sanitize-for-md-row (hash-get doc-map :form)))
		(defq doc-namespace (sanitize-for-md-row (hash-get doc-map :namespace)))
		(defq doc-type (sanitize-for-md-row (hash-get doc-map :type)))
		(defq doc-usage (str-replace (sanitize-for-md-row (hash-get doc-map :usage)) "\n" "<br>"))
		(defq doc-example (hash-get doc-map :example))
		(write-line file "")
		(write-line file "")
		(write-line file
			(str "| <b>" (create-anchor (get-anchor-link-for-doc doc-map)) "<b>" doc-form "</b> "
				"| " doc-type " |"))
		(write-line file (str "| " doc-namespace " | " (format-first-line-as-code doc-usage "<br>") " |"))
		(write-line file "")
		(when (not (nil? doc-example))
		  (progn
			(write-line file "```")
				(for line (str-split "\n" doc-example) (write-line file line))
			(write-line file "```")))))
	(close file)
	file-name))

(defn doc-str-to-md-row (doc-map) (progn
	(defq row (list))
	(defq doc-namespace (hash-get doc-map :namespace))
	(defq doc-type (hash-get doc-map :type))
	(defq doc-usage (hash-get doc-map :usage))
	(defq doc-example (hash-get doc-map :example))
	(append! row
		(progn
		(defq form (hash-get doc-map :form))
		(str (create-anchor (get-anchor-link-for-doc doc-map))  "``" form "``")))
		;;(str "<a id=\"" doc-namespace "__" form  "\" class=\"anchor\" aria-hidden=\"true\" href=\"#sl-sh-form-documentation\">`" form "`</a>")))
	(append! row doc-type)
	(append! row doc-namespace)
	(append! row (format-first-line-as-code doc-usage "<br>"))
	(when (not (= doc-example ""))
	  (append! row (str "``" doc-example "``")))
	;;(append! row (hash-get doc-map :example))
	row))

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

;;TODO generate table of contents
(write-heading "Table of Contents" index-file)
(for key (qsort (hash-keys docstrings-map)) (progn
	(defq docstrings (hash-get docstrings-map key))
	(table-of-contents key docstrings index-file)))

(write-heading "Documentation structure" index-file)
(doc-structure index-file)

;; generate markdown body
(write-heading "Documentation" index-file)
(for key (qsort (hash-keys docstrings-map)) (progn
	(defq docstrings (hash-get docstrings-map key))
	(write-md-table  key docstrings index-file)))

;;TODO formatting issues
;; - too many <br>s in certain sections;
;; - why does my docstrings-map have " " as a key?
;;		(println "length of parsing: " (hash-keys docstrings-map))
;;		(println "length of parsing: " (hash-get docstrings-map " "))
;; - tables have extra columns, miiight go away when we switch from tables?
;; - make pre push hook: https://victorafanasev.info/tech/deploy-jekyll-build-to-github-pages-using-git-pre-push-hook
