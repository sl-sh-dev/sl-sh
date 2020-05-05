(load "parse-docstrings.lisp")
(if (ns-exists? 'docmd) (ns-enter 'docmd) (ns-create 'docmd))

(core::ns-import 'core)
(ns-import 'shell)
(ns-import 'docparse)

(defn create-header (index-file) (progn
	(defq new-file (open index-file :create :truncate))
	(write-string new-file "---
layout: default
title: Sl-sh form documentation
---

# Sl-sh

")
	(close new-file)))

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

(defn make-md-link-able (link-display-text link)
	(str "[" link-display-text "](" link ")"))

(defn write-heading (heading file-name) (progn
	(defq file (open file-name :append))
	(write-line file "")
	(write-line file (str "## " heading))
	(write-line file "")
	(close file)
	file-name))

(defn get-anchor-link-id (doc-map) (progn
	(defq doc-form (hash-get doc-map :form))
	(defq doc-namespace (hash-get doc-map :namespace))
	(str doc-form "-" doc-namespace)))

(defn table-of-contents (key docstrings file-name) (progn
	(defq file (open file-name :append))
	(defq name (human-readable-name key))
	(write-line file (str "### "
		(create-anchor (str name "-contents" ))
		(make-md-link-able name (str "#" name "-body"))))
	(write-line file "")
	(write-line file "")
	(for doc-map docstrings (progn
		(defq form (hash-get doc-map :form))
		(defq doc-namespace (hash-get doc-map :namespace))
		(write-string file
			(str
				(create-anchor (str (get-anchor-link-id doc-map) "-contents"))
				(make-md-link-able (str "``" form "``") (str "#" (get-anchor-link-id doc-map) "-body"))
				", "
				))))
	(write-line file "")
	(close file)
	file-name))

(defn doc-structure (file-name) (progn
	(defq file (open file-name :append))
	(write-line file "")
	(write-line file "")
	(write-line file
		(str "| <b>form name</b> | <b>type</b> (see: "
			 (make-md-link-able (human-readable-name "type") (str "#" (human-readable-name "type") "-contents"))
			 ") |"))
	(write-line file "| <b>namespace</b> (fully qualified names are of format namespace::symbol) | <b>usage</b> |")
	(write-line file "")
	(write-line file "```")
	(write-line file "example code if exists")
	(write-line file "```")
	(close file)
	file-name))

(defn format-first-line-as-code (text-slice delim)
	(if (or (nil? text-slice) (str-empty? (str-trim text-slice)))
	text-slice
	(progn
	(defq arr (str-split delim text-slice))
	;; if first char in str is delim, 0th elem is "" when we don't need to
	;; bracket with backticks
	(defq trim-arr (if (= "" (first arr)) (rest arr) arr))
	(str-cat-list delim (append (list (str "``" (first trim-arr) "``")) (rest trim-arr))))))

(defn sanitize-for-md-row (to-santiize)
		(str-replace to-santiize "|" "\|"))

(defn write-md-table (key docstrings file-name) (progn
	(defq file (open file-name :append))
	(defq name (human-readable-name key))
	(write-line file (str "### "
				(create-anchor (str name "-body" ))
				(make-md-link-able name (str "#" name "-contents"))))
	(for doc-map docstrings (progn
		(defq doc-form (sanitize-for-md-row (hash-get doc-map :form)))
		(defq doc-namespace (sanitize-for-md-row (hash-get doc-map :namespace)))
		(defq doc-type (sanitize-for-md-row (hash-get doc-map :type)))
		(defq doc-usage (str-replace (sanitize-for-md-row (hash-get doc-map :usage)) "\n" "<br>"))
		(defq doc-example (hash-get doc-map :example))
		(write-line file "")
		(write-line file "")
		(write-line file
			(str "| "
					(create-anchor (str (get-anchor-link-id doc-map) "-body"))
					(make-md-link-able (str "``" doc-form "``")
					(str "#" (get-anchor-link-id doc-map) "-contents"))
				" | " doc-type " |"))
		(write-line file 
			(str "| " doc-namespace "::" doc-form
				 " | " (format-first-line-as-code doc-usage "<br>") " |"))
		(write-line file "")
		(when (not (nil? doc-example))
		  (progn
			(write-line file "```")
				(for line (str-split "\n" doc-example) (write-line file line))
			(write-line file "```")))))
	(close file)
	file-name))

(defn make-md-file
	"meow meow meow"
	(index-file sym-list) (progn
	(defq docstrings-map (parse-docstrings-for-syms sym-list))
	(create-header index-file)
	;; explain format
	(write-heading "Documentation structure for each form" index-file)
	(doc-structure index-file)
	;; generate table of contents
	(write-heading "Table of Contents" index-file)
	(for key (qsort (hash-keys docstrings-map)) (progn
		(defq docstrings (hash-get docstrings-map key))
		(table-of-contents key docstrings index-file)))
	;; generate markdown body
	(write-heading "Documentation" index-file)
	(for key (qsort (hash-keys docstrings-map)) (progn
		(defq docstrings (hash-get docstrings-map key))
		(write-md-table  key docstrings index-file)))))

(ns-export '(make-md-file))
