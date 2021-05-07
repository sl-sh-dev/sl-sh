(load "docstruct.lisp")

(ns-push 'docmd)

(ns-import 'iterator)
(ns-import 'shell)
(ns-import 'docstruct)

(defn create-header (index-file)
	(var new-file (open index-file :create :truncate))
	(write-string new-file "---
title: Sl-sh Standard Library
tags: [documentation]
keywords: forms, sl-sh, examples, api, standard library
last_updated: March 1, 2021
sidebar: mydoc_sidebar
permalink: mydoc_api.html
toc: false
---

# Sl-sh

")
	(close new-file))

(defn section-metadata (key attr)
	;; TODO why does this need to be stringified?
	(var idx (match attr
			(:name 0)
			(:description 1)
			(nil (err "Unknown attribute of table heading."))))
	(match (str key)
		("sequence" (vec-nth '#("Sequence forms"
"These macros will work on either a vector or a pair made into a proper list
(cons list).  Use these in preference to the vector/list specific versions when
possible (i.e. first vs car).
NOTE: list on this table can be a vector or a list.") idx))
		("char" (vec-nth '#("Char forms" nil) idx))
		("conditional" (vec-nth '#("Conditional forms" nil) idx))
		("stats" (vec-nth '#("Stats forms" nil) idx))
		("random" (vec-nth '#("Random forms" nil) idx))
		("struct" (vec-nth '#("Struct forms" nil) idx))
		("core" (vec-nth '#("Core forms" nil) idx))
		("file" (vec-nth '#("File forms"
" Options to open, one or more of these can be added to open after the filename.
A file can only be opened for reading or writing (read is default).

Option | Description
-------|-----------
:read | Open file for reading, this is the default.
:write | Open file for writing.
:append | Open file for writing and append new data to end.
:truncate | Open file for write and delete all existing data.
:create | Create the file if it does not exist and open for writing.
:create-new | Create if does not exist, error if it does and open for writing.
:on-error-nil | If open has an error then return nil instead of producing an error.

Notes on closing.  Files will close when they go out of scope.  Using close will
cause a reference to a file to be marked close (removes that reference).  If
there are more then one references to a file it will not actually close until
all are released.  Close will also flush the file even if it is not the final
reference.  If a reference to a file is captured in a closure that can also keep
it open (closures currently capture the entire scope not just used symbols).") idx))
		("hashmap" (vec-nth '#("Hashmap forms" nil) idx))
		("scripting" (vec-nth '#("Scripting forms" nil) idx))
		("math" (vec-nth '#("Math forms" nil) idx))
		("namespace" (vec-nth '#("Namespace forms" nil) idx))
		("globals" (vec-nth '#("Global symbols" "In sl-sh global symbols (made
by 'def) are wrapped in *earmuffs* like in common lisp. Some of these symbols
contain information used by the standard library and may be useful to
end users, while others are intended for use in scripting.") idx))
		("pair" (vec-nth '#("Pair forms"
"Operations on the 'Pair' type (aka Cons Cell) that can be used to create
traditional Lisp list structures. These are the default list structure and
are produced with bare parentheses in code. These lists can also be created by
building them up with joins or with the list form.") idx))
		("shell" (vec-nth '#("Shell forms"
"Forms to do shell operations like file tests, pipes, redirects, etc.") idx))
		("string" (vec-nth '#("String forms" nil) idx))
		("type" (vec-nth '#("Type forms"
"These forms provide information/tests about an objects underlying type.") idx))
		("vector" (vec-nth '#("Vector forms"
"Forms ending in '!' are destructive and change the underlying vector, other forms
do not make changes to the the provided vector.  They are usable in place of a
list for purposes of lambda calls, parameters, etc (they work the same as a list
made from pairs but are vectors not linked lists).  Use #() to declare them in
code (i.e. '#(1 2 3) or #(+ 1 2)).") idx))
		(":uncategorized" (vec-nth '#("Uncategorized forms" nil) idx))
        (nil (if (= idx 0) (str key " forms") ""))))

(defn create-anchor (id)
	(str "<a id=\"" id "\" class=\"anchor\" aria-hidden=\"true\" href=\"#sl-sh-form-documentation\"></a>"))

(defn make-md-link-able (link-display-text link)
	(str "[" link-display-text "](" link ")"))

(defn write-heading (heading file-name)
	(var file (open file-name :append))
	(write-line file "")
	(write-line file (str "## " heading))
	(write-line file "")
	(close file)
	file-name)

(defn write-version (file-name)
	(var file (open file-name :append))
	(write-line file "")
	(write-line file (str "version: " (version)))
	(write-line file "")
	(close file)
	file-name)

(defn get-anchor-link-id (doc-struct)
	(var doc-form (do
		(var form (doc-struct :name))
		(if (= "\|" form) "pipe-shorthand" form)))
	(var doc-namespace (doc-struct :namespace-only))
	(str doc-namespace "::" doc-form))

(defn table-of-contents (key docstrings file-name)
	(var file (open file-name :append))
	(var name (section-metadata key :name))
	(write-line file (str "### "
		(create-anchor (str name "-contents" ))
		(make-md-link-able name (str "#" name "-body"))))
	(write-line file "")
	(write-line file "")
	(var is-first #t)
	(for doc-struct in docstrings (do
		(if is-first
			(set! is-first nil)
			(write-string file ", "))
		(var doc-form (do
			(var form (doc-struct :name))
			(if (= "\|" form) "|" form)))
		(var doc-namespace (doc-struct :namespace))
		(write-string file
			(str
				(create-anchor (str (get-anchor-link-id doc-struct) "-contents"))
				(make-md-link-able (str "``" doc-form "``") (str "#" (get-anchor-link-id doc-struct)))))))
	(write-line file "")
	(close file)
	file-name)

(defn doc-structure (file-name)
	(var file (open file-name :append))
	(write-line file "")
	(write-line file "")
	(write-line file
		(str "| <b>form name</b> | <b>type</b> (see: "
			 (make-md-link-able (section-metadata "type" :name) (str "#" (section-metadata "type" :name) "-contents"))
			 ") |"))
	(write-line file "| <b>namespace</b> (fully qualified names are of format namespace::symbol) | <b>usage</b> |")
	(write-line file "")
	(write-line file "```")
	(write-line file "example code if exists")
	(write-line file "```")
	(close file)
	file-name)

(defn check-if-pipe-shorthand (item)
	(if (= item "(\| &rest body)") "(| &rest body)" item))

(defn format-first-line-as-code (text-slice delim)
	(if (or (nil? text-slice) (str-empty? (str-trim text-slice)))
	text-slice
	(do
	(var arr (str-split delim text-slice))
	;; if first char in str is delim, 0th elem is "" when we don't need to
	;; bracket with backticks
	(var trim-arr (if (= "" (first arr)) (rest arr) arr))
	(str-cat-list delim (collect-vec (append (list (str "``" (check-if-pipe-shorthand (first trim-arr)) "``")) (rest trim-arr)))))))

(defn sanitize-for-md-row (to-sanitize)
		(str-replace to-sanitize "|" "\|"))

(defn write-doc-struct-to-file (doc-struct file)
		(var doc-form (do
			(var form (doc-struct :name))
			(if (= "\|" form) "|" (sanitize-for-md-row form))))
		(var doc-namespace (sanitize-for-md-row (doc-struct :namespace-only)))
		(var doc-type (sanitize-for-md-row (doc-struct :sl-sh-type)))
		(var doc-usage (str-replace (sanitize-for-md-row (doc-struct :usage)) "\n" "<br>"))
		(var doc-desc (doc-struct :description))
		(var doc-example (doc-struct :example))
		(write-line file "")
		(write-line file "")
		(write-line file
			(str "| "
					(create-anchor (str (get-anchor-link-id doc-struct)))
					(make-md-link-able (str "``" doc-form "``")
					(str "#" (get-anchor-link-id doc-struct) "-contents"))
				" | " doc-type " |"))
		(when (not (nil? doc-namespace))
          (write-line file
			(str "| ``" doc-namespace "::" doc-form
				 "`` | " (format-first-line-as-code doc-usage "<br>") " |")))
		(write-line file "")
        (when (not (nil? doc-desc))
          (do (write-line file (str "<span style=\"padding-left: 5px\">" doc-desc "</span>"))))
		(if (not (nil? doc-example))
			(do
				(write-line file "<details style=\"cursor: pointer; padding-bottom: 15px; padding-left: 10px\">")
				(write-line file "<code>")
				(for line in (str-split "\n" doc-example) (write-line file (str line "<br>")))
				(write-line file "</code>")
				(write-line file "</details>")
				(write-line file "<br>"))
			(write-line file "<br>")))

(defn write-md-table (key docstrings file-name)
	(var file (open file-name :append))
	(var name (section-metadata key :name))
	(write-line file (str "### "
				(create-anchor (str name "-body"))
				(make-md-link-able name (str "#" name "-contents"))))
	(write-line file (do
		 (var data (section-metadata key :description))
		 (if (nil? data)
			""
			data)))
	(for doc-struct in docstrings (write-doc-struct-to-file doc-struct file))
	(close file)
	file-name)

(defn gen-std-lib-md-with-sections
"Create markdown file at given path and populate with documentation from
provided hash-map of section-key to list of doc struct pairs. The structure
was chosen becuase the markdown file is grouped by documentation sections.
"
    (index-file docs-by-section)
    (create-header index-file)
    (write-heading "Documentation structure for each form" index-file)
    (doc-structure index-file)
	;; generate table of contents
    (write-heading "Table of Contents" index-file)
	(for key in (qsort (hash-keys docs-by-section)) (do
		(var docstrings (hash-get docs-by-section key))
		(table-of-contents key docstrings index-file)))
	;; generate markdown body
	(write-heading "Documentation" index-file)
	(for key in (qsort (hash-keys docs-by-section)) (do
		(var docstrings (hash-get docs-by-section key))
		(write-md-table key docstrings index-file)))
	(write-version index-file)
	(var uncat-syms (hash-get docs-by-section :uncategorized))
	(when (not (empty-seq? uncat-syms)) (do
		(println "Found :uncategorized symbols: ")
		(for symbol in uncat-syms (println "symbol: " symbol))
		nil))
	 #t)

(defn gen-std-lib-md-file
"Create markdown file at given path and populate with documentation from
provided list of symbols.
"
    (index-file sym-list)
    (var docs-by-section (make-hash))
    (for doc-struct in (map make-doc-struct sym-list)
          (let ((section-key (doc-struct :section-key)))
            (if (hash-haskey docs-by-section section-key)
                (append-to! (hash-get docs-by-section section-key) doc-struct)
            (hash-set! docs-by-section section-key (list doc-struct)))))
    (gen-std-lib-md-with-sections index-file docs-by-section))

(ns-export '(gen-std-lib-md-file gen-std-lib-md-with-sections))

(ns-pop)
