#!/bin/sl-sh

(core::ns-import 'core)
(ns-import 'shell)

(defn make-md-row (&rest args) (progn
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

(defn gen-md-table (table file-name) (progn
	(defq file (open file-name :write))
	(for row table (progn
		(println "row: " row)
		(defq line (apply str row))
		(write-line file line)))
	(close file)
	file-name))

(defq has-no-doc " ")

(defn get-doc-section-if-exists (key idx docstring) (progn
	(defq full-key (str key ":"))
	(if (str-contains full-key docstring)
		(if (= key "Section")
			(str-replace (str-trim (vec-nth idx (str-split full-key docstring))) "\n" "")
			(str-replace (str-trim (vec-nth idx (str-split full-key docstring))) "\n" "<br>"))
		has-no-doc)))

(defn get-mid-doc-section (sym key second-key docstring required) (progn
	(defq type-doc (get-doc-section-if-exists key 1 docstring))
	(if (= type-doc has-no-doc)
		(if required
			(err (str ("Every docstring must have: " key ", but " sym " does not.")))
			:none)
		(get-doc-section-if-exists second-key 0 type-doc))))

(defn get-example-doc-section (key docstring)
	(get-doc-section-if-exists key 1 docstring))

;; TODO maybe write a function that verifies order is correct in the docstrings?
;; to prevent future headaches?
(defn parse-doc (sym) (progn
	(defq docstring (doc sym))
	(defq doc-map (make-hash))
	(hash-set! doc-map :form (if (= sym '|) (str \ sym) (str sym)))
	(hash-set! doc-map :type (get-mid-doc-section sym "Type" "Namespace" docstring #t))
	(hash-set! doc-map :namespace (get-mid-doc-section sym "Namespace" "Usage" docstring #t))
	(hash-set! doc-map :usage (get-mid-doc-section sym "Usage" "Section" docstring #t))
	(hash-set! doc-map :section (get-mid-doc-section sym "Section" "Example" docstring nil))
	(hash-set! doc-map :example (get-example-doc-section "Example" docstring))
	doc-map))

(defn doc-str-to-md-row (to-doc) (progn
	(defq doc-map (parse-doc to-doc))
	(defq row (list))
	(append! row (hash-get doc-map :form))
	(append! row (hash-get doc-map :type))
	(append! row (hash-get doc-map :namespace))
	(append! row (hash-get doc-map :usage))
	;; TODO user typed explanation should be in it's own non-usage section
	;;(append! row (hash-get doc-map :explanation))
	(append! row (hash-get doc-map :example))
	row))

(defq file-name "sample-wiki.md")
(out> file-name "")

(defmacro gen-md-table-for (namespace) (progn
	(println "namespace " namespace)
	(defq md-row-lists (list))
	`(for sym (ns-symbols (to-symbol ,namespace))
		(append! md-row-lists (make-doc-md-row (doc-str-to-md-row sym))))
	md-row-lists))

#| (gen-md-table
	(join-md-rows
		(make-md-row "form" "type" "namespace" "usage" "example")
		(make-md-row "----" "----" "----" "----" "----")
		(make-doc-md-row (doc-str-to-md-row (first (ns-symbols 'root)))))
		;;(gen-md-table-for 'root)
	file-name) |#

(defmacro make-things (things)
	`(for l (ns-symbols ,things) (join l ,things)))

(println (str "my row " (length (doc-str-to-md-row 'doc))))
(println (str "my row " (make-md-row (doc-str-to-md-row 'doc))))
(println (str "my-macro " (expand-macro-all (make-things "a"))))

;;(cat file-name)
