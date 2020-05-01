#!/bin/sl-sh

(core::ns-import 'core)
(ns-import 'shell)
(error-stack-on)

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

(defn write-md-table (table file-name) (progn
	(defq file (open file-name :create))
	(for row table (progn
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
			(err (str "Every docstring must have: " key ", but " sym " does not."))
			:none)
		(get-doc-section-if-exists second-key 0 type-doc))))

(defn get-example-doc-section (key docstring)
	(get-doc-section-if-exists key 1 docstring))

;; TODO maybe write a function that verifies order is correct in the docstrings?
;; to prevent future headaches?
(defn parse-doc (sym) (progn
	(defq docstring (doc sym))
	(defq doc-map (make-hash))
	(hash-set! doc-map :form (if (= sym '|) (str '\ sym) (str sym)))
	(hash-set! doc-map :type (get-mid-doc-section sym "Type" "Namespace" docstring #t))
	(hash-set! doc-map :namespace (get-mid-doc-section sym "Namespace" "Usage" docstring #t))
	(hash-set! doc-map :usage (get-mid-doc-section sym "Usage" "Section" docstring #t))
	(hash-set! doc-map :section (get-mid-doc-section sym "Section" "Example" docstring nil))
	(hash-set! doc-map :example (get-example-doc-section "Example" docstring))
	doc-map))

(defn doc-str-to-md-row (doc-map) (progn
	(defq row (list))
	(append! row (hash-get doc-map :form))
	(append! row (hash-get doc-map :type))
	(append! row (hash-get doc-map :namespace))
	(append! row (hash-get doc-map :usage))
	;; TODO user typed explanation should be in it's own non-usage section
	;;(append! row (hash-get doc-map :explanation))
	(append! row (hash-get doc-map :example))
	row))

(defn make-doc-map-md-row (doc-str)
	(make-doc-md-row (doc-str-to-md-row doc-str)))

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
			(not (= x 'args))))
		sym-list)))

(defn parse-all-docstrings () (progn
	(defq docstring-sections (make-hash))
	(defq section-builder (fn (syms)
		(when (not (empty-seq? syms))
			(progn
				(defq doc-map (parse-doc (first syms)))
				(defq section-key (hash-get doc-map :section))
				(if (hash-haskey docstring-sections section-key)
					(append! (hash-get docstring-sections section-key) doc-map)
					(hash-set! docstring-sections section-key (list doc-map)))
				(recur (rest syms))))))
	(section-builder (list-of-all-slsh-syms))
	docstring-sections))

(defq docstrings-map (parse-all-docstrings))
;; TODO why does my docstrings-map have " " as a key?
;;(println "length of parsing: " (hash-keys docstrings-map))
;;(println "length of parsing: " (hash-get docstrings-map " "))

(defn md-file-dir ()
	(if (fs-dir? "md") "md" (progn (mkdir "md") "md")))

(defn get-file (key)
	(defq file (str (md-file-dir) "/_" key ".md")))

(for key (hash-keys docstrings-map) (progn
	(defq docstrings (hash-get docstrings-map key))
	(defq make-tables (list 'write-md-table
			(reduce
				(macro (existing new-item) `(append ,existing (list (list 'make-doc-map-md-row ,new-item))))
				(list 'join-md-rows
					(list 'make-str-md-row  "form" "type" "namespace" "usage" "example")
					(list 'make-str-md-row  "----" "----" "----" "----" "----")) 
					docstrings)
			(get-file key)))
	(eval make-tables)))

