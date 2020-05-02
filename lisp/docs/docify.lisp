#!/bin/sl-sh

(core::ns-import 'core)
(ns-import 'shell)
(load "parse-docstrings.lisp")
(ns-import 'docparse)

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

(defq human-readable-name (make-hash))
(hash-set! human-readable-name :none "Uncategorized forms")
(hash-set! human-readable-name "char" "Char forms")
(hash-set! human-readable-name "condictional" "Conditional forms")
(hash-set! human-readable-name "core" "Core forms")
(hash-set! human-readable-name "file" "File forms")
(hash-set! human-readable-name "hashmap" "Hashmap forms")
(hash-set! human-readable-name "math" "Math forms")
(hash-set! human-readable-name "namespace" "Namespace forms")
(hash-set! human-readable-name "pair" "Pair forms")
(hash-set! human-readable-name "shell" "Shell forms")
(hash-set! human-readable-name "string" "String forms")
(hash-set! human-readable-name "type" "Type forms")
(hash-set! human-readable-name "vector" "Vector forms")
(hash-set! human-readable-name " " "Unknown forms")

(defn write-md-table (key table file-name) (progn
	(defq file (open file-name :append))
	(defq name (hash-get human-readable-name key))
	(write-line file (str "## " (if (not name) "Unknown forms" name)))
	(write-line file "")
	(write-line file "")
	(for row table (progn
		(defq line (apply str row))
		(write-line file line)))
	(write-line file "")
	(close file)
	file-name))

(defn doc-str-to-md-row (doc-map) (progn
	(defq row (list))
	(append! row (hash-get doc-map :form))
	(append! row (hash-get doc-map :type))
	(append! row (hash-get doc-map :namespace))
	(append! row (hash-get doc-map :usage))
	(append! row (hash-get doc-map :example))
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
			(not (= x 'args))))
		(qsort sym-list))))

(defq docstrings-map (parse-docstrings-for-syms (list-of-all-slsh-syms)))

(defq header "---
layout: default
title: Sl-sh form documentation
---

# Sl-sh form documentation

;;TODO need anchor links / table of contents to all forms here
")

(defn get-file (key)
	(progn
	(defq file (str key ".md"))
	(defq new-file (open file :create :truncate))
	(write-string new-file header)
	(close new-file)
	file))

(defq index-file (get-file "index"))

(for key (qsort (hash-keys docstrings-map)) (progn
	(defq docstrings (hash-get docstrings-map key))
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
