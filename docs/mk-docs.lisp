#!/usr/bin/env sl-sh

(load "docstrings-to-md.lisp")

(if (ns-exists? 'mkdocs) (ns-enter 'mkdocs) (ns-create 'mkdocs))
(core::ns-import 'core)
(ns-import 'shell)

(defn filter-user-undocable-forms (sym-list)
	(filter (fn (x)
		(and (not (= x 'custom-lisp-config))
			(not (= x '*repl-settings*))
			(not (= x '__completion_hook))
			(not (= x '__line_handler))
			(not (= x '__exec_hook))
				(not (= x '__prompt))))
			sym-list))

(defn filter-undocable-forms (sym-list)
	(filter (fn (x)
		(and
			(not (= x '*ns*))
			(not (= x '*repl-settings*))
			(not (= x '*ns-exports*))
			(not (= x 'tok-slsh-form-color))
			(not (= x 'tok-slsh-fcn-color))
			(not (= x 'tok-default-color))
			(not (= x 'tok-sys-command-color))
			(not (= x 'tok-sys-alias-color))
			(not (= x 'tok-string-color))
			(not (= x 'tok-invalid-color))
			(not (= x '*last-status*))
			(not (= x '*repl-settings*))
			(not (= x 'get-doc-list-for))
			(not (= x 'make-md-file))
			(not (= x 'make-md-file-with-docstrings))
			(not (= x '__completion_hook))
			(not (= x '__line_handler))
			(not (= x '__exec_hook))
			(not (= x '__prompt))
			(not (= x 'args))))
		sym-list))

(defn list-of-all-slsh-syms () (progn
	(defq sym-list (ns-symbols 'root))
	(for a-ns (filter (fn (x) (and
						(not (= x "docmd"))
						(not (= x "docparse"))
						(not (= x "docify"))
						(not (= x "root"))
						(not (= x "user")))) (ns-list)) (progn
		(append! sym-list (eval (to-symbol (str a-ns "::*ns-exports*"))))))
	(filter-undocable-forms (qsort sym-list))))

(defn get-doc-list-for
	(target-doc-form)
	(match target-doc-form
			(:single (append '() (last (list-of-all-slsh-syms))))
			(:lang (list-of-all-slsh-syms))))

(defn make-md-file-with-docstrings (index-file docstrings)
	(docmd::make-md-file index-file docstrings))

(defn make-md-file (index-file target-doc-form)
	(docmd::make-md-file
		index-file
		(map! (fn (x) (doc (to-symbol x)))
			  (get-doc-list-for target-doc-form))))

(defn make-post
	"execute in docs dir, create a new post. pass in title of post, the header,
	for the post, and optionally, cateogires.
	Section: scripting"
	(title header &rest categories) (progn
	(defq post-body (str "---
layout: default
title: " title "
categories: [" (if (= 0 (length categories)) "general" (str-cat-list "," categories)) "]
---
# " header "
<hr>
[<-- back to the docs]( {{ site.url }} )
"))
	(defq date (str-trim (str (date +%Y-%m-%d))))
	(defq post (open (str "_posts/" date "-" (str-replace title " " "-") ".md") :create :append))
	(write-line post post-body)
	(close post)))

(defq begin-comment "{% comment %}")
(defq end-comment "{% endcomment %}")
(defq code-block-delim "```")

(defn -get-directive-hash (file) (block file-read
	(defq line (read-line file))
	(when (nil? line)
		(return-from file-read nil))
	(if (and
			(str-contains begin-comment (line))
			(str-contains end-comment (line)))
		(progn
			(defq directive-metadata (vec-nth 1 (str-split begin-comment (vec-nth 0 (str-split end-comment line)))))
			(eval (read directive-metadata))
			(hash-set! do :trigger-line line)
			do)
		(progn
			(recur file)))
))

(defn -read-in-code-block (file directive-map code-snippets) (progn
	(defq line (str-trim (read-line file)))
	(println "line is::: " line)
	(defq contents (list))
	(when (not (= line code-block-delim))
		(err "First line after :entrypoint or :lib directive must be ```"))
	;; read code block into list of strings, store list in directive map as
	;; contents and return.
	(loop (file contents) (file contents) (progn
		(setq line (str-trim (read-line file)))
		(println "line: " line)
		(if (= line code-block-delim)
			contents
			(recur file (append! contents line)))))
	(hash-set! directive-map :contents contents)
	(hash-set! code-snippets (hash-get directive-map :name) directive-map)
	directive-map))

(defn -eval-file (md-file-name directive-map code-snippets) (progn
	(defq temp-dir (str-replace (str (mktemp -d)) "\n" ""))
	(defq entrypoint nil)
	(for file-name (hash-get directive-map :files) (progn
		(defq snippet (hash-get code-snippets file-name))
		(defq target-file-name (str temp-dir "/" file-name))
		(println "target-file-name: " target-file-name)
		(defq target-file (open target-file-name :create :truncate))
		(for line (hash-get snippet :contents) (progn
			(write-line target-file line)))
		(when (= :entrypoint (hash-get snippet :type)) (progn
			(setq entrypoint target-file-name)
			(chmod +x target-file-name)))
		(close target-file)))
	(when (nil? entrypoint) (err "No defined for :type :entrypoint in files found in :files for given :eval directive."))
	(defq temp-out (str temp-dir "/output" ))
	(pushd temp-dir)
	;;TOD) is the eval needed
	(out-err> temp-out (eval (entrypoint)))
	(popd)
	(println "output located: " temp-out)))

(defn eval-post
	"enumerate different directive and expectations
	Section: scripting"
	(file-name) (progn
		(defq code-snippets (make-hash))
		(defq file (open file-name :read))
		(loop (file) (file) (progn
			(defq directive-map (-get-directive-hash file))
			(when (not (nil? directive-map)) (progn
				(match (hash-get directive-map :type)
					(:entrypoint (-read-in-code-block file directive-map code-snippets))
					(:lib (-read-in-code-block file directive-map code-snippets))
					(:eval (-eval-file file-name directive-map code-snippets))
					(nil (err "Unknown hash map found")))
				(recur file)))))))


(ns-auto-export 'mkdocs)
(ns-pop)
