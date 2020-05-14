#!/usr/bin/env sl-sh

;; "load" calls go above here but below interpreter directive.
(if (ns-exists? 'mkpost) (ns-enter 'mkpost) (ns-create 'mkpost))
(core::ns-import 'core)
(ns-import 'shell) ;; imports from load calls & body below

(defn -make-jekyll-post-file
	"execute in docs dir, create a jekyll style post. pass in a destination
	directory, title of post, the header, for the post, and optionally,
	categories.
	Section: post"
	(post-dir title header &rest categories) (progn
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
	(defq post (open (str post-dir "/evalable-" date "-" (str-replace title " " "-") ".md") :create :append))
	(write-line post post-body)
	(close post)))

(defmacro make-draft
	"execute in docs dir, create a new draft. pass in title of post, the header,
	for the post, and optionally, categories.
	Section: post"
	(title header &rest categories)
	`(-make-jekyll-post-file "_posts" ,title ,header ,@categories))

(defmacro make-post
	"execute in docs dir, create a new post. pass in title of post, the header,
	for the post, and optionally, categories.
	Section: post"
	(title header &rest categories)
	`(-make-jekyll-post-file "_posts" ,title ,header ,@categories))

(defq begin-comment "{% comment %}")
(defq end-comment "{% endcomment %}")
(defq code-block-delim "```")

(defn -get-directive-hash-map
	"By convention sl-sh hashmaps in lisp strings exit betwen liquid templating
	comment blocks located on the same line. these hash maps are directives that
	aid in transforming an evalable md file into an evaled md file.
	also, continue to write the line in the evalable md file into the
	evaled md file.
	Section: post"
	(src-file dest-file) (block file-read
	(defq line (read-line src-file))
	(when (nil? line)
		(return-from file-read nil))
	(progn
		(write-string dest-file line)
		(if (and
			(str-contains begin-comment (line))
			(str-contains end-comment (line)))
		(progn
			(defq directive-metadata (vec-nth 1 (str-split begin-comment (vec-nth 0 (str-split end-comment line)))))
			(eval (read directive-metadata))
			(hash-set! do :trigger-line line)
			do)
		(progn
			(recur src-file dest-file))))))

(defn -read-in-code-block
"read in code block from src-file into the code-snippets map.  also,
continue to write the lines in the evalable md file into the evaled
md file.
Section: post"
	(src-file dest-file directive-map code-snippets) (progn
	(defq line (str-trim (read-line src-file)))
	(write-line dest-file line)
	(println "codeblockstart::: " line)
	(defq contents (list))
	(when (not (= line code-block-delim))
		(err "First line after :entrypoint or :lib directive must be ```"))
	;; read code block into list of strings, store list in directive map as
	;; contents and return.
	(loop (src-file contents) (src-file contents) (progn
		(setq line (read-line src-file))
		(write-string dest-file line)
		(setq line (str-trim line))
		(println "code: " line)
		(if (= line code-block-delim)
			contents
			(recur src-file (append! contents line)))))
	(hash-set! directive-map :contents contents)
	(hash-set! code-snippets (hash-get directive-map :name) directive-map)
	directive-map))

;;TODO better name
(defn -eval-file (src-file dest-file directive-map code-snippets) (progn
	(defq temp-dir (str-replace (str (mktemp -d)) "\n" ""))
	(defq entrypoint nil)
	;; write all the files to a temp directory so the entrypoint(s) can be
	;; evaled
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
	;; TODO is the eval needed
	;; eval executable file and write output to temp-out
	(defq return-value (error-or-ok (out-err> temp-out (eval (entrypoint)))))
	;; write output in temp-out to the dest-file
	(popd)
	(loop (input-file) ((open temp-out :read)) (progn
			(defq line (read-line input-file))
			(when (not (nil? line)) (progn
				(write-string dest-file (str ";; " line))
				(recur input-file)))))
	(write-line dest-file (str "==> " return-value))
	(println "output located: " temp-out)))

(defn -eval-post
	"enumerate different directive and expectations
	Section: scripting"
	(src-file dest-file code-snippets) (progn
			(defq directive-map (-get-directive-hash-map src-file dest-file))
			(when (not (nil? directive-map)) (progn
				(match (hash-get directive-map :type)
					;; :entrypoint and :lib directives indicate that directly
					;; below is a git flavored markdown (gfm) code block that
					;; needs to be read into the code-snippets hashmap. The code
					;; snippets hashmap is where all :entrypoint and :lib code
					;; is stored.
					(:entrypoint (-read-in-code-block src-file dest-file directive-map code-snippets))
					(:lib (-read-in-code-block src-file dest-file directive-map code-snippets))
					(:eval (-eval-file src-file dest-file directive-map code-snippets))
					(nil (err "Unknown hash map found")))
				(recur src-file dest-file code-snippets)))))

(defn eval-post
	"enumerate different directive and expectations
	Section: scripting"
	(evalable-post-file-name evaled-post-file-name) (progn
		(defq code-snippets (make-hash))
		(defq src-file (open evalable-post-file-name :read))
		(defq dest-file (open evaled-post-file-name :create :truncate))
		(-eval-post src-file dest-file code-snippets)
		(close src-file)
		(close dest-file)))

(ns-auto-export 'mkpost) ;; export any ns symbols that should be importable
(ns-pop) ;; must be after body
