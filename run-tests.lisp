#!/bin/sl-sh

(core::ns-import 'core)
(ns-import 'shell)
(load "tests/test.lisp")
(ns-import 'test)

(defq tests-dir "tests")

(defn all-items-by-whitespace (producer)
	(str-trim (str (| (producer) (tr "\n" " ") (tr -s ":blank:")))))

(defq test-list
	(reduce
		(fn (lst filename) (append! lst (fn () (load (str tests-dir "/" filename)))))
		'()
		(filter
		(fn (filename) (not (= filename "test.lisp")))
			(str-split " " (all-items-by-whitespace (fn () (ls tests-dir)))))))

(defn print-test-output (outfile)
	(progn
		(defq output (str (cat outfile)))
		(when (not (str-empty? output))
			(println (str
				shell::*fg-black* shell::*bg-white*
				(str-trim output)
				shell::*fg-default* shell::*bg-default*
					 )))))

(defn report-pretty-printer (pass test-name)
	(if pass
		(println
			(str shell::*fg-black* shell::*bg-green*
			"PASS:"
			shell::*fg-default* shell::*bg-default*
			" " test-name))
		(println
			(str shell::*fg-black* shell::*bg-red*
			"FAIL:"
			shell::*fg-default* shell::*bg-default*
			" " test-name))))


;; use internal state of dyn, if it gets a non zero error code
;; we know the test failed.

(defn report-test-results (tests)
(progn
	(defq exit-status #t)
	(dyn 'exit (fn (x) (progn (println (str "mm: " (line-no))) (when (not (= x "0")) (setq exit-status nil)) x))
	(progn
		(defq fst (first tests))
		(when fst
			(progn
				(progn
					;; TODO only make 1 outfile
					(defq outfile (str (mktemp "/tmp/sl-sh.test-log.XXXXXXXXX")))
					(out-err> outfile (fst))
					(report-pretty-printer exit-status fst)
					(print-test-output outfile)
					(rm outfile)
					(recur (rest tests)))))))))

(report-test-results test-list)
