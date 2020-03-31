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
		(fn (lst filename) (append! lst (progn
										(defq name (str tests-dir "/" filename))
										(defq load-fcn (fn () (load filepath)))
										(defq test-hash (make-hash))
										(hash-set! test-hash :name name)
										(hash-set! test-hash :load-fcn load-fcn)
										test-hash)))
		'()
		(filter
		(fn (filename) (not (= filename "test.lisp")))
			(str-split " " (all-items-by-whitespace (fn () (ls tests-dir)))))))

(defn print-until (reader printer)
	(progn
	(defq line (read-line reader))
	(when line
		(progn
			(printer line)
			(recur reader printer)))))

(defn print-each-line (file printer)
	(progn
	(defq reader (open file :read))
	(print-until reader printer)))

(defn print-test-output (outfile)
	(progn
	(defq printer (fn (output)
		(println (str
			shell::*fg-black* shell::*bg-white*
			(str-trim output)
			shell::*fg-default* shell::*bg-default*))))
	(print-each-line outfile printer)))

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
	(dyn 'exit (fn (x) (progn
				(echo -n (str ". line number: " (line-no) "."))
				(when (not (= x "0")) (setq exit-status nil))
				x))
	(progn
	(defq fst (first tests))
	(when fst
		(progn
			(progn
				;; TODO only make 1 outfile
				(defq outfile (str (mktemp "/tmp/sl-sh.test-log.XXXXXXXXX")))
				(out-err> outfile (:load-fcn fst))
				(report-pretty-printer exit-status (:name fst))
				(print-test-output outfile)
				(rm outfile)
				(recur (rest tests)))))))))

(report-test-results test-list)
