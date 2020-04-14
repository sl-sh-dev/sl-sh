#!/bin/sl-sh

(core::ns-import 'core)
(ns-import 'shell)
(load "tests/test.lisp")
(ns-import 'test)

;;TODO remove this error stack on call when "it" becomes an environment variable
(error-stack-on)

(defq tests-dir "tests")

(defn all-items-by-whitespace (producer)
	(str-trim (str (| (producer) (tr "\n" " ") (tr -s ":blank:")))))

(defn make-test-list-from-symbols (symbols-list prepend)
  (progn
	(defq test-list '())
	(for sym symbols-list (progn
	(when (not (or
				(str-starts-with "ns-" (str sym))
				(= "*ns*" (str sym)))) (progn
		(if (str-contains "Example:" (doc (to-symbol (str prepend "::" sym))))
			(progn
				(defq test-set-item (make-hash))
				(hash-set! test-set-item :name (str sym))
				(hash-set! test-set-item :load-fcn (fn () (test::run-example (to-symbol (str prepend "::" sym)))))
				(append! test-list test-set-item))
			(progn
				(defq test-set-item (make-hash))
				(hash-set! test-set-item :name (str sym))
				(hash-set! test-set-item :load-fcn :no-test)))))))
	test-list))

(defq file-test-list
	(reduce
		(fn (lst filename) (append! lst (progn
			(defq name (str tests-dir "/" filename))
			(defq load-fcn (fn () (load name)))
			(defq test-set-item (make-hash))
			(hash-set! test-set-item :name name)
			(hash-set! test-set-item :load-fcn load-fcn)
			test-set-item)))
		'()
		(filter
		(fn (filename) (not (= filename "test.lisp")))
			(str-split " " (all-items-by-whitespace (fn () (ls tests-dir)))))))

(defn printer (output)
	(println (str
		shell::*fg-black* shell::*bg-white*
		(str-trim output)
		shell::*fg-default* shell::*bg-default*)))

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
(defn report-test-results (tests test-report)
(progn
	(defq exit-status #t)
	(dyn 'exit (fn (x) (progn
				(when (not (= x "0"))
					(progn
						(setq exit-status nil)
						(hash-set! test-report :failed (+ 1 (hash-get test-report :failed)))
					))
				x))
	(progn
	(defq fst (first tests))
	(when fst
		(progn
			(hash-set! test-report :total (+ 1 (hash-get test-report :total)))
			((hash-get fst :load-fcn))
			(report-pretty-printer exit-status (hash-get fst :name))
			(recur (rest tests) test-report)))))))

;;TODO handle :no-test form keyword.
(defq final-test-report '())

(defn run-tests-for (test-name test-list test-report)
	(progn
		(defq test-data (make-hash))
		(hash-set! test-data :name test-name)
		(hash-set! test-data :total 0)
		(hash-set! test-data :failed 0)
		(report-test-results test-list test-data)
		(append! test-report test-data)))

(printer "Tests from test directory")
(run-tests-for "module tests" file-test-list final-test-report)

(for a-ns (filter (fn (x) (and (not (= x "test")) (not (= x "user")))) (ns-list)) (progn
	(printer (str "Tests from " a-ns " namspace"))
	(defq sym-list (qsort (ns-symbols (to-symbol a-ns))))
	(defq sym-list (make-test-list-from-symbols sym-list a-ns))
	(run-tests-for (str a-ns " namespace unit tests") sym-list final-test-report)))

;;TODO import ns tests from run-root-test.lisp

;;TODO make pretty final report
(println final-test-report)
