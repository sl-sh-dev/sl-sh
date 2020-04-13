#!/bin/sl-sh

(core::ns-import 'core)
(ns-import 'shell)
(load "tests/test.lisp")
(ns-import 'test)

(defq tests-dir "tests")

(defn all-items-by-whitespace (producer)
	(str-trim (str (| (producer) (tr "\n" " ") (tr -s ":blank:")))))

(defn make-test-list-from-symbols (symbols-list)
  (progn
	(defq test-list '())
	(for sym symbols-list (progn
	(if (not (or (str-starts-with "ns-" (str sym))(= "*ns*" (str sym)))) (progn
		;;(set 'total-syms (+ 1 total-syms))
		(if (str-contains "Example:" (doc (to-symbol (str "root::" sym))))
			(progn
				;;(set 'total-tests (+ 1 total-tests))
				;;(print sym " ")
				(defq test-set-item (make-hash))
				(hash-set! test-set-item :name (str sym))
				(hash-set! test-set-item :load-fcn (fn () (test::run-example (to-symbol (str "root::" sym)))))
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

(defmacro inc (item)
	`(setq ,item (+ ,item 1)))

;; use internal state of dyn, if it gets a non zero error code
;; we know the test failed.
(defn report-test-results (tests)
(progn
	(defq exit-status #t)
	(defq total 0)
	(defq passed 0)
	(defq failed 0)
	(dyn 'exit (fn (x) (progn
				(setq total (+ 1 total))
				(if (not (= x "0"))
					(progn
						(setq exit-status nil))
						(inc passed)
				(inc failed))
				x))
	(progn
	(defq fst (first tests))
	(when fst
		(progn
			(progn
				((hash-get fst :load-fcn))
				(report-pretty-printer exit-status (hash-get fst :name))
				(recur (rest tests)))))))))

				;;(hash-set! test-report :total total)
				;;(hash-set! test-report :passed passed)
				;;(hash-set! test-report :failed failed)

;;TODO handle :no-test form keyword.
(defq final-test-report '())

(printer "Tests from test directory")

;;(defq file-test-report (make-hash))
;;(hash-set! file-test-report :name "tests")
;;(hash-set! file-test-report :report (make-hash))
;;(report-test-results file-test-list (hash-get file-test-report :report))
(report-test-results file-test-list)
;;(append! final-test-report file-test-report)

;;TODO de-dupe this pattern
(printer "Tests from root namespace")
(defq root-list (qsort (ns-symbols 'root)))
(defq root-list (make-test-list-from-symbols root-list))
;;(defq root-test-report (make-hash))
;;(hash-set! root-test-report :name "root namespace tests")
;;(hash-set! root-test-report :report (make-hash))
;;(report-test-results root-list (hash-get root-test-report :report))
(report-test-results root-list)
;;(append! final-test-report file-test-report)

;;(println final-test-report)
