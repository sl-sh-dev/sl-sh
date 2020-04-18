#!/bin/sl-sh

(core::ns-import 'core)
(ns-import 'shell)
(load "tests/test.lisp")
(ns-import 'test)

;;TODO replace get-error with prog-error
(defmacro prog-error (&rest args) `(progn
	(defq ret (get-error ,@args))
	(if (and (vec? ret) (= :error (first ret)))
		(progn
			(defq err-map (make-hash))
			(hash-set! err-map :error (first (rest ret))))
		ret)))

;;TODO remove this error stack on call when "it" becomes an environment variable
(error-stack-on)

(defq tests-dir "tests")

(defn all-items-by-whitespace (producer)
	(str-trim (str (| (producer) (tr "\n" " ") (tr -s ":blank:")))))

(defn make-test-list-from-symbols (symbols-list prepend)
  (progn
	(defq test-list '())
	(for sym symbols-list (progn
	(defq fully-qualified-symbol (to-symbol (str prepend "::" sym)))
	(when (and
			(func? (eval fully-qualified-symbol))
			(not (or
				(str-starts-with "ns-" (str sym))
				(= "*ns*" (str sym))))) (progn
		(defq test-set-item (make-hash))
		(hash-set! test-set-item :name (str sym))
		(if (str-contains "Example:" (doc fully-qualified-symbol))
			(progn
				;; TODO
				;; need to put check b/c reader macro will fail if invalid lisp
				;; use get-error here to catch an error if it loads and report
				;; invalid unit test
				;; or maybe do it... when it' is being eval'd?
				(hash-set! test-set-item :load-fcn (fn () (test::run-example fully-qualified-symbol)))
				(append! test-list test-set-item))
			(progn
				(hash-set! test-set-item :load-fcn :no-test)
				(append! test-list test-set-item))
			)))))
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

;;TODO the hash-set! test-report pattern should be fcnized once this is all working?
(defn report-test-results (tests test-report) (progn
	(defq exit-status #t)
	(dyn 'exit (fn (x) (progn
				(when (not (= x "0"))
					(progn
						(setq exit-status nil)
						(hash-set! test-report :failed (+ 1 (hash-get test-report :failed)))))
					x)) (progn
	(defq fst (first tests))
	(when fst (progn
			(hash-set! test-report :total (+ 1 (hash-get test-report :total)))
			(if (hash-haskey fst :no-test)
				(hash-set! test-report :no-test (+ 1 (hash-get test-report :no-test)))
				(progn
					(defq test-result
						(prog-error
							((hash-get fst :load-fcn))))
					(when (and (hash? test-result) (hash-haskey test-result :error)) (progn
						(setq exit-status (hash-get test-result :error))
						(hash-set! test-report :failed (+ 1 (hash-get test-report :failed)))
						))))
			(report-pretty-printer exit-status (hash-get fst :name))
			(recur (rest tests) test-report)))))))

(defq final-test-report '())

(defn run-tests-for (test-name test-list test-report)
	(progn
		(defq test-data (make-hash))
		(hash-set! test-data :name test-name)
		(hash-set! test-data :total 0)
		(hash-set! test-data :failed 0)
		(hash-set! test-data :no-test 0)
		(report-test-results test-list test-data)
		(append! test-report test-data)))

(printer "Tests from test directory")
(run-tests-for "module tests" file-test-list final-test-report)

(for a-ns (filter (fn (x) (and (not (= x "test")) (not (= x "user")))) (ns-list)) (progn
	(printer (str "Tests from " a-ns " namspace"))
	(defq sym-list (ns-symbols (to-symbol a-ns)))
	(defq sym-list (make-test-list-from-symbols sym-list a-ns))
	(println "count sym list " (length sym-list))
	(run-tests-for (str a-ns " namespace unit tests") sym-list final-test-report)
	))

;;TODO import ns tests from run-root-test.lisp

;;TODO make pretty final report
(printer "Test Summary")
(println final-test-report)
