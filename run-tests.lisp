#!./target/debug/sl-sh

(ns-import 'core)
(ns-import 'iterator)
(ns-import 'shell)
(ns-import 'test)

;;TODO sstanfield replace get-error with prog-error
(defmacro prog-error (&rest args) `(progn
	(defq ret (get-error ,@args))
	(if (and (vec? ret) (= :error (first ret)))
		(progn
			(defq err-map (make-hash))
			(hash-set! err-map :error (first (rest ret))))
		ret)))

;;TODO gpwclark remove this error stack on call when "(error-stack-on)" becomes an environment variable
(error-stack-on)

(defq tests-dir "tests")

(defn has-example (docstring)
	(str-contains "Example:" docstring))

(defn exec-str (docstring) (progn
	(defq test (vec-nth 1 (str-split "Example:" docstring)))
	(fn () (eval (str "(progn " test ")")))))

(defn all-items-by-whitespace (producer)
	(str-trim (str (| (producer) (tr "\n" " ") (tr -s ":blank:")))))

(defn make-test-list-from-symbols (symbols-list a-ns) (progn
	(defq test-list (list))
	(for sym in symbols-list (progn
	(defq fully-qualified-symbol (to-symbol (str a-ns "::" sym)))
	(when (and
			(func? (eval fully-qualified-symbol))
			(not (or
				(str-starts-with "ns-" (str sym))
				(= "*ns*" (str sym))))) (progn
		(defq test-set-item (make-hash))
		(hash-set! test-set-item :name (str sym))
		(defq docstring (doc fully-qualified-symbol))
		(if (has-example docstring)
			(progn
				(hash-set! test-set-item :load-fcn (exec-str docstring))
				(append! test-list test-set-item))
			(progn
				(hash-set! test-set-item :load-fcn :no-test)
				(append! test-list test-set-item)))))))
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

(defn report-pretty-printer (result test-name)
	(match result
		(:no-test
		 (println
			(str shell::*fg-black* shell::*bg-yellow*
			"NONE:"
			shell::*fg-default* shell::*bg-default*
			" " test-name)))
		(:passed
		 (println
			(str shell::*fg-black* shell::*bg-green*
			"PASS:"
			shell::*fg-default* shell::*bg-default*
			" " test-name)))
		(:failed
		 (println
			(str shell::*fg-black* shell::*bg-red*
			"FAIL:"
			shell::*fg-default* shell::*bg-default*
			" " test-name)))
        (:error
		 (println
			(str shell::*fg-black* shell::*bg-red*
			"ERR: "
			shell::*fg-default* shell::*bg-default*
			" " test-name)))
		(nil (err (str "Invalid test result status for test name " test-name "\n Error: " result)))))

(defn report-test-results (tests test-report) (progn
	(defq exit-status :passed)
	(dyn 'exit (fn (x) (progn
				(when (not (= x "0"))
					(progn
						(setq exit-status :failed)
						(hash-set! test-report :failed (+ 1 (hash-get test-report :failed)))))
					x)) (progn
	(defq fst (first tests))
	(when fst (progn
		(hash-set! test-report :total (+ 1 (hash-get test-report :total)))
		(if (= :no-test (hash-get fst :load-fcn))
			(progn
				(hash-set! test-report :no-test (+ 1 (hash-get test-report :no-test)))
				(setq exit-status :no-test))
			(progn
				(defq test-result
					(prog-error
						((hash-get fst :load-fcn))))
				(when (and (hash? test-result) (hash-haskey test-result :error)) (progn
					(setq exit-status (hash-get test-result :error))
					(hash-set! test-report :failed (+ 1 (hash-get test-report :failed)))))))
		(report-pretty-printer exit-status (hash-get fst :name))
		(recur (rest tests) test-report)))))))

(defq final-test-report '())

(defn run-tests-for (test-name test-list test-report) (progn
    (defq test-data (make-hash))
    (hash-set! test-data :name test-name)
    (hash-set! test-data :total 0)
    (hash-set! test-data :failed 0)
    (hash-set! test-data :no-test 0)
    (report-test-results test-list test-data)
    (append! test-report test-data)))

(printer "Tests from test directory")
(run-tests-for "module tests" file-test-list final-test-report)

;; run tests for non-root namespaces
(for a-ns in (filter (fn (x) (and (not (= x "root")) (not (= x "test")) (not (= x "user")))) (ns-list)) (progn
	(printer (str "Tests from " a-ns))
	(defq sym-list (qsort (eval (to-symbol (str a-ns "::*ns-exports*")))))
	(defq sym-list (make-test-list-from-symbols sym-list a-ns))
	(run-tests-for (str a-ns " unit tests") sym-list final-test-report)))

;; run tests for root namespaces
(progn
(printer (str "Tests from root"))
(defq sym-list (qsort (ns-symbols 'root)))
(defq sym-list (make-test-list-from-symbols sym-list "root"))
(run-tests-for (str "root unit tests") sym-list final-test-report))

;; run tests for root namespace special namespace tests (ns cmd can not be run
;; inside a fcn

(defq ns-test-set-item (make-hash))
(hash-set! ns-test-set-item :name "namespace unit tests")
(hash-set! ns-test-set-item :total 0)
(hash-set! ns-test-set-item :failed 0)
(hash-set! ns-test-set-item :passed 0)
(hash-set! ns-test-set-item :no-test 0)

(printer "Tests from namespace")

(progn
	(hash-set! ns-test-set-item :total (+ 1 (hash-get ns-test-set-item :total)))
	(if (test::run-ns-example 'root::*ns*)
		(progn
			(hash-set! ns-test-set-item :passed (+ 1 (hash-get ns-test-set-item :passed)))
			(report-pretty-printer :passed "*ns* "))
		(progn
			(hash-set! ns-test-set-item :failed (+ 1 (hash-get ns-test-set-item :failed)))
			(report-pretty-printer :failed "*ns* "))))

(progn
	(hash-set! ns-test-set-item :total (+ 1 (hash-get ns-test-set-item :total)))
	(if (test::run-ns-example 'ns-create)
		(progn
			(hash-set! ns-test-set-item :passed (+ 1 (hash-get ns-test-set-item :passed)))
			(report-pretty-printer :passed "ns-create"))
		(progn
			(hash-set! ns-test-set-item :failed (+ 1 (hash-get ns-test-set-item :failed)))
			(report-pretty-printer :failed "ns-create"))))

(progn
	(hash-set! ns-test-set-item :total (+ 1 (hash-get ns-test-set-item :total)))
	(if (test::run-ns-example 'ns-enter)
		(progn
			(hash-set! ns-test-set-item :passed (+ 1 (hash-get ns-test-set-item :passed)))
			(report-pretty-printer :passed "ns-enter"))
		(progn
			(hash-set! ns-test-set-item :failed (+ 1 (hash-get ns-test-set-item :failed)))
			(report-pretty-printer :failed "ns-enter"))))

(progn
	(hash-set! ns-test-set-item :total (+ 1 (hash-get ns-test-set-item :total)))
	(if (test::run-ns-example 'ns-exists?)
		(progn
			(hash-set! ns-test-set-item :passed (+ 1 (hash-get ns-test-set-item :passed)))
			(report-pretty-printer :passed "ns-exists"))
		(progn
			(hash-set! ns-test-set-item :failed (+ 1 (hash-get ns-test-set-item :failed)))
			(report-pretty-printer :failed "ns-exists"))))

(progn
	(hash-set! ns-test-set-item :total (+ 1 (hash-get ns-test-set-item :total)))
	(if (test::run-ns-example 'ns-list)
		(progn
			(hash-set! ns-test-set-item :passed (+ 1 (hash-get ns-test-set-item :passed)))
			(report-pretty-printer :passed "ns-list"))
		(progn
			(hash-set! ns-test-set-item :failed (+ 1 (hash-get ns-test-set-item :failed)))
			(report-pretty-printer :failed "ns-list"))))

(progn
	(hash-set! ns-test-set-item :total (+ 1 (hash-get ns-test-set-item :total)))
	(if (test::run-ns-example 'ns-pop)
		(progn
			(hash-set! ns-test-set-item :passed (+ 1 (hash-get ns-test-set-item :passed)))
			(report-pretty-printer :passed "ns-pop"))
		(progn
			(hash-set! ns-test-set-item :failed (+ 1 (hash-get ns-test-set-item :failed)))
			(report-pretty-printer :failed "ns-pop"))))

(progn
	(hash-set! ns-test-set-item :total (+ 1 (hash-get ns-test-set-item :total)))
	(if (test::run-ns-example 'ns-symbols)
		(progn
			(hash-set! ns-test-set-item :passed (+ 1 (hash-get ns-test-set-item :passed)))
			(report-pretty-printer :passed "ns-symbols"))
		(progn
			(hash-set! ns-test-set-item :failed (+ 1 (hash-get ns-test-set-item :failed)))
			(report-pretty-printer :failed "ns-symbols"))))

(append! final-test-report ns-test-set-item)

(println)


(defq *global-failed* 0)
(defn pprint-final-test-report (report-list) (progn
	(println (str shell::*fg-black* shell::*bg-white*))
	(defq global-total 0)
	(defq global-notest 0)
	(defq global-passed 0)
	(for test in report-list (progn
		(defq total (hash-get test :total))

		(defq failed (hash-get test :failed))
		(defq notest (hash-get test :no-test))
		(defq passed (- total failed notest))

		(setq global-notest (+ notest global-notest))
		(setq *global-failed* (+ failed *global-failed*))
		(setq global-total (+ total global-total))
		(setq global-passed (+ passed global-passed))

		(println "-----------------------------")
		(println (hash-get test :name))
		(println "total: " total)
		(println "failed: " failed)
		(println "no test " notest)
		(println "passed: " passed)))

		(println "-----------------------------")
		(println "All tests:")
		(println "total: " global-total)
		(println "failed: " *global-failed*)
		(println "passed: " global-passed)
		(println "no test " global-notest)
	(println (str shell::*fg-default* shell::*bg-default*))))

(println)
(printer "Test Summary")
(pprint-final-test-report final-test-report)
(if (> *global-failed* 0) (exit 1))
