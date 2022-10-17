#!./target/debug/sl-sh

(ns-import 'iterator)
(ns-import 'shell)
(ns-import 'test)

;;TODO gpwclark remove this error stack on call when "(error-stack-on)" becomes an environment variable
(error-stack-on)

(def *tests-dir* "tests")

(def *padding* 42)

(defn print-with-padding (to-print)
      (print (with-padding to-print *padding* ".")))

(defn has-example (docstring)
	(str-contains "Example:" docstring))

(defn exec-str (docstring)
	(var test (vec-nth (str-split "Example:" docstring) 1))
	(fn ()
		(var exp (read test :done))
		(if (not (= exp :done))
			(do (eval exp) (recur)))))

(defn all-items-by-whitespace (producer)
	(str-trim (str (| (producer) $(tr "\n" " ") $(tr -s ":blank:")))))

(defn make-test-list-from-symbols (symbols-list a-ns) (do
	(var test-list (list))
	(for symbol in symbols-list (do
	(var fully-qualified-symbol (sym a-ns "::" symbol))
	(when (and
			(func? (eval fully-qualified-symbol))
			(not (or
				(str-starts-with "ns-" (str symbol))
				(= "*ns*" (str symbol))))) (do
		(var test-set-item (make-hash))
		(hash-set! test-set-item :name (str symbol))
		(var docstring (doc fully-qualified-symbol))
		(if (has-example docstring)
			(do
				(hash-set! test-set-item :load-fcn (exec-str docstring))
				(append-to! test-list test-set-item))
			(do
				(hash-set! test-set-item :load-fcn :no-test)
				(append-to! test-list test-set-item)))))))
	test-list))

(def file-test-list
	(reduce
		(fn (lst name) (append-to! lst (do
			(let ((load-fcn (fn () (load name)))
			      (test-set-item (make-hash)))
			(hash-set! test-set-item :name name)
			(hash-set! test-set-item :load-fcn load-fcn)
			test-set-item))))
		'()
		(glob "${*tests-dir*}/*")))

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
			"NONE"
			shell::*fg-default* shell::*bg-default*)))
		(:passed
		 (println
			(str shell::*fg-black* shell::*bg-green*
			"PASS"
			shell::*fg-default* shell::*bg-default*)))
		(:failed
		 (println
			(str shell::*fg-black* shell::*bg-red*
			"FAIL"
			shell::*fg-default* shell::*bg-default*)))
		(:error
		 (println ""))
		(nil
		 (println
			(str shell::*fg-black* shell::*bg-red*
			"WTF?"
			shell::*fg-default* shell::*bg-default*
			" " test-name)))))

(defn report-test-results (tests test-report)
  (let ((exit-status :passed))
    (dyn exit (fn (x)
                  (when (not (= x "0"))
                    (do
                     (set! exit-status :failed)
                     (hash-set! test-report :failed (+ 1 (hash-get test-report :failed)))))
                  x)
         ; Keep the recursion outside the dyn or will not have TCO.
         ; Dyn is a macro around unwind-protect and this will act as
         ; 'break' for recur and TCO (tail call optimization).
         ((fn (tests test-report)
              (set! exit-status :passed)
              (let ((fst (first tests)))
                (when fst
                  (do
                (print-with-padding (hash-get (first tests) :name))
                   (hash-set! test-report :total (+ 1 (hash-get test-report :total)))
                   (if (= :no-test (hash-get fst :load-fcn))
                       (do
                        (hash-set! test-report :no-test (+ 1 (hash-get test-report :no-test)))
                        (set! exit-status :no-test))
                       (let ((test-result (get-error ((hash-get fst :load-fcn)))))
                        (when (= (car test-result) :error)
                          (do
                           (print (str shell::*fg-black* shell::*bg-red*
                                       "ERR!"
                                       shell::*fg-default* shell::*bg-default*))
                           (println "")
                           (print-error test-result)
                           ;(println (cdr test-result))
                           (set! exit-status :error)
                           (hash-set! test-report :failed (+ 1 (hash-get test-report :failed)))))))
                    (report-pretty-printer exit-status (hash-get fst :name))
                    (recur (rest tests) test-report))) ))tests test-report))))

(def final-test-report '())

(defn run-tests-for (test-name test-list test-report) (do
    (var test-data (make-hash))
    (hash-set! test-data :name test-name)
    (hash-set! test-data :total 0)
    (hash-set! test-data :failed 0)
    (hash-set! test-data :no-test 0)
    (report-test-results test-list test-data)
    (append-to! test-report test-data)))

(printer "Tests from test directory")
(run-tests-for "module tests" file-test-list final-test-report)

;; run tests for non-root namespaces
(for a-ns in (filter (fn (x) (and (not (= x "root")) (not (= x "test")) (not (= x "user")))) (ns-list)) (do
	(printer (str "Tests from " a-ns))
	(var sym-list (qsort (eval (sym a-ns "::*ns-exports*"))))
	(set! sym-list (make-test-list-from-symbols sym-list a-ns))
	(run-tests-for (str a-ns " unit tests") sym-list final-test-report)))

;; run tests for root namespaces
(let ((sym-list (qsort (ns-symbols 'root))))
(printer (str "Tests from root"))
(set! sym-list (make-test-list-from-symbols sym-list "root"));"root"))
(run-tests-for (str "root unit tests") sym-list final-test-report))

;; run tests for root namespace special namespace tests (ns cmd can not be run
;; inside a fcn

(def ns-test-set-item (make-hash))
(hash-set! ns-test-set-item :name "namespace unit tests")
(hash-set! ns-test-set-item :total 0)
(hash-set! ns-test-set-item :failed 0)
(hash-set! ns-test-set-item :passed 0)
(hash-set! ns-test-set-item :no-test 0)

(printer "Tests from namespace")

(do
	(hash-set! ns-test-set-item :total (+ 1 (hash-get ns-test-set-item :total)))
	(print-with-padding "*ns*")
	(if (test::run-ns-example 'root::*ns*)
		(do
			(hash-set! ns-test-set-item :passed (+ 1 (hash-get ns-test-set-item :passed)))
			(report-pretty-printer :passed "*ns* "))
		(do
			(hash-set! ns-test-set-item :failed (+ 1 (hash-get ns-test-set-item :failed)))
			(report-pretty-printer :failed "*ns* "))))

(do
	(hash-set! ns-test-set-item :total (+ 1 (hash-get ns-test-set-item :total)))
	(print-with-padding "ns-create")
	(if (test::run-ns-example 'ns-create)
		(do
			(hash-set! ns-test-set-item :passed (+ 1 (hash-get ns-test-set-item :passed)))
			(report-pretty-printer :passed "ns-create"))
		(do
			(hash-set! ns-test-set-item :failed (+ 1 (hash-get ns-test-set-item :failed)))
			(report-pretty-printer :failed "ns-create"))))

(do
	(hash-set! ns-test-set-item :total (+ 1 (hash-get ns-test-set-item :total)))
	(print-with-padding "ns-enter")
	(if (test::run-ns-example 'ns-enter)
		(do
			(hash-set! ns-test-set-item :passed (+ 1 (hash-get ns-test-set-item :passed)))
			(report-pretty-printer :passed "ns-enter"))
		(do
			(hash-set! ns-test-set-item :failed (+ 1 (hash-get ns-test-set-item :failed)))
			(report-pretty-printer :failed "ns-enter"))))

(do
	(hash-set! ns-test-set-item :total (+ 1 (hash-get ns-test-set-item :total)))
	(print-with-padding "ns-exists")
	(if (test::run-ns-example 'ns-exists?)
		(do
			(hash-set! ns-test-set-item :passed (+ 1 (hash-get ns-test-set-item :passed)))
			(report-pretty-printer :passed "ns-exists"))
		(do
			(hash-set! ns-test-set-item :failed (+ 1 (hash-get ns-test-set-item :failed)))
			(report-pretty-printer :failed "ns-exists"))))

(do
	(hash-set! ns-test-set-item :total (+ 1 (hash-get ns-test-set-item :total)))
	(print-with-padding "ns-list")
	(if (test::run-ns-example 'ns-list)
		(do
			(hash-set! ns-test-set-item :passed (+ 1 (hash-get ns-test-set-item :passed)))
			(report-pretty-printer :passed "ns-list"))
		(do
			(hash-set! ns-test-set-item :failed (+ 1 (hash-get ns-test-set-item :failed)))
			(report-pretty-printer :failed "ns-list"))))

(do
	(hash-set! ns-test-set-item :total (+ 1 (hash-get ns-test-set-item :total)))
	(print-with-padding "ns-pop")
	(if (test::run-ns-example 'ns-pop)
		(do
			(hash-set! ns-test-set-item :passed (+ 1 (hash-get ns-test-set-item :passed)))
			(report-pretty-printer :passed "ns-pop"))
		(do
			(hash-set! ns-test-set-item :failed (+ 1 (hash-get ns-test-set-item :failed)))
			(report-pretty-printer :failed "ns-pop"))))

(do
	(hash-set! ns-test-set-item :total (+ 1 (hash-get ns-test-set-item :total)))
	(print-with-padding "ns-push")
	(if (test::run-ns-example 'ns-push)
		(do
			(hash-set! ns-test-set-item :passed (+ 1 (hash-get ns-test-set-item :passed)))
			(report-pretty-printer :passed "ns-push"))
		(do
			(hash-set! ns-test-set-item :failed (+ 1 (hash-get ns-test-set-item :failed)))
			(report-pretty-printer :failed "ns-push"))))

(do
	(hash-set! ns-test-set-item :total (+ 1 (hash-get ns-test-set-item :total)))
	(print-with-padding "ns-symbols")
	(if (test::run-ns-example 'ns-symbols)
		(do
			(hash-set! ns-test-set-item :passed (+ 1 (hash-get ns-test-set-item :passed)))
			(report-pretty-printer :passed "ns-symbols"))
		(do
			(hash-set! ns-test-set-item :failed (+ 1 (hash-get ns-test-set-item :failed)))
			(report-pretty-printer :failed "ns-symbols"))))

(append-to! final-test-report ns-test-set-item)

(println)

(defn print-medium-face ()
	(println)
	(println (shell::fg-color-rgb 220 220 220) " - - " shell::*fg-default*)
	(println (shell::fg-color-rgb 220 220 220) " --- " shell::*bg-default*))

(defn print-sad-face ()
	(println)
	(println (shell::fg-color-rgb 255 0 0) " -  - " shell::*fg-default*)
	(println (shell::fg-color-rgb 255 0 0) "  /\\ " shell::*fg-default*))

(defn print-happy-face ()
	(println)
	(println (shell::fg-color-rgb 0 255 0) " +  + " shell::*fg-default*)
	(println (bhell::fg-color-rgb 0 255 0) "  \\/ " shell::*fg-default*))

(def *global-failed* 0)
(defn pprint-final-test-report (report-list) (do
	(println (str shell::*fg-default* shell::*bg-default*))
	(var global-total 0)
	(var global-notest 0)
	(var global-passed 0)
	(for test in report-list (do
		(var total (hash-get test :total))

		(var failed (hash-get test :failed))
		(var notest (hash-get test :no-test))
		(var passed (- total failed notest))

		(set! global-notest (+ notest global-notest))
		(set! *global-failed* (+ failed *global-failed*))
		(set! global-total (+ total global-total))
		(set! global-passed (+ passed global-passed))

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
		(if (> *global-failed* 0)
			(print-sad-face)
			(if (> global-notest 0)
				(print-medium-face)
				(print-happy-face)))
	(println (str shell::*fg-default* shell::*bg-default*))))

(println)
(printer "Test Summary")
(pprint-final-test-report final-test-report)
(if (> *global-failed* 0) (exit 1))
