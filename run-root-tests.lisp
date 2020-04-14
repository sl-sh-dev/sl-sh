#!/bin/sl-sh

(core::ns-import 'core)
(load "tests/test.lisp")

#|(defn quick (lst) (progn
    ;(println "Sorting: " lst)
    (if (<= (length lst) 1)
        lst
        ;(let ((pivot (first lst)) (less nil) (greater nil))
        (let ((pivot (first lst)) (less (vec)) (greater (vec)))
            (for i (rest lst)
                ;(if (< i pivot) (setq less (join i less)) (setq greater (join i greater))))
                (if (< i pivot) (vec-push! less i) (vec-push! greater i)))
            ;(println less " XXXX " pivot " XXXX " greater)
            (append! (quick less) pivot (quick greater))))))
            ;(progn (quick less) (quick greater)(vec)))))
|#
(def 'total-syms 0)
(def 'total-tests 0)
(println "Namespace forms:")
(progn (set 'total-syms (+ 1 total-syms)) (set 'total-tests (+ 1 total-tests))
	(print "ns-create ") (if (test::run-example 'ns-create) (println "PASSED") (println "FAILED")))
(progn (set 'total-syms (+ 1 total-syms)) (set 'total-tests (+ 1 total-tests))
	(print "ns-enter ") (if (test::run-example 'ns-enter) (println "PASSED") (println "FAILED")))
(progn (set 'total-syms (+ 1 total-syms)) (set 'total-tests (+ 1 total-tests))
	(print "ns-exists? ") (if (test::run-example 'ns-exists?) (println "PASSED") (println "FAILED")))
(progn (set 'total-syms (+ 1 total-syms)) (set 'total-tests (+ 1 total-tests))
	(print "ns-list ") (if (test::run-example 'ns-list) (println "PASSED") (println "FAILED")))
(progn (set 'total-syms (+ 1 total-syms)) (set 'total-tests (+ 1 total-tests))
	(print "ns-pop ") (if (test::run-example 'ns-pop) (println "PASSED") (println "FAILED")))
(progn (set 'total-syms (+ 1 total-syms)) (set 'total-tests (+ 1 total-tests))
	(print "ns-symbols ") (if (test::run-example 'ns-symbols) (println "PASSED") (println "FAILED")))
(progn (set 'total-syms (+ 1 total-syms)) (set 'total-tests (+ 1 total-tests))
	(print "*ns* ") (if (test::run-example 'root::*ns*) (println "PASSED") (println "FAILED")))
(println)

(def 'root-list (qsort (ns-symbols 'root)))

(for sym root-list (progn
	(if (not (or (str-starts-with "ns-" (str sym))(= "*ns*" (str sym)))) (progn
		(set 'total-syms (+ 1 total-syms)) 
		(if (str-contains "Example:" (doc (to-symbol (str "root::" sym))))
			(progn (set 'total-tests (+ 1 total-tests))
				(print sym " ") (if (test::run-example (to-symbol (str "root::" sym))) (println "PASSED") (println "FAILED")))
			(println sym " NO TEST"))))))

(println "Tests run: " total-tests ", total symbols: " total-syms ", missing tests: " (- total-syms total-tests))

