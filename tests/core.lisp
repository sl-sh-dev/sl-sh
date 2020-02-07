(load "tests/test.lisp")
(core::ns-import 'core)
(ns-import 'test)

;; check list contains in? fcn
(let ((vowels-list (list "a" "e" "i" "o" "u")))
    (assert-true (in? vowels-list "u"))
    (assert-false (in? vowels-list "c")))

;; check reduce
(assert-true (= 15 (reduce + 0 (list 1 2 3 4 5))))
(assert-false (= 15 (reduce + 1 (list 1 2 3 4 5))))
(assert-true (= "one hoopy frood" (reduce str "" (list "one " "hoopy " "frood"))))

;; check when
(assert-true (when #t #t))
(assert-false (when #t nil))
(assert-false (when nil nil))
