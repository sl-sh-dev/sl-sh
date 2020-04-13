(core::ns-import 'core)
(ns-import 'test)

;; check list contains in? fcn
(let ((vowels-list (list "a" "e" "i" "o" "u")))
    (assert-true (in? vowels-list "u") (str ". line number: " (meta-line-no)))
    (assert-false (in? vowels-list "c")) (str ". line number: " (meta-line-no)))

;; check reduce
(assert-true (= 15 (reduce + 0 (list 1 2 3 4 5))) (str ". line number: " (meta-line-no)))
(assert-false (= 15 (reduce + 1 (list 1 2 3 4 5))) (str ". line number: " (meta-line-no)))
(assert-true (= "one hoopy frood" (reduce str "" (list "one " "hoopy " "frood"))) (str ". line number: " (meta-line-no)))

;; check when
(assert-true (when #t #t) (str ". line number: " (meta-line-no)))
(assert-false (when #t nil) (str ". line number: " (meta-line-no)))
(assert-false (when nil nil) (str ". line number: " (meta-line-no)))
