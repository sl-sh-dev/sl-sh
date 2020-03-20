#|
	Test the comment blocks while we are at it...

|#
#|
	Test the #|comment blocks while we are at it...

|#
Some stuff
|#

(core::ns-import 'core)
(ns-import 'test)

(let ((non-empty-vec (vec 'a))
	(empty-vec (make-vec))
	(non-empty-list (list 'a))
	(empty-list (list)))
;; test non empty vec
	(assert-true (seq? non-empty-vec))
	(assert-true (vec? non-empty-vec))
	(assert-true (non-empty-seq? non-empty-vec))
;; test empty vec
	(assert-true (seq? empty-vec))
	(assert-true (vec? empty-vec))
	(assert-false (non-empty-seq? empty-vec))
;; test non empty list
	(assert-true (seq? non-empty-list))
	(assert-true (list? non-empty-list))
	(assert-true (non-empty-seq? non-empty-list))
;; test empty list
	(assert-true (seq? nil))
	(assert-true (seq? empty-list))
	(assert-true (list? empty-list))
	(assert-true (list? nil))
	(assert-false (non-empty-seq? empty-list))
	(assert-false (non-empty-seq? nil)))

(let ((l1 '#(1 2 3)) (l2 '#(a b c)) (l3 '#(1 2 #(a b c) 3)) (l4))
    (assert-equal (first l1) 1)
    (assert-equal l1 '(1 2 3))
    (assert-equal (rest l1) '(2 3))
    (assert-equal l1 '#(1 2 3))
    (assert-equal (length l1) 3)
    (assert-true (and (= (last l1) 3) (assert-equal l1 '(1 2 3))))
    (assert-true (and (assert-equal (butlast l1) '(1 2)) (assert-equal l1 '(1 2 3))))
    (assert-true (and (= (vec-nth 1 l1) 2) (assert-equal l1 '(1 2 3))))
    (assert-true (and (assert-equal (vec-setnth! 1 'x l1) '(1 x 3)) (assert-equal l1 '(1 x 3))))
    (assert-true (and (assert-equal (append l1 l2) '(1 x 3 a b c)) (assert-equal l1 '(1 x 3)) (assert-equal l2 '(a b c))))
    (assert-true (and (assert-equal (vec-push! l1 4) '(1 x 3 4)) (assert-equal l1 '(1 x 3 4))))
    (setq l4 (vec-nth 2 l3))
    (vec-push! l4 'd)
    (assert-true (and (assert-equal (vec-nth 2 l3) '(a b c d)) (assert-equal l4 (vec-nth 2 l3))))
    (assert-true (and (= (vec-pop! l1) 4) (assert-equal l1 '(1 x 3))))
    (assert-true (and (assert-equal (vec-remove-nth! 1 l1) '(1 3)) (assert-equal l1 '(1 3))))
    (assert-false (vec-empty? l1))
    (assert-true (and (assert-equal (vec-clear! l1) '()) (assert-equal l1 '())))
    (assert-true (vec-empty? l1))
    (assert-true (and (assert-equal (vec-insert-nth! 0 2 l1) '(2)) (assert-equal l1 '(2))))
    (assert-true (and (assert-equal (vec-insert-nth! 0 1 l1) '(1 2)) (assert-equal l1 '(1 2))))
    (assert-true (and (assert-equal (vec-insert-nth! 2 3 l1) '(1 2 3)) (assert-equal l1 '(1 2 3))))
    (assert-true (and (assert-equal (vec-insert-nth! 1 'a l1) '(1 a 2 3)) (assert-equal l1 '(1 a 2 3)))))

(let ((l1 (list 1 2 3)) (l2 (list 'a 'b 'c)) (l3 (list 1 2 (list 'a 'b 'c) 3)) (l4))
    (assert-true (and (= (first l1) 1) (assert-equal l1 '(1 2 3))))
    (assert-true (and (assert-equal (rest l1) '(2 3)) (assert-equal l1 '(1 2 3))))
    (assert-equal (length l1) 3)
    (assert-true (and (= (last l1) 3) (assert-equal l1 '(1 2 3))))
    (assert-true (and (assert-equal (butlast l1) '(1 2)) (assert-equal l1 '(1 2 3))))
    (assert-true (and (= (car (cdr l1)) 2) (assert-equal l1 '(1 2 3))))
    (assert-true (and (assert-equal (join (car l1) (xar! (cdr l1) 'x)) '(1 x 3)) (assert-equal l1 '(1 x 3))))
    (assert-true (and (assert-equal (join (car l1) (join (car (cdr l1)) (join (car (cdr (cdr l1))) l2))) '(1 x 3 a b c)) (assert-equal l1 '(1 x 3)) (assert-equal l2 '(a b c))))
    (setq l4 (car (cdr (cdr l3))))
    (assert-equal l4 '(a b c)))

;; ensure mechanism for list creation doesn't affect how lists are updated
(let ((long-list (list "A" (list "B")))
    (short-list '("A" ("B")))
    (model-list (list "A" (list "B" "C"))))
    (assert-equal model-list (progn (append! (last long-list) "C") long-list))
    (assert-equal model-list (progn (append! (last short-list) "C") short-list)))

;; check mutability of nested lists
(let ((list-of-lists (list (list "1") (list "A")))
    (list-of-empty-lists (list (list) (list)))
    (add-to-front-and-back-list (fn (target to-first to-last)
        (progn
            (append! (first target) to-first)
            (append! (last target) to-last)
            target))))
    (assert-equal
        (list (list "1" "2") (list "A" "B"))
        (add-to-front-and-back-list list-of-lists "2" "B"))
    (assert-equal
        (list (list "1") (list "A"))
        (add-to-front-and-back-list list-of-empty-lists "1" "A")))

;; check adding lists to lists
(let ((initial-list (list "A"))
    (complete-list (list "A" "B")))
    (assert-equal complete-list (append initial-list "B"))
    (assert-equal (list "A" "B" (list "C")) (append complete-list (list (list "C")))))
