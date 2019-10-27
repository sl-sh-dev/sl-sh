#|
	Test the comment blocks while we are at it...

|#
#|
	Test the #|comment blocks while we are at it...

|#
Some stuff
|#

(defn lists= (list1 list2)
    (if (not (= (length list1)(length list2)))
        nil
        (if (= (length list1) 0)
            t
            (if (not (= (first list1)(first list2)))
                nil
                (recur (rest list1) (rest list2))))))
                

(let ((l1 '(1 2 3)) (l2 '(a b c)) (l3 '(1 2 (a b c) 3)) (l4))
    (if (and (= (first l1) 1) (lists= l1 '(1 2 3)))
        (println "PASS 1") (println "FAIL"))
    (if (and (lists= (rest l1) '(2 3)) (lists= l1 '(1 2 3)))
        (println "PASS 2") (println "FAIL"))
    (if (= (length l1) 3)
        (println "PASS 3") (println "FAIL"))
    (if (and (= (last l1) 3) (lists= l1 '(1 2 3)))
        (println "PASS 4") (printnl "FAIL"))
    (if (and (lists= (butlast l1) '(1 2)) (lists= l1 '(1 2 3)))
        (println "PASS 5") (println "FAIL"))
    (if (and (= (vnth 1 l1) 2) (lists= l1 '(1 2 3)))
        (println "PASS 6") (println "FAIL"))
    (if (and (lists= (vsetnth! 1 'x l1) '(1 x 3)) (lists= l1 '(1 x 3)))
        (println "PASS 7") (println "FAIL"))
    (if (and (lists= (vappend l1 l2) '(1 x 3 a b c)) (lists= l1 '(1 x 3)) (lists= l2 '(a b c)))
        (println "PASS 8") (println "FAIL"))
    (if (and (lists= (push! l1 4) '(1 x 3 4)) (lists= l1 '(1 x 3 4)))
        (println "PASS 9") (println "FAIL"))
    (setq l4 (vnth 2 l3))
    (push! l4 'd)
    (if (and (lists= (vnth 2 l3) '(a b c d)) (lists= l4 (vnth 2 l3)))
        (println "PASS 10") (println "FAIL"))
    (if (and (= (pop! l1) 4) (lists= l1 '(1 x 3)))
        (println "PASS 11") (println "FAIL"))
    (if (and (lists= (vremove-nth! 1 l1) '(1 3)) (lists= l1 '(1 3)))
        (println "PASS 12") (println "FAIL"))
    (if (not (is-empty l1))
        (println "PASS 13") (println "FAIL"))
    (if (and (lists= (vclear! l1) '()) (lists= l1 '()))
        (println "PASS 14") (println "FAIL"))
    (if (is-empty l1)
        (println "PASS 15") (println "FAIL"))
    (if (and (lists= (vinsert-nth! 0 2 l1) '(2)) (lists= l1 '(2)))
        (println "PASS 16") (println "FAIL"))
    (if (and (lists= (vinsert-nth! 0 1 l1) '(1 2)) (lists= l1 '(1 2)))
        (println "PASS 17") (println "FAIL"))
    (if (and (lists= (vinsert-nth! 2 3 l1) '(1 2 3)) (lists= l1 '(1 2 3)))
        (println "PASS 18") (println "FAIL"))
    (if (and (lists= (vinsert-nth! 1 'a l1) '(1 a 2 3)) (lists= l1 '(1 a 2 3)))
        (println "PASS 19") (println "FAIL" l1))
    )

println "Testing Lists from Pairs (Cons Lists)"
(let ((l1 (list 1 2 3)) (l2 (list 'a 'b 'c)) (l3 (list 1 2 (list 'a 'b 'c) 3)) (l4))
    (if (and (= (first l1) 1) (lists= l1 '(1 2 3)))
        (println "PASS 1") (println "FAIL"))
    (if (and (lists= (rest l1) '(2 3)) (lists= l1 '(1 2 3)))
        (println "PASS 2") (println "FAIL"))
    (if (= (length l1) 3)
        (println "PASS 3") (println "FAIL"))
    (if (and (= (last l1) 3) (lists= l1 '(1 2 3)))
        (println "PASS 4") (println "FAIL" (last l1) l1))
    (if (and (lists= (butlast l1) '(1 2)) (lists= l1 '(1 2 3)))
        (println "PASS 5") (println "FAIL"))
    (if (and (= (car (cdr l1)) 2) (lists= l1 '(1 2 3)))
        (println "PASS 6") (println "FAIL"))
    (if (and (lists= (join (car l1) (xar! (cdr l1) 'x)) '(1 x 3)) (lists= l1 '(1 x 3)))
        (println "PASS 7") (println "FAIL " l1))
    (if (and (lists= (join (car l1) (join (car (cdr l1)) (join (car (cdr (cdr l1))) l2))) '(1 x 3 a b c)) (lists= l1 '(1 x 3)) (lists= l2 '(a b c)))
        (println "PASS 8") (println "FAIL l1: " l1 ", l2: " l2))
    ;(if (and (lists= (push! l1 4) '(1 x 3 4)) (lists= l1 '(1 x 3 4)))
    ;    (println "PASS") (println "FAIL"))
    (setq l4 (car (cdr (cdr l3))))
    (if (lists= l4 '(a b c))
        (println "PASS 9") (println "FAIL")))

