(ns-import 'test)

(assert-equal '(1 (back-quote (back-quote (back-quote (unquote (unquote-splice (unquote 3)))))) 4) `(1 ```,,@,,@(list (+ 1 2)) 4))
(assert-equal '(1 (back-quote (unquote (+ 1 5))) 4) `(1 `,(+ 1 ,(+ 2 3)) 4))
(assert-equal '#(1 1 2 4) `#(1 ,@(list 1 2) 4))

; test a macro generating macro
(defmacro def-bqtest-caller (name proc) `(defmacro ,name (var expr args) `(,',proc (fn (,var) ,expr) ,args)))
(def-bqtest-caller dtest apply)
(assert-equal 6 (dtest x (+ x 1) '(5)))

; Quine from https://letoverlambda.com/index.cl/guest/chap4.html attributed to Mike McMahon
(def let-quine1 '(let ((let '`(let ((let ',let))
                    ,let)))
      `(let ((let ',let)) ,let)))
(def let-quine2 (let ((let '`(let ((let ',let))
                    ,let)))
      `(let ((let ',let)) ,let)))
(assert-equal let-quine1 let-quine2)

(let ((x '(4 5 6)) (xv '#(7 8 9)) (y 10))
  (assert-equal '(1 2 3 4 5 6) `(1 2 3 ,@x))
  (assert-equal '(1 2 3 7 8 9) `(1 2 3 ,@xv))
  (assert-equal '(1 2 3 #(7 8 9)) `(1 2 3 ,xv))
  (assert-equal '#(1 2 3 7 8 9) `#(1 2 3 ,@xv))
  (assert-equal '#(1 2 3 #(7 8 9)) `#(1 2 3 ,xv))
  (assert-equal '(4 5 6 1 2 3) `(,@x 1 2 3))
  (assert-equal '(1 2 3 4 5 6) `(1 2 3 . ,x))
  (assert-equal '(1 2 3 . 10) `(1 2 3 . ,y)))
