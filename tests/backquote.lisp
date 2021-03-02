(ns-import 'test)

(assert-equal '(1 (back-quote (back-quote (back-quote (unquote (unquote-splice (unquote 3)))))) 4) `(1 ```,,@,,@(list (+ 1 2)) 4))
(assert-equal '(1 (back-quote (unquote (+ 1 5))) 4) `(1 `,(+ 1 ,(+ 2 3)) 4))
(assert-equal '#(1 1 2 4) `#(1 ,@(list 1 2) 4))

; test a macro generating macro
(defmacro def-bqtest-caller (name proc) `(defmacro ,name (var expr args) `(,',proc (fn (,var) ,expr) ,args)))
(def-bqtest-caller dtest apply)
(assert-equal 6 (dtest x (+ x 1) '(5)))

