#!/usr/bin/env slosh_test

(pr (loop (lst i output) ((get-exemptions) 0 "")
    (if (>= (+ i 1) (len lst))
        (break output)
        (recur lst (inc! i)  (str output lst.~i ",\n")))))

(def meow (make-vec))
(set! meow.0 "abc")
(prn (get meow 0))
