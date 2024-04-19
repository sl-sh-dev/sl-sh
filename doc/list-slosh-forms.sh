#!/usr/bin/env slosh_test

(pr (loop (lst i output) ((get-globals-sorted) 0 "")
    (if (>= (+ i 1) (len lst))
        (break output)
        (recur lst (inc! i)  (str output lst.~i ",\n")))))
