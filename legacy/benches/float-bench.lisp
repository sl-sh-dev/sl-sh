#!/usr/bin/env sl-sh

(defn eval-pol (n x)
  (let ((su 0.0)
        (mu 10.0)
        (pu 0.0)
        (pol (make-vec 100 0.0)))
    (dotimes-i i n (do
        (set! su 0.0)
        (dotimes-i j 100
         (do
           (set! mu (/ (+ mu 2.0) 2.0))
           (vec-set! pol j mu)))
        (dotimes-i j 100
          (set! su (+ (vec-nth pol j) (* su x))))
            (set! pu (+ pu su))))
    pu))

(print (eval-pol 10000 0.2))
