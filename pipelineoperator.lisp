#!/usr/bin/env sl-sh

(ns-import 'iterator)
;; TODO to implement
;;    - nest
;;    - nest-reverse
;;    - chain-and
;;    - chain-when
;;    - chain-lambda
;;    - chain-lambda
;; TODO need tests and a docstring
;; TODO check scheme spec
;; TODO throw err if multiple _ or perhaps... insert arg into them?
(defn verify-chain-args (args)
      (if (= 0 (length args))
;; TODO feature specify min number of var args to get rid of common checking
;; pattern?
        (err "chain operator requires at least two arguments")
        (do
          (for elem in args (when (not (and (non-empty-seq? elem)
                                            (> (length elem) 1)
                                            (in? (rest elem) '_)))
             (err "All args in non 0 position must be non-empty sequences that
                  contain the _ symbol in the non 0 position.")))
          #t)))


(defmacro chain (init &rest args)
  (verify-chain-args args)
  (let* ((reducer (fn (fst nxt)
                      (substitute fst '_ nxt))))
    (reduce reducer init args)))


(println "test chain" (chain "a properly worded " (str "will " " become " _) (str "I " _ "statement.") (str "Ordered sentence: " _)))
(println "Chain macro expansion: " (expand-macro '(chain "my" (str _ " with " " sauce") (str "Pasta " _ ", yes please."))))
