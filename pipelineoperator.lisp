#!/usr/bin/env sl-sh

(ns-import 'iterator)

(defmacro testchain (init &rest args)
    (let
      ((copy-args-vec
         (fn (args)
            (var new-vec (make-vec (length args) nil))
            (for i in (iterator::range (length args))
            (vec-insert! new-vec i (collect-copy (vec-nth args i)))) new-vec)))
          (do
            (var output (loop (elem elems) (init (copy-args-vec args)) (do
            (if (empty-seq? (first elems))
                elem
                (recur
                    (loop (replacement lst orig-lst) (elem (first elems) (first elems))
                        (if (= (first lst) '_)
                            (do 
                               ;;TODO put in a list copy before the replacement so we don't
                               ;; change the value of something the user passed in?
                               ;; deleteme? (println "stuff: " (xdr! (xar! lst "woof") expr))
                               (xar! lst replacement)
                               orig-lst)
                            (if (empty-seq? (first lst))
                                orig-lst
                               (recur replacement (rest lst) orig-lst))))
                    (rest elems))))))
        output)))

;; TODO need tests and a docstring
;; TODO check scheme spec
(defmacro chain (init &rest args)
  ;; syntax of chain requires all elems after 0th elem of args must be non-empty
  ;; sequences that contain the _ symbol in the non 0 position.
      (if (= 0 (length args))
        (err "chain operator requires at least two arguments")
        (do
          (for elem in args (when (not (and (non-empty-seq? elem)
                                            (> (length elem) 1)
                                            (in? (rest elem) '_)))
             (err "All args in non 0 position must be non-empty sequences that
                  contain the _ symbol in the non 0 position.")))
          (let ((copy-args-vec (fn (args)
                                (var new-vec (make-vec (length args) nil))
                                (for i in (iterator::range (length args))
                                (vec-insert! new-vec i (collect-copy (vec-nth args i)))) new-vec)))
            (loop (elem elems) (init (copy-args-vec args))
            (if (empty-seq? (first elems))
                elem
                (recur
                    (loop (replacement lst orig-lst) (elem (first elems) (first elems))
                        (if (= (first lst) '_)
                            (do (xar! lst replacement) orig-lst)
                            (if (empty-seq? (first lst))
                                orig-lst
                               (recur replacement (rest lst) orig-lst))))
                    (rest elems))))))))

(println "testchain ret: " (chain "my" (str "with " _ " sauce") (str "Pasta " _ ", yes please.")))
(println "testchain ret2: " (expand-macro '(chain "my" (str "with " _ " sauce") (str "Pasta " _ ", yes please."))))

;;(chain "meow" (echo _ ", and how!"))

