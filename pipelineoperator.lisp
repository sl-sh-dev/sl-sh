#!/usr/bin/env sl-sh

(ns-import 'iterator)

(defn chain (&rest args)
  ;; syntax of chain requires all elems after 0th elem of args must be non-empty
  ;; sequences that contain the _ symbol in the non 0 position.
  (println "the args: " args)
    (let ((thread (macro (fst rst)
             (println "fst: " fst ", rst: " rst)))) (do
      (if nil
        (err "chain operator requires at least two arguments")
        (for elem in (rest args) (do
          (println "rest of the args are: " (rest args))
          (println "elem: " elem)
          (when (not (and (non-empty-seq? elem)
                          (> (length elem) 1)
                          (in? (rest elem) '_)))
             (err "All args in non 0 position must be non-empty sequences that
                  contain the _ symbol in the non 0 position.")))))
      (thread (first args) (rest args)))))

;;(chain "meow" (echo _ ", and how!"))

(defmacro testchain (init &rest args)
    (println "vecnth: " (vec-nth args 0) 2)
    (println "args: " args)
    (loop (elem elems) (init args) (do
        (println "elem: " elem)
        (println "elems: " elems)
        (if (empty-seq? (first elems))
            elem
            (recur
                (loop (replacement lst orig-lst) (elem (first elems) (first elems))
                    (if (= (first lst) '_)
                        (do (println "is _")
                           ;;TODO put in a list copy before the replacement so we don't
                           ;; change the value of something the user passed in?
                           ;; deleteme? (println "stuff: " (xdr! (xar! lst "woof") expr))
                           (xar! lst replacement)
                           (println "stuff: " orig-lst)
                           orig-lst)
                        (if (empty-seq? (first lst))
                            orig-lst
                           (recur replacement (rest lst) orig-lst))))
                (rest elems))))))

(println "testchain ret: " (testchain "my" (str "with " _ "sauce") (str "Pasta " _ ", yes please.")))

