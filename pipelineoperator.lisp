#!/usr/bin/env sl-sh

(ns-import 'iterator)
(ns-import 'test)

(defn visit-all (consumer seq)
  (if (non-empty-seq? seq)
    (do
      (visit consumer (first seq))
      (visit consumer (rest seq)))
    (when (not (empty-seq? seq))
      (consumer seq))))

(defn in-any? (seq-to-search item-to-match)
  (when (non-empty-seq? seq-to-search)
    (if (in? seq-to-search item-to-match)
      #t
      (if (in-recur? (first seq-to-search) item-to-match)
        #t
        (in-recur? (rest seq-to-search) item-to-match)))))

;; TODO to implement
;;    - chain-and (need to implement)
;;    - chain-when (ne
;;    - chain-lambda
;; TODO need tests and a docstring
(defn verify-chain-args (arg0 args)
      (vec-insert! args 0 arg0)
      (for elem in args (when (not (and (non-empty-seq? elem)
                                        (> (length elem) 1)
                                        (in? elem '_)))
         (err "All args must be non-empty sequences that
              contain the _ symbol.")))
      #t)

(defn count
"Counts instances of item in sequence.

Section: core

Example:
(test::assert-equal 3 (count (list 1 3 5 2 4 10 2 4 88 2 1) 2))
(test::assert-equal 0 (count (list 1 3 5 2 4 10 2 4 88 2 1) 42))"
      (sequence item)
    (let ((add-if-equals-item (fn (fst nxt) (if (= nxt item) (+ 1 fst) fst))))
      (reduce add-if-equals-item 0 sequence)))


(defmacro chain (init arg0 &rest args)
  (verify-chain-args arg0 args)
  (let* ((reducer (fn (fst nxt)
          (substitute fst '_ nxt))))
    (reduce reducer init args)))

(test::assert-equal "Ordered sentence: I will become a properly worded statement." (chain "a properly worded " (str "will " "become " _) (str "I " _ "statement.") (str "Ordered sentence: " _)))


;;TODO it's not good that these error message strings are somewhat duplicated.
;;  is there a good way to make them accessible for use in the std lib
;;  w/o leaking. would such a language feature be desirable anyway.
(defmacro and-let* (vals &rest let-body)
    (let* ((rev (iterator::reverse vals))
           ;; if there is no let-body the last binding is returned
           (init
             (if (empty-seq? let-body)
               (let ((last-binding (next! rev)))
                 (if (= 1 (length last-binding))
                   (car last-binding)
                   (if (= 2 (length last-binding))
                     (cdr last-binding)
                     (err "ERROR: invalid bindings on and-let*"))))
               let-body)))
        (car (iterator::reduce
             (fn (fst nxt)
                (if (= 1 (length nxt))
                    `(if ,nxt
                         ((fn () ,@fst))
                         nil)
                    (if (= 2 (length nxt))
                        (let ((val (car nxt))
                              (bind (cdr nxt)))
                            (let ((binding (gensym)))
                              `((let ((,binding ,@bind))
                                (if ,binding
                                  ((fn (,val) ,@fst) ,binding)
                                   nil)))))
                          (err "ERROR: invalid bindings on and-let*"))))
             init
             rev))))

(test::assert-equal "charlie bravo alpha."
                    (and-let* ((val (do (str "alpha.")))
                               (other-val (do (str "bravo " val)))) (str "charlie " other-val)) "]")

(test::assert-equal "alpha, bravo." (and-let* ((val (do
                                         (str "bravo.")))
                                  (other-val (do
                                                 (str "alpha, " val))))))

(test::assert-equal nil (and-let* ((val (do (str "alpha, ") nil))
                                   (other-val (do (str "bravo " val))))))

#|
(defmacro chain-and (init &rest meows)
  (let* ((reducer (fn (fst nxt)
            (append-to! fst (substitute fst '_ nxt)))))
    (reduce reducer `((,binding ,init)) meows)))
(defmacro chain-and (init &rest args)
  (let* ((reducer (fn (fst nxt)
            (append-to! fst (substitute ,binding '_ nxt)))))
    (reduce reducer `((,binding ,init)) args)))
|#

(defmacro chain-and (init arg0 &rest args)
  (verify-chain-args arg0 args)
  (let* ((rev-args (collect (reverse args)))
         (first-rev (first rev-args))
         (rest-rev (collect (append-to! (rest rev-args) (list init))))
         (p (println "restrev " rest-rev))
         (reducer (fn (fst nxt)
                     `((fn (_) (when _ ,fst)) ,nxt))))
        (reduce reducer first-rev rest-rev)))
(println "expand chain-and: " (expand-macro '(chain-and "howdy" (string? _) (= _ "howdy"))))

(when (string? "howdy") (= (string? "howdy") "howdy"))
((fn (x) (when x
           ((fn (y) (when y
                      (= y "howdy")))
            (string? x))))
     "howdy")
#|

(println "ok: " (chain-and "howdy"
                           (string? _)
                           (= _ "howdy"))
(defn has-args (m &rest ns)
      (let* ((reverse-ns (reverse ns))
             (first-n (first reverse-ns))
             (rest-n (rest reverse-ns)))
        ;;(println "rest: " (collect (append-iter rest-n m)))
        (println "rest: " rest-n)
        ))

(defn has-args (m &rest ns)
      (println "ns: " (collect (first (reverse ns)))))

(has-args "emm" 1 2 3 4)
         |#
