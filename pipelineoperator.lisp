#!/usr/bin/env sl-sh

(ns-import 'iterator)
(ns-import 'test)

;; TODO need tests and a docstring

(defn verify-chain-when-args (arg0 args)
      (vec-insert! args 0 arg0)
      (for elem in args
           (do
             (if (empty-seq? elem)
                 (err "All args must be non-empty sequences that
                  contain the _ symbol.")
                 (when (not (= 2 (length elem)))
                   (err "Each clause in chain-when args must be of length 2.")))))
      #t)

(defn verify-chain-args (arg0 args)
      (vec-insert! args 0 arg0)
      (for elem in args (when (empty-seq? elem)
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

(defmacro chain-and (init arg0 &rest args)
  (verify-chain-args arg0 args)
  (let* ((rev-args (collect (reverse args)))
         (first-rev (first rev-args))
         (rest-rev (collect (append-to! (rest rev-args) (list init))))
         (reducer (fn (fst nxt)
                     `((fn (_) (when _ ,fst)) ,nxt))))
        (reduce reducer first-rev rest-rev)))

(test::assert-false (chain-and "howdy" (string? _) (= _ "howdy")))
(test::assert-true  (chain-and "howdy" (str _ " partner") (= _ "howdy partner")))

(defmacro chain-when (init arg0 &rest args)
  (verify-chain-when-args arg0 args)
  (let* ((reducer
           (fn (fst nxt)
             (let* ((if-true (car nxt))
                    (test (car (cdr nxt))))
               `((fn (_) (if ,if-true ,test _)) ,fst)))))
        (reduce reducer init args)))

(defn describe-number (n)
    (chain-when ""
        ((not (= (% n 2) 0)) (str "odd" _))
        ((= (% n 2) 0) (str "even" _))
        ((= 0 n) (str "zero" _))
        ((> n 0) (str "positive" _))))

(test::assert-true (str-contains "odd" (describe-number 3)))
(test::assert-true (str-contains "positive" (describe-number 3)))
(test::assert-true (str-contains "even" (describe-number 4)))
(test::assert-true (str-contains "positive" (describe-number 4)))
