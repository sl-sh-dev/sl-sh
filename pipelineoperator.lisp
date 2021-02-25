#!/usr/bin/env sl-sh

(ns-import 'iterator)
(ns-import 'test)

;; TODO need tests and a docstring

(defn verify-chain-when-args (arg0 args)
      (vec-insert! args 0 arg0)
      (for elem in args
           (do
             (if (empty-seq? elem)
                 (err "All args must be non-empty sequences.")
                 (when (not (= 2 (length elem)))
                   (err "Each clause in chain-when args must be of length 2.")))))
      #t)

(defn verify-chain-args (arg0 args)
      (vec-insert! args 0 arg0)
      (for elem in args (when (empty-seq? elem)
         (err "All args must be non-empty sequences.")))
      #t)

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

(test::assert-equal "Ordered sentence: I will become a properly worded statement."
    (chain "a properly worded "
           (str "will " "become " _)
           (str "I " _ "statement.")
           (str "Ordered sentence: " _)))

;;(test::assert-equal 42
;;   (chain 7 (% _ 4)))

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

(defn add-number-attributes (n)
    (chain-when (make-hash)
        ((not (= (% n 2) 0)) (hash-set! _ :property "odd"))
        ((= (% n 2) 0) (hash-set! _ :property "even"))
        ((= 0 n) (hash-set! _ :zero "zero"))
        ((> n 0) (hash-set! _ :symmetry "positive"))
        ((< n 0) (hash-set! _ :symmetry "negative"))))

(test::assert-equal "odd" (hash-get (add-number-attributes 3) :property))
(test::assert-equal "even" (hash-get (add-number-attributes 4) :property))
(test::assert-false (hash-get (add-number-attributes 4) :zero))
(test::assert-equal "positive" (hash-get (add-number-attributes 4) :symmetry))
(test::assert-equal "negative" (hash-get (add-number-attributes -4) :symmetry))
(test::assert-equal "zero" (hash-get (add-number-attributes 0) :zero))
