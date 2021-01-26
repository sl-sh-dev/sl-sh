#!/usr/bin/env sl-sh

(ns-import 'iterator)
;; TODO to implement
;;    - nest
;;    - nest-reverse
;;    - chain-and
;;    - chain-when
;;    - chain-lambda
;;    - chain-lambda
;; replace chain impl with let*?
;; support variadic placeholder symbol
;; support ellipsis
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


(println "test chain: " (chain "a properly worded " (str "will " " become " _) (str "I " _ "statement.") (str "Ordered sentence: " _)))
(println "Chain macro expansion: " (expand-macro '(chain "my" (str _ " with " " sauce") (str "Pasta " _ ", yes please."))))

;; let* lives here so it lives in the root namespace but can use lib functions
;; from iterator.
(defmacro and-let*
"Takes list, vals, of form ((binding0 sexp0) (binding1 sexp1) ...) and evaluates
let-body with all values of binding bound to the result of the evaluation of
sexp. Differs from let in that sexps can reference bindings from previous items
in the list of vals.

Section: core

Example:
    (let* ((add-one (fn (x) (+ 1 x)))
       (double-add (fn (x) (add-one (add-one x)))))
       (test::assert-equal 4 (add-one 3))
       (test::assert-equal 6 (double-add 4)))"
(vals &rest let-body)
  (let ((reducer (fn (fst nxt)
                (var val (car nxt))
                (var bind (cadr nxt))
                (if (= 1 (length nxt))
                  `(((fn (,val) ,@fst) nil))
                  (if (= 2 (length nxt))
                         `(((fn (,val) ,@fst) ,bind))
                         (err "ERROR: invalid bindings on let*"))))))
      (car (iterator::reduce reducer let-body (iterator::reverse vals)))))

;;TODO it's not good that these error message strings are somewhat duplicated.
;;  is there a good way to make them accessible for use in the std lib
;;  w/o leaking. would such a language feature be desirable anyway.
(defmacro and-let* (vals &rest let-body)
      (let* ((rev (iterator::reverse vals))
             (init (if nil ;; (empty-seq? let-body)
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

(println "test stuff with let-body: "
         (expand-macro '(and-let* ((val (str "alpha")) (other-val (str "bravo"))) (str "charlie: (" other-val "i)"))))
(println "test stuff w/o let body:"
         (expand-macro '(and-let* ((val (str "alpha")) (other-val (str "bravo"))))))
(println "test stuff: [" (and-let* ((val (do
                                         (println "alpha")
                                         (str "alpha, ")))
                                  (other-val (do
                                                 (println "bravo " val)
                                                 (str "bravo " val)))) (str "charlie " other-val)) "]")
(println "test stuff: [" (and-let* ((val (do
                                         ;;(println "alpha")
                                         (str "alpha, ")))
                                  (other-val (do
                                                 ;;(println "bravo " val)
                                                 (str "bravo " val))))) "]")
