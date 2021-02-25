(defmacro let*
"Takes list, vals, of form ((binding0 sexp0) (binding1 sexp1) ...) and evaluates
let-body with all values of binding bound to the result of the evaluation of
sexp. Differs from let in that sexps can reference bindings from previous items
in the list of vals.

Section: Threading-macros

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

(defn count
"Counts instances of item in sequence.

Section: core

Example:
(test::assert-equal 3 (count (list 1 3 5 2 4 10 2 4 88 2 1) 2))
(test::assert-equal 0 (count (list 1 3 5 2 4 10 2 4 88 2 1) 42))"
      (sequence item)
    (let ((add-if-equals-item (fn (fst nxt) (if (= nxt item) (+ 1 fst) fst))))
      (reduce add-if-equals-item 0 sequence)))

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
