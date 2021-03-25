(defmacro let*
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

(defn occurs
"Usage: (occurs (list 1 2 ...) 7) (occurs (list 1 2 ...) 0 (fn (x) (% x 2)))

Counts instances of item in sequence. Optional third argument is a function
that can be applied to the specific element in the list before equality
is tested with item.

Section: core

Example:
(test::assert-equal 7 (occurs (list 1 3 5 2 4 10 2 4 88 2 1) 0 (fn (x) (% x 2))))
(test::assert-equal 3 (occurs (list 1 3 5 2 4 10 2 4 88 2 1) 2))
(test::assert-equal 0 (occurs (list 1 3 5 2 4 10 2 4 88 2 1) 42))
(test::assert-equal 2 (occurs (list 1 3 5 2 4 10 2 4 88 2 1) 8 (fn (x) (* x 2))))
"
      (sequence item &rest args)
    (let* ((xform (if (= 0 (length args))
                   (fn (x) x)
                   (if (and (= 1 (length args)) (= (type (fn x)) (type (first args)))))
                     (first args)
                     (err "Optional third argument must be a function.")))
           (add-if-equals-item (fn (fst nxt) (if (= (xform nxt) item) (+ 1 fst) fst))))
      (iterator::reduce add-if-equals-item 0 sequence)))

(defmacro chain
"Inserts result of previous expression into place indicated by the _ symbol
in the next expression. This macro is useful when nested actions
need to occur but when a more sequential enumeration of steps is preferable.
To demonstrate a sequence of operations:
    (fn (x) (delta (charlie (bravo (alpha x)))))
can be unwound, and more idiomatically stated with the chain macro:
    (fn (x) (chain x (alpha _) (bravo _) (charlie _) (delta _)))

The flexibility of using the _ symbol as a placeholder means subsequent
results can be \"threaded\" into any desired place in a clause which is an
advantage over the thread first, [->](#root::->) and thread last,
[->>](#root::->>), macros

Section: Threading-macros

Example:

(test::assert-equal \"Ordered sentence: I will become a properly worded statement.\"
    (chain \"a properly worded \"
           (str \"will \" \"become \" _)
           (str \"I \" _ \"statement.\")
           (str \"Ordered sentence: \" _)))

(test::assert-equal 3 (chain 7 (% _ 4)))

(test::assert-equal 9 (chain 7 (% _ 4) (+ 1 8)))

(test::assert-equal (list 6 12 18) (collect (chain 7
                                    (% _ 4)
                                    (range _ 10)
                                    (map (fn (x) (* x 2)) _)
                                    (filter (fn (x) (= 0 (% x 3))) _))))"

(init arg0 &rest args)
  (do
      (if (not (nil? args))
        (vec-insert! args 0 arg0)
        (set! args (vec arg0)))
      (iterator::for elem in args (when (empty-seq? elem)
         (err "All args must be non-empty sequences.")))
      #t)
  (let* ((reducer (fn (fst nxt)
          (substitute fst '_ nxt))))
    (iterator::reduce reducer init args)))

(defmacro and-let*
"Takes list, vals, of form ((binding0 sexp0) (binding1 sexp1) ...) checking
if result of each sexp evaluates to false, short circuiting and returning nil
if so. If all vals bindings evaluate to true, then the let-body is evaluated
with all values of binding bound to the result of the evaluation of sexp.
Sexps can reference
bindings from previous items in the list of vals. If no let-body is supplied
the last binding in the list of vals is returned.

Section: core
Example:
(test::assert-equal \"charlie bravo alpha.\"
                    (and-let* ((val (str \"alpha.\"))
                               (other-val (str \"bravo \" val)))
                              (str \"charlie \" other-val)) \"]\")
(test::assert-equal \"alpha, bravo.\" (and-let* ((val (do
                                         (str \"bravo.\")))
                                  (other-val (do
                                                 (str \"alpha, \" val))))))
(test::assert-false (and-let* ((val (do (str \"alpha, \") nil))
                                   (other-val (do (str \"bravo \" val))))))
(test::assert-false (and-let* ((a-list (list 1 2 3 4 5 6 7 8))
                              (evens (filter (fn (x) (= 0 (% x 2))) a-list))
                              (doubled (map (fn (x) (* x 2)) evens))
                              (odds (collect (filter (fn (x) (= 1 (% x 2))) doubled))))
                              #t))"
  (vals &rest let-body)
    (let* ((rev (iterator::reverse vals))
           ;; if there is no let-body the last binding is returned
           (init
             (if (empty-seq? let-body)
               (let ((last-binding (iterator::next! rev)))
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

(defmacro chain-when
"Tests the car of each arg0/args pair. If true the cdr of the arg0/args pair
is evaluated. The chaining component of this macro works by threading init
through the cdr of each pair whose car evaluates to true. Useful for building
or modifying an object based on a set of boolean conditions.

Section: Threading-macros

Example:
(defn add-number-attributes (n)
    (chain-when (make-hash)
        ((not (= (% n 2) 0)) (hash-set! _ :property :odd))
        ((= (% n 2) 0) (hash-set! _ :property :even))
        ((= 0 n) (hash-set! _ :zero :zero))
        ((= 42 n) (err \"N CAN NOT BE 42\"))
        ((> n 0) (hash-set! _ :symmetry :positive))
        ((< n 0) (hash-set! _ :symmetry :negative))))

(test::assert-equal :odd (hash-get (add-number-attributes 3) :property))
(test::assert-equal :even (hash-get (add-number-attributes 4) :property))
(test::assert-false (hash-get (add-number-attributes 4) :zero))
(test::assert-equal :positive (hash-get (add-number-attributes 4) :symmetry))
(test::assert-equal :negative (hash-get (add-number-attributes -4) :symmetry))
(test::assert-equal :zero (hash-get (add-number-attributes 0) :zero))
(test::assert-error-msg (add-number-attributes 42) \"N CAN NOT BE 42\")"
  (init arg0 &rest args)
  (do
      (if (not (nil? args))
        (vec-insert! args 0 arg0)
        (set! args (vec arg0)))
      (iterator::for elem in args
           (do
             (if (empty-seq? elem)
                 (err "All args must be non-empty sequences.")
                 (when (not (= 2 (length elem)))
                   (err "Each clause in chain-when args must be of length 2.")))))
      #t)
  (let* ((reducer
           (fn (fst nxt)
             (let* ((if-true (car nxt))
                    (test (car (cdr nxt))))
               `((fn (_) (if ,if-true ,test _)) ,fst)))))
        (iterator::reduce reducer init args)))

(defmacro chain-and
"Evaluates each sexp, if true, inserts result of previous expression into place
indicated by the _ symbol. This macro is useful when nested actions
need to take place but the operations should stop and return nil if any step
evaluates to false.

Section: Threading-macros

Example:
(test::assert-false (chain-and \"howdy\" (string? _) (= _ \"howdy\")))
(test::assert-true  (chain-and \"howdy\" (str _ \" partner\") (= _ \"howdy partner\")))

(defn formatted? (alleged-time-str)
    (let* ((do-fst (fn (tst start end)
                     (fn (orig-string)
                     (let ((result (get-error
                         (chain orig-string
                            (str-sub start (- end start) _)
                            (str->int _)
                            (tst _)))))
                     (if (= :ok (car result))
                       (if (cdr result) orig-string nil)
                       nil)))))
           (valid-year? (do-fst (fn (x) (> x -1)) 0 4))
           (valid-month? (do-fst (fn (x) (and (> x 0) (< x 13))) 4 6))
           (valid-day? (do-fst (fn (x) (and (> x 0) (< x 32))) 6 8))
           (valid-hour? (do-fst (fn (x) (and (> x 0) (< x 24))) 9 11))
           (valid-minute? (do-fst (fn (x) (and (> x -1) (< x 60))) 11 13))
           (valid-second? (do-fst (fn (x) (and (> x -1) (< x 60))) 13 15))
           (is-zulu? (fn (orig-string) (= \"Z\" (str-sub 15 1 orig-string)))))
      (chain-and alleged-time-str
            (valid-year? _)
            (valid-month? _)
            (valid-day? _)
            (valid-hour? _)
            (valid-minute? _)
            (valid-second? _)
            (is-zulu? _)
            #t)))

(test::assert-true (formatted? \"20210227T154705Z\"))
(test::assert-false (formatted? \"20210227T154705P\"))
(test::assert-false (formatted? \"2021022TT154705Z\"))
(test::assert-false (formatted? \"20210237T154705Z\"))
(test::assert-false (formatted? \"2021222TT154705Z\"))
(test::assert-false (formatted? \"20210227T254705Z\"))
(test::assert-false (formatted? \"20210227T158705Z\"))
(test::assert-false (formatted? \"20210227T154795Z\"))
"
  (init arg0 &rest args)
  (do
      (if (not (nil? args))
        (vec-insert! args 0 arg0)
        (do
          (set! args (vec arg0))))
      (iterator::for elem in args (when (empty-seq? elem)
         (err "All args must be non-empty sequences.")))
      #t)
  (let* ((rev-args (iterator::collect (iterator::reverse args)))
         (first-rev (first rev-args))
         (rest-rev (iterator::collect (iterator::append-to! (rest rev-args) (list init))))
         (reducer (fn (fst nxt)
                     `((fn (_) (when _ ,fst)) ,nxt))))
        (iterator::reduce reducer first-rev rest-rev)))

(defmacro ns-auto-export
"Macro that takes a symbol, the symbol of the current namespace, and writes an
ns-export statement that includes all symbols defined in the namespaces scope
that do not begin with the '-' symbol. This is a convenience method that allows
user to avoid enumerating all symbols while also introducing a mechanism to
exclude symbols from being excluded. Note, if using ns-auto-export, it is
not possible to export a symbol that is already defined in another namespace,
if said functionality is desired the symbol must be manually exported with
another ns-export statement; ns-auto-export can be used in conjunction with
ns-export.

Section: namespace"
  (symbol)
  `(ns-export (let* ((curr-syms (iterator::filter (fn (x) (nil? (hash-get *std-lib-syms-hash* x))) (ns-symbols ,symbol)))
         (public-syms
           (iterator::filter
             (fn (x) (chain x (sym->str _) (str-starts-with "-" _) (not _)))
             curr-syms)))
    (iterator::collect public-syms))))
