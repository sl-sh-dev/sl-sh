(ns-push 'test)

(ns-import 'iterator)

(defn lists=
    "Test list/vec for equality

    Section: test

    Example:
    (test::assert-true (list= (list 1 2 3) (vec 1 2 3)))
    (test::assert-false (list= (list 1 2 3) (vec 1 2 4)))
    "
    (list1 list2)
    (if (not (= (length list1)(length list2)))
        nil
        (if (= (length list1) 0)
            t
            (if (not (= (first list1)(first list2)))
                nil
                (recur (rest list1) (rest list2))))))

(defn pair=
    "Test pair for equality

    Section: test

    Example:
    (test::assert-true (pair= (join 1  2) (join 1 2)))
    (test::assert-false (pair= (join 1 3) (join 1 4)))
    "
    (pair1 pair2)
    (if (not (and (pair? pair1)(pair? pair2))) nil
        (and (null pair1)(null pair2)) t
        (and (null pair1)(not (null pair2))) nil
        (and (not (null pair1))(null pair2)) nil
        (not (= (car pair1)(car pair2))) nil
        (not (= (cdr pair1)(cdr pair2))) nil
        t))

(defn assert-equal
    "Test expected and actual for equality.

    Section: test

    Example:
    (test::assert-true (test::assert-equal 1 1))
    (test::assert-false (test::assert-equal 1 2))
    "
   (expected-val right-val &rest args)
   (if (or (list? expected-val)(vec? expected-val))
          (if (lists= expected-val right-val) t
              (do (println (apply str "Expected " expected-val " got " right-val args))(exit 2)))
      (pair? expected-val)
          (if (pair= expected-val right-val) t
              (do (println (apply str "Expected " expected-val " got " right-val args))(exit 1)))
      (= expected-val right-val) t
      (do (println (apply str "Expected " expected-val " got " right-val args))(exit 1))))

(defn assert-not-equal
    "Test expected and actual are not equal.

    Section: test

    Example:
    (test::assert-false (test::assert-not-equal 1 1))
    (test::assert-true (test::assert-not-equal 1 2))
    "
      (expected-val right-val &rest args)
      (if (or (list? expected-val)(vec? expected-val))
              (if (not (lists= expected-val right-val)) t
                  (do (println (apply str "Did not expect " expected-val " got " right-val args))(exit 2)))
          (pair? expected-val)
              (if (not (pair= expected-val right-val)) t
                  (do (println (apply str "Did not expect " expected-val " got " right-val args))(exit 1)))
          (not (= expected-val right-val)) t
          (do (println (apply str "Did not expect " expected-val " got " right-val args))(exit 1))))

(defn assert-true
    "Test for truthiness.

    Section: test

    Example:
    (test::assert-true #t)
    "
      (value &rest args)
      (apply assert-equal t value args))

(defn assert-false
    "Test for falsiness.

    Section: test

    Example:
    (test::assert-false #f)
    (test::assert-false nil)
    "
      (value &rest args)
      (apply assert-equal nil value args))

(defn assert-includes (value seq)
  (let ((found nil))
    (for v in seq (if (= v value) (set! found t)))
    (if (not found) (do (println (str value " not found in " seq))(exit 3)))))

(defn assert-not-includes (value seq)
  (let ((found nil))
    (for v in seq (if (= v value) (set! found t)))
    (if found (do (println (str value " found in " seq))(exit 3)))))

(defmacro assert-error (form)
    `(test::assert-equal :error (car (get-error ,form)) ". Expected :error"))

(defmacro assert-error-msg (form msg)
    `((fn (ret) (test::assert-true (and (= :error (car ret)) (= ,msg (cadr ret)))
     ". Expected :error, with message: \n" ,msg "\n => Test returned:\n\"" (if (pair? (cdr ret)) (cadr ret) (cdr ret)) "\""))
        (get-error ,form)))

; Make this a macro to it will not create a scope and will work for namespace tests.
(defmacro run-ns-example (sym)
    `(eval (read (str "(dyn exit (fn (x) (err (str \"Got assert error \" x))) (do "(vec-nth (str-split "Example:" (doc ,sym)) 1) "))"))))

(defmacro run-example
      "Given a symbol, run any tests found in example section if exists."
     (sym)
    `(let
        ((doc-list (str-split "Example:" (str (doc ,sym)))))
        (if (> (length doc-list) 1)
            (do
             (eval (read (str "(do " (println (vec-nth doc-list 1)) (str (vec-nth doc-list 1)) ")"))))
            (do
             :no-test))))

(ns-export '(assert-equal assert-not-equal assert-true assert-false assert-error assert-error-msg run-example))

(ns-pop)

