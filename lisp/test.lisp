(if (ns-exists? 'test) (ns-enter 'test) (ns-create 'test))

(ns-import 'iterator)

(defn lists= (list1 list2)
    (if (not (= (length list1)(length list2)))
        nil
        (if (= (length list1) 0)
            t
            (if (not (= (first list1)(first list2)))
                nil
                (recur (rest list1) (rest list2))))))

(defn pair= (pair1 pair2)
    (if (not (and (pair? pair1)(pair? pair2))) nil
        (and (null pair1)(null pair2)) t
        (and (null pair1)(not (null pair2))) nil
        (and (not (null pair1))(null pair2)) nil
        (not (= (car pair1)(car pair2))) nil
        (not (= (cdr pair1)(cdr pair2))) nil
        t))

(defn assert-equal (expected-val right-val &rest args)
      (if (or (list? expected-val)(vec? expected-val))
              (if (lists= expected-val right-val) t
                  (progn (println (apply str "Expected " expected-val " got " right-val args))(exit 2)))
          (pair? expected-val)
              (if (pair= expected-val right-val) t
                  (progn (println (apply str "Expected " expected-val " got " right-val args))(exit 1)))
          (= expected-val right-val) t
          (progn (println (apply str "Expected " expected-val " got " right-val args))(exit 1))))

(defn assert-not-equal (expected-val right-val &rest args)
      (if (or (list? expected-val)(vec? expected-val))
              (if (not (lists= expected-val right-val)) t
                  (progn (println (apply str "Did not expect " expected-val " got " right-val args))(exit 2)))
          (pair? expected-val)
              (if (not (pair= expected-val right-val)) t
                  (progn (println (apply str "Did not expect " expected-val " got " right-val args))(exit 1)))
          (not (= expected-val right-val)) t
          (progn (println (apply str "Did not expect " expected-val " got " right-val args))(exit 1))))

(defn assert-true (value &rest args)
      (apply assert-equal t value args))

(defn assert-false (value &rest args)
      (apply assert-equal nil value args))

(defn assert-includes (value seq)
      (progn
          (def 'found nil)
          (for v in seq (if (= v value) (set 'found t)))
          (if (not found) (progn (println (str value " not found in " seq))(exit 3)))))

(defn assert-not-includes (value seq)
      (progn
          (def 'found nil)
          (for v in seq (if (= v value) (set 'found t)))
          (if found (progn (println (str value " found in " seq))(exit 3)))))

(defmacro assert-error (form)
    `(test::assert-equal :error (car (get-error ,form)) " Expected ERROR, did not get it!"))

; Make this a macro to it will not create a scope and will work for namespace tests.
(defmacro run-ns-example (sym)
    `(eval (str "(dyn 'exit (fn (x) (err (str \"Got assert error \" x))) (progn "(vec-nth 1 (str-split "Example:" (doc ,sym))) "))")))

(defmacro run-example (sym)
    `(progn
        (defq doc-list (str-split "Example:" (str (doc ,sym))))
        (if (> (length doc-list) 1)
            (progn
             (eval (str "(progn " (println (vec-nth 1 doc-list)) (str (vec-nth 1 doc-list)) ")")))
            (progn
             :no-test))))

(ns-export '(assert-equal assert-not-equal assert-true assert-false assert-error run-example))

(ns-pop)
