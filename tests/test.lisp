(if (ns-exists? 'test) (ns-enter 'test) (ns-create 'test))
(core::ns-import 'core)

(defn lists= (list1 list2)
    (if (not (= (length list1)(length list2)))
        nil
        (if (= (length list1) 0)
            t
            (if (not (= (first list1)(first list2)))
                nil
                (recur (rest list1) (rest list2))))))

(defmacro assert-equal (expected-val right-val &rest args)
      `(if (or (list? ,expected-val)(vec? ,expected-val))
          (if (lists= ,expected-val ,right-val) t (progn (println (apply str "Expected " ,expected-val " got " ,right-val ,args (meta-file-name)(meta-line-no)(meta-column-no)))(exit 2)))
          (if (= ,expected-val ,right-val) t (progn (println (apply str "Expected " ,expected-val " got " ,right-val (meta-file-name)(meta-line-no)": "(meta-column-no),args))(exit 1)))))

(defn assert-not-equal (expected-val right-val &rest args)
      (if (or (list? expected-val)(vec? expected-val))
          (if (not (lists= expected-val right-val)) t (progn (println (apply str "Did not expect " expected-val " got " right-val args))(exit 2)))
          (if (not (= expected-val right-val)) t (progn (println (apply str "Did not expect " expected-val " got " right-val args))(exit 1)))))

(defn assert-true (value &rest args)
      (apply assert-equal t value args))

(defn assert-false (value &rest args)
      (apply assert-equal nil value args))

(defn assert-includes (value seq)
      (progn
          (def 'found nil)
          (for v seq (if (= v value) (set 'found t)))
          (if (not found) (progn (println (str value " not found in " seq))(exit 3)))))

(defn assert-not-includes (value seq)
      (progn
          (def 'found nil)
          (for v seq (if (= v value) (set 'found t)))
          (if (found) (progn (println (str value " found in " seq))(exit 3)))))

; Make this a macro to it will not create a scope and will work for namespace tests.
(defmacro run-example (sym)
	;`(eval (str "(progn "(vec-nth 1 (str-split "Example:" (doc ,sym))) ")")))
	`(eval (str "(dyn 'exit (fn (x) (err (str \"Got assert error \" x))) (progn "(vec-nth 1 (str-split "Example:" (doc ,sym))) "))")))

(ns-export '(assert-equal assert-true assert-false run-example))

(ns-pop)

