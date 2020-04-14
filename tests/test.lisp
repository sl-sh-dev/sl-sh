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

(defn assert-equal (expected-val right-val &rest args)
      (if (or (list? expected-val)(vec? expected-val))
          (if (lists= expected-val right-val) t (progn (println (apply str "Expected " expected-val " got " right-val args))(exit 2)))
          (if (= expected-val right-val) t (progn (println (apply str "Expected " expected-val " got " right-val args))(exit 1)))))

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

(defmacro run-example (sym)
	`(progn
		(defq doc-list (str-split "Example:" (doc ,sym)))
		(if (> (length doc-list) 1)
			(eval (str "(progn " (str (vec-nth 1 doc-list)) ")"))
			:no-test)))

(ns-export '(assert-equal assert-true assert-false run-example))

(ns-pop)

