(ns-create 'test)
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

(defn assert-true (value &rest args)
      (apply assert-equal t value args))

(defn assert-false (value &rest args)
      (apply assert-equal nil value args))

(ns-export '(assert-equal assert-true assert-false))
