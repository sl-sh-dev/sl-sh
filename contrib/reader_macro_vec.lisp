(defn reader_macro_vec (stream ch) (progn
    (def 'ret (vec))
    (loop (val) ((read stream))
        (progn
          (vec-push! ret val)
          (if (and (not (str-iter-empty? stream))(not (= (str-iter-peek stream) #\])))
              (recur (read stream)))))
    (if (not (= #\] (next! stream))) (err "Malformed []"))
    ret))

(def '*read-table* (make-hash '((#\[ . reader_macro_vec))))
(def '*read-table-end-char* (make-hash '((#\[ . #\]))))
