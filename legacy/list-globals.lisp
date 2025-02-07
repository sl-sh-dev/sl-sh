#!./target/debug/sl-sh

(ns-import 'iterator)

(def m (make-hash))
;; run tests for non-root namespaces
(for a-ns in (filter (fn (x) (not (= x "user"))) (ns-list)) (do
	(def sym-list (qsort (ns-symbols a-ns)))
    (for sym in sym-list (do
                           (hash-set! m sym a-ns)
                          ))))

(for k in (qsort (hash-keys m))
     (do
       (println k)))

