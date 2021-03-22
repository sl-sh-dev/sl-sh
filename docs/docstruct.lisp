;;#!/usr/bin/env sl-sh

(ns-push 'docstruct)
(ns-import 'shell)
(ns-import 'iterator)

(defn -lst-to-doc-section (section)
  (apply str (collect-vec (filter (fn (x) (not (str-empty? (str-trim x)))) section))))

(defn -until (lst stop-conditions section)
    (let ((idx 0))
      (block went-until (do
          (for line in lst
                (let ((should-stop ;; if any of the stop conditions are not filtered
                                    ;; it means a match was found and
                         (not (nil? (collect (filter (fn (x) (str-starts-with x (str-trim line))) stop-conditions))))))
                  (set! idx (+ idx 1))
                  (if should-stop
                    (return-from went-until (values (- idx 1) (-lst-to-doc-section section)))
                    (append-to! section (list (str line "\n"))))))
              (values (- idx 1) (-lst-to-doc-section section))))))

(defn make-doc-struct (a-sym) (let* ((ret (get-error (lex
    (def doc-str nil)
    (def sl-sh-type nil)
    (def namespace nil)
    (def usage nil)
    (def desc (values nil nil))
    (def sect (values nil nil))
    (def sect-key "Undefined")
    (def example (values nil nil))
    (def raw-doc (get-error (doc-raw a-sym)))
    (if (or (and (= :ok (car raw-doc))
                 (str-empty? (cdr raw-doc)))
            (not (= :ok (car raw-doc))))
      (set! doc-str (str a-sym "\n\nsee source."))
      (do
        (set! doc-str (doc a-sym))
        (def doc-vec (str-split "\n" doc-str))
        (when (>= (length doc-vec) 2)
          (set! sl-sh-type (vec-nth doc-vec 1)))
        (when (>= (length doc-vec) 3)
          (set! namespace (vec-nth doc-vec 2)))
        (when (>= (length doc-vec) 5)
          (set! usage (vec-nth doc-vec 4)))
        (when (> (length doc-vec) 5) (do
          (set! desc (-until (vec-slice doc-vec 6 (length doc-vec))
                            (list "Section:" "Example:")
                            (list)))
        (when (and (values? desc) (> (length doc-vec) (+ 5 desc))) (do
          (set! sect (-until (vec-slice doc-vec (+ 6 desc) (length doc-vec))
                              (list "Example:")
                              (list)))
          (set! sect-key (str-trim (vec-nth (str-split "Section:" (values-nth 1 sect)) 1)))
        (when (and (values? sect) (> (length doc-vec) (+ 5 desc sect))) (do
          (set! example (-until (vec-slice doc-vec (+ 6 desc sect) (length doc-vec))
                              (list)
                              (list)))))))))))
    (struct::defstruct doc-struct
        (full-doc "Full docstring" doc-str :ro)
        (name "Symbol" (str a-sym) :ro)
        (sl-sh-type "Type" sl-sh-type :ro)
        (namespace "Namespace" namespace :ro)
        (usage "Usage" usage :ro)
        (description "" (values-nth 1 desc) :ro)
        (section "Section" (values-nth 1 sect) :ro)
        (section-key "Section key" sect-key :ro)
        (example "Example" (values-nth 1 example) :ro)
        (:fn to-str (self) (do
            (println (self :full-doc)))))
       (def ds (doc-struct))
       ds))))
        (if (= :ok (car ret))
          (cdr ret)
          (do
            (println (cdr ret))
            nil))))

;;(println "meow")
;;(println "meow: " ((make-doc-struct 'doc) :to-str))
;;(println "eow: " (for d in (map make-doc-struct (list '-until 'match 'doc))(do
;;                   (println "d: " (d :to-str)))))

(ns-auto-export 'docstruct)
(ns-pop)

