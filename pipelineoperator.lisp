#!/usr/bin/env sl-sh

(ns-import 'iterator)
(ns-import 'test)

(defmacro chain-and
"Evaluates each sexp, if true, inserts result of previous expression into place
inidicated by the _ symbol. This macro is useful when nested actions
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
                            (str-slice _ start end)
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
           (is-zulu? (fn (orig-string) (= \"Z\" (str-slice orig-string 15 16)))))
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
(test::assert-false (formatted? \"20210227T154795Z\"))"
  (init arg0 &rest args)
  (do
      (if (not (nil? args))
        (vec-insert! args 0 arg0)
        (do
          (set! args (vec arg0))))
      (for elem in args (when (empty-seq? elem)
         (err "All args must be non-empty sequences.")))
      #t)
  (let* ((rev-args (collect (iterator::reverse args)))
         (first-rev (first rev-args))
         (rest-rev (collect (append-to! (rest rev-args) (list init))))
         (reducer (fn (fst nxt)
                     `((fn (_) (when _ ,fst)) ,nxt))))
        (iterator::reduce reducer first-rev rest-rev)))
