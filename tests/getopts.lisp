
(def sparse-options-map
   (make-hash
      (list
        (join :-b (make-hash (list))))))

(build-getopts-param 1 (list "bar") nil)

(assert-error-msg (getopts sparse-options-map '("-b" "1")) (bad-option-arity "-b" 0))
(assert-true (= (getopts sparse-options-map '("-b")) (make-hash (list (join :-b #t)))))
(assert-true (= (getopts sparse-options-map '()) (make-hash (list (join :-b nil)))))

(def test-options-map
    (make-hash
      (list
        (join :-l (build-getopts-param 0 #t nil))
        (join :-m (build-getopts-param 0 nil nil))
        (join :-a (build-getopts-param 1 "foo" nil))
        (join :--c-arg (build-getopts-param 1 (list "bar") nil))
        (join :--d-arg (build-getopts-param 2 nil nil))
        (join :-b (build-getopts-param 3 nil nil)))))

(assert-error-msg (getopts "foo" '("a")) options-map-is-map)
(assert-error-msg (getopts test-options-map '("a")) bad-first-arg)
(assert-error-msg (getopts test-options-map '("abc")) bad-first-arg)

(assert-error-msg (getopts test-options-map '("-a")) (bad-option-arity "-a" 1))
(assert-error-msg (getopts test-options-map '("--c-arg")) (bad-option-arity "--c-arg" 1))
(assert-error-msg (getopts test-options-map '("-a" "one-arg" "2-arg" "3")) (bad-option-arity "-a" 1))
(assert-error-msg (getopts test-options-map '("-l" "-a" "one-arg" "2-arg" "3")) (bad-option-arity "-a" 1))
(assert-error-msg (getopts test-options-map '("-b" "1" "2" "3" "-a")) (bad-option-arity "-a" 1))
(assert-error-msg (getopts test-options-map '("-b" "1" "3" "-a")) (bad-option-arity "-b" 3))
(assert-error-msg (getopts test-options-map '("-b" "1" "a" "-a" "2")) (bad-option-arity "-b" 3))
(assert-error-msg (getopts test-options-map '("-b" "1" "b")) (bad-option-arity "-b" 3))
(assert-error-msg (getopts test-options-map '("-b" "1" "2" "3" "4")) (bad-option-arity "-b" 3))
(assert-error-msg (getopts test-options-map '("-b" "1" "2" "3" "4" "--c-arg")) (bad-option-arity "-b" 3))
(assert-error-msg (getopts test-options-map '("-b" "1" "2" "3" "--c-arg")) (bad-option-arity "--c-arg" 1))
(assert-error-msg (getopts test-options-map '("-lma" "aaa" "-b" "1" "2" "3" "--c-arg" "1" "-d" "0")) (illegal-option "-d"))
(assert-error-msg (getopts test-options-map '("-lma" "aaa" "-b" "1" "2" "3" "--c-arg" "1" "-e")) (illegal-option "-e"))

(assert-error-msg (getopts test-options-map '("-ab" "an-argument")) (bad-option-arity "-a" 1))
(assert-error-msg (getopts test-options-map '("-lb" "an-argument")) (bad-option-arity "-b" 3))

(defn map= (m n)
    (var keys-in-map (fn (m n) (loop (m-keys m n last-ret) ((hash-keys m) m n #t)
                       (if (nil? last-ret)
                         nil
                         (if (empty-seq? m-keys)
                           #t
                           (recur
                             (rest m-keys)
                             m
                             n
                             (and
                               (hash-haskey n (first m-keys))
                               (= (hash-get m (first m-keys)) (hash-get n (first m-keys))))))))))
    (and (hash? m) (hash? n) (= (length (hash-keys m)) (length (hash-keys n)))
      (keys-in-map m n)
      (keys-in-map n m)))

(assert-true
    (map=
        (getopts test-options-map '("-lmb" "1" "2" "3"))
        (make-hash
          (list
            (join :-l #t)
            (join :-m #t)
            (join :-a "foo")
            (join :--c-arg (list "bar"))
            (join :--d-arg nil)
            (join :-b '#("1" "2" "3"))))))

(assert-true
    (map=
        (getopts test-options-map '("-b" "1" "2" "3" "-lm" "--c-arg" "bar"))
        (make-hash
          (list
            (join :-l #t)
            (join :-m #t)
            (join :-a "foo")
            (join :--c-arg "bar")
            (join :--d-arg nil)
            (join :-b '#("1" "2" "3"))))))

(assert-false
    (map=
        (getopts test-options-map '("-b" "1" "2" "3" "-lm"))
        (make-hash
          (list
            (join :-l #t)
            (join :-m #t)
            (join :-a "foo")
            (join :--c-arg "bar")
            (join :--d-arg nil)
            (join :-b '#("1" "2" "3"))))))

(assert-true
    (map=
        (getopts test-options-map '("-b" "1" "2" "3" "-lm"))
        (make-hash
          (list
            (join :-l #t)
            (join :-m #t)
            (join :-a "foo")
            (join :--c-arg (list "bar"))
            (join :--d-arg nil)
            (join :-b '#("1" "2" "3"))))))

(assert-true
  (map=
    (getopts test-options-map '("-a" "1"))
    (make-hash
      (list
        (join :-l #t)
        (join :-m nil)
        (join :-a "1")
        (join :--c-arg (list "bar"))
        (join :--d-arg nil)
        (join :-b nil)))))

(assert-true
    (map=
        (getopts test-options-map '("-l" "-a" "one-arg"))
        (make-hash
          (list
            (join :-l #t)
            (join :-m nil)
            (join :-a "one-arg")
            (join :--c-arg (list "bar"))
            (join :--d-arg nil)
            (join :-b nil)))))

(assert-true
    (map=
        (getopts test-options-map '("-b" "1" "2" "3"))
        (make-hash
          (list
            (join :-l #t)
            (join :-m nil)
            (join :-a "foo")
            (join :--c-arg (list "bar"))
            (join :--d-arg nil)
            (join :-b '#("1" "2" "3"))))))

(assert-true
    (map=
        (getopts test-options-map '("-lma" "aaa" "-b" "1" "2" "3" "--c-arg" "1"))
        (make-hash
          (list
            (join :-l #t)
            (join :-m #t)
            (join :-a "aaa")
            (join :--c-arg "1")
            (join :--d-arg nil)
            (join :-b '#("1" "2" "3"))))))

(assert-true
    (map=
        (getopts test-options-map '("-lma" "aaa" "-b" "1" "2" "3" "--c-arg" "1" "--d-arg" "1" "2"))
        (make-hash
          (list
            (join :-l #t)
            (join :-m #t)
            (join :-a "aaa")
            (join :--c-arg "1")
            (join :--d-arg '#("1" "2"))
            (join :-b '#("1" "2" "3"))))))

(assert-true
    (map=
    (getopts test-options-map '("-l"))
    (make-hash
      (list
        (join :-l #t)
        (join :-m nil)
        (join :-a "foo")
        (join :--c-arg (list "bar"))
        (join :--d-arg nil)
        (join :-b nil)))))

(assert-true
    (map=
    (getopts test-options-map '("-lb" "1" "2" "3" "-a" "1"))
    (make-hash
      (list
        (join :-l #t)
        (join :-m nil)
        (join :-a "1")
        (join :--c-arg (list "bar"))
        (join :--d-arg nil)
        (join :-b '#("1" "2" "3"))))))

(assert-error-msg
  (getopts
    (make-hash
      (list
        (join :-l (build-getopts-param 0 #t :true?))))
    '("-l"))
  invalid-type-function)

(assert-error-msg
  (getopts
    (make-hash
      (list
        (join :-l (build-getopts-param 0 #t :true?))))
    '("-l"))
  invalid-type-function)


(assert-error-msg
  (getopts (make-hash (list (join :-c (build-getopts-param 1 nil :float?)))) '("-c" "#\a"))
  "str->float: string is not a valid float")

(lex

(assert-error-msg
  (getopts (make-hash (list (join :-i (build-getopts-param 1 nil :int?)))) '("-i" "1.23"))
  "str->int: string is not a valid integer")

(var i-int-bindings
 (hash-get
   (getopts (make-hash (list (join :-i (build-getopts-param 1 nil :int?)))) '("-i" "1"))
   :-i))
(assert-true (int? i-int-bindings) ". Return value should be of type Int.")

(var f-float-bindings
 (hash-get
   (getopts (make-hash (list (join :-f (build-getopts-param 1 nil :float?)))) '("-f" "1.3"))
   :-f))
(assert-true (float? f-float-bindings) ". Return value should be of type Float.")

(var f-float-vec-bindings
 (hash-get
   (getopts (make-hash (list (join :-f (build-getopts-param 2 nil :float?)))) '("-f" "1.3" "0.12"))
   :-f))
(for f in f-float-vec-bindings (assert-true (float? f) ". Return value should be of type Float."))

(var fs-file-vec-bindings
(hash-get
(getopts (make-hash (list (join :-f (build-getopts-param 2 nil :fs-file?))))
         '("-f" "/etc/fstab" "/etc/passwd"))
           :-f))
(for f in fs-file-vec-bindings (assert-true (fs-file? f) ". Return value should pass test fs-file?."))

(var fs-dir-bindings
     (hash-get
       (getopts (make-hash (list (join :--fs-dir (build-getopts-param 1 nil :fs-dir?))))
                '("--fs-dir" "/tmp"))
       :--fs-dir))
(assert-true (fs-dir? fs-dir-bindings) ". Return value should pass test fs-dir?.")

(var fs-exists-bindings
    (hash-get
      (getopts (make-hash (list (join :--exists (build-getopts-param 1 nil :fs-exists?))))
               '("--exists" "/etc/fstab"))
      :--exists))
(assert-true (fs-exists? fs-exists-bindings) ". Return value should pass test fs-exists?.")

(assert-error-msg
  (getopts (make-hash (list (join :--not-exists (build-getopts-param 1 nil :fs-exists?))))
           '("--not-exists" "/tmp/this/file/does/not/exist/i/hope/lakjdslfakjdlkfjdlkjfaslfkjlksdj"))
  "Argument, /tmp/this/file/does/not/exist/i/hope/lakjdslfakjdlkfjdlkjfaslfkjlksdj, should pass test fs-exists?")

(var symbol-bindings
    (hash-get
      (getopts (make-hash (list (join :-s (build-getopts-param 1 nil :symbol?))))
               '("-s" ":a-keyword-symbol"))
      :-s))
(assert-true (symbol? symbol-bindings) ". Return value should be of type Symbol?.")

(var c-char-bindings
 (hash-get
   (getopts (make-hash (list (join :-c (build-getopts-param 1 nil :char?)))) (list "-c" #\a))
   :-c))
(assert-true (char? c-char-bindings) ". Return value should be of type Char.")

(assert-error-msg
  (getopts
    (make-hash (list (join :-c (build-getopts-param 1 nil :char?))))
       (list "-c" (list #\a)))
  (type-error-message ":-c" (list #\a) ":char?"))

(var hash-bindings
 (hash-get
   (getopts (make-hash (list (join :-h (build-getopts-param 1 nil :hash?)))) (list "-h" (make-hash (list (join :-h "meow")))))
   :-h))
(assert-true (hash? hash-bindings) ". Return value should be of type HashMap.")

(var nil-bindings
 (hash-get
   (getopts (make-hash (list (join :-h (build-getopts-param 1 nil :nil?)))) '("-h" nil))
   :-h))
(assert-true (nil? nil-bindings) ". Return value should be of type Nil.")

(assert-error-msg
   (getopts (make-hash (list (join :-h (build-getopts-param 1 nil :nil?)))) '("-h" "nickel"))
   (type-error-message ":-h" "nickel" ":nil?"))

(var list-bindings
 (hash-get
;;   (getopts (make-hash (list (join :-h (build-getopts-param 1 nil :list?)))) '("-h" (arg1 arg2)))
   (getopts (make-hash (list (join :-h (build-getopts-param 1 nil :list?)))) (list "-h" (list 'arg1 'arg2)))
   :-h))
(assert-true (= 'arg1 (car list-bindings)) ". First arg not correct symbol.")
(assert-true (= 'arg2 (cadr list-bindings)) ". Second arg not correct symbol.")

(var list-bindings2
 (hash-get
   (getopts (make-hash (list (join :-h (build-getopts-param 1 nil :list?)))) (list "-h" (list "arg1" "arg2")))
   :-h))
(assert-true (= "arg1" (car list-bindings2)) ". First arg not correct string.")
(assert-true (= "arg2" (cadr list-bindings2)) ". Second arg not correct string.")

(assert-error-msg
   (getopts (make-hash (list (join :-h (build-getopts-param 1 nil :list?)))) '("-h" "(list nxx nxx)"))
   (type-error-message  ":-h" "(list nxx nxx)" ":list?"))

(var vec-bindings
 (hash-get
   (getopts (make-hash (list (join :-h (build-getopts-param 1 nil :vec?)))) (list "-h" '#(1 2 3 4)))
   :-h))
(assert-true (vec? vec-bindings) ". Return value should be of type Vector.")
(assert-true (= (vec-nth vec-bindings 0) '1) ". Idx 0 is wrong.")
(assert-true (= (vec-nth vec-bindings 1) '2) ". Idx 2 is wrong.")
(assert-true (=(vec-nth vec-bindings 2) '3) ". Idx 3 is wrong.")
(assert-true (= (vec-nth vec-bindings 3) '4) ". Idx 3 is wrong.")

(var vec-bindings2
 (hash-get
   (getopts (make-hash (list (join :-h (build-getopts-param 1 nil :vec?)))) (list "-h" (vec 1 2 3 4)))
   :-h))
(assert-true (vec? vec-bindings2) ". Return value should be of type Vector.")
(assert-true (= (vec-nth vec-bindings2 0) 1) ". Idx 0 is wrong.")
(assert-true (= (vec-nth vec-bindings2 1) 2) ". Idx 2 is wrong.")
(assert-true (=(vec-nth vec-bindings2 2) 3) ". Idx 3 is wrong.")
(assert-true (= (vec-nth vec-bindings2 3) 4) ". Idx 3 is wrong.")

(assert-error-msg
   (getopts (make-hash (list (join :-h (build-getopts-param 1 nil :vec?)))) (list "-h" (list 'nxx 'nxx)))
   (type-error-message  ":-h" (list 'nxx 'nxx) ":vec?"))

(var macro-bindings
     (hash-get
         (getopts
           (make-hash
             (list (join :--macro (build-getopts-param 1 nil :macro?))))
           (list "--macro" (macro (x) x)))
         :--macro))

(assert-true (macro? macro-bindings) ". Return value should be of type macro.")
(var lambda-bindings
     (hash-get
         (getopts
           (make-hash
             (list (join :--lambda (build-getopts-param 1 nil :lambda?))))
           (list "--lambda" (fn (x) x)))
         :--lambda))
(assert-true (lambda? lambda-bindings) ". Return value should be of type lambda."))
