#!/home/price/development/slsh/target/debug/sl-sh

(ns-import 'shell)
(ns-import 'test)
(ns-import 'iterator)

;; TODO do keys have to start with :-?
;; TODO need to validate keys in the options-map :(
(defmacro debugln (&rest args)
    (if (nil? nil)
        `(println "=> " ,@args)))

(def 'sample "-la -c -b")

(def 'token-delim "-")

(def 'options-map-is-map "Getopts first argument, options-map, must pass test hash?.")

(def 'bad-first-arg "First argument must be a flag.")

(defn bad-option-arity (option expected)
    (str "Wrong number of arguments passed to " option ". Expected " expected
         " arguments."))

(defn type-error-message (key binding opt-type-fun)
    (str "Input types did not match :type specified in options-map. At argument " key ", failed to read in provided " binding " as type " opt-type-fun ". Binding, " binding ", was of type " (type binding) "."))

(defn is-single-char-arg (token)
    (and (= 2 (length token)) (str-starts-with token-delim token)))

(defn is-multi-char-arg (token)
    (str-starts-with (str token-delim token-delim) token))

(defn is-multi-single-char-args (token)
    (str-starts-with token-delim token))

(defn get-next-params
"this function looks through the vec-args and returns only those vec-args that
are meant to be the parameters to the command flag at idx, i.e.
the vec-args #(\"-a\" \"-b\" \"foo\" \"-c\")
-a has no intended params, because there are no string values after -a and
before the next token delimeted (-) option, in this case, -b, so if idx
was 0, get-next-params would return the empty vector. If the idx was 1,
get-next-params would return \"foo\" since that is the rest of the vector
up until the next token delimeted option, -c.
"
    (idx vec-args)
    (var 'possible-params (vec-slice vec-args (+ idx 1) (length vec-args)))
    ;; possible params is nil if at the end of a list, return an empty vec 
    ;; indicating no paramaters
    (var 'potential-params (make-vec))
    (if (nil? possible-params)
      potential-params
      (loop (params) (possible-params)
          (if (or (empty-seq? params) (str-starts-with token-delim (first params)))
            potential-params
            (progn
                (vec-push! potential-params (first params))
                (recur (rest params)))))))

(defn is-getopts-option-string (arg)
    (and (string? arg) (str-starts-with token-delim arg)))

(defn illegal-option (key)
    (str "Illegal option " key ", not in allowable arguments provided to getopts."))

(defn verify-arity (idx given-args options-map bindings-map)
    (var 'option (vec-nth given-args idx))
    (var 'key (to-symbol (str ":" option)))
    (var 'arity-map (hash-get options-map key))
    (var 'arity (if (nil? arity-map)
                  0
                  (hash-get arity-map :arity 0)))
    (when (nil? arity-map)
      (err (illegal-option option)))
    ;; in case we are at end of args vector but the last option expects more
    ;; params than rest of args vector has after idx.
    (when (>= (+ idx arity) (length given-args))
      (err (bad-option-arity option arity)))
    ;; since all options start with a " -" use a str-split/str-cat trick
    ;; to get a vector representing all args to current option
    (var 'potential-args (get-next-params idx given-args))
    (debugln "potential-args: " potential-args ", type: " (type potential-args))
      (when (not (= (length potential-args) arity))
          (err (bad-option-arity option arity)))
    (hash-set! bindings-map key (if (empty-seq? potential-args) #t potential-args)))

(defn verify-all-options-valid (cmd-line-args options-map bindings-map)
    (var 'vec-args (collect-vec cmd-line-args))
    (debugln "vec-args: " vec-args)
    (for-i idx cmd in vec-args
         (do
           (debugln "cmd: " (vec-nth vec-args idx) ", idx: " idx)
           (cond
             ((is-multi-char-arg cmd) (verify-arity idx vec-args options-map bindings-map))
             ((is-single-char-arg cmd) (verify-arity idx vec-args options-map bindings-map))
             ((is-multi-single-char-args cmd)
                 (progn
                 ;; if the command in question looked like "-ab", de-multi-single-arged-str
                 ;; will be "-a -b" this way the new cmd-line-args list can
                 ;; be fed recursively to verify-all-options-valid
                 (var 'de-multi-single-arged-str
                      (str-split " " (str token-delim
                           (str-cat-list (str " " token-delim)
                           (collect-vec (str-replace (vec-nth vec-args idx) token-delim ""))))))
                 (var 'sub-vec (map str (append de-multi-single-arged-str (slice vec-args (+ 1 idx) (length vec-args)))))
                   (verify-all-options-valid sub-vec options-map bindings-map)
                   "a"))))))

(defn printtype (thing)
      (println  "thing: " thing ", type: " (type thing)))

(defn build-getopts-param (arity default type-fun)
    (printtype arity)
    (printtype default)
    (printtype type-fun)
    (make-hash
        (list
            (join :arity arity)
            (join :default default)
            (join :type type-fun))))

(defn valid-first-arg? (args)
    (when (not (is-getopts-option-string (first args)))
        (err bad-first-arg)))

(def 'nyi "not-yet-implemented")

(defn make-hash-with-keys (hmap)
    (make-hash (collect (map (fn (x) (join (to-symbol x) nil)) (hash-keys hmap)))))

(defn fix-one-arg-bindings
"Ensure binding with arity of 1 is bound to actual value passed in, not
a vector whose only element is the desired binding."
      (options-map bindings-map)
    (loop (keys bindings-map) ((hash-keys bindings-map) bindings-map)
        (when (not (empty-seq? keys))
            (progn
             (var 'key (first keys))
             (var 'opt-config (hash-get options-map key))
             (var 'opt-arity (hash-get opt-config :arity))
             (var 'binding (hash-get bindings-map key))
             (when (and (not (nil? opt-arity)) (= 1 opt-arity) (seq? binding) (= 1 (length binding)))
               (hash-set! bindings-map key (first binding)))
             (recur (rest keys) bindings-map)))))

(defn apply-defaults (options-map bindings-map)
    (loop (keys bindings-map) ((hash-keys options-map) bindings-map)
        (when (not (empty-seq? keys))
            (progn
             (var 'key (first keys))
             (var 'opt-config (hash-get options-map key))
             (when (and (hash-haskey opt-config :default) (nil? (hash-get bindings-map key)))
               (hash-set! bindings-map key (hash-get opt-config :default)))
             (recur (rest keys) bindings-map)))))

(defn check
"accept a predicate, return a function that takes a string and a custom
error message. If the given (predicate string) is true the string is returned,
otherwise the error message is thrown."
  (predicate)
  (fn (string custom-message)
    (if (predicate string)
      string
      (err custom-message))))

(defn check-custom
"accept a predicate and a custom error message, return a function that takes a
string and an unused argument. If the given (predicate string) is true the string is returned,
otherwise the error message is thrown."
  (predicate custom-message)
  (fn (string unused)
    (if (predicate string)
      string
      (err (custom-message string)))))

(defn fn-to-predicate
"accept a fn and a predicate, return a function that takes a string and a custom
error message. If the given (applier (predicate string)) is true the string is returned,
otherwise the error message is thrown."
  (applier predicate)
  (fn (string custom-message)
    (var 'res (applier string))
    (if (predicate res)
      res
      (err custom-message))))

(def 'supported-types-map
    (make-hash
      (list
        (join :int? (fn (x unused) (str->int x)))
        (join :float? (fn (x unused) (str->float x)))
        (join :fs-file? (check-custom fs-file? (fn (x) (str "Argument, " x ", should pass test fs-file?"))))
        (join :fs-dir? (check-custom fs-dir? (fn (x) (str "Argument, " x ", should pass test fs-dir?"))))
        (join :fs-exists? (check-custom fs-exists? (fn (x) (str "Argument, " x ", should pass test fs-exists?"))))
        (join :symbol? (fn-to-predicate to-symbol symbol?))
        (join :string? (fn (x unused) (str x)))
        (join :char? (check char?))
        (join :hash? (check hash?))
        (join :nil? (check nil?))
        (join :list? (check list?))
        (join :vec? (check vec?))
        (join :lambda? (check lambda?))
        (join :macro? (check macro?)))))

(def 'invalid-type-function (str "Type function must be one of " (hash-keys supported-types-map)))

(defn enforce-types (options-map bindings-map)
    (loop (keys bindings-map) ((hash-keys options-map) bindings-map)
        (when (not (empty-seq? keys))
            (progn
             (var 'key (first keys))
             (var 'opt-config (hash-get options-map key))
             (var 'opt-type-arity (hash-get opt-config :arity))
             (var 'opt-type-fun (hash-get opt-config :type))
             (var 'default (hash-get opt-config :default))
             (var 'binding (hash-get bindings-map key))
             (when (and (not (nil? opt-type-arity))
                        (not (nil? opt-type-fun)))
               (progn
                 (if (not (in? (hash-keys supported-types-map) opt-type-fun))
                   (err invalid-type-function)
                   (progn
                     (when (and (not (= binding default)) (not (= 0 opt-type-arity)))
                       (progn
                         (debugln "look our binding is: " binding ", of type: " (type binding))
                         (var 'err-str (type-error-message key binding opt-type-fun))
                         (if (= 1 opt-type-arity)
                             (hash-set!
                               bindings-map
                               key
                               ((hash-get supported-types-map opt-type-fun) binding err-str))
                             (hash-set!
                               bindings-map
                               key
                               (collect-vec
                                 (map
                                   (fn (x) ((hash-get supported-types-map opt-type-fun) x err-str))
                                   binding))))
                         (debugln "look our binding is now: " (hash-get bindings-map key) ", of type: " (type (hash-get bindings-map key)))
                         ))))))
             (recur (rest keys) bindings-map)))))

(defn getopts
"Getopts takes a hash map and a vector of args and returns a hash map with all
the values extracted from the args and bound to the corresponding keys in the
provided hash map.

Take this example script:
sample-getopts.lisp
----------------
#!/usr/bin/env sl-sh

(println \"Passing: \" args \" to getopts\")
;; getopts is given a hash map with one key, :-m, that corresponds to the flag,
;; -m, that it configures.
(var 'bindings
     (getopts
       (make-hash
         (list (join
                 :-m
                 (make-hash (list
                              (join :arity 1)
                              (:default 0)
                              (:type :int?))))))
        args))
(println \"The binding for -m is: \" (hash-get bindings :-m) \"of type \" (type (hash-get bindings :-m)))
----------------

Running the script with one argument to the -m flag yields:
$> ./sample-getopts.lisp -m 7
=> Passing: #(\"-m\" \"7\") to getopts
=> The binding for -m is 7 of type Int

The hash map for the key :-m showcases the configuration keys that exist for
each flag: arity, :default, and :type. :arity specifies that the -m flag will
take one argument. :default specifies the bindng for :-m should be 0 if the
-m flag is not seen in args. :type :int? specifies that the binding should
conform to that type, in this case int?. Running the script again with no -m
flag yields:
$> ./sample-getopts.lisp
=> Passing: #() to getopts
=> The binding for -m is 0 of type Int

Demonstrating the :default binding of 0 for the symbol :-m since the -m flag
was not provided as an argument to the script.


Configuration keys for flags:
- :arity (optional)
    Defaults to 0. If the arity is 0 the returned binding will be #t or nil.
Can be any integer. The integer value for the :arity corresponds to the
number of arguments getopts will enforce for this flag, if the number of
arguments provided to the flag is not equal to the specified arity an error
is thrown.
- :default (optional)
    Use this as a default if the given flag is not provided at execution time.
- :type (optional)
    Specify a type for every provided argument for the given flag. Types can be
any of: $(str (collect (map (fn (x) (var 'func (first (rest (str-split \":\" x)))) (str \"[\" func \"](#root::\" func \")\")) (hash-keys supported-types-map))))


Rules for flags:
- Flags can be single character: -m -n -c etc.
- Flags of a single single character with arity 0 can be adjacent without the
need for additional dashes: -mnc
- Multiple flags of a single character with arity 0 can precede a flag of a
single character with arity N as long as said character appears last: -mne \"foo\"
- Flags can be multi-character as long as they are preceded by two dashes: --multi-char-arg
"
(options-map args)
    (when (not (hash? options-map)) (err options-map-is-map))
    (when (> (length args) 0) (valid-first-arg? args))
    (var 'bindings-map (make-hash-with-keys options-map))
    (verify-all-options-valid args options-map bindings-map)
    ;; perform after setting defaults, in case user desires binding with
    ;; (= arity 1) to be a sequence.
    (fix-one-arg-bindings options-map bindings-map)
    (apply-defaults options-map bindings-map)
    ;; after apply-defaults, bindings with (= arity 1) will not be in a seqeunce.
    (debugln "options " options-map)
    (enforce-types options-map bindings-map)
    (debugln "bindings-map: " bindings-map)
    bindings-map)

(def 'sparse-options-map
    (make-hash
      (list
        (join :-b (make-hash (list))))))

(build-getopts-param 1 (list "bar") nil)

;;(assert-error-msg (getopts sparse-options-map '("-b" "1")) (bad-option-arity "-b" 0))
;;(assert-true (= (getopts sparse-options-map '("-b")) (make-hash (list (join :-b #t)))))
;;(assert-true (= (getopts sparse-options-map '()) (make-hash (list (join :-b nil)))))

(def 'test-options-map
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
    (var 'keys-in-map (fn (m n) (loop (keys m n last-ret) ((hash-keys m) m n #t)
                       (if (nil? last-ret)
                         nil
                         (if (empty-seq? keys)
                           #t
                           (recur
                             (rest keys)
                             m
                             n
                             (and
                               (hash-haskey n (first keys))
                               (= (hash-get m (first keys)) (hash-get n (first keys))))))))))
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
            (join :--c-arg '#("bar"))
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
            (join :--c-arg '#("bar"))
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
        (join :--c-arg '#("bar"))
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
            (join :--c-arg '#("bar"))
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
            (join :--c-arg '#("bar"))
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
        (join :--c-arg '#("bar"))
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
        (join :--c-arg '#("bar"))
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

(var 'i-int-bindings
 (hash-get
   (getopts (make-hash (list (join :-i (build-getopts-param 1 nil :int?)))) '("-i" "1"))
   :-i))
(assert-true (int? i-int-bindings) ". Return value should be of type Int.")

(var 'f-float-bindings
 (hash-get
   (getopts (make-hash (list (join :-f (build-getopts-param 1 nil :float?)))) '("-f" "1.3"))
   :-f))
(assert-true (float? f-float-bindings) ". Return value should be of type Float.")

(var 'f-float-vec-bindings
 (hash-get
   (getopts (make-hash (list (join :-f (build-getopts-param 2 nil :float?)))) '("-f" "1.3" "0.12"))
   :-f))
(for f in f-float-vec-bindings (assert-true (float? f) ". Return value should be of type Float."))

(var 'fs-file-vec-bindings
(hash-get
(getopts (make-hash (list (join :-f (build-getopts-param 2 nil :fs-file?))))
         '("-f" "/etc/fstab" "/etc/passwd"))
           :-f))
(for f in fs-file-vec-bindings (assert-true (fs-file? f) ". Return value should pass test fs-file?."))

(var 'fs-dir-bindings
     (hash-get
       (getopts (make-hash (list (join :--fs-dir (build-getopts-param 1 nil :fs-dir?))))
                '("--fs-dir" "/tmp"))
       :--fs-dir))
(assert-true (fs-dir? fs-dir-bindings) ". Return value should pass test fs-dir?.")

(var 'fs-exists-bindings
    (hash-get
      (getopts (make-hash (list (join :--exists (build-getopts-param 1 nil :fs-exists?))))
               '("--exists" "/etc/fstab"))
      :--exists))
(assert-true (fs-exists? fs-exists-bindings) ". Return value should pass test fs-exists?.")

(assert-error-msg
  (getopts (make-hash (list (join :--not-exists (build-getopts-param 1 nil :fs-exists?))))
           '("--not-exists" "/tmp/this/file/does/not/exist/i/hope/lakjdslfakjdlkfjdlkjfaslfkjlksdj"))
  "Argument, /tmp/this/file/does/not/exist/i/hope/lakjdslfakjdlkfjdlkjfaslfkjlksdj, should pass test fs-exists?")

(var 'symbol-bindings
    (hash-get
      (getopts (make-hash (list (join :-s (build-getopts-param 1 nil :symbol?))))
               '("-s" ":a-keyword-symbol"))
      :-s))
(assert-true (symbol? symbol-bindings) ". Return value should be of type Symbol?.")

(var 'c-char-bindings
 (hash-get
   (getopts (make-hash (list (join :-c (build-getopts-param 1 nil :char?)))) '("-c" #\a))
   :-c))
(assert-true (char? c-char-bindings) ". Return value should be of type Char.")

(assert-error-msg
  (getopts
    (make-hash (list (join :-c (build-getopts-param 1 nil :char?))))
       (list "-c" (list #\a)))
  (type-error-message ":-c" (list #\a) ":char?"))

(var 'hash-bindings
 (hash-get
   (getopts (make-hash (list (join :-h (build-getopts-param 1 nil :hash?)))) (list "-h" (make-hash (list (join :-h "meow")))))
   :-h))
(assert-true (hash? hash-bindings) ". Return value should be of type HashMap.")

(var 'nil-bindings
 (hash-get
   (getopts (make-hash (list (join :-h (build-getopts-param 1 nil :nil?)))) '("-h" nil))
   :-h))
(assert-true (nil? nil-bindings) ". Return value should be of type Nil.")

(assert-error-msg
   (getopts (make-hash (list (join :-h (build-getopts-param 1 nil :nil?)))) '("-h" "nickel"))
   (type-error-message ":-h" "nickel" ":nil?"))

(var 'list-bindings
 (hash-get
;;   (getopts (make-hash (list (join :-h (build-getopts-param 1 nil :list?)))) '("-h" (arg1 arg2)))
   (getopts (make-hash (list (join :-h (build-getopts-param 1 nil :list?)))) (list "-h" (list 'arg1 'arg2)))
   :-h))
(assert-true (= 'arg1 (car list-bindings)) ". First arg not correct symbol.")
(assert-true (= 'arg2 (cadr list-bindings)) ". Second arg not correct symbol.")

(var 'list-bindings2
 (hash-get
   (getopts (make-hash (list (join :-h (build-getopts-param 1 nil :list?)))) (list "-h" (list "arg1" "arg2")))
   :-h))
(assert-true (= "arg1" (car list-bindings2)) ". First arg not correct string.")
(assert-true (= "arg2" (cadr list-bindings2)) ". Second arg not correct string.")

(assert-error-msg
   (getopts (make-hash (list (join :-h (build-getopts-param 1 nil :list?)))) '("-h" "(list nxx nxx)"))
   (type-error-message  ":-h" "(list nxx nxx)" ":list?"))

(var 'vec-bindings
 (hash-get
   (getopts (make-hash (list (join :-h (build-getopts-param 1 nil :vec?)))) (list "-h" '#(1 2 3 4)))
   :-h))
(assert-true (vec? vec-bindings) ". Return value should be of type Vector.")
(assert-true (= (vec-nth 0 vec-bindings) '1) ". Idx 0 is wrong.")
(assert-true (= (vec-nth 1 vec-bindings) '2) ". Idx 2 is wrong.")
(assert-true (=(vec-nth 2 vec-bindings) '3) ". Idx 3 is wrong.")
(assert-true (= (vec-nth 3 vec-bindings) '4) ". Idx 3 is wrong.")

(var 'vec-bindings2
 (hash-get
   (getopts (make-hash (list (join :-h (build-getopts-param 1 nil :vec?)))) (list "-h" (vec 1 2 3 4)))
   :-h))
(assert-true (vec? vec-bindings2) ". Return value should be of type Vector.")
(assert-true (= (vec-nth 0 vec-bindings2) 1) ". Idx 0 is wrong.")
(assert-true (= (vec-nth 1 vec-bindings2) 2) ". Idx 2 is wrong.")
(assert-true (=(vec-nth 2 vec-bindings2) 3) ". Idx 3 is wrong.")
(assert-true (= (vec-nth 3 vec-bindings2) 4) ". Idx 3 is wrong.")

(assert-error-msg
   (getopts (make-hash (list (join :-h (build-getopts-param 1 nil :vec?)))) (list "-h" (list 'nxx 'nxx)))
   (type-error-message  ":-h" (list 'nxx 'nxx) ":vec?"))

(var 'macro-bindings
     (hash-get
         (getopts
           (make-hash
             (list (join :--macro (build-getopts-param 1 nil :macro?))))
           (list "--macro" (macro (x) x)))
         :--macro))

(assert-true (macro? macro-bindings) ". Return value should be of type macro.")
(var 'lambda-bindings
     (hash-get
         (getopts
           (make-hash
             (list (join :--lambda (build-getopts-param 1 nil :lambda?))))
           (list "--lambda" (fn (x) x)))
         :--lambda))
(assert-true (lambda? lambda-bindings) ". Return value should be of type lambda."))

(println (doc 'getopts))

(println "Passing: " args " to getopts.")
(def 'bindings
     (getopts (make-hash (list (join :-m (make-hash (list (join :arity 1) (join :default 0) (join :type :int?))))))  args))
(println "My Bindings: " bindings)
(println "Binding for -m is " (hash-get bindings :-m))
(ns-pop) ;; must be last line
