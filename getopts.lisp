#!/home/price/development/slsh/target/debug/sl-sh

(ns-import 'shell)
(ns-import 'test)
(ns-import 'iterator)

;; TODO make debugln use env var?
;; TODO do keys have to start with :-?
;; TODO need some FAILING test cases for TYPES
;; TODO TYPES
(defmacro debugln (&rest args)
    (if (nil? #t)
        `(println "=> " ,@args)))

(def 'sample "-la -c -b")

(def 'token-delim "-")

(def 'no-args "Getopts requires arguments.")

(def 'options-map-is-map "Getopts first argument, options-map, must pass test hash?.")

(def 'bad-first-arg "First argument must be a flag.")

(defn bad-option-arity (option expected)
    (str "Wrong number of arguments passed to " option ". Expected " expected
         " arguments."))

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
    (when (nil? possible-params)
      (return-from get-next-params '#()))
    (var 'no-token-delim
         (str-split
           (str " " token-delim)
           (str-cat-list " " possible-params)))
    (var 'with-token-delim (str-split :whitespace (first no-token-delim)))
    ;; special case if this is no argument to a variable. with-token-delim
    ;; variable will be the rest of the string, must manually return an empty
    ;; vec indicating no parameters
    (if (str-starts-with token-delim (str-cat-list "" with-token-delim)) '#() with-token-delim))

(defn is-getopts-option-string (arg)
    (and (string? arg) (str-starts-with token-delim arg)))

(defn illegal-option (key)
    (str "Illegal option " key ", not in allowable arguments provided to getopts."))

(defn verify-arity (idx given-args options-map bindings-map)
    (var 'option (vec-nth idx given-args))
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
    (when (not (= (length potential-args) arity))
      (err (bad-option-arity option arity)))
    (hash-set! bindings-map key (if (empty-seq? potential-args) #t potential-args)))

(defn verify-all-options-valid (cmd-line-args options-map bindings-map)
    (var 'vec-args (collect-vec cmd-line-args))
    (debugln "vec-args: " vec-args)
    (for-i idx cmd in vec-args
         (do
           (debugln "cmd: " (vec-nth idx vec-args) ", idx: " idx)
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
                           (collect-vec (str-replace (vec-nth idx vec-args) token-delim ""))))))
                 (var 'sub-vec (map str (append de-multi-single-arged-str (slice vec-args (+ 1 idx) (length vec-args)))))
                   (verify-all-options-valid sub-vec options-map bindings-map)
                   "a"))))))

(defn build-getopts-param (arity default type-fun)
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

(defn read-then-check-self (func)
  (fn (x)
    (var 'read-x (read x))
    (var 'func-ret (func read-x))
    ;; TOOD if we pass the test, then return the value? otherwise, throw a
    ;; crappy error? we can throw a better error?
    (if func-ret
      read-x
      (err "Bad types"))))

(def 'supported-types-map
    (make-hash
      (list
        (join 'int? (fn (x) (str->int x)))
        (join 'float? (fn (x) (str->float x)))
        (join 'file? (fn (x) (file? x)))
        (join 'symbol? (fn (x) (to-symbol x)))
        (join 'string? (fn (x) x))
        (join 'char? (read-then-check-self char?))
        (join 'func? (read-then-check-self func?))
        (join 'builtin? (read-then-check-self builtin?))
        (join 'macro? (read-then-check-self macro?))
        (join 'hash? (read-then-check-self hash?))
        (join 'nil? (read-then-check-self nil?))
        (join 'pair? (read-then-check-self pair?))
        (join 'lambda? (read-then-check-self lambda?))
        (join 'list? (read-then-check-self list?))
        (join 'vec? (read-then-check-self vec?))
        (join 'non-empty-seq? (read-then-check-self non-empty-seq?))
        (join 'empty-seq? (read-then-check-self empty-seq?)))))

(def 'invalid-type-function (str "Type function must be one of " (hash-keys supported-types-map)))

(defn enforce-types (options-map bindings-map)
    (loop (keys bindings-map) ((hash-keys options-map) bindings-map)
        (when (not (empty-seq? keys))
            (progn
             (var 'key (first keys))
             (var 'opt-config (hash-get options-map key))
             (var 'opt-type-arity (hash-get opt-config :arity))
             (var 'opt-type-fun (hash-get opt-config :type))
             (var 'binding (hash-get bindings-map key))
             (when (and (not (nil? opt-type-arity))
                        (not (nil? opt-type-fun)))
               (progn
                 (if (not (in? (hash-keys supported-types-map) opt-type-fun))
                   (err invalid-type-function)
                   (progn
                     (when (not (= 0 opt-type-arity))
                       (progn
                         (debugln "look our binding is: " binding ", of type: " (type binding))
                         (if (= 1 opt-type-arity)
                             (hash-set! bindings-map key ((hash-get supported-types-map opt-type-fun) binding))
                             (hash-set! bindings-map key (collect-vec (map (hash-get supported-types-map opt-type-fun) binding))))
                         (debugln "look our binding is now: " (hash-get bindings-map key) ", of type: " (type (hash-get bindings-map key)))
                         ))
                     ))))
             (recur (rest keys) bindings-map)))))

(def 'test-options-map
    (make-hash
      (list
        (join :-l (build-getopts-param 0 #t nil))
        (join :-m (build-getopts-param 0 nil nil))
        (join :-a (build-getopts-param 1 "foo" nil))
        (join :--c-arg (build-getopts-param 1 '#("bar") nil))
        (join :--d-arg (build-getopts-param 2 nil nil))
        (join :-b (build-getopts-param 3 nil nil)))))

(defn getopts
"Pass in option-map of the form:

$(str test-options-map)

followed by an optstring client passed in from the command line.

Supported type arguments: DO THE RELATIVE LINK THING

$(str (hash-keys supported-types-map))
"
(options-map &rest args)
    (when (not (hash? options-map))
        (err options-map-is-map))
    (when (not (> (length args) 0))
        (err no-args))
    (valid-first-arg? args)
    (var 'bindings-map (make-hash-with-keys options-map))
    (verify-all-options-valid args options-map bindings-map)
    ;; perform after setting defaults, in case user desires binding with
    ;; (= arity 1) to be a sequence.
    (fix-one-arg-bindings options-map bindings-map)
    (apply-defaults options-map bindings-map)
    (debugln "options " options-map)
    (enforce-types options-map bindings-map)
    (debugln "bindings-map: " bindings-map)
    bindings-map)

(assert-error-msg (getopts "a") options-map-is-map)
(assert-error-msg (getopts test-options-map) no-args)
(assert-error-msg (getopts test-options-map "a") bad-first-arg)
(assert-error-msg (getopts test-options-map "abc") bad-first-arg)

(assert-error-msg (getopts test-options-map "-a") (bad-option-arity "-a" 1))
(assert-error-msg (getopts test-options-map "--c-arg") (bad-option-arity "--c-arg" 1))
(assert-error-msg (getopts test-options-map "-a" "one-arg" "2-arg" "3") (bad-option-arity "-a" 1))
(assert-error-msg (getopts test-options-map "-l" "-a" "one-arg" "2-arg" "3") (bad-option-arity "-a" 1))
(assert-error-msg (getopts test-options-map "-b" "1" "2" "3" "-a") (bad-option-arity "-a" 1))
(assert-error-msg (getopts test-options-map "-b" "1" "3" "-a") (bad-option-arity "-b" 3))
(assert-error-msg (getopts test-options-map "-b" "1" "a" "-a" "2") (bad-option-arity "-b" 3))
(assert-error-msg (getopts test-options-map "-b" "1" "b") (bad-option-arity "-b" 3))
(assert-error-msg (getopts test-options-map "-b" "1" "2" "3" "4") (bad-option-arity "-b" 3))
(assert-error-msg (getopts test-options-map "-b" "1" "2" "3" "4" "--c-arg") (bad-option-arity "-b" 3))
(assert-error-msg (getopts test-options-map "-b" "1" "2" "3" "--c-arg") (bad-option-arity "--c-arg" 1))
(assert-error-msg (getopts test-options-map "-lma" "aaa" "-b" "1" "2" "3" "--c-arg" "1" "-d" "0") (illegal-option "-d"))
(assert-error-msg (getopts test-options-map "-lma" "aaa" "-b" "1" "2" "3" "--c-arg" "1" "-e") (illegal-option "-e"))

(assert-error-msg (getopts test-options-map "-ab" "an-argument") (bad-option-arity "-a" 1))
(assert-error-msg (getopts test-options-map "-lb" "an-argument") (bad-option-arity "-b" 3))

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
                             (and (hash-haskey n (first keys)) (= (hash-get m (first keys)) (hash-get n (first keys))))))))))
    (and (hash? m) (hash? n) (= (length (hash-keys m)) (length (hash-keys n)))
      (keys-in-map m n)
      (keys-in-map n m)))

(assert-true
    (map=
        (getopts test-options-map "-lmb" "1" "2" "3")
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
        (getopts test-options-map "-b" "1" "2" "3" "-lm" "--c-arg" "bar")
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
        (getopts test-options-map "-b" "1" "2" "3" "-lm")
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
        (getopts test-options-map "-b" "1" "2" "3" "-lm")
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
    (getopts test-options-map "-a" "1")
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
        (getopts test-options-map "-l" "-a" "one-arg")
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
        (getopts test-options-map "-b" "1" "2" "3")
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
        (getopts test-options-map "-lma" "aaa" "-b" "1" "2" "3" "--c-arg" "1")
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
        (getopts test-options-map "-lma" "aaa" "-b" "1" "2" "3" "--c-arg" "1" "--d-arg" "1" "2")
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
    (getopts test-options-map "-l")
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
    (getopts test-options-map "-lb" "1" "2" "3" "-a" "1")
    (make-hash
      (list
        (join :-l #t)
        (join :-m nil)
        (join :-a "1")
        (join :--c-arg '#("bar"))
        (join :--d-arg nil)
        (join :-b '#("1" "2" "3"))))))

(def 'test-options-map-types
    (make-hash
      (list
        (join :-l (build-getopts-param 0 #t nil))
        (join :-m (build-getopts-param 0 nil nil))
        (join :-a (build-getopts-param 1 "foo" nil))
        (join :--c-arg (build-getopts-param 1 '#("bar") nil))
        (join :--d-arg (build-getopts-param 2 nil nil))
        (join :-b (build-getopts-param 3 nil nil)))))

(assert-error-msg
  (getopts
    (make-hash
      (list
        (join :-l (build-getopts-param 0 #t 'true?))))
    "-l") 
  invalid-type-function)

(assert-error-msg
  (getopts
    (make-hash
      (list
        (join :-l (build-getopts-param 0 #t 'true?))))
    "-l")
  invalid-type-function)

(assert-error-msg (getopts (make-hash (list (join :-c (build-getopts-param 1 nil 'float?)))) "-c" "#\a") "str->float: string is not a valid float")

(lex
    (var 'f-float-bindings (hash-get (getopts (make-hash (list (join :-f (build-getopts-param 1 nil 'float?)))) "-f" "1.3") :-f))
    (assert-true (float? f-float-bindings) ". Return value should be of type float.")

    (var 'f-float-vec-bindings (hash-get (getopts (make-hash (list (join :-f (build-getopts-param 2 nil 'float?)))) "-f" "1.3" "0.12") :-f))
    (for f in f-float-vec-bindings (assert-true (float? f) ". Return value should be of type float."))

    (var 'c-char-bindings (hash-get (getopts (make-hash (list (join :-c (build-getopts-param 1 nil 'char?)))) "-c" "#\a") :-c))
    (assert-true (char? c-char-bindings) ". Return value should be of type char.")

    (assert-error-msg (getopts (make-hash (list (join :-c (build-getopts-param 1 nil 'char?)))) "-c" "'#(\"a\")") "Bad types")
)

(ns-pop) ;; must be last line
