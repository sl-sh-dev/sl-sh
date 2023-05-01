(def *getopts-log* ((logger) :init "getopts" :off))

(def token-delim "-")

(def getopts-invalid-type-function (str "Type not supported. See (doc 'getopts) for supported types."))

(def getopts-options-map-is-map "Getopts first argument, options-map, must pass test hash?.")

(def getopts-help--options-map-is-map "Getopts help first argument, options-map, must pass test hash?.")

(def getopts-bad-first-arg "First argument must be a flag.")

(defn arity-zero-can-not-be-required (x)
      (str "Options with required #t must have arity > 0, bad option " (apply str (rest (collect (iter (str x))))) "."))

(defn required-argument (option)
    (str "Wrong number of arguments passed to " (apply str (rest (collect (iter (str option))))) ". Expected 1 argument."))

(defn getopts-bad-option-arity (option expected)
    (str "Wrong number of arguments passed to " option ". Expected " expected
         " arguments."))

(defn getopts-type-error-message (key binding opt-type-fun)
    (str "Input types did not match :type specified in options-map. At argument " key ", failed to read in provided " binding " as type " opt-type-fun ". Binding, " binding ", was of type " (type binding) "."))


(defn getopts-illegal-option (key)
    (str "Illegal option " key ", not in allowable arguments provided to getopts."))

(defn is-single-char-arg (token)
    (and (not (or (char? token) (macro? token) (lambda? token))) (= 2 (length token)) (str-starts-with token-delim (str token))))

(defn is-multi-char-arg (token)
    (str-starts-with (str token-delim token-delim) (str token)))

(defn is-multi-single-char-args (token)
    (str-starts-with token-delim (str token)))

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
    ()
    ;; possible params is nil if at the end of a list, return an empty vec 
    ;; indicating no paramaters
    (let ((possible-params (vec-slice vec-args (+ idx 1) (length vec-args)))
          (potential-params (make-vec)))
      (if (nil? possible-params)
      potential-params
      (loop (params) (possible-params)
          (if (or (empty-seq? params) (str-starts-with token-delim (str (first params))))
            potential-params
            (do
                (vec-push! potential-params (first params))
                (recur (rest params))))))))

(defn is-getopts-option-string (arg)
    (and (string? arg) (str-starts-with token-delim arg)))

(defn verify-arity (idx given-args options-map bindings-map)
    (let* ((option (vec-nth given-args idx))
          (key (sym (str ":" option)))
          (arity-map (hash-get options-map key))
          (arity (if (nil? arity-map)
                  0
                  (get-arity arity-map))))
    (*getopts-log* :trace "arity: " arity)
    (when (nil? arity-map)
      (err (getopts-illegal-option option)))
    ;; in case we are at end of args vector but the last option expects more
    ;; params than rest of args vector has after idx.
    (when (>= (+ idx arity) (length given-args))
      (err (getopts-bad-option-arity option arity)))
    ;; since all options start with a " -" use a str-split/str-cat trick
    ;; to get a vector representing all args to current option
    (let ((potential-args (get-next-params idx given-args)))
    (*getopts-log* :trace "potential-args: " potential-args ", type: " (type potential-args))
    (when (not (= (length potential-args) arity))
        (err (getopts-bad-option-arity option arity)))
    (hash-set! bindings-map key (if (empty-seq? potential-args) #t potential-args)))))

(defn verify-all-options-valid (cmd-line-args options-map bindings-map)
    (let ((vec-args (collect-vec cmd-line-args)))
    (*getopts-log* :trace "vec-args: " vec-args)
    (for-i idx cmd in vec-args
         (do
           (*getopts-log* :trace "cmd: " (vec-nth vec-args idx) ", idx: " idx)
           (cond
             ((is-multi-char-arg cmd) (verify-arity idx vec-args options-map bindings-map))
             ((is-single-char-arg cmd) (verify-arity idx vec-args options-map bindings-map))
             ((is-multi-single-char-args cmd)
                 ;; if the command in question looked like "-ab", de-multi-single-arged-str
                 ;; will be "-a -b" this way the new cmd-line-args list can
                 ;; be fed recursively to verify-all-options-valid
                 (let* ((de-multi-single-arged-str
                          (str-split " " (str token-delim
                               (str-cat-list (str " " token-delim)
                               (collect-vec (str-replace (vec-nth vec-args idx) token-delim ""))))))
                        (sub-vec
                          (map str
                               (append de-multi-single-arged-str (slice vec-args (+ 1 idx) (length vec-args))))))
                 
                   (verify-all-options-valid sub-vec options-map bindings-map)
                   "a")))))))

(defn valid-first-arg? (args)
    (when (not (is-getopts-option-string (first args)))
        (err getopts-bad-first-arg)))

(def nyi "not-yet-implemented")

(defn make-hash-with-keys (hmap)
    (make-hash (collect (map (fn (x) (join (sym x) nil)) (hash-keys hmap)))))

(defn fix-one-arg-bindings
"Ensure binding with arity of 1 is bound to actual value passed in, not
a vector whose only element is the desired binding."
      (options-map bindings-map)
    (loop (keys bindings-map) ((hash-keys bindings-map) bindings-map)
        (when (not (empty-seq? keys))
            (let* ((key (first keys))
                 (opt-config (hash-get options-map key))
                 (opt-arity (hash-get opt-config :arity))
                 (binding (hash-get bindings-map key)))
             (when (and (not (nil? opt-arity)) (= 1 opt-arity) (seq? binding) (= 1 (length binding)))
               (hash-set! bindings-map key (first binding)))
             (recur (rest keys) bindings-map)))))

(defn apply-defaults (options-map bindings-map)
    (loop (keys bindings-map) ((hash-keys options-map) bindings-map)
        (when (not (empty-seq? keys))
            (let* ((key (first keys))
                 (opt-config (hash-get options-map key)))
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
    (let ((res (applier string)))
      (if (predicate res)
      res
      (err custom-message)))))

(def supported-types-map
    (make-hash
      (list
        (join :int? (fn (x unused) (str->int x)))
        (join :float? (fn (x unused) (str->float x)))
        (join :fs-file? (check-custom fs-file? (fn (x) (str "Argument, " x ", should pass test fs-file?"))))
        (join :fs-dir? (check-custom fs-dir? (fn (x) (str "Argument, " x ", should pass test fs-dir?"))))
        (join :fs-exists? (check-custom fs-exists? (fn (x) (str "Argument, " x ", should pass test fs-exists?"))))
        (join :symbol? (fn-to-predicate sym symbol?))
        (join :string? (fn (x unused) (str x)))
        (join :char? (check char?))
        (join :hash? (check hash?))
        (join :falsey? (check falsey?))
        (join :list? (check list?))
        (join :vec? (check vec?))
        (join :lambda? (check lambda?))
        (join :macro? (check macro?)))))

(defn enforce-constrains (options-map bindings-map)
    (loop (keys bindings-map) ((hash-keys options-map) bindings-map)
        (when (not (empty-seq? keys))
            (let* ((key (first keys))
                 (opt-config (hash-get options-map key))
                 (opt-type-arity (hash-get opt-config :arity))
                 (opt-type-fun (hash-get opt-config :type))
                 (default (hash-get opt-config :default))
                 (required (hash-get opt-config :required nil))
                 (binding (hash-get bindings-map key)))
             (when (and (= opt-type-arity 0) required)
               (err (arity-zero-can-not-be-required key)))
             (when (and (> opt-type-arity 0) required (nil? binding))
               (if (= opt-type-arity 1)
                 (err (required-argument key))
                 (err (getopts-bad-option-arity (apply str (rest (collect (iter (str key))))) opt-type-arity))
                 ))
             (when (and (not (nil? opt-type-arity))
                        (not (nil? opt-type-fun)))
               (do
                 (if (not (in? (hash-keys supported-types-map) opt-type-fun))
                   (err getopts-invalid-type-function)
                   (do
                     (when (and (not (= binding default)) (not (= 0 opt-type-arity)))
                       (let ((debug-strm (*getopts-log* :trace "look our binding is: " binding ", of type: " (type binding)))
                            (err-str (getopts-type-error-message key binding opt-type-fun)))
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
                         (*getopts-log* :trace "look our binding is now: " (hash-get bindings-map key) ", of type: " (type (hash-get bindings-map key)))
                         ))))))
             (recur (rest keys) bindings-map)))))

(defn get-arity (bindings)
      (hash-get bindings :arity 0))

(defn getopts-help
"Companion function to getopts, call this function in the docstring of
any function that relies on getopts. getopts-help takes as an argument
the same bindings hash map that getopts does, and returns the documentation
for those getopts settings. getopts-help optionally supports a :doc keyword
for each command key. The value of doc is included in the resultant doc string
getopts-help returns.

Section: shell

Example:
;;; The following represents a full example usage of getopts
;;; with documentation generated in the doc string of the function that uses
;;; getopts:

(def getopts-bindings
		(make-hash
			(list
(def getopts-help-bindings
       (make-hash
         (list (join
                 :-m
                 (make-hash '((:arity . 1)
                              (:default . 0)
                              (:type . :int?)
                              (:doc \"this is displayed as\ntwo indented lines.\"))))
                 (join
                   :-b
                     (make-hash '((:doc \"this opts doc for -b
goes on two lines!\"))))
                 (join
                   :-a
                     (make-hash '((:arity . 1)
                                  (:doc \"this doc is for -a.\"))))
                 (join
                   :-k
                     (make-hash '((:arity . 1)
                              (:type . :int?)))))))
(test::assert-equal
    (str-split :whitespace (getopts-help getopts-help-bindings))
    (str-split :whitespace (str-push! (str) #\u{9} \"-a\" #\u{9} \"arity          1\" #\u{9} \"this doc is for -a.\" #\u{9} \"-b\" #\u{9} \"arity          0\" #\u{9} \"this opts doc for -b\" #\u{9} \"goes on two lines!\" #\u{9} \"-k\" #\u{9} \"arity          1\" #\u{9} \"required type  :int?\" #\u{9} \"-m\" #\u{9} \"arity          1\" #\u{9} \"default value  0\" #\u{9} \"required type  :int?\" #\u{9} \"this is displayed as\" #\u{9} \"two indented lines.\")))
"
(bindings)
(when (not (hash? bindings)) (err getopts-help--options-map-is-map))
(let ((options-str (str "OPTIONS" #\u{a})))
  (for key in (qsort (hash-keys bindings))
     (do
       (str-push! options-str
            #\u{9} (apply str (rest (collect (iter (str key)))))
            #\u{a}
            #\u{9} #\u{9} (with-padding "arity" 15 " ") (get-arity (hash-get bindings key))
            #\u{a}
            (if (not (nil? (hash-get (hash-get bindings key) :default)))
              (str #\u{9} #\u{9}
                   (with-padding "default value" 15 " ") (hash-get (hash-get bindings key) :default))
              "")
            (if (not (nil? (hash-get (hash-get bindings key) :type)))
              (str #\u{9} #\u{9}
                   (with-padding "required type" 15 " ") (hash-get (hash-get bindings key) :type) #\u{a} )
              "")
            (if (not (nil? (hash-get (hash-get bindings key) :doc)))
              (str #\u{a} (apply str (collect (map (fn (x) (str #\u{a} #\u{9} #\u{9} x)) (str-split "\n" (hash-get (hash-get bindings key) :doc))))))
              "")
           #\u{a})))
  (str-push! options-str #\u{a})))

(defn getopts
"Getopts takes a hash map and a vector of args and returns a hash map with all
the values extracted from the args and bound to the corresponding keys in the
provided hash map. Support for automatically generating documentation is
available with helper function [getopts-help](#shell::getopts-help).

Take this example script:
`sample-getopts.lisp`
```
#!/usr/bin/env sl-sh

(println \"Passing: \" args \" to getopts\")
;; getopts is given a hash map with one key, :-m, that corresponds to the flag,
;; -m, that it configures.
(def *sample-getopts-bindings*
     (getopts
       (make-hash
         (list (join
                 :-m
                 (make-hash '((:arity . 1)
                              (:default . 0)
                              (:type . :int?))))))
        args))
(println \"The binding for -m is: \" (hash-get *sample-getopts-bindings* :-m) \"of type \" (type (hash-get *sample-getopts-bindings* :-m)))
```

Running the script with one argument to the -m flag yields:
```
$ ./sample-getopts.lisp -m 7
Passing: #(\"-m\" \"7\") to getopts
The binding for -m is 7 of type Int
```

The hash map for the key :-m showcases the configuration keys that exist for
each flag: arity, :default, and :type. :arity specifies that the -m flag will
take one argument. :default specifies the bindng for :-m should be 0 if the
-m flag is not seen in args. :type :int? specifies that the binding should
be of that type, in this case int?. Running the script again with no -m
flag yields:
```
$ ./sample-getopts.lisp
Passing: #() to getopts
The binding for -m is 0 of type Int
```

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
any of: $((str (collect (map (fn (x) (let ((func (first (rest (str-split ":" x))))) (str "[" func "](#root::" func ")"))) (hash-keys supported-types-map)))))


Rules for flags:
- Flags can be single character: -m -n -c etc.
- Flags of a single single character with arity 0 can be adjacent without the
need for additional dashes: -mnc
- Multiple flags of a single character with arity 0 can precede a flag of a
single character with arity N as long as said character appears last: -mne \"foo\"
- Flags can be multi-character as long as they are preceded by two dashes: --multi-char-arg

Section: shell

Example:
;See tests/getopts.lisp
#t
"
(options-map args)
    (when (not (hash? options-map)) (err getopts-options-map-is-map))
    (when (> (length args) 0) (valid-first-arg? args))
    (let ((bindings-map (make-hash-with-keys options-map)))
        (verify-all-options-valid args options-map bindings-map)
        ;; perform after setting defaults, in case user desires binding with
        ;; (= arity 1) to be a sequence.
        (fix-one-arg-bindings options-map bindings-map)
        (apply-defaults options-map bindings-map)
        ;; after apply-defaults, bindings with (= arity 1) will not be in a seqeunce.
        (*getopts-log* :trace "options " options-map)
        (enforce-constrains options-map bindings-map)
        (*getopts-log* :trace "bindings-map: " bindings-map)
        bindings-map))
