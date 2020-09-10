#!/home/price/development/slsh/target/debug/sl-sh

(ns-import 'shell)
(ns-import 'test)
(ns-import 'iterator)

(def 'sample "-la -c -b")

(def 'token-delim "-")

(def 'no-args "Getopts requires arguments.")

(def 'bad-first-arg "First argument must be a flag.")

(defn bad-option-arity (option expected)
    (str "Wrong number of arguments passed to " option ". Expected " expected
         " arguments."))

(defn is-single-char-arg (token)
    (and (= 2 (length token)) (str-starts-with token-delim token)))

(defn is-multi-char-arg (token)
    (and (> 2 (length token)) (str-starts-with (str token-delim token-delim) token)))

(defn is-multi-single-char-args (token)
    (str-starts-with token-delim token))

(defn get-next-params (idx vec-args)
    (var 'no-token-delim
         (str-split
           (str " " token-delim)
           (str-cat-list " " (vec-slice vec-args (+ idx 1) (length vec-args)))))
    (str-split :whitespace (first no-token-delim)))

(defn is-getopts-option-string (arg)
    (and (string? arg) (str-starts-with token-delim arg)))

(defn verify-proper-arity (option vector expected)
    (for v in vector
        (when (not (is-getopts-option-string (str v)))
            (err (bad-option-arity option expected)))))

(defn verify-arity (idx given-args allowable-args)
    (var 'option (vec-nth idx given-args))
    (var 'key (to-symbol (str ":" option)))
    (var 'arity-map (hash-get allowable-args key nil))
    (var 'arity (if (nil? arity-map)
                  0
                  (hash-get arity-map :arity 0)))
    ;; in case we are at end of args vector but the last option expects more
    ;; params than rest of args vector has after idx.
    (when (>= (+ idx arity) (length given-args))
      (err (bad-option-arity option arity)))
    ;; since all options start with a " -" use a str-split/str-cat trick
    ;; to get a vector representing all args to current option
    (var 'potential-args (get-next-params idx given-args))
    (when (not (= (length potential-args) arity))
      (err (bad-option-arity option arity))))

(defn verify-all-options-valid (cmd-line-args allowable-args)
    (var 'vec-args (collect-vec cmd-line-args))
    (for-i idx cmd in cmd-line-args
         (do
           (println "cmd: " cmd ", idx: " idx)
           (cond ((is-single-char-arg cmd) (verify-arity idx vec-args allowable-args))
               ((is-multi-char-arg cmd) "a")
               ((is-multi-single-char-args cmd) "a")))))

(defn build-getopts-param (arity)
    (make-hash
        (list
        (join :arity arity))))

(defn valid-first-arg? (args)
    (when (not (is-getopts-option-string (first args)))
        (err bad-first-arg)))

(defn getopts (params &rest args)
    ;; TODO verify params is a hash map and that each map the right shape...
    ;; maybe use structs?
    (when (not (> (length args) 0))
        (err no-args))
    (valid-first-arg? args)
    (verify-all-options-valid args params)
    (err "oh no"))

(def 'default-params
    (make-hash
      (list
        '(:-l 0)
        (join :-a (build-getopts-param 1))
        '(:-c 2)
        (join :-b (build-getopts-param 3)))))

(assert-error-msg (getopts default-params) no-args)
(assert-error-msg (getopts default-params "a") bad-first-arg)
(assert-error-msg (getopts default-params "abc") bad-first-arg)
(assert-error-msg (getopts default-params "-a") (bad-option-arity "-a" 1))
(assert-error-msg (getopts default-params "-a" "one-arg" "2-arg" "3") (bad-option-arity "-a" 1))
(assert-error-msg (getopts default-params "-b" "1" "2" "3" "-a") (bad-option-arity "-a" 1))
(assert-error-msg (getopts default-params "-b" "1" "3" "-a") (bad-option-arity "-b" 3))
(assert-error-msg (getopts default-params "-b" "1" "3" "-a" "2") (bad-option-arity "-b" 3))
;;(assert-error-msg (getopts default-params "-a" "an-argument") (bad-option-arity "-a" 1))

(def 'myit ((iterator::list-iter) :init '(1 2 3 4 5 6 7 8 9 10 11)))
(println (myit  :next!))
(def 'myslice  (myit  :slice 8))
(println (myslice  :next!))

(ns-pop) ;; must be last line
