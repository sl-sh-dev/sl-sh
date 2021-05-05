(ns-push 'shell-read)

(defn find-symbol (com)
  (var val (sym *active-ns* "::" com))
  (if (def? (ref val)) val (sym "root::" com)))

(defn callable? (com)
  ; Want the actual thing pointed to by the symbol in com for the test.
  (set! com (shell-read::find-symbol com))
  (if (def? (ref com))
      (do (set! com (eval (sym com)))
          (or (builtin? com) (lambda? com) (macro? com)))
      nil))

(defn flatten-args (vars-vec from)
  (if (list? from)
      ((fn (data)
           (if (pair? data)
               (do (flatten-args vars-vec (car data))
                   (recur (cdr data)))))
       from)
      (and (vec? from)(> (length from) 0))
      ((fn (i arg args-max)
           (flatten-args vars-vec arg)
           (if (< i args-max)
               (recur (+ i 1)(vec-nth from (+ i 1))args-max)))
       0 (vec-nth from 0) (- (length from) 1))
      (vec? from) nil
      (vec-push! vars-vec (str from))))

(defn fncall (com &rest args)
  (let ((new-args (vec)))
    (flatten-args new-args args)
    (ns-push *active-ns*)
    (unwind-protect
         (if (macro? com) (eval (expand-macro-all `(,com ,@new-args)))
             (apply com new-args))
      (ns-pop))))

(defmacro pipe (&rest args)
  `(str (pipe ,@args)))

(defmacro pipe-err (&rest args)
  `(str (pipe :err ,@args)))

; sys-apply needs to be able to handle no args to make the shell reader simpler.
(defmacro sys-apply (&rest args)
  (let ((first-arg)
        (args-len (length args)))
    (if (> args-len 0) (set! first-arg (vec-nth args 0)))
    (if
     (= args-len 0) nil
     (and (= args-len 1)(or (vec? first-arg)(pair? first-arg))) first-arg
     (callable? first-arg) `(shell-read::fncall ,first-arg ,@(vec-slice args 1))
     #t `(syscall ',first-arg ,@(vec-slice args 1)))))

(defmacro var-or-env (key)
    `(if (def? ,key)
        ,key
        (get-env ,key)))

;; This eleminates the trailing (shell-read::sys-apply) that will be on a
;; run-bg-first call if the & was at the end.  Keeps the other endfix code
;; simple and makes sure the $(... &) returns the process object not nil.
(defn run-bg-prep-args (args)
  (let ((args-len (length args)))
    (if (> args-len 0)
        (if (<= (length (vec-nth args (- args-len 1))) 1)
            (vec-slice args 0 (- args-len 1))
            args)
        nil)))

(defmacro run-bg-first (com &rest args)
  `(do (fork ,com) ,@(run-bg-prep-args args)))

(defmacro redir> (exp file) `(out> ,file ,exp))
(defmacro redir>> (exp file) `(out>> ,file ,exp))
(defmacro redir2> (exp file) `(err> ,file ,exp))
(defmacro redir2>> (exp file) `(err>> ,file ,exp))
(defmacro redir&> (exp file) `(out-err> ,file ,exp))
(defmacro redir&>> (exp file) `(out-err>> ,file ,exp))

(defn handle-process (cmd-proc)
	(if (process? cmd-proc) (= 0 (wait cmd-proc)) (not (not cmd-proc))))

(defn consume-whitespace (stream)
  (let ((ch (str-iter-peek stream)))
    (if (and (char? ch)(char-whitespace? ch))
        (do
         (str-iter-next! stream)
         (recur stream)))))

(defn read-string (stream last-ch token first quoted)
  (consume-whitespace stream)
  (if (= (str-iter-peek stream) #\")
      (set! token (read stream))
      (do
       ((fn (done last-ch ch peek-ch pdepth)
            (cond
              ((= last-ch #\\)
               ; Keep $ escaped for when tokens are processed to str forms.
               (if (= ch #\$) (str-push! token last-ch))
               (str-push! token ch))
              ((= ch #\\) nil)  ; skip '\'
              ((= ch #\()
               (str-push! token ch)
               (inc! pdepth))
              ((= ch #\))
               (str-push! token ch)
               (dec! pdepth))
              ((char-whitespace? ch)
               (set! done #t))
              ((or (and (= pdepth 0)(= peek-ch #\))))
               (str-push! token ch)
               (set! done #t))
              ((str-push! token ch)))
            (if (and (not done)(not (str-iter-empty? stream)))
                (recur done ch (str-iter-next! stream) (str-iter-peek stream) pdepth)
                token))
        nil #\space (str-iter-next! stream) (str-iter-peek stream) 0)
       (if (str-contains "~" token) (set! token (expand-dollar (expand-tilde token nil) nil))
           (set! token (expand-dollar token nil)))
       token)))

(defn read-var (stream last-ch ch peek-ch var-bracket add-exp token)
  (let ((done))
    (cond
      ((and (char-whitespace? ch)(not var-bracket))
       (set! done #t))
      ((and (not (= ch #\\))(or (= peek-ch #\))
                                (= peek-ch #\$)
                                (= peek-ch #\:)
                                (= peek-ch #\space)
                                (= peek-ch #\"))
            (not var-bracket))
       (str-push! token ch)
       (set! done #t))
      ((and (= ch #\}) var-bracket)
       (set! done #t))
      ((str-push! token ch) nil))
    (if (and (not done)(not (str-iter-empty? stream)))
        (read-var stream ch (str-iter-next! stream)(str-iter-peek stream) var-bracket add-exp token) ;recur
        (do
         (if (str-empty? token) (err "Syntax error, floating '$'."))
         (add-exp (sym "shell-read::var-or-env"))
          (add-exp (sym token))))))

(defn maybe-glob? (token)
  (or (str-contains "*" token)
      (str-contains "?" token)
      (str-contains "[" token)))

(defn get-home ()
  (let ((home (get-env "HOME")))
    (let ((last-idx (- (length home) 1)))
      (if (= #\/ (str-nth last-idx home)) (str-sub home 0 last-idx)
          home))))

(defn expand-tilde (token first-only)
  (let ((home (get-home)))
    (if (str-starts-with "~" token)
        (set! token (str home (str-sub token 1))))
    (if (not first-only) (do
                          (set! token (str-replace token ":~" (str ":" home)))
                          (set! token (str-replace token "\\~" "~")))))
  token)

(defn rm-esc (token)
  (str-iter-start token)
  (let ((new-token (str)))
    ((fn (last-ch ch)
         (cond
           ((= last-ch #\\)
            (str-push! new-token ch))
           ((= ch #\\))
           ((str-push! new-token ch)))
         (if (not (str-iter-empty? token))
             (recur ch (str-iter-next! token))
             new-token))#\space (str-iter-next! token))))

(defn expand-brace (token)
  (let ((toks (vec))
        (prefix (str))
        (brace (str))
        (bterms (vec))
        (eterm)
        (next-ch)
        (close-brace)
        (lisp-ch? (fn (ch)
                      (or (= ch #\()(= ch #\')(= ch #\"))))
        (postfix (str)))

    (set! close-brace (fn ()
                          (set! brace (str-trim brace))
                          (if (not (str-empty? brace))
                              (vec-push! bterms (str-trim brace)))
                          (set! brace (str))))

    (str-iter-start token)
    ((fn (done last-ch ch)
         (cond
           ((and (not (= last-ch #\\))(not (= last-ch #\$))(= ch #\{))
            (set! done #t))
           ((and (= last-ch #\\)(= ch #\{)) (str-push! prefix ch))
           ((and (= last-ch #\\)(= ch #\})) (str-push! prefix ch))
           ((str-push! prefix ch)))
         (if (and (not done)(not (str-iter-empty? token)))
             (recur done ch (str-iter-next! token))
             (set! next-ch ch)))
     nil #\space (str-iter-next! token))
    (if (not (str-iter-empty? token))
        ((fn (done last-ch ch peek-ch first)
             (cond
               ((and first (= ch #\{)(not (lisp-ch? peek-ch))))
               ((= last-ch #\\)
                (str-push! brace last-ch)
                (str-push! brace ch))
               ((= ch #\\))  ; skip '\'
                                        ; read an inner bracket
               ((and (not first)(= ch #\{))
                (str-push! brace ch)
                ((fn (ch blevel)
                     (if (not (char? ch)) (err "Missing '}'")
                         (= ch #\{) (inc! blevel)
                         (= ch #\}) (dec! blevel))
                     (str-push! brace ch)
                     (if (> blevel 0) (recur (str-iter-next! token) blevel)))
                 (str-iter-next! token) 1))
               ((lisp-ch? peek-ch)
                (close-brace)
                (set! eterm (eval (read token)))
                (cond
                  ((list? eterm)
                                        ; XXX, put t here to get an error with wrong top line number (not this)...
                   ((fn (lst)
                        (if (not (null lst))(do (vec-push! bterms (car lst))(recur (cdr lst)))))
                    eterm))
                  ((vec? eterm)
                   ((fn (vlen i)
                        (if (< i vlen)
                            (do
                             (vec-push! bterms (vec-nth eterm i))
                             (recur vlen (+ i 1)))))(length eterm) 0))
                  ((vec-push! bterms eterm)))
                (consume-whitespace token)
                (if (and (not (= (str-iter-peek token) #\,))(not (= (str-iter-peek token) #\})))
                    (err (str "(..) in bracket must be followed by ',' or '}', got " (str-iter-peek token)))))
               ((= ch #\})
                (set! done #t))
               ((= ch #\,)
                (close-brace))
               ((and (char-whitespace? ch)(str-empty? brace))) ; skip leading whitespace.
               ((str-push! brace ch)))
             (if (and (not done)(not (str-iter-empty? token)))
                 (recur done ch (str-iter-next! token) (str-iter-peek token) nil)
                 brace))
         nil #\space next-ch (str-iter-peek token) #t))
    (close-brace)
    (if (not (str-iter-empty? token))
        ((fn (ch)
             (str-push! postfix ch)
             (if (not (str-iter-empty? token))
                 (recur (str-iter-next! token))))
         (str-iter-next! token)))
    (if (vec-empty? bterms)
        (vec-push! toks token)
        (let ((terms-len (length bterms)))
              ((fn (i)
                   (if (< i terms-len)
                       (do
                        (vec-push! toks (str prefix (vec-nth bterms i) postfix))
                        (recur (+ i 1)))))
                0)))
    toks))

;; If we have a token with embedded $ then break it up and wrap in a str.
(defn expand-dollar (token first)
  (if (str-contains #\$ token)
      (let ((toks (vec))
            (new-token (str)))
        (str-iter-start token)
        (if (= (str-iter-peek token) #\$) (vec-push! toks (read token)))
        ((fn (last-ch ch peek-ch done)
             (cond
               ((str-iter-empty? token)
                (if (char? ch) (str-push! new-token ch))
                (if (not (str-empty? new-token))(vec-push! toks (rm-esc new-token)))
                (set! done #t))
               ((and (= last-ch #\\)(= ch #\$))
                (str-push! new-token ch))
               ((= ch #\\)) ; skip \ for now.
               ((and (not (= ch #\\))(= peek-ch #\$))
                (str-push! new-token ch)
                (vec-push! toks (rm-esc new-token))
                (set! new-token (str))
                (vec-push! toks (read token)))
               (#t
                (if (= last-ch #\\) (str-push! new-token last-ch))
                (str-push! new-token ch)))
            (if (not done) (recur ch (str-iter-next! token)(str-iter-peek token)done)
                (and (= (length toks) 1)first(list? (vec-nth toks 0))) (str-trim (eval (vec-nth toks 0)))
                (and (= (length toks) 1)first) (str-trim (vec-nth toks 0))
                (and (> (length toks) 1)first) (str-trim (eval (apply list (sym "str") toks)))
                (apply list (sym "str") toks)))
         #\space(str-iter-next! token)(str-iter-peek token)nil))
      (rm-esc token)))

(let ((paren-level 0))

  (defn read-list (last-ch ch peek-ch add-exp close-token do-read push-token
                           get-result clear-result)

    (defn setup-chainer (outer-form wrapper last-file)
      (let ((temp-result))
        (close-token)
        (set! temp-result (get-result))
        (clear-result)
        (add-exp (sym outer-form))
        (if wrapper
            (add-exp (list (sym wrapper) temp-result))
            (add-exp temp-result))
        (set! temp-result
              (if last-file
                  (read-string stream #\space (str) #t nil)
                  (if (or (= peek-ch #\$)(= peek-ch #\"))
                      (shell-read-int stream nil)
                      (shell-read-int stream #t))))
        (if wrapper
            (add-exp (list (sym wrapper) temp-result))
            (add-exp temp-result))))

    (let ((just-read)
          (done))
      (cond
        ((= last-ch #\\)
         (push-token last-ch)
         (push-token ch))
        ((= ch #\\) nil)  ; skip '\'
        ((and (= ch #\)) (> paren-level 0))
         (set! paren-level (- paren-level 1))
         (set! done #t))
        ((and (= ch #\)))
         (set! done #t))
        ; read a $(...)
        ((and (= ch #\$)(= peek-ch #\())
         (push-token ch)
         ((fn (ch plevel)
              (if (not (char? ch)) (err "Missing ')'")
                  (= ch #\() (inc! plevel)
                  (= ch #\)) (dec! plevel))
              (push-token ch)
              (if (> plevel 0) (recur (str-iter-next! stream) plevel)))
          (str-iter-next! stream) 0))
        ; read bracket expansion
        ((= ch #\{)
         (push-token ch)
         ((fn (ch blevel)
              (if (not (char? ch)) (err "Missing '}'")
                  (= ch #\{) (inc! blevel)
                  (= ch #\}) (dec! blevel))
              (push-token ch)
              (if (> blevel 0) (recur (str-iter-next! stream) blevel)))
          (str-iter-next! stream) 1))
        ((and (not (= ch #\\))(= peek-ch #\"))
         (do-read stream ch)
         (set! just-read #t))
        ((and (= ch #\&)(= peek-ch #\&)) ; AND
         (str-iter-next! stream)
         (setup-chainer "and" "shell-read::handle-process" nil)
         (set! done #t))
        ((and (= ch #\|)(= peek-ch #\|)) ; OR
         (str-iter-next! stream)
         (setup-chainer "or" "shell-read::handle-process" nil)
         (set! done #t))
        ((= ch #\;) ; DO
         (setup-chainer "do" nil nil)
         (set! done #t))
        ((and (= ch #\|)(= peek-ch #\&)) ; PIPE with stderr
         (str-iter-next! stream)
         (setup-chainer "shell-read::pipe-err" nil nil)
         (set! done #t))
        ((= ch #\|) ; PIPE
         (setup-chainer "shell-read::pipe" nil nil)
         (set! done #t))
        ((and (= ch #\>)(= peek-ch #\>)) ; out>>
         (str-iter-next! stream)
         (setup-chainer "shell-read::redir>>" nil #t))
        ((= ch #\>) ; out>
         (setup-chainer "shell-read::redir>" nil #t))
        ((and (= ch #\&)(= peek-ch #\>)) ; out-err>(>)
         (str-iter-next! stream)
         (if (= (str-iter-peek stream) #\>)
             (do (str-iter-next! stream)
                 (setup-chainer "shell-read::redir&>>" nil #t))
             (setup-chainer "shell-read::redir&>" nil #t)))
        ((and (= ch #\2)(= peek-ch #\>)) ; err>(>)
         (str-iter-next! stream)
         (if (= (str-iter-peek stream) #\>)
             (do (str-iter-next! stream)
                 (setup-chainer "shell-read::redir2>>" nil #t))
             (setup-chainer "shell-read::redir2>" nil #t)))
        ((= ch #\&) ; Background
         (setup-chainer "shell-read::run-bg-first" nil nil)
         (set! done #t))
        ((or (not (char? ch))(char-whitespace? ch))
         (close-token))
        ((push-token ch) nil))
      (if (and (not done)(not (str-iter-empty? stream)))
          (if just-read
              (do
               (set! just-read nil)
               (read-list #\space #\space (str-iter-peek stream) add-exp
                close-token do-read push-token get-result clear-result)) ;recur
              (read-list ch (str-iter-next! stream)(str-iter-peek stream)
                         add-exp close-token do-read push-token get-result
                         clear-result)) ;recur
          (close-token))))

  (defn shell-read-int (stream in-paren)
    (let ((result)
          (token)
          (var-bracket nil)
          (last-pair (list))
          (add-exp)
          (add-token-arg)
          (brace-expand-token)
          (close-token)
          (push-token)
          (do-read)
          (get-result)
          (clear-result)
          (first-sym #t)
          (ch)
          (peek-ch))

      (set! token (str))
      (set! add-exp
            (fn (exp)
                (let ((new-pair (join exp nil)))
                  (if (nil? last-pair) (set! result new-pair))
                  (xdr! last-pair new-pair)
                  (set! last-pair new-pair))))

      (set! add-token-arg
            (fn (token)
                (if
                 (maybe-glob? token) (add-exp (list 'glob (expand-dollar (expand-tilde token nil) nil)))
                 (str-contains "~" token) (add-exp (expand-dollar (expand-tilde token nil) nil))
                 (add-exp (expand-dollar token nil)))))

      (set! brace-expand-token
            (fn (token)
                (let ((toks)
                      (toks-len))
                  (if (str-contains "{" token)
                      (do
                       (set! toks (expand-brace token))
                       (set! toks-len (length toks))
                        (if (= toks-len 0) nil
                            (= toks-len 1) (add-token-arg (vec-nth toks 0))
                            ((fn (i) (if (< i toks-len)
                                         (do (brace-expand-token (vec-nth toks i))
                                             (recur (+ i 1)))))0)))
                      (add-token-arg token)))))

      (set! close-token
            (fn ()
                (if (not (str-empty? token))
                    (if first-sym (let ((tng (expand-dollar (expand-tilde token #t) #t)))
                                    (if (string? tng) (add-exp (sym (rm-esc tng)))
                                        ; XXX TODO- if this is a (str...) list then deal with that.
                                        (add-exp tng))
                                    (set! first-sym nil))
                        (brace-expand-token token)))
                (set! token (str))))

      (set! push-token (fn (ch) (str-push! token ch)))

      (set! do-read (fn (stream ch)
                        (if (not (char-whitespace? ch))
                            (str-push! token ch))
                        (close-token)
                        (add-exp (read stream))))

      (set! get-result (fn () result))
      (set! clear-result (fn () (set! last-pair (list))(set! result nil)))

      (set! ch (str-iter-next! stream))
      (set! peek-ch (str-iter-peek stream))
      (cond
        (in-paren
         (add-exp (sym "shell-read::sys-apply"))
         (read-list #\space ch peek-ch add-exp close-token do-read push-token
                    get-result clear-result))
        ((and (= ch #\()(= peek-ch #\())
         (set! result (read stream))
         (consume-whitespace stream)
         (set! ch (str-iter-next! stream))
         (if (not (= #\) ch))
             (err (str "Unbalanced ) in '\$' shell read macro, got " ch)))
         result)
        ((= ch #\()
         (set! paren-level (+ paren-level 1))
         (add-exp (sym "shell-read::sys-apply"))
         (read-list #\space (str-iter-next! stream)(str-iter-peek stream)
                    add-exp close-token do-read push-token get-result
                    clear-result))
        ((= ch #\{)
         (read-var stream #\space (str-iter-next! stream)(str-iter-peek stream) #t add-exp token))
        (#t
         (read-var stream #\space ch peek-ch nil add-exp token)))
      result)))

(defn shell-read (stream ch_start)
                (list 'str (shell-read::shell-read-int stream nil)))

(def *ns-exports* nil)

(ns-pop)

(hash-set! *read-table* #\$ 'shell-read::shell-read)
(hash-set! *string-read-table* #\$ 'shell-read::shell-read)
