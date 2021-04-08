(ns-push 'shell-read)

(defn callable? (com)
  ; Want the actual thing pointed to by the symbol in com for the test.
  (set! com (shell-read::find-symbol com))
  (if (def? (ref com))
      (do (set! com (eval (sym com)))
          (or (builtin? com) (lambda? com) (macro? com)))
      nil))

(defmacro sys-apply (com &rest args)
  (if (callable? com)
      `(,com ,@args)
      `(syscall ,com ,@args)))

(defn find-symbol (com)
  (var val (sym *active-ns* "::" com))
  (if (def? (ref val)) val (sym "root::" com)))

(defmacro var-or-env (key)
  (let ((key-new (find-symbol key)))
    (if (def? (ref key-new))
        `,key
        `(get-env ,key))))

(defmacro redir> (exp file)
  `(out> ,file ,exp))

(defmacro redir>> (exp file)
  `(out>> ,file ,exp))

(defn handle-process (cmd-proc)
	(if (process? cmd-proc) (= 0 (wait cmd-proc)) (not (not cmd-proc))))

(defmacro proc-wait ()
	(fn (cmd) `(handle-process ,cmd)))

(let ((paren-level 0))

  (defn maybe-glob? (token)
    (or (str-contains "*" token)
        (str-contains "?" token)
        (str-contains "[" token)
        (str-contains "{" token)))

  (defn read-var (last-ch ch peek-ch var-bracket add-exp token)
    (let ((done))
      (cond
        ((and (char-whitespace? ch)(not var-bracket))
         (set! done #t))
        ((and (not (= ch #\\))(or (= peek-ch #\))
                                  (= peek-ch #\$)
                                  (= peek-ch #\space)
                                  (= peek-ch #\"))
              (not var-bracket))
         (str-push! token ch)
         (set! done #t))
        ((and (= ch #\}) var-bracket)
         (set! done #t))
        ((str-push! token ch) nil))
      (if (and (not done)(not (str-iter-empty? stream)))
                                        ; recur
          (read-var ch (str-iter-next! stream)(str-iter-peek stream) var-bracket add-exp token)
          (do
           (if (str-empty? token) (err "Syntax error, floating '$'."))
           (add-exp (sym "shell-read::var-or-env"))
            (add-exp (sym token))))))

  (defn read-list (last-ch ch peek-ch add-exp close-token do-read push-token get-result clear-result)

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
              (if (and (= peek-ch #\$)(= peek-ch #\"))
                  (shell-read-int2 stream nil nil)
                  (shell-read-int2 stream #t last-file)))
        (if wrapper
            (add-exp (list (sym wrapper) temp-result))
            (add-exp temp-result))))

    (let ((just-read)
          (done))
      (cond
        ((and (not (= last-ch #\\))(= ch #\)) (> paren-level 0))
         (set! paren-level (- paren-level 1))
         (set! done #t))
        ((and (not (= last-ch #\\))(= ch #\)))
         (set! done #t))
        ((and (not (= ch #\\))(or (= peek-ch #\")(= peek-ch #\$)))
         (do-read stream ch)
         (set! just-read #t))
        ((and (not (= last-ch #\\))(= ch #\&)(= peek-ch #\&)) ; AND
         (str-iter-next! stream)
         (setup-chainer "and" "shell-read::handle-process" nil)
         (set! done #t))
        ((and (not (= last-ch #\\))(= ch #\|)(= peek-ch #\|)) ; OR
         (str-iter-next! stream)
         (setup-chainer "or" "shell-read::handle-process" nil)
         (set! done #t))
        ((and (not (= last-ch #\\))(= ch #\@)(= peek-ch #\@)) ; DO
         (str-iter-next! stream)
         (setup-chainer "do" nil nil)
         (set! done #t))
        ((and (not (= last-ch #\\))(= ch #\|)) ; PIPE
         (setup-chainer "root::pipe" nil nil)
         (set! done #t))
        #|((and (not (= last-ch #\\))(= ch #\>)(= peek-ch #\>)) ; out>>
         (str-iter-next! stream)
         (setup-chainer "shell-read::redir>>" nil #t)
         (set! done #t))
        ((and (not (= last-ch #\\))(= ch #\>)) ; out>
         (setup-chainer "shell-read::redir>" nil #t)
         (set! done #t))|#
        ((char-whitespace? ch)
         (close-token))
        ((push-token ch) nil))
      (if (and (not done)(not (str-iter-empty? stream)))
          (if just-read
              (do
               (set! just-read nil)
               (read-list #\space #\space (str-iter-peek stream) add-exp close-token do-read push-token get-result clear-result)) ;recur
              (read-list ch (str-iter-next! stream)(str-iter-peek stream) add-exp close-token do-read push-token get-result clear-result)) ;recur
          (close-token))))

  (defn shell-read-int2 (stream in-paren one-string)
    (let ((result)
          (token)
          (var-bracket nil)
          (last-pair (list))
          (add-exp)
          (close-token)
          (push-token)
          (do-read)
          (get-result)
          (clear-result)
          (first-sym #t)
          (ch)
          (peek-ch))

      (set! token (str))
      (set! add-exp (fn (exp)
                        (let ((new-pair (join exp nil)))
                          (if (nil? last-pair) (set! result new-pair))
                          (xdr! last-pair new-pair)
                          (set! last-pair new-pair))))

      (set! close-token (fn ()
                            (if (not (str-empty? token))
                                (if first-sym (do (add-exp (sym token)) (set! first-sym nil))
                                    (maybe-glob? token) (add-exp (list 'glob token))
                                    (add-exp token)))
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
        (one-string
         ((fn () (if (and (char? ch)(char-whitespace? ch)) (do (set! ch (str-iter-next! stream))(recur)))))
         (read-var #\space ch peek-ch nil add-exp token))
        (in-paren
         (add-exp (sym "shell-read::sys-apply"))
         (read-list #\space ch peek-ch add-exp close-token do-read push-token get-result clear-result))
        ((and (= ch #\()(= peek-ch #\())
         (set! result (read stream))
         (set! ch (str-iter-next! stream))
         ((fn () (if (and (char? ch)(char-whitespace? ch)) (do (set! ch (str-iter-next! stream))(recur)))))
         (if (not (= #\) ch))
             (err "Unbalanced ) in '\$' shell read macro"))
         result)
        ((= ch #\()
         (set! paren-level (+ paren-level 1))
         (add-exp (sym "shell-read::sys-apply"))
         (read-list #\space (str-iter-next! stream)(str-iter-peek stream) add-exp close-token do-read push-token get-result clear-result))
        ((= ch #\{)
         (read-var #\space (str-iter-next! stream)(str-iter-peek stream) #t add-exp token))
        (#t
         (read-var #\space ch peek-ch nil add-exp token)))
      result)))

(defn shell-read-int (stream in-paren) (shell-read-int2 stream in-paren nil))

(defn shell-read (stream ch_start) (shell-read::shell-read-int stream nil))

(def *ns-exports* nil)

(ns-pop)

(hash-set! *read-table* #\$ 'shell-read::shell-read)
(hash-set! *string-read-table* #\$ 'shell-read::shell-read)
