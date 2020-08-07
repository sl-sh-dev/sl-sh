(if (ns-exists? 'shell) (ns-enter 'shell) (ns-create 'shell))

(ns-import 'iterator)

;;; Macros to make working with the shell easier.

(defmacro alias
"
Usage: (alias name body) or (alias name docstring body).

Create an alias, intended to be used with executables not lisp code (use defn
for that).

Section: shell
"
	(name &rest args) (progn
	(defq usage "Usage: (alias ll (ls -haltr)) or (alias ll \"ls show all files in reverse chronological order abd display size of each file..\" (ls -haltr)).")
	(shell::register-alias name)
	(match (length args)
		(2 (progn
			(defq docstring (vec-nth 0 args))
			(defq body (vec-nth 1 args))
			`(defmacro ,name ,docstring (&rest ars)
				(iterator::collect (iterator::append (quote ,body) ars)))))
		(1 (progn
			(defq body (vec-nth 0 args))
			`(defmacro ,name (&rest ars)
				(iterator::collect (iterator::append (quote ,body) ars)))))
		(0 (err usage))
		(nil (err usage)))))

;; Remove an alias, this should be used instead of a raw undef for aliases.
(defn unalias (name) (progn
	(shell::unregister-alias name)
	(undef name)))

(defmacro out>>
"
Redirect stdout to file, append the output.

Section: shell
"
	(file body)
	`(if (file? ,file)
		(dyn '*stdout* ,file ,body)
		(dyn '*stdout* (open ,file :create :append) ,body)))

(defmacro out>
"
Redirect stdout to file, truncate the file first.

Section: shell
"
	(file body)
	`(if (file? ,file)
		(dyn '*stdout* ,file ,body)
		(dyn '*stdout* (open ,file :create :truncate) ,body)))

(defmacro err>>
"
Redirect stderr to file, append the output.

Section: shell
"
	(file body)
	`(if (file? ,file)
		(dyn '*stderr* ,file ,body)
		(dyn '*stderr* (open ,file :create :append) ,body)))

(defmacro err>
"
Redirect stderr to file, truncate the file first.

Section: shell
"
	(file body)
	`(if (file? ,file)
		(dyn '*stderr* ,file ,body)
		(dyn '*stderr* (open ,file :create :truncate) ,body)))

(defmacro out-err>>
"
Redirect both stdout and stderr to the same file, append the output.

Section: shell
"
	(file body)
	`(if (file? ,file)
		(dyn '*stdout* ,file (dyn '*stderr* ,file ,body))
		(dyn '*stdout* (open ,file :create :append) (dyn '*stderr* *stdout* ,body))))

(defmacro out-err>
"
Redirect both stdout and stderr to the same file, truncate the file first.

Section: shell
"
	(file body)
	`(if (file? ,file)
		(dyn '*stdout* ,file (dyn '*stderr* ,file ,body))
		(dyn '*stdout* (open ,file :create :truncate) (dyn '*stderr* *stdout* ,body))))

(defmacro out>null
"
Redirect stdout to null (/dev/null equivelent).

Section: shell
"
	(body)
	`(dyn '*stdout* (open "/dev/null" :write) ,body))

(defmacro err>null
"
Redirect stderr to null (/dev/null equivelent).

Section: shell
"
(body)
	`(dyn '*stderr* (open "/dev/null" :write) ,body))

(defmacro out-err>null
"
Redirect both stdout and stderr to null (/dev/null equivelent).

Section: shell
"
	(body)
	`(dyn '*stdout* (open "/dev/null" :write) (dyn '*stderr* *stdout* ,body)))

(defmacro |
"
Shorthand for pipe builtin.

Section: shell
"
	(&rest body)
	`(pipe ,@body))

(defq pushd nil)
(defq popd nil)
(defq dirs nil)
(defq get-dirs nil)
(defq clear-dirs nil)
(defq set-dirs-max nil)
;; Scope to contain then pushd/popd/dirs functions.
(let ((dir_stack (make-vec 20)) (dir_stack_max 20))
	(setfn pushd
"
Push current directory on the directory stack and change to new directory.

Section: shell

Example:
(def 'cur-test-path (str (pwd)))
(pushd \"/tmp\")
(def 'cur-test-path2 (str (pwd)))
(assert-equal cur-test-path2 (str (pwd)))
(popd)
(assert-equal cur-test-path (str (pwd)))
"
		(dir) (if (form (cd dir))
		(progn
			(vec-push! dir_stack $OLDPWD)
			(if (> (length dir_stack) dir_stack_max) (vec-remove-nth! 0 dir_stack))
			t)
		nil))
	(setfn popd
"
Pop first directory from directory stack and change to it.

Section: shell

Example:
(def 'cur-test-path (str (pwd)))
(pushd \"/tmp\")
(def 'cur-test-path2 (str (pwd)))
(assert-equal cur-test-path2 (str (pwd)))
(popd)
(assert-equal cur-test-path (str (pwd)))
"
		() (if (> (length dir_stack) 0)
		(cd (vec-pop! dir_stack))
		(println "Dir stack is empty")))
	(setfn dirs
"
List the directory stack.

Section: shell

Example:
(clear-dirs)
(def 'cur-test-path (str (pwd)))
(dyn '*stdout* (open \"/tmp/sl-sh.dirs.test\" :create :truncate) (dirs))
(test::assert-equal nil (read-line (open \"/tmp/sl-sh.dirs.test\" :read)))
(pushd \"/tmp\")
(def 'cur-test-path2 (str (pwd)))
(dyn '*stdout* (open \"/tmp/sl-sh.dirs.test\" :create :truncate) (dirs))
(test::assert-equal cur-test-path (read-line (open \"/tmp/sl-sh.dirs.test\" :read)))
(pushd (str-trim cur-test-path))
(dyn '*stdout* (open \"/tmp/sl-sh.dirs.test\" :create :truncate) (dirs))
(def 'test-dirs-file (open \"/tmp/sl-sh.dirs.test\" :read))
(test::assert-equal cur-test-path (read-line test-dirs-file))
(test::assert-equal cur-test-path2 (read-line test-dirs-file))
(close test-dirs-file)
(popd)
(dyn '*stdout* (open \"/tmp/sl-sh.dirs.test\" :create :truncate) (dirs))
(test::assert-equal cur-test-path (read-line (open \"/tmp/sl-sh.dirs.test\" :read)))
(popd)
(dyn '*stdout* (open \"/tmp/sl-sh.dirs.test\" :create :truncate) (dirs))
(test::assert-equal nil (read-line (open \"/tmp/sl-sh.dirs.test\" :read)))
"
		()
		(for d in dir_stack (println d)))
	(setfn get-dirs
"
Return the vector of directories.

Section: shell

Example:
(clear-dirs)
(def 'cur-test-path (str-trim (str (pwd))))
(test::assert-equal '() (get-dirs))
(pushd \"/tmp\")
(def 'cur-test-path2 (str-trim (str (pwd))))
(test::assert-equal `(,cur-test-path) (get-dirs))
(pushd (str-trim cur-test-path))
(test::assert-equal `(,cur-test-path ,cur-test-path2) (get-dirs))
(popd)
(test::assert-equal `(,cur-test-path) (get-dirs))
(popd)
(test::assert-equal '() (get-dirs))
"
		() dir_stack)
	(setfn clear-dirs
"
Clears the directory stack.

Section: shell

Example:
(clear-dirs)
(def 'cur-test-path (str-trim (str (pwd))))
(test::assert-equal '() (get-dirs))
(pushd \"/tmp\")
(def 'cur-test-path2 (str-trim (str (pwd))))
(test::assert-equal `(,cur-test-path) (get-dirs))
(pushd (str-trim cur-test-path))
(test::assert-equal `(,cur-test-path ,cur-test-path2) (get-dirs))
(clear-dirs)
(test::assert-equal '() (get-dirs))
"
		()
		(vec-clear! dir_stack))
	(setfn set-dirs-max
"
Sets the max number of directories to save in the stack.

Section: shell

Example:
(clear-dirs)
(def 'cur-test-path (str-trim (str (pwd))))
(pushd \"/tmp\")
(def 'cur-test-path2 (str-trim (str (pwd))))
(pushd (str-trim cur-test-path))
(pushd \"/tmp\")
(pushd (str-trim cur-test-path))
(test::assert-equal `(,cur-test-path ,cur-test-path2 ,cur-test-path ,cur-test-path2) (get-dirs))
(clear-dirs)
(set-dirs-max 3)
(pushd \"/tmp\")
(pushd (str-trim cur-test-path))
(pushd \"/tmp\")
(pushd (str-trim cur-test-path))
(test::assert-equal `(,cur-test-path2 ,cur-test-path ,cur-test-path2) (get-dirs))
"
		(max)
		(if (and (= (type max) "Int")(> max 1))
			(setq dir_stack_max max)
			(err "Error, max must be a positive Int greater then one"))))


(defmacro let-env
"
Like let but sets environment variables that are reset after the macro finishes.

Section: shell

Example:
(test::assert-false \$LET-ENV-TEST-VAR-NOT-HERE)
(let-env ((LET-ENV-TEST-VAR-NOT-HERE \"here\"))
    (test::assert-equal \"here\" \$LET-ENV-TEST-VAR-NOT-HERE))
(test::assert-false \$LET-ENV-TEST-VAR-NOT-HERE)
"
	(vals &rest let_body)
	((fn (params bindings olds) (progn
		(iterator::for-i idx el in vals
			(if (= 1 (length el))
				(progn
					(vec-insert-nth! idx (iterator::nth 0 el) params)
					(vec-insert-nth! idx nil bindings)
					(vec-insert-nth! idx (eval (to-symbol (str "\$" (iterator::nth 0 el)))) olds))
				(if (= 2 (length el))
					(progn
						(def 'binding (iterator::nth 1 el))
						(if (or (list? binding)(vec? binding)) (set 'binding (eval binding)))
						(vec-insert-nth! idx (iterator::nth 0 el) params)
						(vec-insert-nth! idx binding bindings)
						(vec-insert-nth! idx (eval (to-symbol (str "\$" (iterator::nth 0 el)))) olds))
					(err "ERROR: invalid bindings on let-env"))))
		`((fn (params bindings olds)
			(unwind-protect
				(progn
					(iterator::for-i i p in params
						(if (null (iterator::nth i bindings))
							(unexport p)
							(export p (iterator::nth i bindings))))
					,@let_body)
				(iterator::for-i i p in params
					(if (null (iterator::nth i olds))
						(unexport p)
						(export p (iterator::nth i olds))))))
		(quote ,params) (quote ,bindings) (quote ,olds))))
	(make-vec (length vals)) (make-vec (length vals)) (make-vec (length vals))))


;; https://wiki-dev.bash-hackers.org/scripting/terminalcodes
(def '*fg-default* "\x1b[39m")
(def '*fg-black* "\x1b[30m")
(def '*fg-red* "\x1b[31m")
(def '*fg-green* "\x1b[32m")
(def '*fg-yellow* "\x1b[33m")
(def '*fg-blue* "\x1b[34m")
(def '*fg-magenta* "\x1b[35m")
(def '*fg-cyan* "\x1b[36m")
(def '*fg-white* "\x1b[37m")

(def '*bg-default* "\x1b[49m")
(def '*bg-black* "\x1b[40m")
(def '*bg-red* "\x1b[41m")
(def '*bg-green* "\x1b[42m")
(def '*bg-yellow* "\x1b[43m")
(def '*bg-blue* "\x1b[44m")
(def '*bg-magenta* "\x1b[45m")
(def '*bg-cyan* "\x1b[46m")
(def '*bg-white* "\x1b[47m")

(defn fg-color-rgb
"
Usage: (fg-color-rgb red-val green-val blue-val)

Set the foreground color to the desired rgb where each arg is an integer between 0 and 255 inclusive.

Section: shell
"
	(R G B)
		(get-rgb-seq R G B :font))

(defn bg-color-rgb
"
Usage: (bg-color-rgb red-val green-val blue-val)

Set the background color to the desired rgb where each arg is an integer between 0 and 255 inclusive.

Section: shell
"
	(R G B)
		(get-rgb-seq R G B :bkrd))

(defn get-rgb-seq (R G B color-type)
      (let ((make-color (fn (color-code) (str "\x1b[" color-code ";2;" R ";" G ";" B "m"))))
      (match color-type
             (:font (make-color 38))
             (:bkrd (make-color 48))
             (nil (make-color 38)))))

(defmacro sys-alias?
"
True if the supplied command is an alias for a system command.

Section: shell
"
	(com) `(progn
	(def 'ret nil)
	(def 'val (to-symbol ,com))
	(if (def? val)
		(set 'val (eval val)))
	(if (macro? val) (get-error  ; If the next two lines fail it was not an alias...
		(def 'expansion (expand-macro (val)))
		(set 'ret (sys-command? (symbol-name (first expansion))))))
	ret))

(defn sys-command?
"
True if the supplied command is a system command.

Section: shell

Example:
(assert-true (sys-command? \"ls\"))
(assert-false (sys-command? \"rst-not-a-comand-strsnt\"))
"
	(com) (progn
	(def 'ret nil)
	(if (or (str-empty? com)(= (str-nth 0 com) #\/)(= (str-nth 0 com) #\.))
		(if (fs-exists? com) (set 'ret t))
		(if (and (str-contains "/" com)(fs-exists? (str "./" com)))
			(set 'ret t)
			(if (and (str-starts-with "~/" com)(fs-exists? (str-replace "~/" $HOME com)))
				(set 'ret t)
				(for p in (str-split ":" $PATH) (progn
					(def 'path (str p "/" com))
					(if (and (fs-exists? path)(not ret)) (set 'ret t)))))))
	ret))

(defq register-alias nil)
(defq unregister-alias nil)
(defq alias? nil)
(let ((alias (make-hash)))
	(setfn register-alias
		"
		Registers an alias to the current scope. Useful if unregistering or
		ability to know whether an alias has been registered is desirable.

		Section: shell
		"
		(name) (hash-set! alias name t))
	(setfn unregister-alias
		"
		Unregisters an alias, removing it from scope.

		Section: shell
		"
		(name) (hash-remove! alias name))
	(setfn alias?
		"
		Provides boolean value confirming or denying given alias' presence
		in set of registered aliases.

		Section: shell
		"
		(name) (hash-haskey alias name)))

; These will be imported with syntax-on (ie copied into another namespace).
; Since syntax-on is a macro these copies are what will be read/used in that
; namespace vs these originals changing.
(defq tok-slsh-form-color shell::*fg-blue*)
(defq tok-slsh-fcn-color shell::*fg-cyan*)
(defq tok-default-color shell::*fg-default*)
(defq tok-sys-command-color shell::*fg-white*)
(defq tok-sys-alias-color shell::*fg-default*)
(defq tok-string-color shell::*fg-magenta*)
(defq tok-invalid-color shell::*fg-red*)

(defmacro syntax-on
"
Turn on syntax highlighting at the repl.

Section: shell
"
() '(progn
; Syntax highlight the supplied line.
(def '__line_handler nil)
(let ((plev 0)
	(ch nil)
	(bad-syms (make-hash))
	(sys-syms (make-hash))
	(out (str ""))
	(token (str ""))
	(in-sys-command nil)
	(tok-command t))

	(defn func? (com) (progn
		; Want the actual thing pointed to by the symbol in com for the test.
		(if (def? com)
			(set 'com (eval (to-symbol com))))
		(or (builtin? com)(lambda? com)(macro? com))))

	(defn paren-color (level) (progn
		(def 'col (% level 4))
		(if (= col 0) shell::*fg-white*
			(if (= col 1) shell::*fg-cyan*
				(if (= col 2) shell::*fg-yellow*
					(if (= col 3) shell::*fg-blue*))))))

	(defn my-sys-command? (command) (progn
		(def 'ret nil)
		(if (hash-haskey sys-syms command)
			(set 'ret t)
			(if (not (hash-haskey bad-syms command)) (progn
				(set 'ret (sys-command? command))
				(if ret (hash-set! sys-syms command t) (hash-set! bad-syms command t)))))
		ret))

	(defn command-color (command)
		(if (not tok-command)
			(if (def? command)
				(if (func? command)
					(if in-sys-command
						(str tok-default-color command shell::*fg-default*)
						(str tok-slsh-fcn-color command shell::*fg-default*))
					(str tok-slsh-form-color command shell::*fg-default*))
				(str tok-default-color command shell::*fg-default*))
			(if (func? command)
				(if (or (shell::alias? command)(shell::sys-alias? command))
					(progn (set 'in-sys-command t)(str tok-sys-alias-color command shell::*fg-default*))
					(str tok-slsh-fcn-color command shell::*fg-default*))
				(if (my-sys-command? command)
					(progn (set 'in-sys-command t)(str tok-sys-command-color command shell::*fg-default*))
					(str tok-invalid-color command shell::*fg-default*)))))

	(defn prrawtoken () (progn
		(def 'ttok token)
		(set 'token (str ""))
		ttok))
	(defn prtoken () (progn
		(def 'ttok token)
		(set 'token (str ""))
		(command-color ttok)))
	(defn paren-open () (progn
		(def 'ret (str (prtoken) (paren-color plev) #\( shell::*fg-default*))
		(set 'plev (+ plev 1))
		(set 'tok-command t)
		ret))
	(defn paren-close () (progn
		(set 'in-sys-command nil)
		(if (> plev 0)
			(progn
				(set 'plev (- plev 1))
				(str (prtoken) (paren-color plev) #\) shell::*fg-default*))
			(str (prtoken) shell::*fg-red* #\) shell::*fg-default*))))

	(defn whitespace (ch) (progn
		(def 'ret (str (prtoken) ch))
		;(def 'ret (str token ch))
		(set 'token (str ""))
		(set 'tok-command nil)
		ret))

	(setfn __line_handler (line) (progn
		(def 'in-quote nil)
		(def 'last-ch #\ )
		(set 'plev 0)
		(set 'ch nil)
		(set 'token (str ""))
		(set 'tok-command t)
		(set 'in-sys-command nil)
		(if (<= (length line) 1) (progn
			(hash-clear! bad-syms)
			(hash-clear! sys-syms)))
		(set 'out (str-map (fn (ch) (progn
			(def 'ret (if in-quote
				(progn
					(str-push! token ch)
					(if (and (not (= last-ch #\\))(= ch #\"))
						(progn
							(set 'in-quote nil)
							(str-push! token shell::*fg-default*)
							(prrawtoken))
						""))
				(if (and (not (= last-ch #\\))(= ch #\"))
					(progn (str-push! token (str tok-string-color ch))(set 'in-quote t) "")
					(if (= ch #\()
						(paren-open)
						(if (= ch #\))
							(paren-close)
							(if (char-whitespace? ch)
								(whitespace ch)
								(progn (str-push! token ch) "")))))))
			(progn (set 'last-ch ch) ret))) line))
		(if in-quote (str-push! out (prrawtoken)) (str-push! out (prtoken)))
		(str out)))
		nil)))

(defmacro syntax-off
"
Turn off syntax highlighting at the repl.

Section: shell
"
	() '(undef '__line_handler))

(load "endfix.lisp")
(defmacro endfix-on "
Allows use of infix notation for common shell forms. The following is the
complete mapping in lisp/endfix.lisp of all supported infix operators and
the corresponding sl-sh function they map to:
	'|| 'or
	'| '|
	'@@ 'progn (@@ is used instead of ; because ; is a comment in lisp)
	'&& 'and
	'out> 'out>
	'out>> 'out>>
	'err> 'err>
	'err>> 'err>>
	'out>null 'out>null
	'out-err> 'out-err>
	'out-err>> 'out-err>>
	'out-err>null 'out-err>null


Section: shell
"
	() '(def '__exec_hook shell::endfix-hook))



(def '*last-status* 0)
(def '*last-command* "")

(defn print-backtrace (backtrace) 
    (for b in backtrace
        (print (if (def 'file (meta-file-name b)) file "??????") ":\t"
               "line " (if (def 'line (meta-line-no b)) line "??") ":\t"
               "column " (if (def 'col (meta-column-no b)) col "??") "\n")))

(defn print-error (error)
    (if (= :error (car error))
        (progn
            (println (cadr error))
            (print-backtrace (caddr error)))
        (err "Not an error!")))

(defn repl-eof (result)
      (progn
        (if (and (values? result)
                 (= 2 (values-length result))
                 (= :unexpected-eof (values-nth 1 result))))))

(defn path_list_trunc (plist)
	(if (> (length plist) 1)
		(if (> (length (first plist)) 0) 
			(vec-insert-nth! 0 (str-sub 0 1 (first plist)) (path_list_trunc (rest plist)))
			(path_list_trunc (rest plist)))
		plist))

(defn get_pwd ()
	(str-cat-list "/" (path_list_trunc (str-split "/" (str-replace (str-trim $PWD) $HOME "~")))))

(defn set_prompt_tail ()
	(if (= *last-status* 0) "\x1b[32m>\x1b[39m " (format "\x1b[31m(" *last-status* ")>\x1b[39m ")))

(defn __prompt ()
	(str "\x1b[32m[" *active-ns* "]:" $HOST ":\x1b[34m" (str-trim (get_pwd)) (set_prompt_tail)))


(defn repl-line (line line-len)
      (progn
        (export 'LAST_STATUS "0")
        (set '*last-status* 0)
        (def 'ast (if (and (def? '__exec_hook)(lambda? __exec_hook))
                    (__exec_hook line)
                    (read-all line)))
        (set 'ast (if (string? ast) (read-all ast) ast))
        (def 'result (loose-symbols (get-error (eval-in-namespace *active-ns* ast))))
        (if (= :ok (car result))
          (progn
            (if (process? (cdr result)) nil
              (nil? (cdr result)) nil
              (file? (cdr result)) nil
              (println (cdr result)))
            (if (> line-len 0)
              (progn
                ; Save history
                (prompt-history-push :repl line)
                (set '*last-command* line))))
          (progn 
            ; Save temp history
            (if (> line-len 0) (prompt-history-push-throwaway :repl line))
            (print-error result)))))

(defn repl () (progn
      (defn get-prompt ()
            (progn
              (def 'ns-prompt (to-symbol (str *active-ns* "::__prompt")))
              (if (def? ns-prompt) (apply ns-prompt nil) (__prompt))))
      (defn repl-inner ()
            (progn
              (prompt-history-context :repl $PWD)
              (reap-jobs)
              ;(def 'result '(:ok . nil))
              (def 'save-last-status *last-status*)
              (def 'line (prompt :repl (str "XX " (get-prompt)) "~/.local/share/sl-sh/history"))
              (export 'LAST_STATUS save-last-status)
              ;(def (to-symbol (str *active-ns* "::*last-status*")) save-last-status)
              (set '*last-status* save-last-status)
              (def 'line-len (length (str-trim line)))
              (if (and (> line-len 0)(not (values? line))) (repl-line line line-len))
              (if (not (repl-eof line)) (recur))))
      ((fn () (progn
                (def 'result (get-error (repl-inner)))
                (if (= :error (car result)) (progn (print-error result)(recur))))))))

(ns-export '(
	alias
	register-alias
	unregister-alias
	alias?
	out>>
	out>
	err>>
	err>
	out-err>>
	out-err>
	out>null
	err>null
	out-err>null
	|
	pushd
	popd
	dirs
	get-dirs
	clear-dirs
	set-dirs-max
	let-env
	sys-command?
	syntax-on
	syntax-off
	tok-slsh-form-color
	tok-slsh-fcn-color
	tok-default-color
	tok-sys-command-color
	tok-sys-alias-color
	tok-string-color
	tok-invalid-color
	fg-color-rgb
	bg-color-rgb
	endfix-on))

