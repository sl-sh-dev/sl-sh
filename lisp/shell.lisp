(ns-create 'shell)
(core::ns-import 'core)

;;; Macros to make working with the shell easier.

;; Create an alias, intended to be used with executables not lisp code (use defn for that).
(defmacro alias (name body)
	`(defmacro ,name (&rest args)
		(append (quote ,body) args)))

;; Redirect stdout to file, append the output.
(defmacro out>> (file body)
	`(if (file? ,file)
		(dyn '*stdout* ,file ,body)
		(dyn '*stdout* (open ,file :create :append) ,body)))

;; Redirect stdout to file, truncate the file first.
(defmacro out> (file body)
	`(if (file? ,file)
		(dyn '*stdout* ,file ,body)
		(dyn '*stdout* (open ,file :create :truncate) ,body)))

;; Redirect stderr to file, append the output.
(defmacro err>> (file body)
	`(if (file? ,file)
		(dyn '*stderr* ,file ,body)
		(dyn '*stderr* (open ,file :create :append) ,body)))

;; Redirect stderr to file, truncate the file first.
(defmacro err> (file body)
	`(if (file? ,file)
		(dyn '*stderr* ,file ,body)
		(dyn '*stderr* (open ,file :create :truncate) ,body)))

;; Redirect both stdout and stderr to the same file, append the output.
(defmacro out-err>> (file body)
	`(if (file? ,file)
		(dyn '*stdout* ,file (dyn '*stderr* ,file ,body))
		(dyn '*stdout* (open ,file :create :append) (dyn '*stderr* *stdout* ,body))))

;; Redirect both stdout and stderr to the same file, truncate the file first.
(defmacro out-err> (file body)
	`(if (file? ,file)
		(dyn '*stdout* ,file (dyn '*stderr* ,file ,body))
		(dyn '*stdout* (open ,file :create :truncate) (dyn '*stderr* *stdout* ,body))))

;; Redirect stdout to null (/dev/null equivelent).
(defmacro out>null (body)
	`(dyn '*stdout* (open "/dev/null" :write) ,body))

;; Redirect stderr to null (/dev/null equivelent).
(defmacro err>null (body)
	`(dyn '*stderr* (open "/dev/null" :write) ,body))

;; Redirect both stdout and stderr to null (/dev/null equivelent).
(defmacro out-err>null (body)
	`(dyn '*stdout* (open "/dev/null" :write) (dyn '*stderr* *stdout* ,body)))

;; Shorthand for pipe builtin.
(defmacro | (&rest body)
	`(pipe ,@body))

(defq pushd nil)
(defq popd nil)
(defq dirs nil)
(defq get-dirs nil)
(defq clear-dirs nil)
(defq set-dirs-max nil)
;; Scope to contain then pushd/popd/dirs functions.
(let ((dir_stack (make-vec 20)) (dir_stack_max 20))
	;; Push current directory on the directory stack and change to new directory.
	(setfn pushd (dir) (if (form (cd dir))
		(progn
			(vec-push! dir_stack $OLDPWD)
			(if (> (length dir_stack) dir_stack_max) (vec-remove-nth! 0 dir_stack))
			t)
		nil))
	;; Pop first directory from directory stack and change to it.
	(setfn popd () (if (> (length dir_stack) 0)
		(cd (vec-pop! dir_stack))
		(println "Dir stack is empty")))
	;; List the directory stack.
	(setfn dirs ()
		(for d dir_stack (println d)))
	;; Return the vector of directories.
	(setfn get-dirs () dir_stack)
	;; Clears the directory stack.
	(setfn clear-dirs ()
		(vec-clear! dir_stack))
	;; Sets the max number of directories to save in the stack.
	(setfn set-dirs-max (max)
		(if (and (= (type max) "Int")(> max 1))
			(setq dir_stack_max max)
			(err "Error, max must be a positive Int greater then one"))))


;; Like let but sets environment variables that are reset after the macro finishes.
(defmacro let-env (vals &rest let_body)
	((fn (params bindings olds) (progn
		(fori idx el vals
			(if (= 1 (length el))
				(progn
					(vec-insert-nth! idx (nth 0 el) params)
					(vec-insert-nth! idx nil bindings)
					(vec-insert-nth! idx (eval (to-symbol (str "$" (nth 0 el)))) olds))
				(if (= 2 (length el))
					(progn
						(vec-insert-nth! idx (nth 0 el) params)
						(vec-insert-nth! idx (nth 1 el) bindings)
						(vec-insert-nth! idx (eval (to-symbol (str "$" (nth 0 el)))) olds))
					(err "ERROR: invalid bindings on let-env"))))
		`((fn (params bindings olds)
			(unwind-protect
				(progn
					(fori i p params
						(if (null (nth i bindings))
							(unexport p)
							(export p (nth i bindings))))
					,@let_body)
				(fori i p params
					(if (null (nth i olds))
						(unexport p)
						(export p (nth i olds))))))
		(quote ,params) (quote ,bindings) (quote ,olds))))
	(make-vec (length vals)) (make-vec (length vals)) (make-vec (length vals))))


(def '*fg-default* "\x1b[39m")
(def '*fg-black* "\x1b[30m")
(def '*fg-red* "\x1b[31m")
(def '*fg-green* "\x1b[32m")
(def '*fg-yellow* "\x1b[33m")
(def '*fg-blue* "\x1b[34m")
(def '*fg-magenta* "\x1b[35m")
(def '*fg-cyan* "\x1b[36m")
(def '*fg-white* "\x1b[37m")

;; True if the supplied command is a system command.
(defn sys-command? (com) (progn
	(def 'ret nil)
	(if (or (str-empty? com)(= (str-nth 0 com) #\/)(= (str-nth 0 com) #\.))
		(if (fs-exists? com) (set 'ret t))
		(for p (str-split ":" $PATH) (progn
			(def 'path (str p "/" com))
			(if (and (fs-exists? path)(not ret)) (set 'ret t)))))
	ret))

;; Turn on syntax highlighting at the repl.
(defmacro syntax-on  () '(progn
; Syntax highlight the supplied line.
(defn __line_handler (line) (progn
	(def 'plev 0)
	(def 'ch nil)
	(def 'out (str-buf ""))
	(def 'token (str-buf ""))
	(def 'tok-command t)

	(defn func? (com) (progn
		(def 'com-sym (to-symbol com))
		; Want the actual thing pointed to by the symbol in com for the test.
		(if (def? (to-symbol com))
			(set 'com (eval (to-symbol com))))
		(or (builtin? com)(lambda? com)(macro? com))))
	(defn paren-color (level) (progn
		(def 'col (% level 4))
		(if (= col 0) shell::*fg-white*
			(if (= col 1) shell::*fg-cyan*
				(if (= col 2) shell::*fg-yellow*
					(if (= col 3) shell::*fg-blue*))))))

	(defn command-color (command)
		(if (not tok-command)
			(if (def? (to-symbol command))
				(str shell::*fg-blue* command shell::*fg-default*)
				command)
			(if (func? command)
				(str shell::*fg-blue* command shell::*fg-default*)
				(if (sys-command? command)
					(str shell::*fg-white* command shell::*fg-default*)
					(str shell::*fg-red* command shell::*fg-default*)))))

	(defn prtoken () (progn
		(def 'ttok (str token))
		(str-buf-clear! token)
		(command-color ttok)))
	(defn paren-open () (progn
		(str-buf-push! out (prtoken) (paren-color plev) #\( shell::*fg-default*)
		(set 'plev (+ plev 1))
		(set 'tok-command t)))
	(defn paren-close ()
		(if (> plev 0)
			(progn
				(set 'plev (- plev 1))
				(str-buf-push! out (prtoken) (paren-color plev) #\) shell::*fg-default*))
			(str-buf-push! out (prtoken) shell::*fg-red* #\) shell::*fg-default*)))

	(defn whitespace (ch) (progn
		(str-buf-push! out (prtoken) ch)
		(set 'tok-command nil)))

	(dotimesi i (length line)
		(progn
		(set 'ch (str-nth i line))
		(if (char= ch #\()
			(paren-open)
			(if (char= ch #\))
				(paren-close)
				(if (char-whitespace? ch)
					(whitespace ch)
					(str-buf-push! token ch))))))
	(str-buf-push! out (prtoken))
	(str out)))
	nil))

;; Turn off syntax highlighting at the repl.
(defmacro syntax-off () '(undef '__line_handler))

(ns-export '(alias out>> out> err>> err> out-err>> out-err> out>null err>null out-err>null | pushd popd dirs get-dirs clear-dirs set-dirs-max let-env sys-command? syntax-on syntax-off))

