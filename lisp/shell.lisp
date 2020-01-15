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
						(def 'binding (nth 1 el))
						(if (or (list? binding)(vec? binding)) (set 'binding (eval binding)))
						(vec-insert-nth! idx (nth 0 el) params)
						(vec-insert-nth! idx binding bindings)
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

;; given 3 numbers 0-255 representing RGB values,
;; return corresponding ANSI font color code
(defn fg-color-rgb (R G B)
  (get-rgb-seq R G B :font))

;; given 3 numbers 0-255 representing RGB values,
;; return corresponding ANSI background color code
(defn bg-color-rgb (R G B)
  (get-rgb-seq R G B :bkrd))

(defn get-rgb-seq (R G B color-type)
      (let ((make-color (fn (color-code) (str "\x1b[" color-code ";2;" R ";" G ";" B "m"))))
      (match color-type
             (:font (make-color 38))
             (:bkrd (make-color 48))
             (nil (make-color 38)))))

;; True if the supplied command is a system command.
(defn sys-command? (com) (progn
	(def 'ret nil)
	(if (or (str-empty? com)(= (str-nth 0 com) #\/)(= (str-nth 0 com) #\.))
		(if (fs-exists? com) (set 'ret t))
		(for p (str-split ":" $PATH) (progn
			(def 'path (str p "/" com))
			(if (and (fs-exists? path)(not ret)) (set 'ret t)))))
	ret))

(defq set-tok-colors nil)

;; Turn on syntax highlighting at the repl.
(defmacro syntax-on  () '(progn
; Syntax highlight the supplied line.
(def '__line_handler nil)
(let ((plev 0)
	(ch nil)
	(bad-syms (make-hash))
	(sys-syms (make-hash))
	(out (str-buf ""))
	(token (str-buf ""))
	(tok-command t))

;; TODO make this a macro
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



	(defn my-sys-command? (command) (progn
		(def 'ret nil)
		(if (hash-haskey sys-syms command)
			(set 'ret t)
			(if (not (hash-haskey bad-syms command)) (progn
				(set 'ret (sys-command? command))
				(if ret (hash-set! sys-syms command t) (hash-set! bad-syms command t)))))
		ret))

	(defq tok-slsh-form-color shell::*fg-blue*)
	(defq tok-slsh-fcn-color shell::*fg-cyan*)
	(defq tok-default-color shell::*fg-default*)
	(defq tok-sys-command-color shell::*fg-white*)
	(defq tok-invalid-color shell::*fg-red*)

	(setfn set-tok-colors (slsh-form-color slsh-fcn-color default-color sys-command-color invalid-color)
		(progn
			(setq tok-slsh-form-color slsh-form-color)
			(setq tok-slsh-fcn-color slsh-fcn-color)
			(setq tok-default-color default-color)
			(setq tok-sys-command-color sys-command-color)
			(setq tok-invalid-color invalid-color)))

	(defn command-color (command)
		(if (not tok-command)
			(if (def? (to-symbol command))
				(str tok-slsh-form-color command shell::*fg-default*)
				(str tok-default-color command shell::*fg-default*))
			(if (func? command)
				(str tok-slsh-fcn-color command shell::*fg-default*)
				(if (sys-command? command)
					(str tok-sys-command-color command shell::*fg-default*)
					(str tok-invalid-color command shell::*fg-default*)))))

	(defn prtoken () (progn
		(def 'ttok token)
		(set 'token (str-buf ""))
		(command-color ttok)))
	(defn paren-open () (progn
		(def 'ret (str (prtoken) (paren-color plev) #\( shell::*fg-default*))
		(set 'plev (+ plev 1))
		(set 'tok-command t)
		ret))
	(defn paren-close ()
		(if (> plev 0)
			(progn
				(set 'plev (- plev 1))
				(str (prtoken) (paren-color plev) #\) shell::*fg-default*))
			(str (prtoken) shell::*fg-red* #\) shell::*fg-default*)))

	(defn whitespace (ch) (progn
		(def 'ret (str (prtoken) ch))
		;(def 'ret (str token ch))
		(set 'token (str-buf ""))
		(set 'tok-command nil)
		ret))

	(setfn __line_handler (line) (progn
		(set 'plev 0)
		(set 'ch nil)
		(set 'token (str-buf ""))
		(set 'tok-command t)
		(if (<= (length line) 1) (progn
			(hash-clear! bad-syms)
			(hash-clear! sys-syms)))
		(set 'out (str-buf-map (fn (ch) (progn
			(if (char= ch #\()
				(paren-open)
				(if (char= ch #\))
					(paren-close)
					(if (char-whitespace? ch)
						(whitespace ch)
						(progn (str-buf-push! token ch) "")))))) line))
		(str-buf-push! out (prtoken))
		(str out)))
		nil)))

;; Turn off syntax highlighting at the repl.
(defmacro syntax-off () '(undef '__line_handler))

(ns-export '(alias out>> out> err>> err> out-err>> out-err> out>null err>null out-err>null | pushd popd dirs get-dirs clear-dirs set-dirs-max let-env sys-command? syntax-on syntax-off set-tok-colors fg-color-rgb bg-color-rgb))
