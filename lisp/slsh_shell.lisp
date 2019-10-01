;;; Macros to make working with the shell easier.

;; Create an alias, intended to be used with executables not lisp code (use defn for that).
(defmacro alias (name body)
	`(defn ,name (&rest args)
		(use-stdout (loose-symbols (eval (append (quote ,body) args))))))

;; Redirect stdout to file, append the output.
(defmacro out>> (file body)
	`(use-stdout (stdout-to ,file ,body)))

;; Redirect stdout to file, truncate the file first.
(defmacro out> (file body)
	`(progn (file-trunc ,file) (use-stdout (stdout-to ,file ,body))))

;; Redirect stderr to file, append the output.
(defmacro err>> (file body)
	`(use-stdout (stderr-to ,file ,body)))

;; Redirect stderr to file, truncate the file first.
(defmacro err> (file body)
	`(progn (file-trunc ,file) (use-stdout (stderr-to ,file ,body))))

;; Redirect both stdout and stderr to the same file, append the output.
(defmacro out-err>> (file body)
	`(stdout-to ,file (stderr-to ,file ,body)))

;; Redirect both stdout and stderr to the same file, truncate the file first.
(defmacro out-err> (file body)
	`(progn (file-trunc ,file) (stdout-to ,file (stderr-to ,file ,body))))

;; Redirect stdout to null (/dev/null equivelent).
(defmacro out>null (body)
	`(out-null ,body))

;; Redirect stderr to null (/dev/null equivelent).
(defmacro err>null (body)
	`(err-null ,body))

;; Redirect both stdout and stderr to null (/dev/null equivelent).
(defmacro out-err>null (body)
	`(out-null (err-null ,body)))

;; Shorthand for pipe builtin.
(defmacro | (&rest body)
	`(pipe ,@body))

;; Scope to contain then pushd/popd/dirs functions.
(let ((dir_stack '()) (dir_stack_max 20))
	;; Push current directory on the directory stack and change to new directory.
	(defn pushd (dir) (if (form (cd dir))
		(progn
			(setq dir_stack (setfirst $OLDPWD dir_stack))
			(if (> (length dir_stack) dir_stack_max) (setq dir_stack (butlast dir_stack)))
			t)
		nil))
	;; Pop first directory from directory stack and change to it.
	(defn popd () (if (> (length dir_stack) 0)
		(progn (cd (first dir_stack))
		(setq dir_stack (if (<= (length dir_stack) 1) '() (rest dir_stack))) nil)
		(println "Dir stack is empty")))
	;; List the directory stack.
	(defn dirs ()
		(for d dir_stack (println d)))
	;; Clears the directory stack.
	(defn clear-dirs ()
		(setq dir_stack '()))
	;; Sets the max number of directories to save in the stack.
	(defn set-dirs-max (max)
		(if (and (= (get-type max) "Int")(> max 1))
			(setq dir_stack_max max)
			(println "Error, max must be a positive Int greater then one"))))
