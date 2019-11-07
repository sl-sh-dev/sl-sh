;;; Macros to make working with the shell easier.

;; Create an alias, intended to be used with executables not lisp code (use defn for that).
(defmacro alias (name body)
	`(defn ,name (&rest args)
		(loose-symbols (eval (append (quote ,body) args)))))

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
			(push! dir_stack $OLDPWD)
			(if (> (length dir_stack) dir_stack_max) (vremove-nth! 0 dir_stack))
			t)
		nil))
	;; Pop first directory from directory stack and change to it.
	(setfn popd () (if (> (length dir_stack) 0)
		(cd (pop! dir_stack))
		(println "Dir stack is empty")))
	;; List the directory stack.
	(setfn dirs ()
		(for d dir_stack (println d)))
	;; Return the vector of directories.
	(setfn get-dirs () dir_stack)
	;; Clears the directory stack.
	(setfn clear-dirs ()
		(vclear! dir_stack))
	;; Sets the max number of directories to save in the stack.
	(setfn set-dirs-max (max)
		(if (and (= (type max) "Int")(> max 1))
			(setq dir_stack_max max)
			(err "Error, max must be a positive Int greater then one"))))
