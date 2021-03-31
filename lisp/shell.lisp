(ns-push 'shell)

(ns-import 'iterator)

;;; Macros to make working with the shell easier.

(defmacro alias
"
Usage: (alias name body) or (alias name docstring body).

Create an alias, intended to be used with executables not lisp code (use defn
for that).

Section: shell
"
	(name &rest args)
	(var usage "Usage: (alias ll (ls -haltr)) or (alias ll \"ls show all files in reverse chronological order abd display size of each file..\" (ls -haltr)).")
	(shell::register-alias name)
	(var docstring nil)
	(var body nil)
	(match (length args)
		(2 (do
			(set! docstring (vec-nth args 0))
			(set! body (vec-nth args 1))
			`(defmacro ,name ,docstring (&rest ars)
				(iterator::collect (iterator::append (quote ,body) ars)))))
		(1 (do
			(set! body (vec-nth args 0))
			`(defmacro ,name (&rest ars)
				(iterator::collect (iterator::append (quote ,body) ars)))))
		(0 (err usage))
		(nil (err usage))))

;; Remove an alias, this should be used instead of a raw undef for aliases.
(defn unalias (name)
	(shell::unregister-alias name)
	(undef name))

(defmacro out>>
"
Redirect stdout to file, append the output.

Section: shell

Example:
(out> \"/tmp/sl-sh.out>>.test\" (syscall echo \"stdout redir one\"))
(def topen (open \"/tmp/sl-sh.out>>.test\" :read))
(test::assert-equal \"stdout redir one\n\" (read-line topen))
(test::assert-false (read-line topen))
(close topen)
(out>> \"/tmp/sl-sh.out>>.test\" (syscall echo \"stdout redir two\"))
(def topen (open \"/tmp/sl-sh.out>>.test\" :read))
(test::assert-equal \"stdout redir one\n\" (read-line topen))
(test::assert-equal \"stdout redir two\n\" (read-line topen))
(test::assert-false (read-line topen))
(close topen)
"
	(file body)
	`(if (file? ,file)
		(dyn *stdout* ,file ,body)
		(dyn *stdout* (open ,file :create :append) ,body)))

(defmacro out>
"
Redirect stdout to file, truncate the file first.

Section: shell

Example:
(out> \"/tmp/sl-sh.out>.test\" (syscall echo \"stdout redir one\"))
(def topen (open \"/tmp/sl-sh.out>.test\" :read))
(test::assert-equal \"stdout redir one\n\" (read-line topen))
(test::assert-false (read-line topen))
(close topen)
(out> \"/tmp/sl-sh.out>.test\" (syscall echo \"stdout redir two\"))
(def topen (open \"/tmp/sl-sh.out>.test\" :read))
(test::assert-equal \"stdout redir two\n\" (read-line topen))
(test::assert-false (read-line topen))
(close topen)
"
	(file body)
	`(if (file? ,file)
		(dyn *stdout* ,file ,body)
		(dyn *stdout* (open ,file :create :truncate) ,body)))

(defmacro err>>
"
Redirect stderr to file, append the output.

Section: shell

Example:
(err> \"/tmp/sl-sh.err>>.test\" (eprintln \"stderr redir one\"))
(def topen (open \"/tmp/sl-sh.err>>.test\" :read))
(test::assert-equal \"stderr redir one\n\" (read-line topen))
(test::assert-false (read-line topen))
(close topen)
(err>> \"/tmp/sl-sh.err>>.test\" (eprintln \"stderr redir two\"))
(def topen (open \"/tmp/sl-sh.err>>.test\" :read))
(test::assert-equal \"stderr redir one\n\" (read-line topen))
(test::assert-equal \"stderr redir two\n\" (read-line topen))
(test::assert-false (read-line topen))
(close topen)
"
	(file body)
	`(if (file? ,file)
		(dyn *stderr* ,file ,body)
		(dyn *stderr* (open ,file :create :append) ,body)))

(defmacro err>
"
Redirect stderr to file, truncate the file first.

Section: shell

Example:
(err> \"/tmp/sl-sh.err>.test\" (eprintln \"stderr redir one\"))
(def topen (open \"/tmp/sl-sh.err>.test\" :read))
(test::assert-equal \"stderr redir one\n\" (read-line topen))
(test::assert-false (read-line topen))
(close topen)
(err> \"/tmp/sl-sh.err>.test\" (eprintln \"stderr redir two\"))
(def topen (open \"/tmp/sl-sh.err>.test\" :read))
(test::assert-equal \"stderr redir two\n\" (read-line topen))
(test::assert-false (read-line topen))
(close topen)
"
	(file body)
	`(if (file? ,file)
		(dyn *stderr* ,file ,body)
		(dyn *stderr* (open ,file :create :truncate) ,body)))

(defmacro out-err>>
"
Redirect both stdout and stderr to the same file, append the output.

Section: shell

Example:
(out-err> \"/tmp/sl-sh.out-err>>.test\" (do (println \"stdout redir one\")(eprintln \"stderr redir one\")))
(def topen (open \"/tmp/sl-sh.out-err>>.test\" :read))
(test::assert-equal \"stdout redir one\n\" (read-line topen))
(test::assert-equal \"stderr redir one\n\" (read-line topen))
(test::assert-false (read-line topen))
(close topen)
(out-err>> \"/tmp/sl-sh.out-err>>.test\" (do (println \"stdout redir two\")(eprintln \"stderr redir two\")))
(def topen (open \"/tmp/sl-sh.out-err>>.test\" :read))
(test::assert-equal \"stdout redir one\n\" (read-line topen))
(test::assert-equal \"stderr redir one\n\" (read-line topen))
(test::assert-equal \"stdout redir two\n\" (read-line topen))
(test::assert-equal \"stderr redir two\n\" (read-line topen))
(test::assert-false (read-line topen))
(close topen)
"
	(file body)
	`(if (file? ,file)
		(dyn *stdout* ,file (dyn *stderr* ,file ,body))
		(dyn *stdout* (open ,file :create :append) (dyn *stderr* *stdout* ,body))))

(defmacro out-err>
"
Redirect both stdout and stderr to the same file, truncate the file first.

Section: shell

Example:
(out-err> \"/tmp/sl-sh.out-err>.test\" (do (println \"stdout redir one\")(eprintln \"stderr redir one\")))
(def topen (open \"/tmp/sl-sh.out-err>.test\" :read))
(test::assert-equal \"stdout redir one\n\" (read-line topen))
(test::assert-equal \"stderr redir one\n\" (read-line topen))
(test::assert-false (read-line topen))
(close topen)
(out-err> \"/tmp/sl-sh.out-err>.test\" (do (syscall echo \"stdout echo redir one\")(eprintln \"stderr redir one\")))
(def topen (open \"/tmp/sl-sh.out-err>.test\" :read))
(test::assert-equal \"stdout echo redir one\n\" (read-line topen))
(test::assert-equal \"stderr redir one\n\" (read-line topen))
(test::assert-false (read-line topen))
(close topen)
(out-err> \"/tmp/sl-sh.out-err>.test\" (do (println \"stdout redir two\")(eprintln \"stderr redir two\")))
(def topen (open \"/tmp/sl-sh.out-err>.test\" :read))
(test::assert-equal \"stdout redir two\n\" (read-line topen))
(test::assert-equal \"stderr redir two\n\" (read-line topen))
(test::assert-false (read-line topen))
(close topen)
"
	(file body)
	`(if (file? ,file)
		(dyn *stdout* ,file (dyn *stderr* ,file ,body))
		(dyn *stdout* (open ,file :create :truncate) (dyn *stderr* *stdout* ,body))))

(defmacro out>null
"
Redirect stdout to null (/dev/null equivelent).

Section: shell

Example:
(out> \"/tmp/sl-sh.out>null.test\" (do (println \"stdout redir one\")(out>null (println \"stdnull redir one\"))))
(def topen (open \"/tmp/sl-sh.out>null.test\" :read))
(test::assert-equal \"stdout redir one\n\" (read-line topen))
(test::assert-false (read-line topen))
(close topen)
"
	(body)
	`(dyn *stdout* (open "/dev/null" :write) ,body))

(defmacro err>null
"
Redirect stderr to null (/dev/null equivelent).

Section: shell

Example:
(err> \"/tmp/sl-sh.err>null.test\" (do (eprintln \"stderr redir one\")(err>null (eprintln \"stdnull redir one\"))))
(def topen (open \"/tmp/sl-sh.err>null.test\" :read))
(test::assert-equal \"stderr redir one\n\" (read-line topen))
(test::assert-false (read-line topen))
(close topen)
"
(body)
	`(dyn *stderr* (open "/dev/null" :write) ,body))

(defmacro out-err>null
"
Redirect both stdout and stderr to null (/dev/null equivelent).

Section: shell

Example:
(out-err> \"/tmp/sl-sh.out-err>null.test\" (do
                                             (println \"stdout redir one\")
                                             (eprintln \"stderr redir one\")
                                             (out-err>null (do
                                               (println \"stdnull redir one\")
                                               (eprintln \"stdnull redir one\")))))
(def topen (open \"/tmp/sl-sh.out-err>null.test\" :read))
(test::assert-equal \"stdout redir one\n\" (read-line topen))
(test::assert-equal \"stderr redir one\n\" (read-line topen))
(test::assert-false (read-line topen))
(close topen)
"
	(body)
	`(dyn *stdout* (open "/dev/null" :write) (dyn *stderr* *stdout* ,body)))

(defmacro |
"
Shorthand for pipe builtin.

Section: shell

Example:
(def pipe-test (str (| \$(echo \"one\ntwo\nthree\")\$(grep two))))
(test::assert-equal \"two\n\" pipe-test)
"
	(&rest body)
	`(pipe ,@body))

;; Scope to contain then pushd/popd/dirs functions.
(let ((dir_stack (make-vec 20))
      (dir_stack_max 20))

  (defn pushd
        "
        Push current directory on the directory stack and change to new directory.

        Section: shell

        Example:
        (def cur-test-path $PWD)
        (pushd \"/tmp\")
        (def cur-test-path2 $PWD)
        (assert-equal cur-test-path2 $PWD)
        (popd)
        (assert-equal cur-test-path $PWD)
        "
        (dir) (if (root::cd dir)
                (do
                  (vec-push! dir_stack $OLDPWD)
                  (if (> (length dir_stack) dir_stack_max) (vec-remove! dir_stack 0))
                  t)
                nil))
  (defn popd
        "
        Pop first directory from directory stack and change to it.

        Section: shell

        Example:
        (def cur-test-path $PWD)
        (pushd \"/tmp\")
        (def cur-test-path2 $PWD)
        (assert-equal cur-test-path2 $PWD)
        (popd)
        (assert-equal cur-test-path $PWD)
        "
        () (if (> (length dir_stack) 0)
             (cd (vec-pop! dir_stack))
             (println "Dir stack is empty")))
  (defn dirs
        "
        List the directory stack.

        Section: shell

        Example:
        (clear-dirs)
        (def cur-test-path $PWD)
        (dyn *stdout* (open \"/tmp/sl-sh.dirs.test\" :create :truncate) (dirs))
        (test::assert-equal nil (read-line (open \"/tmp/sl-sh.dirs.test\" :read)))
        (pushd \"/tmp\")
        (def cur-test-path2 $PWD)
        (dyn *stdout* (open \"/tmp/sl-sh.dirs.test\" :create :truncate) (dirs))
        (test::assert-equal cur-test-path (read-line (open \"/tmp/sl-sh.dirs.test\" :read)))
        (pushd (str-trim cur-test-path))
        (dyn *stdout* (open \"/tmp/sl-sh.dirs.test\" :create :truncate) (dirs))
        (def test-dirs-file (open \"/tmp/sl-sh.dirs.test\" :read))
        (test::assert-equal cur-test-path (read-line test-dirs-file))
        (test::assert-equal cur-test-path2 (read-line test-dirs-file))
        (close test-dirs-file)
        (popd)
        (dyn *stdout* (open \"/tmp/sl-sh.dirs.test\" :create :truncate) (dirs))
        (test::assert-equal cur-test-path (read-line (open \"/tmp/sl-sh.dirs.test\" :read)))
        (popd)
        (dyn *stdout* (open \"/tmp/sl-sh.dirs.test\" :create :truncate) (dirs))
        (test::assert-equal nil (read-line (open \"/tmp/sl-sh.dirs.test\" :read)))
        "
        ()
        (for d in dir_stack (println d)))
  (defn get-dirs
        "
        Return the vector of directories.

        Section: shell

        Example:
        (clear-dirs)
        (def cur-test-path $PWD)
        (test::assert-equal '() (get-dirs))
        (pushd \"/tmp\")
        (def cur-test-path2 $PWD)
        (test::assert-equal `(,cur-test-path) (get-dirs))
        (pushd (str-trim cur-test-path))
        (test::assert-equal `(,cur-test-path ,cur-test-path2) (get-dirs))
        (popd)
        (test::assert-equal `(,cur-test-path) (get-dirs))
        (popd)
        (test::assert-equal '() (get-dirs))
        "
        () dir_stack)
  (defn clear-dirs
        "
        Clears the directory stack.

        Section: shell

        Example:
        (clear-dirs)
        (def cur-test-path $PWD)
        (test::assert-equal '() (get-dirs))
        (pushd \"/tmp\")
        (def cur-test-path2 $PWD)
        (test::assert-equal `(,cur-test-path) (get-dirs))
        (pushd (str-trim cur-test-path))
        (test::assert-equal `(,cur-test-path ,cur-test-path2) (get-dirs))
        (clear-dirs)
        (test::assert-equal '() (get-dirs))
        "
        ()
        (vec-clear! dir_stack))
  (defn set-dirs-max
        "
        Sets the max number of directories to save in the stack.

        Section: shell

        Example:
        (clear-dirs)
        (def cur-test-path $PWD)
        (pushd \"/tmp\")
        (def cur-test-path2 $PWD)
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
          (set! dir_stack_max max)
          (err "Error, max must be a positive Int greater then one"))))


(defmacro let-env
"
Like let but sets environment variables that are reset after the macro finishes.

Section: shell

Example:
(test::assert-error \$LET-ENV-TEST-VAR-NOT-HERE)
(let-env ((LET-ENV-TEST-VAR-NOT-HERE \"here\"))
    (test::assert-equal \"here\" \$LET-ENV-TEST-VAR-NOT-HERE))
(test::assert-error \$LET-ENV-TEST-VAR-NOT-HERE)
"
	(vals &rest let_body)
	((fn (params bindings olds)
		(iterator::for-i idx el in vals
			(if (= 1 (length el))
				(do
					(vec-insert! params idx (iterator::nth 0 el))
					(vec-insert! bindings idx nil)
					(vec-insert! olds idx (eval (sym "\$" (iterator::nth 0 el)))))
				(if (= 2 (length el))
					(do
						(var binding (iterator::nth 1 el))
						(if (or (list? binding)(vec? binding)) (set! binding (eval binding)))
						(vec-insert! params idx (iterator::nth 0 el))
						(vec-insert! bindings idx binding)
						(vec-insert! olds idx (eval (sym "\$" (iterator::nth 0 el)))))
					(err "ERROR: invalid bindings on let-env"))))
		`((fn (params bindings olds)
			(unwind-protect
				(do
					(iterator::for-i i p in params
						(if (null (iterator::nth i bindings))
							(unexport p)
							(export p (iterator::nth i bindings))))
					,@let_body)
				(iterator::for-i i p in params
					(if (null (iterator::nth i olds))
						(unexport p)
						(export p (iterator::nth i olds))))))
		(quote ,params) (quote ,bindings) (quote ,olds)))
	(make-vec (length vals)) (make-vec (length vals)) (make-vec (length vals))))


;; https://wiki-dev.bash-hackers.org/scripting/terminalcodes
(def *fg-default* "\x1b[39m")
(def *fg-black* "\x1b[30m")
(def *fg-red* "\x1b[31m")
(def *fg-green* "\x1b[32m")
(def *fg-yellow* "\x1b[33m")
(def *fg-blue* "\x1b[34m")
(def *fg-magenta* "\x1b[35m")
(def *fg-cyan* "\x1b[36m")
(def *fg-white* "\x1b[37m")

(def *bg-default* "\x1b[49m")
(def *bg-black* "\x1b[40m")
(def *bg-red* "\x1b[41m")
(def *bg-green* "\x1b[42m")
(def *bg-yellow* "\x1b[43m")
(def *bg-blue* "\x1b[44m")
(def *bg-magenta* "\x1b[45m")
(def *bg-cyan* "\x1b[46m")
(def *bg-white* "\x1b[47m")

(defn fg-color-rgb
"
Usage: (fg-color-rgb red-val green-val blue-val)

Set the foreground color to the desired rgb where each arg is an integer between 0 and 255 inclusive.

Section: shell

Example:
(test::assert-equal \"\x1b[38;2;128;128;128m\" (fg-color-rgb 128 128 128))
(test::assert-equal \"\x1b[38;2;255;255;255m\" (fg-color-rgb 255 255 255))
(test::assert-equal \"\x1b[38;2;255;0;0m\" (fg-color-rgb 255 0 0))
(test::assert-equal \"\x1b[38;2;0;255;0m\" (fg-color-rgb 0 255 0))
(test::assert-equal \"\x1b[38;2;0;0;255m\" (fg-color-rgb 0 0 255))
(test::assert-equal \"\x1b[38;2;0;0;0m\" (fg-color-rgb 0 0 0))
"
	(R G B)
		(get-rgb-seq R G B :font))

(defn bg-color-rgb
"
Usage: (bg-color-rgb red-val green-val blue-val)

Set the background color to the desired rgb where each arg is an integer between 0 and 255 inclusive.

Section: shell

Example:
(test::assert-equal \"\x1b[48;2;128;128;128m\" (bg-color-rgb 128 128 128))
(test::assert-equal \"\x1b[48;2;255;255;255m\" (bg-color-rgb 255 255 255))
(test::assert-equal \"\x1b[48;2;255;0;0m\" (bg-color-rgb 255 0 0))
(test::assert-equal \"\x1b[48;2;0;255;0m\" (bg-color-rgb 0 255 0))
(test::assert-equal \"\x1b[48;2;0;0;255m\" (bg-color-rgb 0 0 255))
(test::assert-equal \"\x1b[48;2;0;0;0m\" (bg-color-rgb 0 0 0))
"
	(R G B)
		(get-rgb-seq R G B :bkrd))

(defn get-rgb-seq (R G B color-type)
    (varfn make-color (color-code) (str "\x1b[" color-code ";2;" R ";" G ";" B "m"))
    (match color-type
        (:font (make-color 38))
        (:bkrd (make-color 48))
        (nil (make-color 38))))

(defn find-symbol (com)
	(var val (sym *active-ns* "::" com))
	(if (def? (ref val)) val (sym "root::" com)))

(defmacro sys-alias?
"
True if the supplied command is an alias for a system command.

Section: shell
"
	(com) `((fn ()
	(var ret nil)
	(var val (shell::find-symbol ,com))
	(if (def? (ref val))
		(set! val (eval val)))
	(if (macro? val) (get-error  ; If the next two lines fail it was not an alias...
		(var expansion (expand-macro (val)))
		(set! ret (sys-command? (sym->str (first expansion))))))
	ret)))

(defn sys-command?
"
True if the supplied command is a system command.

Section: shell

Example:
(assert-true (sys-command? \"ls\"))
(assert-false (sys-command? \"rst-not-a-comand-strsnt\"))
"
	(com)
	(var ret nil)
	(if (or (str-empty? com)(= (str-nth 0 com) #\/)(= (str-nth 0 com) #\.))
		(if (fs-exists? com) (set! ret t))
		(if (and (str-contains "/" com)(fs-exists? (str "./" com)))
			(set! ret t)
			(if (and (str-starts-with "~/" com)(fs-exists? (str-replace "~/" $HOME com)))
				(set! ret t)
				(for p in (str-split ":" $PATH) (do
					(var path (str p "/" com))
					(if (and (fs-exists? path)(not ret)) (set! ret t)))))))
	ret)

;(let ((alias (make-hash)))
((fn (alias)
	(defn ns::register-alias
		"
		Registers an alias to the current scope. Useful if unregistering or
		ability to know whether an alias has been registered is desirable.

		Section: shell
		"
		(name) (hash-set! alias name t))
	(defn ns::unregister-alias
		"
		Unregisters an alias, removing it from scope.

		Section: shell
		"
		(name) (hash-remove! alias name))
	(defn ns::alias?
		"
		Provides boolean value confirming or denying given alias' presence
		in set of registered aliases.

		Section: shell
		"
		(name) (hash-haskey alias name)))(make-hash))

; These will be imported with syntax-on (ie copied into another namespace).
; Since syntax-on is a macro these copies are what will be read/used in that
; namespace vs these originals changing.
(def tok-slsh-form-color shell::*fg-blue*)
(def tok-slsh-fcn-color shell::*fg-cyan*)
(def tok-default-color shell::*fg-default*)
(def tok-sys-command-color shell::*fg-white*)
(def tok-sys-alias-color shell::*fg-default*)
(def tok-string-color shell::*fg-magenta*)
(def tok-invalid-color shell::*fg-red*)

;(defmacro syntax-on
(defn syntax-on
  "
  Turn on syntax highlighting at the repl.

  Section: shell
  "
  ()
  ;'(lex
     (var plev 0)
     (var ch nil)
     (var bad-syms (make-hash))
     (var sys-syms (make-hash))
     (var out (str ""))
     (var token (str ""))
     (var in-sys-command nil)
     (var tok-command t)

     (varfn syn-func? (com)
            ; Want the actual thing pointed to by the symbol in com for the test.
            (set! com (shell::find-symbol com))
            (if (def? (ref com))
              (do
                (set! com (eval (sym com)))
                (or (builtin? com) (lambda? com) (macro? com)))
              nil))

     (varfn paren-color (level)
            (var col (% level 4))
            (if (= col 0) shell::*fg-white*
              (if (= col 1) shell::*fg-cyan*
                (if (= col 2) shell::*fg-yellow*
                  (if (= col 3) shell::*fg-blue*)))))

     (varfn my-sys-command? (command)
            (var ret nil)
            (if (hash-haskey sys-syms command)
              (set! ret t)
              (if (not (hash-haskey bad-syms command))
                (do
                  (set! ret (shell::sys-command? command))
                  (if ret (hash-set! sys-syms command t) (hash-set! bad-syms command t)))))
            ret)

     (varfn command-color (command)
            (var ns-command (shell::find-symbol command))
            (if (not tok-command)
              (if (def? (ref ns-command))
                (if (syn-func? command)
                  (if in-sys-command
                    (str shell::tok-default-color command shell::*fg-default*)
                    (str shell::tok-slsh-fcn-color command shell::*fg-default*))
                  (str shell::tok-slsh-form-color command shell::*fg-default*))
                (str shell::tok-default-color command shell::*fg-default*))
              (if (syn-func? command)
                (if (or (shell::alias? command)(shell::sys-alias? command))
                  (do
                    (set! in-sys-command t)
                    (str shell::tok-sys-alias-color command shell::*fg-default*))
                  (str shell::tok-slsh-fcn-color command shell::*fg-default*))
                (if (my-sys-command? command)
                  (do
                    (set! in-sys-command t)
                    (str shell::tok-sys-command-color command shell::*fg-default*))
                  (str shell::tok-invalid-color command shell::*fg-default*)))))

     (varfn prrawtoken ()
            (var ttok token)
            (set! token (str ""))
            ttok)
     (varfn prtoken ()
            (var ttok token)
            (set! token (str ""))
            (command-color ttok))
     (varfn paren-open ()
            (var ret (str (prtoken) (paren-color plev) #\( shell::*fg-default*))
            (set! plev (+ plev 1))
            (set! tok-command t)
            ret)
     (varfn paren-close ()
            (set! in-sys-command nil)
            (if (> plev 0)
              (do
                (set! plev (- plev 1))
                (str (prtoken) (paren-color plev) #\) shell::*fg-default*))
              (str (prtoken) shell::*fg-red* #\) shell::*fg-default*)))

     (varfn whitespace (ch)
            (var ret (str (prtoken) ch))
            (set! token (str ""))
            (set! tok-command nil)
            ret)

     (varfn line-handler (line)
            (var in-quote nil)
            (var last-ch #\ )
            (set! plev 0)
            (set! ch nil)
            (set! token (str ""))
            (set! tok-command t)
            (set! in-sys-command nil)
            (if (<= (length line) 1)
              (do
                (hash-clear! bad-syms)
                (hash-clear! sys-syms)))
            (set! out (str-map (fn (ch)
                                   (var ret (if in-quote
                                               (do
                                                 (str-push! token ch)
                                                 (if (and (not (= last-ch #\\))(= ch #\"))
												   (do
												     (set! in-quote nil)
												     (str-push! token shell::*fg-default*)
												     (prrawtoken))
												   ""))
                                               (if (and (not (= last-ch #\\))(= ch #\"))
                                                 (do (str-push! token (str shell::tok-string-color ch))(set! in-quote t) "")
                                                 (if (= ch #\()
												   (paren-open)
												   (if (= ch #\))
												     (paren-close)
												     (if (char-whitespace? ch)
												       (whitespace ch)
												       (do (str-push! token ch) "")))))))
                                   (do (set! last-ch ch) ret)) line))
            (if in-quote (str-push! out (prrawtoken)) (str-push! out (prtoken)))
            (str out))

     (defn __line_handler (line)
           (var result (get-error (line-handler line)))
           (if (= :error (car result)) (shell::print-error result) (cdr result)))

     nil);)

(defmacro syntax-off
  "
  Turn off syntax highlighting at the repl.

  Section: shell
  "
  () '(undef __line_handler))

(load "endfix.lisp")

(defmacro endfix-on "
Allows use of infix notation for common shell forms. The following is the
complete mapping in lisp/endfix.lisp of all supported infix operators and
the corresponding sl-sh function they map to:
	'|| 'or
	'| '|
	'@@ 'do (@@ is used instead of ; because ; is a comment in lisp)
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
	() '(def __exec_hook shell::endfix-hook))


(defn print-backtrace (backtrace)
    (println (first backtrace))
    (for b in backtrace
        (if (builtin? b)(print "BUILTIN")
          (print (if (var file (meta-file-name b)) file "NO FILE") ":\t"
                 "line " (if (var line (meta-line-no b)) line "XX") ":\t"
                 "column " (if (var col (meta-column-no b)) col "XX") "\n"))))

(defn print-error (error)
    (if (= :error (car error))
        (do
            (println (cadr error))
            (print-backtrace (caddr error)))
        (err "Not an error!")))

(defn repl-eof (result)
      (do
        (if (and (values? result)
                 (= 2 (values-length result))
                 (= :unexpected-eof (values-nth 1 result))))))

(defn path_list_trunc (plist)
	(if (> (length plist) 1)
		(if (> (length (first plist)) 0) 
			(vec-insert! (path_list_trunc (rest plist)) 0 (str-sub 0 1 (first plist)))
			(path_list_trunc (rest plist)))
		plist))

(defn get_pwd ()
	(str-cat-list "/" (path_list_trunc (str-split "/" (str-replace (str-trim $PWD) $HOME "~")))))

(defn set_prompt_tail ()
	(if (= *last-status* 0) "\x1b[32m>\x1b[39m " (format "\x1b[31m(" *last-status* ")>\x1b[39m ")))

(defn __prompt ()
	(str "\x1b[32m[" *active-ns* "]:" $HOST ":\x1b[34m" (str-trim (get_pwd)) (set_prompt_tail)))

(defn handle-last-command (line)
    ; Save history
    (if (not (def? *repl-std-only*)) (history-push :repl line))
    ;; Set global var *last-command*
    (set! *last-command* line))

#|(defn repl-line (line line-len)
    (var strict? (and (def? (sym *active-ns* "::repl-strict"))(ref (sym *active-ns* "::repl-strict"))))
    (var my-read (if strict? read read-all))
    (export 'LAST_STATUS "0")
    (set! *last-status* 0)
    (var result nil)

    ; This next section is odd, it makes sure the eval happens in the active
    ; namespace NOT shell since that is the namespace if repl-line is the last
    ; function to be called.
    (ns-push *active-ns*)
    (varfn do-eval fn ()
        (var exec-hook (sym *active-ns* "::__exec_hook"))
        (var ast (if (and (not strict?)(def? (ref exec-hook))(lambda? (eval exec-hook)))
                    (apply exec-hook line nil)
                    (my-read line)))
        (set! ast (if (string? ast) (my-read ast) ast))
        (if strict? (eval ast)(loose-symbols (eval ast))))
    (ns-pop)
    (set! result (get-error (do-eval)))
    ; end weird namespace section

    (if (= :ok (car result))
      (do
        (if (process? (cdr result)) nil
          (and (not strict?)(nil? (cdr result))) nil
          (file? (cdr result)) nil
          (println (cdr result)))
        (if (> line-len 0)
          (do
            (when (not (= "fc" (str-trim line)))
              (handle-last-command line)))))
      (do
        (set! *last-command* line)
        ; Save temp history
        (if (and (> line-len 0)(not (def? *repl-std-only*))) (history-push-throwaway :repl line))
                    (print-error result))))|#

      (defn callable? (com)
          ; Want the actual thing pointed to by the symbol in com for the test.
          (set! com (shell::find-symbol com))
          (if (def? (ref com))
              (do (set! com (eval (sym com)))
                  (or (builtin? com) (lambda? com) (macro? com)))
              nil)
        )

      (defmacro sys-apply (com &rest args)
          (if (callable? com)
              `(,com ,@args)
              `(syscall ,com ,@args)))

(defmacro var-or-env (key)
    (let ((key-new (shell::find-symbol key)))
        (if (def? (ref key-new))
            `,key-new
            `(get-env ,key))))

(let ((paren-level 0))

(defn shell-read-int (stream in-paren)
    (let ((in-quote nil)
          (token (str ""))
          (new-pair (list))
          (last-pair (list))
          (result (list))
          (close-token)
          (add-exp)
          (do-read)
          (first #t)
          (first-sym #t)
          (just-read nil)
          (done nil))
        (set! add-exp (fn (exp)
            (set! new-pair (join exp nil))
            (if (nil? last-pair) (set! result new-pair))
            (xdr! last-pair new-pair)
            (set! last-pair new-pair)))
        (set! close-token (fn ()
            (if (not (str-empty? token))
                (if first-sym
                    (do (add-exp (sym token)) (set! first-sym nil))
                    (add-exp token)))
            (set! token (str ""))))
        (set! do-read (fn (ch)
                    (if (not (char-whitespace? ch))
                        (str-push! token ch))
                    (close-token)
                    (add-exp (read stream))
                    (set! just-read #t)))
        (if in-paren (add-exp (sym "shell::sys-apply")))
        ((fn (last-ch ch peek-ch)
            (cond
                ((and (= ch #\() first (= peek-ch #\())
                    (set! result (read stream))
                    (set! ch (str-iter-next! stream))
                    ((fn () (if (and (char? ch)(char-whitespace? ch)) (do (set! ch (str-iter-next! stream))(recur)))))
                    (if (not (= #\) ch))
                        (err "Unbalanced ) in '\$' shell read macro"))
                    (set! done #t))
                ((and (= ch #\() first)
                    (set! paren-level (+ paren-level 1))
                    (add-exp (sym "shell::sys-apply"))
                    (set! in-paren #t))
                ((and (not (= last-ch #\\))(= ch #\)) (> paren-level 0))
                    (set! paren-level (- paren-level 1))
                    (set! done #t))
                ((and (not (= ch #\\))(= peek-ch #\))(not in-paren))
                    (str-push! token ch)
                    (set! done #t))
                ((and (char-whitespace? ch)(not in-paren))
                    (set! done #t))
                ((and (not (= ch #\\))(or (= peek-ch #\")(= peek-ch #\$)))
                    (do-read ch))
                ((char-whitespace? ch)
                    (close-token))
                ((str-push! token ch) nil))
            (set! first nil)
            (if (and (not done)(not (str-iter-empty? stream)))
                (if just-read
                    (do (set! just-read nil)(recur #\  #\  (str-iter-peek stream)))
                    (recur ch (str-iter-next! stream)(str-iter-peek stream)))
                (if (not (str-empty? token))
                    (if in-paren
                        (close-token)
                        (do (add-exp (sym "shell::var-or-env"))(add-exp (sym token))))))
             )#\ (str-iter-next! stream)(str-iter-peek stream))
        result)))

(defn shell-read (stream ch_start) (shell-read-int stream nil))

(defn repl-line (line line-len)
    (export 'LAST_STATUS "0")
    (set! *last-status* 0)
    (let ((result nil)
          (do-eval)
          (prep-ast (fn (line)
                        (if (string? line)
                            (cond
                              ((= #\$ (str-nth 0 line)) (read line))
                              ((= #\( (str-nth 0 line)) (read line))
                              (#t (shell-read-int (str-iter-start line) #t)))
                            line))))

    ; This next section is odd, it makes sure the eval happens in the active
    ; namespace NOT shell since that is the namespace if repl-line is the last
    ; function to be called.
    (ns-push *active-ns*)
    (set! do-eval (fn ()
          (let* ((exec-hook (sym *active-ns* "::__exec_hook"))
                 (ast (if (and (def? (ref exec-hook))(lambda? (eval exec-hook)))
                          (prep-ast (apply exec-hook line nil))
                          (prep-ast line))))
            (eval ast))))
    (ns-pop)
    (set! result (get-error (do-eval)))
    ; end weird namespace section

    (if (= :ok (car result))
      (do
        (if (process? (cdr result)) nil
          (nil? (cdr result)) nil
          (file? (cdr result)) nil
          (println (cdr result)))
        (if (> line-len 0)
          (do
            (when (not (= "fc" (str-trim line)))
              (handle-last-command line)))))
      (do
        (set! *last-command* line)
        ; Save temp history
        (if (and (> line-len 0)(not (def? *repl-std-only*))) (history-push-throwaway :repl line))
        ;(println (cadr result))))))
        (print-error result)))))

(defn repl ()
      (var get-prompt (fn ()
              (var ns-prompt (sym *active-ns* "::__prompt"))
              (if (def? (ref ns-prompt)) (apply ns-prompt nil) (__prompt))))
      (var repl-inner (fn ()
              (if (not (def? *repl-std-only*)) (history-context :repl $PWD))
              (reap-jobs)
              (var save-last-status *last-status*)
              (var line (if (def? *repl-std-only*) 
                           (do (print (get-prompt))(read-line *stdin*))
                           (prompt :repl (get-prompt) "~/.local/share/sl-sh/history")))
              (export 'LAST_STATUS save-last-status)
              (set! *last-status* save-last-status)
              (var line-len (length (str-trim line)))
              (if (and (> line-len 0)(not (values? line))) (repl-line line line-len))
              (if (not (repl-eof line)) (recur))))
      ((fn ()
           (var result (get-error (repl-inner)))
           (if (= :error (car result)) (do (print-error result)(exit 1))))))

(defn temp-dir
"Returns $TMPDIR environment variable if set, otherwise returns \"/tmp\".
Section: shell"
    ()
    (if (def? $TMPDIR) (str $TMPDIR) "/tmp"))

(defn fc
"Put the contents of the last command into a temporary file
([temp-dir](shell::temp-dir)), and open the temporary file in the text editor,
$EDITOR. If the editor returns with an error code of 0 the contents of the
temporary file are executed. `fc` can be used in succession and the contents of
the temporary file are saved to the sl-sh history.

Section: shell"
    ()
    (var fc-file (str (temp-dir) "/sl-sh-fc.txt"))
    (do
        (out> fc-file (print *last-command*))
        (when (= 0 (wait (eval (read (str "(" $EDITOR " " fc-file ")")))))
            (do
                (var file-contents (str (cat fc-file)))
                (when (not (= "" (str-trim file-contents)))
                    (do
                        (handle-last-command (str-trim file-contents))
                        (eval (read-all (str (cat fc-file))))))))))

(defn mkli
	"Usage: (mkli filepath [namespace] [body])
	\"make lisp\".creates a sl-sh shell script. given a file, a namespace (optional 2nd arg), and a string
	to populate as the body (optional 3rd arg), make a canonincal blank sl-sh script
	complete with all the relevant imports, and boilerplate namespace code taken
	care of to speed up development.

	It is recommended all calls to load are done at the top of the file (before
	the calls to ns-enter or ns-create, in case a library sl-sh script calls a
	library sl-sh script that created a namespace and forgot to call ns-pop.
	This ensures the exported symbols for the first library's scripts
	namespace are importable in the executing script's namespace.

	All calls to ns-import happen after a ns is created and entered so the
	current namespace is the namespace that houses all the imported symbols.

	ns-export must be called before ns-pop so the appropriate symbols are
	associated namespace, the one in which the symbols were created.

	Section: scripting "
	(&rest args) (let* ((filepath nil) (namespace nil) (script-body nil))
	(when (= 0 (length args)) (err "Must have at least 1 argument, the path to the script to be created."))
	(when (< 3 (length args)) (err (str "Too many arguments, see doc ")))
	(when (< 2 (length args)) (set! script-body (vec-nth args 2)))
	(when (< 1 (length args)) (set! namespace (vec-nth args 1)))
	(when (< 0 (length args)) (set! filepath (vec-nth args 0)))
	(var new-file (open filepath :create :append))
	(chmod "+x" filepath)
	(write-line new-file "#!/usr/bin/env sl-sh")
	(write-line new-file "")
	(when (not (nil? namespace))
		(write-line new-file (str "(ns-push '" namespace ")")))
	(write-line new-file "(ns-import 'shell)")
	(if (not (nil? script-body))
		(do
			(write-line new-file "")
			(write-line new-file script-body)
			(write-line new-file ""))
		(write-line new-file ""))
	(when (not (nil? namespace))
		(do
		(write-line new-file (str "(ns-auto-export '" namespace ")"))
		(write-line new-file "(ns-pop)")))
	(close new-file)))

; This puts this in the shell userspace, also in slsh-std.lisp to get it in root.
; Need this so getops.lisp can do string interpolation but this should be better.
(if (def? *read-table*)
    (hash-set! *read-table* #\$ 'shell::shell-read)
    (def *read-table* (make-hash '((#\$ . shell::shell-read)))))

(load "getopts.lisp")

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
	endfix-on
	fc
	getopts
	mkli
	temp-dir))

(ns-pop)
