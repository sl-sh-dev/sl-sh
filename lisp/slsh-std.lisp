; core should be loaded into the root namespace (ie it does not set a namespace).
(load "core.lisp")

(defn error-stack-on () nil)
(defn error-stack-off () nil)

(def '*repl-settings* (make-hash))
(hash-set! *repl-settings* :keybindings :emacs)

(load "seq.lisp")
(load "struct.lisp")
(load "iterator.lisp")
(load "shell.lisp")
(load "test.lisp")


(if (ns-exists? 'user) (ns-enter 'user) (ns-create 'user))

(def '*last-status* 0)
(def '*last-command* "")

(if (def? '*interactive*) (progn (load "slshrc") (shell::repl)))
