; core should be loaded into the root namespace (ie it does not set a namespace).
(load "core.lisp")

(defn error-stack-on () nil)
(defn error-stack-off () nil)

(def '*last-status* 0)
(def '*last-command* "")

(def '*repl-settings* (make-hash))
(hash-set! *repl-settings* :keybindings :emacs)

(load "seq.lisp")
(load "struct.lisp")
(load "iterator.lisp")
(load "shell.lisp")
(load "test.lisp")


(if (ns-exists? 'user) (ns-enter 'user) (ns-create 'user))

(if (def? '*interactive*) (progn (load "slshrc") (shell::repl)))

(if (def? '*run-script*)
  (progn
    (def 'result (get-error (eval (read-all (str "load \"" *run-script* "\"")))))
    (if (= :error (car result))
      (shell::print-error result))))

