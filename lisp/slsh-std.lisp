; core should be loaded into the root namespace (ie it does not set a namespace).
(def prim-print-backtrace (fn (backtrace) 
(def first (fn
    (obj)
    (if (vec? obj) (if (vec-empty? obj) nil (vec-nth obj 0))
        (list? obj) (car obj)
        (err "Not a vector or list"))))
(def rest (fn
    (obj)
    (if (vec? obj) (vec-slice obj 1)  ; XXX deal with empty vector (don't make it nil).
        (list? obj) (cdr obj)
        (err "Not a vector or list"))))

    (if (not (vec-empty? backtrace))
        (do
          (var b (first backtrace))
          (print (if (var file (meta-file-name b)) file "??????") ":\t"
               "line " (if (var line (meta-line-no b)) line "??") ":\t"
               "column " (if (var col (meta-column-no b)) col "??") "\n")
          (recur (rest backtrace))))))

(def prim-print-error (fn (error)
    (if (= :error (car error))
        (do
            (println (car (cdr error)))
            (prim-print-backtrace (car (cdr (cdr error)))))
        (err "Not an error!"))))

(def result (get-error (do

(load "core.lisp")

(defn error-stack-on
"Currently a no-op, used to turn on error stacks.

Section: core
"
      () nil)

(defn error-stack-off
"Currently a no-op, used to turn off error stacks.

Section: core
"
      () nil)

; For compat.
(defmacro progn
"Synonym for 'do', use it instead (this is depricated).

Section: core

Example:
;see do
t
"
  (&rest args) `(do ,@args))

; For compat.
(defmacro to-symbol
"Synonym for 'sym' that only takes one arg, use it instead (this is depricated).

Section: type

Example:
;see sym
t
"
  (arg) `(sym ,arg))

(def *last-status* 0)
(def *last-command* "")

(def *repl-settings* (make-hash))
(hash-set! *repl-settings* :keybindings :emacs)

(load "seq.lisp")
(load "struct.lisp")
(load "iterator.lisp")
(load "shell.lisp")
(load "test.lisp"))))

(if (= :error (car result)) (prim-print-error result))

; Do not leave this stuff laying around the root scope.
(undef result)
(undef prim-print-backtrace)
(undef prim-print-error)

(if (ns-exists? 'user) (ns-enter 'user) (ns-create 'user))

(if (def? *load-slshrc*)
  (do
    (def config-file "$HOME$/.config/sl-sh/slshrc")
    (if (not (fs-exists? config-file)) (write-string (open config-file :create) *slshrc-src*))
    (load "slshrc")))

(if (def? *interactive*)
  (do
    (if (not (def? repl))(def repl shell::repl))
    (repl)))

(if (def? *run-script*)
  (do
    (def result (get-error (eval (read-all (str "load \"" *run-script* "\"")))))
    (if (= :error (car result))
      (shell::print-error result))))

