(def print-error
"Prints out an error with a backtrace.  Used with the return of get-error
when it produces an error

Section: core

Example:
(let
  ((print-error-test (get-error (err \"Oops!\")))
   (file-name \"$(temp-dir)/print-error.test\")
   (topen))
(out> file-name (print-error print-error-test))
(set! topen (open file-name :read))
(test::assert-true (> (length (read-line topen)) 5))
(close topen))
"
    (fn (error)
        ((fn (print-backtrace)
             (set! print-backtrace
                   (fn (backtrace)
                       ((fn (idx len)
                            (if (and (not (vec-empty? backtrace))(< idx len))
                                ((fn :no-recur (b file line col)
                                     (if (builtin? b)(print "BUILTIN")
                                         (print (if (set! file (meta-file-name b)) file "NO FILE") ":\t"
                                                "line " (if (set! line (meta-line-no b)) line "XX") ":\t"
                                                "column " (if (set! col (meta-column-no b)) col "XX") "\n"))
                                     (recur (+ idx 1) len))
                                 (vec-nth backtrace idx) nil nil nil)))
                        0 (length backtrace))))
             (if (= :error (car error))
                 (do
                  (println (car (cdr error)))
                  (print-backtrace (car (cdr (cdr error)))))
                 (err "Not an error!")))
         nil)))

(def load-std-file
    (fn (file)
        ((fn (result)
             (set! result (get-error (load file)))
             (if (= :error (car result))
                 (do
                  (println "Error reading " file ":")
                  (print-error result))))nil)))

; core should be loaded into the root namespace (ie it does not set a namespace).
(load-std-file "core.lisp")


(defn error-stack-on
"Currently a no-op, used to turn on error stacks.

Section: core

Example:
; no-op
(error-stack-on)
"
      () nil)

(defn error-stack-off
"Currently a no-op, used to turn off error stacks.

Section: core

Example:
; no-op
(error-stack-off)
"
      () nil)

(def *last-status*
     "Return code of last run sl-sh command on the repl
     Section: globals"
     0)
(def *last-command*
     "last run command by sl-sh on repl
     Section: globals"
     ""
     )

(def *repl-settings*
     "hash map of repl settings
     Section: globals"
     (make-hash))

(hash-set! *repl-settings* :keybindings :emacs)

(load-std-file "seq.lisp")
(load-std-file "shell-read.lisp")
(load-std-file "struct.lisp")
(load-std-file "iterator.lisp")


(load-std-file "test.lisp")
(load-std-file "lib.lisp")
(load-std-file "shell.lisp")

(undef load-std-file)

(def *std-lib-namespaces*
     "vector of all namespaces present in sl-sh by default
     Section: globals"
     (ns-list))

;;; *std-lib-syms-hash* must be the symbol defined
;;; in the sl-sh standard library. In order to increase speed of ns-auto-export
;;; a list of all symbols in the standard library is pre-computed by iterating
;;; over each namespace in (ns-list), and calling it with (ns-symbols) and
;;; adding that to a hash map, using it as a set to test for membership.
;;; In order for this set of symbols to be complete the calls to (ns-list)
;;; and (ns-symbols) must be done after all symbols and the namespaces they're
;;; defined in have been called.
(def *std-lib-syms-hash*
    "vector of all symbols in sl-sh-standard library
    Section: globals"
    (iterator::reduce
    (fn (fst nxt)
        (iterator::reduce
            (fn (fst x)
                (hash-set! fst x nxt))
            fst
            (ns-symbols nxt)))
    (make-hash)
    *std-lib-namespaces*))

(def *std-lib-exported-syms-hash*
    "vector of all symbols in sl-sh-standard library
    Section: globals"
    (iterator::reduce
    (fn (fst nxt)
        (iterator::reduce
            (fn (fst x)
                (hash-set! fst x nxt))
            fst
            (if (= "root" nxt)
              (ns-symbols nxt)
              (eval (sym (str nxt "::*ns-exports*"))))))
    (make-hash)
    *std-lib-namespaces*))

(if (ns-exists? 'user) (ns-enter 'user) (ns-create 'user))

(if (def? *load-slshrc*)
  (do
    (def config-file "${HOME}/.config/sl-sh/slshrc")
    (if (not (fs-exists? config-file)) (write-string (open config-file :create) *slshrc-src*))

    ((fn (result)
        (set! result (get-error (load "slshrc")))
            (if (= :error (car result))
                (do
                    (println "Error loading config file: ")
                    (print-error result))))nil)))

(if (def? *interactive*)
  (do
    (if (not (def? repl))(def repl shell::repl))
    (repl)))

(if (def? *run-script*)
  (do
    (def result (get-error (load *run-script*)))
    (if (= :error (car result))
      (print-error result))))
