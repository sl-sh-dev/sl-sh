; core should be loaded into the root namespace (ie it does not set a namespace).
(load "core.lisp")

(load "seq.lisp")
(load "struct.lisp")
(load "iterator.lisp")
(load "shell.lisp")
(load "test.lisp")


(if (ns-exists? 'user) (ns-enter 'user) (ns-create 'user))

(def '*repl-settings* (make-hash))
(hash-set! *repl-settings* :keybindings :emacs)

(def '*last-status* 0)

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

(defn repl () (progn
      (defn repl-inner ()
            (progn
              #|
        con.history
            .set_search_context(if let Ok(cur_dir) = env::current_dir() {
                Some(cur_dir.to_string_lossy().to_string())
            } else {
                None
            });
              |#
              (reap-jobs)
              ;(def 'result '(:ok . nil))
              (def 'save-last-status *last-status*)
              (def 'line (prompt :repl (str "XX " (__prompt)) "~/.local/share/sl-sh/history"))
              (export 'LAST_STATUS save-last-status)
              (set '*last-status* save-last-status)
              (def 'line-len (length (str-trim line)))
              ;(eval (shell::endfix-hook (prompt :repl (str "XX " (__prompt)) "~/.local/share/sl-sh/history")))))
              (if (and (> line-len 0)(not (values? line)))
                ;(set 'result (get-error (eval (read-all line)))))
                (progn
                  (export 'LAST_STATUS "0")
                  (set '*last-status* 0)
                  (def 'ast (if (and (def? '__exec_hook)(lambda? __exec_hook))
                              (__exec_hook line)
                              (read-all line)))
                  (set 'ast (if (string? ast) (read-all ast) ast))
                  (def 'result (loose-symbols (get-error (eval ast))))
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
                ;(eval (read-all line)))
              (if (not (repl-eof line)) (recur))))
      ((fn () (progn
                (def 'result (get-error (repl-inner)))
                (if (= :error (car result)) (progn (print-error result)(recur))))))))

