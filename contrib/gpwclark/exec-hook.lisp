;; This is a sl-sh file for people named price, you would put it in ~/.exec-hook.lisp to use it.
(core::ns-import 'core)
(ns-import 'shell)

;; exec hook fcns {{{
;; fcnality
;; 1. entering 1 arg on the CLI that is a valid directory results in changing
;;		to that directory.
;; 2. support $la env variable which is set to the last argument of the previous
;;		command input.
;;		TODO support things like $2la for second to last arg, $3la, etc..

	(defn change-dir-if-arg-is-dir (cmd)
		(if (fs-dir? cmd)
			(list root::cd cmd)
			cmd))

	(defn prefixify-cmd (cmd-toks infix-ast-pair)
		(let ((build-cmd (fn (cmd-list raw-list)
				(progn
					(defq next-tok (first raw-list))
					(defq infix-symbol (car infix-ast-pair))
					(if (not next-tok)
						cmd-list
						(progn
							(recur
								(if (= infix-symbol next-tok)
									(progn (append! cmd-list (vec (make-vec))) cmd-list)
									(progn (append! (last cmd-list) (vec next-tok)) cmd-list))
								(rest raw-list))))))))
			(build-cmd (cdr infix-ast-pair) cmd-toks)))

	(defn check-prefixify-preconditions (cmd-to-execute cmd-as-list infix-ast-pair)
		(let ((infix-symbol (car infix-ast-pair)))
			(or ;; reasons to skip pre-processing

				;; if read returns nil
				(not cmd-as-list)

				;; if cmd doesn't contain symbol
				(not (str-contains infix-symbol cmd-to-execute))

				;; if already in prefix notation
				;;TODO edge case, there are more infix-symbols in ast?
				(= infix-symbol (first cmd-as-list)))))

	(defn check-for-infix-notation (cmd-to-execute infix-ast-pair)
		(let ((cmd-as-list (read :add-parens cmd-to-execute)))
			(if (check-prefixify-preconditions cmd-to-execute cmd-as-list infix-ast-pair)
				cmd-to-execute
				(prefixify-cmd cmd-as-list infix-ast-pair))))

;; for out> and stuff there will need to be a distinction b/w variadic infix
;; notation and num args infix notation
	(defq infix-symbol-base-ast-pairs
		(list
			(join '| (vec '| (make-vec)))
			(join (defq semi-colon (to-symbol ";")) (vec 'progn (make-vec)))
			))

	(defn __exec_hook (cmd-to-execute)
		(let ((args-list (str-split " " cmd-to-execute)))
				(match (length args-list)
					(1 (change-dir-if-arg-is-dir (first args-list)))
					(nil (check-for-infix-notation cmd-to-execute (first infix-symbol-base-ast-pairs))))))

;; TODO for && and || must eval list, check if list is proc (type? proc) if it is
;; wait if it is not check to see if it's nil b/c that's false
;; GO ahead and do for ; as well... why not?
;;
;; }}}
