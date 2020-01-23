;; This is a sl-sh file for people named price, you would put it in ~/.exec-hook.lisp to use it.
(core::ns-import 'core)
(ns-import 'shell)

;; exec hook fcn {{{

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
		(let ((build-cmd (fn (cmd-ast raw-list)
				(progn
					(defq next-tok (first raw-list))
					(defq infix-symbol (car infix-ast-pair))
					(if (not next-tok)
						cmd-ast
						;;TODO remove this!
						;;(progn (println "resultant prefix notation: " cmd-ast) cmd-ast)
						(progn
							(recur
								(if (= infix-symbol next-tok)
									(progn (append! cmd-ast (vec (make-vec))) cmd-ast)
									(progn (append! (last cmd-ast) (vec next-tok)) cmd-ast))
								(rest raw-list))))))))
			(build-cmd (cdr infix-ast-pair) cmd-toks)))

	;; return true if cmd satisfies preconditions for prefixification
	(defn satisfies-prefixify-preconditions (cmd-to-execute cmd-ast infix-ast-pair)
		(let ((infix-symbol (car infix-ast-pair)))
			(not (or ;; reasons to skip pre-processing

				;; if read returns nil
				(not cmd-ast)

				;; if cmd doesn't contain symbol
				(not (str-contains infix-symbol cmd-to-execute))

				;; if already in prefix notation
				;;TODO edge case, there are more infix-symbols in ast?
				(= infix-symbol (first cmd-ast))))))

;; recurse over infix-symbol-base-ast-pairs return nil if there are none but
;; return the pair if it's valid
	(defn confirm-prefix-eligible (cmd-to-execute cmd-ast symbol-ast-pairs)
		(progn (defq next-pair (first symbol-ast-pairs))
			(if (not next-pair)
				nil
				(if (satisfies-prefixify-preconditions cmd-to-execute cmd-ast next-pair)
					next-pair
					(recur cmd-to-execute cmd-ast (rest symbol-ast-pairs))))))

;; to consider:
;; for out> and stuff there will need to be a distinction b/w variadic infix
;; notation and num args infix notation
;; TODO for && and || must eval list, check if list is proc (type? proc) if it is
;; wait if it is not check to see if it's nil b/c that's false
;; GO ahead and do for ; as well... why not?
;; TODO out> err> / other file forms are NOT variadic...

	;; get cmd as ast and confirm it should be prefixified... then do it.
	(defn check-for-infix-notation (cmd-to-execute)
		(progn
				(defq cmd-ast (read :add-parens cmd-to-execute))
				(defq infix-symbol-base-ast-pairs ;; TODO so... is there state persisted if this is only defined once?
					(list
						(join '| (vec '| (make-vec)))
						(join 'meow (vec 'progn (make-vec))))) ;; but meow should be ;
				(defq eligible-pair
					(confirm-prefix-eligible cmd-to-execute cmd-ast infix-symbol-base-ast-pairs))
			(if (not eligible-pair)
				cmd-to-execute
				;; TODO remove this
				;;(progn (str "not eligible: " cmd-to-execute) cmd-to-execute)
				(prefixify-cmd cmd-ast eligible-pair))))

	(defn __exec_hook (cmd-to-execute)
		(let ((args-list (read cmd-to-execute :add-parens)))
				(match (length args-list)
					(1 (change-dir-if-arg-is-dir cmd-to-execute))
					(nil (check-for-infix-notation cmd-to-execute)))))

;; }}}
