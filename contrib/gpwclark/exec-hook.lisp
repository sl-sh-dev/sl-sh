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
		(let ((cmd-str (str cmd)))
			(if (fs-dir? cmd-str)
				(list root::cd cmd-str)
				cmd-str)))

;;TODO organize fcns

	(defn prefixify-cmd (cmd-toks an-infix-hashset)
		(let ((build-cmd (fn (cmd-ast raw-list)
				(progn
					(defq next-tok (first raw-list))
					(defq infix-symbol (hash-get an-infix-hashset :infix-symbol))
					(if (not next-tok)
						cmd-ast
						;;TODO remove this!
						;;(progn (println "resultant prefix notation: " cmd-ast) cmd-ast)
						(progn
							(recur
								(if (= infix-symbol next-tok)
									(progn (append!
											cmd-ast
											(vec ((hash-get an-infix-hashset :xform) (make-vec))))
											cmd-ast)
									(progn (append! (last cmd-ast) (vec next-tok)) cmd-ast))
								(rest raw-list))))))))
			(build-cmd
				(vec
					(hash-get an-infix-hashset :ast-base)
					((hash-get an-infix-hashset :xform) (make-vec)))
				cmd-toks)))

	;; return true if cmd satisfies preconditions for prefixification
	(defn satisfies-prefixify-preconditions (cmd-str cmd-ast infix-hash-set)
		(let ((infix-symbol (hash-get infix-hash-set :infix-symbol)))
			(not (or ;; reasons to skip pre-processing

				;; if read returns nil
				(not cmd-ast)

				;; if cmd doesn't contain symbol
				(not (str-contains infix-symbol cmd-str))

				;; if already in prefix notation
				;;TODO edge case, there are more infix-symbols in ast?
				(= infix-symbol (first cmd-ast))))))

	(defn confirm-prefix-eligible (cmd-str cmd-ast infix-metadata)
		;; recurse over infix-metadata return nil if there are none but
		;; return the pair if it's valid
		(progn (defq an-infix-hashset (first infix-metadata))
			(if (not an-infix-hashset)
				nil
				(if (satisfies-prefixify-preconditions cmd-str cmd-ast an-infix-hashset)
					an-infix-hashset
					(recur cmd-str cmd-ast (rest infix-metadata))))))

;; to consider:
;; for out> and stuff there will need to be a distinction b/w variadic infix
;; notation and num args infix notation
;; TODO for && and || must eval list, check if list is proc (type? proc) if it is
;; wait if it is not check to see if it's nil b/c that's false
;; GO ahead and do for ; as well... why not?
;; TODO out> err> / other file forms are NOT variadic...
	(defn make-infix-data (infix-symbol ast-base cmd-arity xform)
		(progn
			(defq prefix-props (make-hash))
			(hash-set! prefix-props :infix-symbol infix-symbol)
			(hash-set! prefix-props :ast-base ast-base)
			(hash-set! prefix-props :cmd-arity cmd-arity)
			(hash-set! prefix-props :xform xform)
			prefix-props))

	(defn identity ()
		(fn (x) x))

	(defn proc-wait (consumer)
		(fn (cmd)
			(progn (defq cmd-proc (eval cmd)
				(if (process? cmd-proc)
					(progn (defq ret-code (wait cmd-proc))
						 (consumer ret-code)))))))

#|
	(defmacro and-proc-handler (next)
		(fn (ret-code)
			(if (= 0 ret-code)
				(if (not next) ret-code (next))
				ret-code)))

	(defn or-proc-handler ()
		(fn (ret-code)
			(if (= 0 ret-code)
				ret-code
				(if (not-next) ret-code (next)))))
|#

	(defn check-for-infix-notation (cmd-str cmd-ast)
		;; confirm cmd ast needs prefixification ...then call prefixify.
		(progn
				(defq infix-metadata ;; TODO so... is there state persisted if this is only defined once?
					(list
						(make-infix-data '| '|  0 (identity))
						(make-infix-data 'meow 'progn  0 (identity)))
						;;(make-infix-data '&& 'and 0 (proc-wait (and-proc-handler)))
						;;(make-infix-data '|| 'or 0 (proc-wait (or-proc-handler)))
						) ;; but meow should be ;
				(defq prefix-eligible
					(confirm-prefix-eligible cmd-str cmd-ast infix-metadata))
			(if (not prefix-eligible)
				cmd-str
				;; TODO remove this
				;;(progn (str "not eligible: " cmd-str) cmd-str)
				(prefixify-cmd cmd-ast prefix-eligible))))

	(defn __exec_hook (cmd-str)
		(let ((cmd-ast (read :add-parens cmd-str)))
				(match (length cmd-ast)
					(1 (change-dir-if-arg-is-dir (first cmd-ast)))
					(nil (check-for-infix-notation cmd-str cmd-ast)))))

;; }}}
