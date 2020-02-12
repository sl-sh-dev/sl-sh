;; This is a sl-sh file for people named price, you would put it in ~/.exec-hook.lisp to use it.
(core::ns-import 'core)
(ns-import 'shell)

;; exec hook fcn {{{

;; fcnality
;; 1. entering 1 arg on the CLI that is a valid directory results in changing
;;	to that directory.
;; 2. support $la env variable which is set to the last argument of the previous
;;	command input.
;; 3. allows for use of infix notation. e.g.
;; ```
;; cat file | grep -i "username"
;; or
;; cat file | grep -i "username" out> users-file
;; ```
;; can use infix notation in any nested form as well.
;; supports the bash equivalent of ; but use @@ instead (conflict with ; b/c it
;; in lisp that indicates a comment.

;; TODOS
;; 1. support things like $2la for second to last arg, $3la, etc..
;; 2. organize fcns
;; 3. use docstrings
;; 4. need ability to include any number of exec hooks to allow for conditionally
;;	adding completions.

(defn identity ()
	(fn (x) x))

(defn handle-process (cmd-proc)
	(if (process? cmd-proc) (= 0 (wait cmd-proc)) (not (not cmd-proc))))

(defmacro proc-wait ()
	(fn (cmd) `(handle-process ,cmd)))

(defn gen-prefix-data
	"Create hash set of metadata needed to convert infix notation to prefix
	notation"
	(infix-symbol prefix-symbol xform ast-order)
	(progn
		(defq prefix-props (make-hash))
		(hash-set! prefix-props :infix-symbol infix-symbol)
		(hash-set! prefix-props :prefix-symbol prefix-symbol)
		(hash-set! prefix-props :xform xform)
		(hash-set! prefix-props :ast-order ast-order)
		prefix-props))

(defq prefix-metadata
	(list
		(gen-prefix-data '|| 'or proc-wait :as-is #| this '|| form must come before the '| form|#)
		(gen-prefix-data '| '| identity :as-is #| this '| form must come after the '|| form|#)
		(gen-prefix-data '@@ 'progn identity :as-is)
		(gen-prefix-data '&& 'and proc-wait :as-is)
		(gen-prefix-data 'out> 'out> identity :swap-last-for-first)
		(gen-prefix-data 'out>> 'out>> identity :swap-last-for-first)
		(gen-prefix-data 'err> 'err> identity :swap-last-for-first)
		(gen-prefix-data 'err>> 'err>> identity :swap-last-for-first)
		(gen-prefix-data 'out>null 'out>null identity :swap-last-for-first)
		(gen-prefix-data 'out-err> 'out-err> identity :swap-last-for-first)
		(gen-prefix-data 'out-err>> 'out-err>> identity :swap-last-for-first)
		(gen-prefix-data 'out-err>null 'out-err>null identity :swap-last-for-first)))

(defn gen-prefix-set
	"Use to map infix symbol to the prefix symbol with which it corresponds."
	(hashset prefix-props)
	(hash-set! hashset (hash-get prefix-props :infix-symbol) prefix-props))

(defn gen-prefix-infix-map
	"Build a lookup table of infix operators to a hash set of metadata
	about those operators."
	(metadata-list prefix-infix-map)
	(progn
		(defq fst (first metadata-list))
		(if (not fst)
			prefix-infix-map
			(progn
				(gen-prefix-set prefix-infix-map fst)
				(recur (rest metadata-list) prefix-infix-map)))))

(defq prefix-infix-map (gen-prefix-infix-map prefix-metadata (make-hash)))

(defn apply-ast-order (ast order)
	(if (< (length ast) 3)
		(err "Expressions with infix symbols must have at least 3 forms")
		(match order
			(:as-is ast)
			(:swap-last-for-first (append (list (first ast)) (last ast) (butlast (rest ast))))
			(nil (err "Unable to apply ordering, unknown order symbol.")))))

(defn prefixify-cmd (cmd-toks prefix-props)
	(progn
		(defq apply-xform (fn (x) (if (not (= (hash-get prefix-props :infix-symbol) (last x))) (vec-push! x (((hash-get prefix-props :xform)) (vec-pop! x))))))
		(defq build-cmd (fn (cmd-ast raw-list)
			(progn
				(defq next-tok (first raw-list))
				(defq infix-symbol (hash-get prefix-props :infix-symbol))
				(if (not next-tok)
					(apply-xform cmd-ast)
					(progn
						(recur
							(if (= infix-symbol next-tok)
								(progn
									(apply-xform cmd-ast)
										(append!
										cmd-ast
										(vec (make-vec)))
										cmd-ast)
								(progn
									(append!
										(last cmd-ast)
										(if (seq? next-tok) next-tok (vec next-tok)))
									cmd-ast))
							(rest raw-list)))))))
		(defq prefixified-ast
			(build-cmd (vec (hash-get prefix-props :prefix-symbol) (make-vec))
			cmd-toks))
		(setq prefixified-ast (apply-ast-order prefixified-ast (hash-get prefix-props :ast-order)))
		prefixified-ast))

;; return true if cmd satisfies preconditions for prefixification
(defn satisfies-prefixify-preconditions (cmd-ast infix-hash-set)
	(let ((infix-symbol (hash-get infix-hash-set :infix-symbol)))
		(not (or ;; reasons to skip pre-processing
			;; if read returns nil
			(not cmd-ast)
			;; if cmd doesn't contain symbol
			(not (in? cmd-ast infix-symbol))
			;; if already in prefix notation
			(= infix-symbol (first cmd-ast))))))

(defn confirm-prefix-eligible (cmd-ast prefix-metadata)
	;; recurse over prefix-metadata return nil if there are none but
	;; return the pair if it's valid
	(progn (defq prefix-props (first prefix-metadata))
		(if (not prefix-props)
			nil
			(if (satisfies-prefixify-preconditions cmd-ast prefix-props)
				prefix-props
				(recur cmd-ast (rest prefix-metadata))))))

;; TODO this function could/should be applied recursively to handle any
;; prefixification needs in inner forms.
(defn check-for-infix-notation (cmd-ast)
	;; confirm cmd ast needs prefixification ...then call prefixify.
	(progn
		(defq prefix-eligible
			(confirm-prefix-eligible cmd-ast prefix-metadata))
		;;TODO could loop by the infix symbols. this would allow us to
		;; detect forms with infix symbols nested within infix symbols.
		;; would start by finding the first instance of an infix symbol
		;; in the ast. then group all forms after in list, until
		;; occurrence of next of that infix symbol. then recursive
			;; aplication could properly prefixify everything.
		(if (not prefix-eligible)
			cmd-ast
			(prefixify-cmd cmd-ast prefix-eligible))))

(defn recursively-check-for-infix-notation (new-ast orig-ast)
	(progn
		(defq fst (first orig-ast))
		(defq rst (rest orig-ast))
		(if (not fst)
			(check-for-infix-notation new-ast)
			(recur (append new-ast
						(if (non-empty-seq? fst)
							(progn
								(defq prefixified-subform (check-for-infix-notation fst))
								(vec (recursively-check-for-infix-notation (make-vec) prefixified-subform)))
							(vec fst)))
				rst))))

(defn remove-any-infix-notation (cmd-ast)
	(recursively-check-for-infix-notation (make-vec) cmd-ast))

(defn find-infix-symbol
	"starting at idx search for infix-symbol. if found return a parid composed
	of the properties needed to prexfixify the command and the index of the
	infix symbol."
	(cmd-ast idx)
	(if (= idx (length cmd-ast))
	  nil
	  (progn
		(defq nxt (vec-nth idx cmd-ast))
		(if (not nxt)
			nil
			(progn
				(defq tok-match (if (or (symbol? nxt) (string? nxt)) (hash-get prefix-infix-map nxt) nil))
				(if (not tok-match)
					(recur cmd-ast (+ 1 idx))
					  (join tok-match idx)))))))

(defn wrap-infix-notation
	"take cmd-ast and first idx of new infix operator and split lists i.e.
	`(cat file | grep -i user out> users)`
	would become
	`((cat file | grep -i user) out> users)`
	which can be more easily processed and turned into the prefixified version"
	(cmd-ast idx)
	(append (vec (vec-slice cmd-ast 0 idx)) (vec-slice cmd-ast idx (length cmd-ast))))

(defn modify-if-mixed-infix-notation (cmd-ast idx previous)
	(progn
		(defq contains-infix (find-infix-symbol cmd-ast idx))
		(if (not contains-infix)
			cmd-ast
			(progn
				(defq props (car contains-infix))
				(defq idx (cdr contains-infix))
				(defq infix (hash-get props :infix-symbol))
				(defq no-match-yet (not previous))
				(defq infix-changed (and (not no-match-yet) (not (= infix previous))))
				;; if infix-changed the new ast is now something like
				;; `((cat file | grep -i user) out> users)`
				;; which means we want to start searching for infix
				;; notation starting at the 2nd idx.
				(defq new-cmd-ast (if infix-changed (wrap-infix-notation cmd-ast idx) cmd-ast))
				(defq new-idx (if infix-changed 2 (+ 1 idx)))
				(recur new-cmd-ast new-idx infix)))))

(defn recursively-modify-if-mixed-infix-notation
	(new-ast orig-ast)
	(progn
		(defq fst (first orig-ast))
		(if (not fst)
			(modify-if-mixed-infix-notation new-ast 0 nil)
			(progn
				(defq rst (rest orig-ast))
				(defq found-seq (non-empty-seq? fst))
				(if found-seq
					(vec-push! new-ast (recursively-modify-if-mixed-infix-notation (make-vec) fst))
					(vec-push! new-ast fst))
				(recur new-ast rst)))))

(defn remove-any-mixed-infix-notation (cmd-ast)
	(recursively-modify-if-mixed-infix-notation (make-vec) cmd-ast))

;; entrypoint for all multiargument commands, used to allow use of infix
;; notation.
(defn apply-infix-modifications (cmd-ast)
	(remove-any-infix-notation (remove-any-mixed-infix-notation cmd-ast)))

;; entrypoint for all 1 arg commands... used to make filepaths cd commands
;; to themselves.
(defn change-dir-if-arg-is-dir (cmd)
	(let ((cmd-str (str cmd)))
		(if (fs-dir? cmd-str)
			(list root::cd cmd-str)
			cmd-str)))

(defn __exec_hook (cmd-str)
	(if (= :success
		(get-error
			(defq cmd-ast (read :add-parens cmd-str))
			:success))
		cmd-str
		(match (length cmd-ast)
			;; if string is of length 0 either nothing was typed or
			;; everything was a comment
			(0 cmd-ast)
			;; check to see if this single argument is a filepath
			(1 (change-dir-if-arg-is-dir (first cmd-ast)))
			;; check for infix notation
			(nil (apply-infix-modifications cmd-ast)))))

#|
;; TODO need tests
	(defq pipe-test-actual (recursively-check-for-infix-notation (list) (read :add-parens "(echo first-partAsecond-partBthird-part | cut -d \"A\" -f 2 | cut -d \"B\" -f 2)")))
	(defq pipe-test-expected `(vec '| (vec echo first-partAsecond-partBthird-part) (vec cut -d "A" -f 2) (vec cut -d "B" -f 2)))
	(println (str "expected " (pipe-test-expected)
					"\nactual " pipe-test-actual
					"\nactual == expected: " (= pipe-test-actual (pipe-test-expected))
					"\nactual == actual " (= pipe-test-actual pipe-test-actual)
				  ))
|#

;; }}}
