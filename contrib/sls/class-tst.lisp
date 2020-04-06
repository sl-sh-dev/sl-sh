; Playing around with building a class (or really a struct with methods not full class).

; This is scratch code used to build the macros below.
(def 'make-tst nil)
((fn () (progn
    (def 'field-one 1)
    (def 'field-two "two")

	(defn meth-one (x) (+ x field-one))
	(defn meth-two (y) (str field-two ": " y))
	(setfn make-tst ()
	       (fn (msg &rest args)
	           (if (= msg :meth-one) (apply meth-one args)
	             (if (= msg :meth-two) (apply meth-two args)
	               (if (= msg :field-one) field-one
	                 (if (= msg :field-two) field-two
	                   (if (= msg :set-field-one) (apply set 'field-one args)
	               (err "Invalid message to class tst.")))))))))))

; macro to build a "struct"
(defmacro defstruct (name fields methods &rest body)
	((fn (params bindings meth-list) (progn
		(core::fori idx field fields
			(if (= 1 (length field))
				(progn (vec-insert-nth! idx (core::nth 0 field) params) (vec-insert-nth! idx nil bindings))
				(if (= 2 (length field))
					(progn (vec-insert-nth! idx (core::nth 0 field) params) (vec-insert-nth! idx (core::nth 1 field) bindings))
					(err "ERROR: invalid bindings on defstruct"))))
		(def 'method-defs (make-vec (length methods)))
		(core::for method methods (progn
			(vec-push! meth-list (first method))
			(vec-push! method-defs `(defn ,@method))))
		(def 'accessors nil)
		(def 'cur (make-vec (+ 1 (* 2 (length params)))))
		(def 'tmp nil)
		(core::for p params (progn
			(def 'tsym (to-symbol (str ":" p)))
			(set 'tmp `#(if (= msg ,tsym) ,p))
			(vec-push! cur tmp)
			(set 'cur tmp)
			(if (null accessors) (set 'accessors cur))
			(def 'tsym (to-symbol (str ":set-" p)))
			(set 'tmp `#(if (= msg ,tsym) (apply set ',p args)))
			(vec-push! cur tmp)
			(set 'cur tmp)))
		(core::for m meth-list (progn
			(def 'tsym (to-symbol (str ":" m)))
			(set 'tmp `#(if (= msg ,tsym) (apply ,m args)))
			(vec-push! cur tmp)
			(set 'cur tmp)
			(if (null accessors) (set 'accessors cur))))
		(set 'tmp `#(if (= msg :type) (symbol-name ',name)))
		(vec-push! cur tmp)
		(set 'cur tmp)
		(def 'err-msg (str "Invalid message to class: " name))
		(vec-push! cur `(err ,err-msg))
		`(progn
			(def (to-symbol (str "make-" ',name)) nil)
			((fn ,params (progn
				,@method-defs
				,@body
				(set (to-symbol (str "make-" ',name))
					(fn () (fn (msg &rest args) ,accessors)))))
			 ,@bindings)) )) ; bindings for params
	 (make-vec (length fields)) ; params
	 (make-vec (length fields)) ; bindings
	 (make-vec (length methods)))) ; meth-list

; Build a struct
(defstruct sone
  ((x nil)
   (y)
   (z 3))
  ((p-x () (int-fn x))
   (get-a () a))
  ; these things defined in the defstruct will be "private"
  (def 'a "internal")
  (defn int-fn (v) (println a ": " v)))
