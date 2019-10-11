(def 'defmacro (macro (name args body)
	`(progn (def (quote ,name) (macro ,args ,body)) nil)))

(defmacro setq (sym bind)
	`(set (quote ,sym) ,bind))

(defmacro defq (sym bind)
	`(def (quote ,sym) ,bind))

(defmacro defn (name args body)
	`(defq ,name (fn ,args ,body)))

(defmacro setfn (name args body)
	`(setq ,name (fn ,args ,body)))

(defmacro loop (params bindings body)
		`((fn ,params ,body) ,@bindings))

(defmacro dotimes (times body)
	(let ((idx-name (gensym)))
	`(loop (idx-name) (,times) (progn
		(eval ,body)
		(if (> idx-name 1) (recur (- idx-name 1)))))))

(defmacro dotimesi (idx-bind times body)
	(let ((stop-name (gensym)))
	`(loop (,idx-bind stop-name) (0 (- ,times 1)) (progn
		(eval ,body)
		(if (< ,idx-bind stop-name) (recur (+ ,idx-bind 1) stop-name))))))

(defmacro for (bind in_list body)
	`(let ((,bind))
		(if (> (length ,in_list) 0)
			(loop (plist) (,in_list) (progn
				(setq ,bind (first plist))
				(,@body)
				(if (> (length plist) 1) (recur (rest plist))))))))

(defmacro fori (idx_bind bind in_list body)
	`((fn () (progn
		(defq ,bind nil)(defq ,idx_bind nil)
		(if (> (length ,in_list) 0)
			(loop (plist idx) (,in_list 0) (progn
				(setq ,bind (first plist))
				(setq ,idx_bind idx)
				(,@body)
				(if (> (length plist) 1) (recur (rest plist) (+ idx 1))))))))))

(defmacro match (condition &rest branches)
	(let ((cond-name) (out_list '()) (make-cond))
		(setq make-cond (fn (condition val action others)
			(if (null val) action
				(if (null others) `(if (= ,condition ,val) ,action)
					`(if (= ,condition ,val) ,action ,(make-cond condition (first (first others)) (nth 1 (first others)) (rest others)))))))
		(setq cond-name condition)
		(make-cond cond-name (first (first branches)) (nth 1 (first branches)) (rest branches))))

(defmacro let (vals &rest let_body)
	((fn (params bindings) (progn
		(fori idx el vals
			(if (= 1 (length el))
				(progn (insert-nth idx (nth 0 el) params) (insert-nth idx nil bindings))
				(if (= 2 (length el))
					(progn (insert-nth idx (nth 0 el) params) (insert-nth idx (nth 1 el) bindings))
					(println "ERROR"))))
		`((fn ,params (progn ,@let_body)) ,@bindings))) (make-list (length vals)) (make-list (length vals))))
