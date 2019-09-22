(defmacro setq (sym bind)
	`(set (quote ,sym) ,bind))

(defmacro defn (name args body)
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
	`(if (> (length ,in_list) 0)
		(loop (plist) (,in_list) (progn
			(setq ,bind (first plist))
			(eval ,body)
			(if (> (length plist) 1) (recur (rest plist)))))))

(defmacro fori (idx_bind bind in_list body)
	`(if (> (length ,in_list) 0)
		(loop (plist idx) (,in_list 0) (progn
			(setq ,bind (first plist))
			(setq ,idx_bind idx)
			(eval ,body)
			(if (> (length plist) 1) (recur (rest plist) (+ idx 1)))))))
