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

(defn first (obj)
    (if (is-vec obj)
        (vnth 0 obj)
        (if (is-proper-list obj)
            (car obj)
            (err "Not a vector or list"))))

(defn rest (obj)
    (if (is-vec obj)
        (vslice obj 1)
        (if (is-proper-list obj)
            (cdr obj)
            (err "Not a vector or list"))))

(defn last (obj)
    (if (is-vec obj)
        (vnth (- (length obj) 1) obj)
        (if (is-proper-list obj)
            (if (null (cdr obj))
                (car obj)
                (recur (cdr obj)))
            (err "Not a vector or list"))))

(defn butlast (obj)
    (if (is-vec obj)
        (vslice obj 0 (- (length obj) 1))
        (if (is-proper-list obj) (progn
            (defq new-link (join nil nil))
            (if (null (cdr obj))
                (setq new-link nil)
                (setq new-link (join (car obj) (butlast (cdr obj)))))
            new-link)
            (err "Not a vector or list"))))

(defn setnth! (idx obj l)
    (if (is-vec l)
        (progn (vsetnth! idx obj l) nil)
        (if (is-proper-list l)
            (if (= idx 0) (progn (xar! l obj) nil) (recur (- idx 1) obj (cdr l)))
            (err "Not a vector or list"))))

(defn nth (idx obj)
    (if (is-vec obj)
        (vnth idx obj)
        (if (is-proper-list obj)
            (if (= idx 0) (car obj) (recur (- idx 1) (cdr obj)))
            (err "Not a vector or list"))))

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
				(progn (vinsert-nth! idx (nth 0 el) params) (vinsert-nth! idx nil bindings))
				(if (= 2 (length el))
					(progn (vinsert-nth! idx (nth 0 el) params) (vinsert-nth! idx (nth 1 el) bindings))
					(err "ERROR: invalid bindings on let"))))
		`((fn ,params (progn ,@let_body)) ,@bindings))) (make-vec (length vals)) (make-vec (length vals))))

(defn append (l1 l2) (progn
    (def 'ret (make-vec))
    (for el l1 (push! ret el))
    (for el l2 (push! ret el))
    ret))

(defn map (fun items) (progn
	(defq new-items (make-vec (length items)))
	(for i items
		(push! new-items (fun i)))
	new-items))

(defn map! (fun items) (progn
	(fori i it items
		(vsetnth i (fun it) items))
	items))

(defn reverse (items) (progn
	(defn irev (items new-items num)
		(if (>= num 0) (progn (push! new-items (vnth num items))(recur items new-items (- num 1)))))
	(defq new-items (make-vec (length items)))
	(irev items new-items (- (length items) 1))
	new-items))

(defn reverse! (items) (progn
	(defn irev (items first last)
		(if (> last first) (progn
			(defq ftemp (vnth first items))
			(vsetnth first (vnth last items) items)
			(vsetnth last ftemp items)
			(recur items (+ first 1) (- last 1)))))
	(irev items 0 (- (length items) 1))
	items))

