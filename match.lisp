(defn match-make-cond (condition val action others) (progn
;                                                     (println "OTHER: " others)
;                                                     (println "VAL: " val)
;                                                     (println "ACTION: " action)
    (if (null val) action
    (if (null others) `(if (= ,condition ,val) ,action)
    `(if (= ,condition ,val) ,action ,(match-make-cond condition (first (first others)) (nth 1 (first others)) (rest others))))) ))

(defmacro match (condition &rest branches)
	(let ((cond-name) (out_list '()) (make-cond))
        (setq make-cond (fn (condition val action others)
            (if (null val) action
                (if (null others) `(if (= ,condition ,val) ,action)
                    `(if (= ,condition ,val) ,action ,(match-make-cond condition (first (first others)) (nth 1 (first others)) (rest others)))))))
        (setq cond-name condition)
        (make-cond cond-name (first (first branches)) (nth 1 (first branches)) (rest branches))))

        ;(append out_list `(if (= cond_name ,(first (first cond-name))) `(progn ,(rest __b))
	;`(loop (idx-name) (,times) (progn
	;	(eval ,body)
	;	(if (> idx-name 1) (recur (- idx-name 1)))))))

(defn brancht (cond)
(if (= cond "one") (println "one")
    (if (= cond "two") (println "two")
        (if (= cond "three") (println "three")
            (println "default")))))

(brancht "two")
(brancht 'one)

(defn matcht (x)
(match x
       ('one (println "m one"))
       ('two (println "m two"))
       ('three (println "m three"))
       (nil (println "default"))))

(matcht 'two)
(matcht 'one)
(matcht 'three)
