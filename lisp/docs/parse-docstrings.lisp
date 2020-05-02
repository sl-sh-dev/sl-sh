(if (ns-exists? 'docparse) (ns-enter 'docparse) (ns-create 'docparse))

(core::ns-import 'core)
(ns-import 'shell)

(defq has-no-doc " ")

(defn get-doc-section-if-exists (key idx docstring) (progn
	(defq full-key (str key ":"))
	(if (str-contains full-key docstring)
		(if (= key "Section")
			(str-replace (str-trim (vec-nth idx (str-split full-key docstring))) "\n" "")
			(str-replace (str-trim (vec-nth idx (str-split full-key docstring))) "\n" "<br>"))
		has-no-doc)))

(defn get-mid-doc-section (sym key second-key docstring required) (progn
	(defq type-doc (get-doc-section-if-exists key 1 docstring))
	(if (= type-doc has-no-doc)
		(if required
			(err (str "Every docstring must have: " key ", but " sym " does not."))
			:none)
		(get-doc-section-if-exists second-key 0 type-doc))))

(defn get-example-doc-section (key docstring)
	(get-doc-section-if-exists key 1 docstring))

;; TODO maybe write a function that verifies order is correct in the docstrings?
;; to prevent future headaches?
(defn parse-doc (sym) (progn
	(defq docstring (doc sym))
	(defq doc-map (make-hash))
	(hash-set! doc-map :form (if (= sym '|) (str '\ sym) (str sym)))
	(hash-set! doc-map :type (get-mid-doc-section sym "Type" "Namespace" docstring #t))
	(hash-set! doc-map :namespace (get-mid-doc-section sym "Namespace" "Usage" docstring #t))
	(hash-set! doc-map :usage (get-mid-doc-section sym "Usage" "Section" docstring #t))
	(hash-set! doc-map :section (get-mid-doc-section sym "Section" "Example" docstring nil))
	(hash-set! doc-map :example (get-example-doc-section "Example" docstring))
	doc-map))

(defn parse-docstrings-for-syms (sym-list) (progn
	(defq docstring-sections (make-hash))
	(defq section-builder (fn (syms)
		(when (not (empty-seq? syms))
			(progn
				(defq doc-map (parse-doc (first syms)))
				(defq section-key (hash-get doc-map :section))
				(if (hash-haskey docstring-sections section-key)
					(append! (hash-get docstring-sections section-key) doc-map)
					(hash-set! docstring-sections section-key (list doc-map)))
				(recur (rest syms))))))
	(section-builder sym-list)
	docstring-sections))

(ns-export '(parse-docstrings-for-syms))
