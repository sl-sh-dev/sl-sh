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

(defn get-mid-doc-section (sym key key-idx second-key second-key-idx docstring required) (progn
	(defq type-doc (get-doc-section-if-exists key key-idx docstring))
	(if (= type-doc has-no-doc)
		(if required
			(err (str "Every docstring must have: " key ", but " sym " does not."))
			:none)
		(get-doc-section-if-exists second-key second-key-idx type-doc))))

(defn get-example-doc-section (key docstring)
	(get-doc-section-if-exists key 1 docstring))

;; TODO maybe write a function that verifies order is correct in the docstrings?
;; to prevent future headaches?
(defn parse-doc (sym) (progn
	(defq docstring (doc sym))
	(defq doc-map (make-hash))
	(hash-set! doc-map :form (if (= sym '|) (str '\ sym) (str sym)))
	(hash-set! doc-map :type (get-mid-doc-section sym "Type" 1 "Namespace" 0 docstring #t))
	(hash-set! doc-map :namespace (get-mid-doc-section sym "Namespace" 1 "Usage" 0 docstring #t))
	(hash-set! doc-map :usage (get-mid-doc-section sym "Namespace" 1 "Section" 0 docstring #t))
	(hash-set! doc-map :section (get-mid-doc-section sym "Section" 1 "Example" 0 docstring nil))
	(hash-set! doc-map :example (get-example-doc-section "Example" docstring))
	doc-map))

(defn parse-docstrings-for-syms
"Takes a list of slsh symbols and returns a hashmap. To build the hashmap this
function gets the docstring  (calls the root:doc function which return the
docstring) for each symbol. The value for the \"Section:\" declaration in the
doc symbol is used as the key in the returned map.  The value for each key in the
map is a list of hashmaps representing structured data parsed from the
docstrings. The string following \"Section:\" can be anything but using a
coherent set of values for the section declaration allows for flexible
organization of docstrings.
"
	(sym-list) (progn
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
