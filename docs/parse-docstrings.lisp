(if (ns-exists? 'docparse) (ns-enter 'docparse) (ns-create 'docparse))
(core::ns-import 'core)
(ns-import 'shell)

(defn get-doc-section-if-exists (key idx docstring) (progn
	(defq full-key (str key ":"))
	(when (str-contains full-key docstring)
		(str-trim (vec-nth idx (str-split full-key docstring))))))

(defn cut-target-str
	"Function is used to cut the appropriate piece of the doc string out of the
	target-str. The docstring api is a text block separated by predefined
	strings delimiting sections. It is necessary, after identifying the start
	of the block of text needed (by splitting on an ordered list of predefined
	strings), to cut the remaining text (target-str) to just above the start
	of the next prefedined string."
	(second-keys second-key-idx target-str)
	(if (not (empty-seq? second-keys))
		(progn
			(defq text-slice (get-doc-section-if-exists (first second-keys) second-key-idx target-str))
			(if (not (nil? text-slice))
					text-slice
				(recur (rest second-keys) second-key-idx target-str)))
		target-str))

(defn get-mid-doc-section (sym key key-idx second-key-idx docstring required &rest second-keys) (progn
	(defq type-doc (get-doc-section-if-exists key key-idx docstring))
	(if (nil? type-doc)
		(when required
			(err (str "Every docstring must have: " key ", but " sym " does not.")))
		(cut-target-str second-keys second-key-idx type-doc))))

(defn get-example-doc-section (key docstring)
	(get-doc-section-if-exists key 1 docstring))

;; TODO maybe write a function that verifies order is correct in the docstrings?
;; to prevent future headaches?
(defn parse-doc (sym) (progn
	(defq docstring (doc sym))
	(defq doc-map (make-hash))
	(hash-set! doc-map :form
		(if (= sym '|) (str '\ sym) (str sym)))
	(hash-set! doc-map :type
		(progn
		(defq ms (get-mid-doc-section sym "Type" 1 0 docstring #t "Namespace"))
		ms))
	(hash-set! doc-map :namespace
		(get-mid-doc-section sym "Namespace" 1 0 docstring #t "Usage" "Section" "Example"))
	(hash-set! doc-map :usage
		(get-mid-doc-section sym "Usage" 1 0 docstring #t "Section" "Example"))
	(hash-set! doc-map :section
		(progn
		(defq sec (get-mid-doc-section sym "Section" 1 0 docstring nil "Example"))
		(if (or (nil? sec) (str-empty? (str-trim sec))) :uncategorized sec)))
	(hash-set! doc-map :example
		(get-example-doc-section "Example" docstring))
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
