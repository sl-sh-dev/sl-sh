;;; to make scripting easier

;;TODO sstanfield replace get-error with code like this
(defmacro error-or-ok
	"like progn but always returns a hashmap. hashmap will either one key value
	pair: :ok or :error.  If the key is :ok the value is the result of the last
	form in error-or-ok and if the key is :error the value is the error string.
	Section: scripting"
	(&rest args) `(progn
	(defq ret (get-error ,@args))
	(defq ret-map (make-hash))
	(if (and (vec? ret) (= :error (first ret)))
		(hash-set! ret-map :error (first (rest ret)))
		(hash-set! ret-map :ok ret))))

;;TODO implement getopts

(defn mkli
	"Usage: (mkli filepath [namespace] [body])
	\"MaKe LIsp file\", creates a sl-sh shell script. given a file, a namespace
	(optional 2nd arg), and a string to populate as the body (optional 3rd arg),
	make a canonincal blank sl-sh script complete with all the relevant imports,
	and boilerplate namespace code taken care of to speed up development.

	it is recommended all calls to load are done at the top of the file (before
	the calls to [ns-enter](#root:ns-enter), [ns-create](#root::ns-create), and [ns-import](#root::ns-import)).

	all calls to ns-import happen after a namespace is created and entered so the
	current namespace is the namespace that houses all the imported symbols.

	[ns-export](#root::ns-export) or [ns-auto-export](#root:ns-auto-export) must
	be called before [ns-pop](root::ns-pop) so the appropriate symbols are
	associated namespace, the one in which the symbols were created.

	Section: scripting"
	(&rest args) (progn
	(defq filepath nil)
	(defq namespace nil)
	(defq script-body nil)
	(when (= 0 (length args)) (err "Must have at least 1 argument, the path to the script to be created."))
	(when (< 3 (length args)) (err (str "Too many arguments, see doc ")))
	(when (< 2 (length args)) (setq script-body (vec-nth 2 args)))
	(when (< 1 (length args)) (setq namespace (vec-nth 1 args)))
	(when (< 0 (length args)) (setq filepath (vec-nth 0 args)))
	(defq new-file (open filepath :create :append))
	(chmod "+x" filepath)
	(write-line new-file "#!/usr/bin/env sl-sh")
	(write-line new-file "")
	(write-line new-file ";; \"load\" calls go above here but below interpreter directive.")
	(when (not (nil? namespace))
		(write-line new-file (str "(if (ns-exists? '" namespace ") (ns-enter '" namespace ") (ns-create '" namespace "))" )))
	(write-line new-file "(core::ns-import 'core)")
	(write-line new-file "(ns-import 'shell) ;; imports from load calls & body below")
	(if (not (nil? script-body))
		(progn
			(write-line new-file "")
			(write-line new-file script-body)
			(write-line new-file ""))
		(write-line new-file ""))
	(when (not (nil? namespace))
		(progn
		(write-line new-file (str "(ns-auto-export '" namespace ") ;; export any ns symbols that should be importable"))
		(write-line new-file "(ns-pop) ;; must be after body and exports")))
	(close new-file)))

(ns-export '(mkli error-or-ok))

