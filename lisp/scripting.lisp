;;; to make scripting easier


;;TODO implement getopts

(defn mkli
	"\"make lisp\".creates a sl-sh shell script. given a file, a namespace (optional 2nd arg), and a string
	to populate as the body (optional 3rd arg), make a canonincal blank sl-sh script
	complete with all the relevant imports, and boilerplate namespace code taken
	care of to speed up development.

	it is recommended all calls to load are done at the top of the file (before
	the calls to ns-enter or ns-create, in case a library sl-sh script  calls a
	library sl-sh script that created a namespace and forgot to call ns-pop.
	this ensures the exported symbols for the first library's scripts
	namespace are importable in the executing script's namespace.

	all calls to ns-import happen after a ns is created and entered so the
	current namespace is the namespace that houses all the imported symbols.

	ns-export must be called before ns-pop so the appropriate symbols are
	associated namespace, the one in which the symbols were created.

	Section: scripting "
	(&rest args) (progn
	(defq filepath nil)
	(defq namespace nil)
	(defq script-body nil)
	(when (= 0 (length args)) (err "Must have at least 1 argument, the path to the script to be created."))
	(when (< 3 (length args)) (err (str "Too many arguments, see doc " #| (doc 'mk-sh) |# )))
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
		(write-line new-file "(ns-export '()) ;; export any ns symbols that should be importable")
		(write-line new-file "(ns-pop) ;; must be after body")))
	(close new-file)))

(ns-export '(mkli))

