#!/usr/bin/env sl-sh

(ns-push 'notify)
(ns-import 'shell)
(ns-import 'iterator)

(defn get-file-md (file)
	(chain (make-hash) (hash-set! _ :modified (fs-modified file))))

(defn add-file-md (file-map file)
	(hash-set! file-map file (get-file-md file)))

(defn get-file-map (file)
	(let ((file-map (make-hash)))
		   (fs-crawl file (fn (x) (add-file-md file-map x)))
		   file-map))

(defn collate-fs-changes
	  (new-map prev-map)
	(let ((changes (chain (make-hash)
						  (hash-set! _ :created (make-vec))
						  (hash-set! _ :modified (make-vec))
						  (hash-set! _ :deleted (make-vec)))))
	   (for file in (hash-keys new-map)
			(let ((prev-file-md (hash-remove! prev-map file)))
			  (if (nil? prev-file-md) ;; file not in previous scan, it's new!
				(vec-push! (hash-get changes :created) file)
				(let ((new-file-md (hash-get new-map file)))
				  (when (> (hash-get new-file-md :modified)
						  (hash-get prev-file-md :modified))
					;; modified time increased, file was changed.
					(vec-push! (hash-get changes :modified) file))))))
	   ;; prev-map will now only have keys that were not in new-map indicating
	   ;; files were deleted.
	   (for file in (hash-keys prev-map)
			(vec-push! (hash-get changes :deleted) file))
	  changes))

(defn fs-notify (callback to-watch to-sleep)
	 (loop (prev-map) ((get-file-map to-watch))
	   (let* ((slept (sleep to-sleep))
			(new-map (get-file-map to-watch))
			(changes (collate-fs-changes new-map prev-map)))
	   (for event in (hash-keys changes)
			(for file in (hash-get changes event)
				(callback file event)))
	   (recur new-map))))

(println (let ((file-map (make-hash))
			   (to-watch (first args)))
		   (fs-notify
			 (fn (f e) (println "to-watch " f ", event " e))
			 to-watch
			 250)))

(ns-auto-export 'notify)
(ns-pop)
