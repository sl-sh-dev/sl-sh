#!/usr/bin/env sl-sh

(ns-push 'notify)
(ns-import 'shell)
(ns-import 'iterator)

(defn get-file-md (file)
	(chain (make-hash)
		(hash-set! _ :modified (fs-modified file))
		(hash-set! _ :created (fs-created file))
		(hash-set! _ :accessed (fs-accessed file))))

(defn add-file-md (file-map file)
	(hash-set! file-map file (get-file-md file)))

(defn get-file-map (file)
	(let ((file-map (make-hash)))
		   (fs-crawl file (fn (x) (add-file-md file-map x)))
		   file-map))

(defn fs-notify (callback to-watch prev-map)
	 (let ((slept (sleep 250))
		   (new-map (get-file-map to-watch)))
	   (for key in (hash-keys new-map)
			(let ((prev-file-md (hash-remove! prev-map key)))
			  (if (nil? prev-file-md)
				(callback key :created) ;; key not in previous scan, it's new!
				(let ((new-file-md (hash-get new-map key)))
				  (when (> (hash-get new-file-md :modified)
						  (hash-get prev-file-md :modified))
					;; modified time increased, file was changed.
					(callback key :modified))))))
	   ;; prev-map will now only have keys that were not in new-map indicating
	   ;; files were deleted.
	   (for key in (hash-keys prev-map)
			(callback key :deleted))
	   (recur callback to-watch new-map)))

(println (let ((file-map (make-hash))
			   (to-watch (first args)))
		   (fs-notify
			 (fn (f e) (println "to-watch " f ", event " e))
			 to-watch
			 (get-file-map to-watch))))

(ns-auto-export 'notify)
(ns-pop)
