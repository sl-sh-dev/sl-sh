#!/usr/bin/env sl-sh

(ns-push 'notify)
(ns-import 'shell)
(ns-import 'iterator)

(defn get-file-md (file)
	(chain (make-hash)
		(hash-set! _ :mod (fs-mod file))
		(hash-set! _ :created (fs-created file))
		(hash-set! _ :accessed (fs-accessed file))))

(defn add-file-md (file-map file)
	(hash-set! file-map file (get-file-md file)))

(def file-map (make-hash))

(defn fs-notify (file)
	 (fs-crawl file (fn (x) (add-file-md file-map x)))
	 file-map)

(fs-notify (first args))

(ns-auto-export 'notify)
(ns-pop)
