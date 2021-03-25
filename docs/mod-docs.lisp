#!/usr/bin/env sl-sh

(ns-push 'moddocs)

(defn set-version (src-file dest-file)
    (loop (read-file write-file) ((open src-file :read)
                        (open dest-file :create :truncate))
        (do (var line (read-line read-file))
        (var vers (vec-nth (str-split :whitespace (version)) 1))
            (if (not (nil? line))
                (do
                 (var line2 (str-replace line "\$(version)" vers))
                 (write-string write-file line2)
                  (recur read-file write-file))
                (do
                    (close read-file)
                    (close write-file))))))

(when (> (length args) 0)
    (set-version (vec-nth args 0) (vec-nth args 1)))

(ns-auto-export 'moddocs)
(ns-pop)
