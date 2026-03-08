#!/usr/bin/env slosh

;; Dynamic File Demo
;; Demonstrates FUSE integration with static content evaluated at registration time

;; Mount a dynamic filesystem
(def mount-id (mount-eval-fs "/tmp/slosh-dynamic"))

;; Register a systemd-style environment file
;; Content is evaluated NOW and sent to the FUSE server
(register-eval-file mount-id "my-service.env"
  (str "HOST=" (or (env "HOSTNAME") "localhost") "\n"
       "CUSTOM_TYPE=" (or *custom-type* "default") "\n"))

;; Register another file showing system info
(register-eval-file mount-id "system-info.txt"
  (str "User: " (env "USER") "\n"
       "Home: " (env "HOME") "\n"
       "Shell: " (env "SHELL") "\n"))

;; List registered files
(println "Registered files:")
(doseq [file (list-eval-files mount-id)]
  (println "  " file))

;; Files are now readable at the mount point:
;;   cat /tmp/slosh-dynamic/my-service.env
;;   cat /tmp/slosh-dynamic/system-info.txt

;; Clean up
(unmount-eval-fs mount-id)
(println "Filesystem unmounted.")
