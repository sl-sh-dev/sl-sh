#!/usr/bin/env slosh

;; Dynamic File Demo
;; This demonstrates how dynamic files would work with the FUSE integration

;; Simulate mounting a dynamic filesystem
(def mount-id (mount-eval-fs "/tmp/slosh-dynamic"))

;; Register a systemd-style environment file
;; This would dynamically generate content based on slosh expressions
(register-eval-file mount-id "my-service.env" 
  "(str \"HOST=\" (or (env \"HOSTNAME\") \"localhost\") \"\\n\"
        \"CUSTOM_TYPE=\" (or *custom-type* \"default\") \"\\n\"
        \"TIMESTAMP=\" (current-time-millis) \"\\n\")")

;; Register another dynamic file showing system info
(register-eval-file mount-id "system-info.txt"
  "(str \"User: \" (env \"USER\") \"\\n\"
        \"Home: \" (env \"HOME\") \"\\n\"
        \"Shell: \" (env \"SHELL\") \"\\n\"
        \"PWD: \" (env \"PWD\") \"\\n\")")

;; List registered files
(println "Registered files:")
(doseq [file (list-eval-files mount-id)]
  (println "  " file))

;; Demonstrate what the content would look like
(println "\nFile contents would be:")
(println "--- my-service.env ---")
(println (eval-file-expression mount-id "my-service.env"))
(println "\n--- system-info.txt ---")
(println (eval-file-expression mount-id "system-info.txt"))

;; Clean up
(unmount-eval-fs mount-id)
(println "\nFilesystem unmounted.")