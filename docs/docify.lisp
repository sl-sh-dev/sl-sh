#!/usr/bin/env sl-sh

;; TODO general
;; - could put all functions in glossary type datastructure, would make them searchable potentially?
;;      would be cool if search feature of website worked for docs too.
;; - get /news panel back!
;; - fix syntax highlighting in html output.


(load "mk-docs.lisp")
(load "mk-post.lisp")
(load "mod-docs.lisp")

(ns-import 'iterator)
(ns-import 'shell)
(ns-import 'mkdocs)
(ns-import 'mkpost)
(ns-import 'moddocs)

(error-stack-on)

(let* ((result (get-error
   ;; set version number properly in sidebar yaml
  (set-version "_evalable_data/sidebars/mydoc_sidebar.yml" "_data/sidebars/mydoc_sidebar.yml")
  ;; create std lib md file
  (make-md-file "pages/mydoc/mydoc_api.md" :lang)
  ;;  `
  (eval-post "_evalable_pages/mydoc/mydoc_namespaces.md" "pages/mydoc/mydoc_namespaces.md"))))
  (if (= (car result) :ok)
    (cdr result)
    (do
      (println (car result))
      (println (cdr result)))))

