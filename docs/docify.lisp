#!/usr/bin/env sl-sh

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
  (set-version "_evalable_data/sidebars/mydoc_sidebar.yml" "_data/sidebars/mydoc_sidebar.yml")
  (make-md-file "pages/mydoc/mydoc_api.md" :lang)
  (eval-post "_evalable_posts/evalable-2020-05-10-namespaces.md" "_posts/2020-05-10-namespaces.md"))))
  (if (= (car result) :ok)
    (cdr result)
    (do
      (println (car result))
      (println (cdr result)))))

