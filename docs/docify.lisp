#!/usr/bin/env sl-sh

(load "mk-docs.lisp")
(load "mod-docs.lisp")
(ns-import 'iterator)
(ns-import 'shell)
(ns-import 'mkdocs)
(ns-import 'moddocs)

(error-stack-on)

(get-error
  (set-docs-version "_evalable_data/sidebars/mydoc_sidebar.yml" "_data/sidebars/mydoc_sidebar.yml")
  (make-md-file pages/mydoc/mydoc_api.md :lang))

