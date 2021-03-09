#!/usr/bin/env sl-sh

;; mk-post.lisp ... which is a little convultues and needs to be at least
;; explicityly more verbose about being a general sl-sh md file evalable thing
;; not just someething built for jekyll. this might require ability to append
;; to existing file to preserver jekyll frontmatter. anyway, we should re-implement
;; ns-auto-export because i can't find it because it's useful. maybe we rename it
;; to like, ns-export-no-dash or something, but if it's documented i like the
;; convention of using the dash.
(ns-push 'nae)
(ns-import 'shell)
(ns-import 'iterator)

(def foo "the fu: ")
(def -ns-not-in-curr (collect (filter (fn (x) (not (= "nae" x))) (ns-list))))
(println "nsnotincurr: " -ns-not-in-curr)
(println "ns-list " (reduce (fn (fst nxt)
                                (reduce (fn (fst x) (hash-set! fst x #t)) fst (ns-symbols nxt))) (make-hash) -ns-not-in-curr))
(println "ns-symbols: " (type (first (ns-symbols (first -ns-not-in-curr)))))

(def -symbols-not-in-curr (collect (reduce (fn (fst nxt)
                                    (append-to! fst (ns-symbols nxt))) (list) -ns-not-in-curr)))

(def -this-syms (collect (filter (fn (x) (not (in? -symbols-not-in-curr x))) (ns-symbols 'nae))))
(defn cd (x) (root::cd x))

(println "ok")
(println "ok: " -this-syms)

(defmacro ns-auto-export (symbol)
  `(ns-export (let* ((ns-not-curr  (filter (fn (x) (not (= (str ,symbol) x))) (ns-list)))
      (symbols-not-curr (reduce (fn (fst nxt)
                                (reduce (fn (fst x) (hash-set! fst x #t)) fst (ns-symbols nxt))) (make-hash) ns-not-curr))
         (curr-syms (filter (fn (x) (nil? (hash-get symbols-not-curr x))) (ns-symbols ,symbol)))
         (public-syms (filter (fn (x)
                                  (chain x (sym->str _) (str-starts-with "-" _) (not _))
                                  (not (str-starts-with "-" (sym->str x)))) curr-syms)))
    (collect public-syms))))

(ns-export '(cd))
(println "lol: " (fast-ns-auto-export 'nae))
(ns-pop)

(println "syms: " (eval (sym (str 'nae "::*ns-exports*"))))
