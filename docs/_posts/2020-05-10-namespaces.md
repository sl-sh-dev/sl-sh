---
layout: default
title: namespaces
categories: [stuff,things,youknow]
---
# namespaces usage and formatting library files properly
<hr>

### mkli
mkli is a tool that helps write executable sl-sh files.
It is used in the following three ways:

add1.lisp
{% comment %} (defq do (make-hash (list (join :type :entrypoint) (join :name "mkli1.lisp")))) {% endcomment %}
```
#!/usr/bin/env sl-sh
(core::ns-import 'core)
(ns-import 'shell)

(mkli "./myfile.lisp")
```

result:
```
./ns-test.lisp
{% comment %} (defq do (make-hash (list (join :type :eval) (join :files (list "mkli1.lisp"))))) {% endcomment %}
==> (make-hash ((:ok . #<PID: 244950, EXIT STATUS: 0,  Complete>)))
```


### writing library sl-sh code with proper namespaces


## An example

add1.lisp
{% comment %} (defq do (make-hash (list (join :type :lib) (join :name "add1.lisp")))) {% endcomment %}
```
(if (ns-exists? 'add1) (ns-enter 'add1) (ns-create 'add1))

(core::ns-import 'core)
(ns-import 'shell)

(defn add1 (x)
	(+ x 1))

(ns-auto-export 'add1)
```

add2.lisp
{% comment %} (defq do (make-hash (list (join :type :lib) (join :name "add2.lisp")))) {% endcomment %}
```
(load "./add1.lisp")
(if (ns-exists? 'add2) (ns-enter 'add2) (ns-create 'add2))
(core::ns-import 'core)
(ns-import 'shell)
(ns-import 'add1)

(error-stack-on)

(defn make-adder (to-add iterations)
	(loop (add iter) (to-add 0) (if (> iter (- iterations 1)) add (recur (add1 add) (add1 iter)))))

(defn add2 (x)
	(make-adder x 2))

(ns-auto-export 'add2)
(ns-pop)
```

ns-test.lisp
{% comment %} (defq do (make-hash (list (join :type :entrypoint) (join :name "ns-test.lisp")))) {% endcomment %}
```
#!/bin/sl-sh

(load "./add2.lisp")
(if (ns-exists? 'ns-test) (ns-enter 'ns-test) (ns-create 'ns-test))
(core::ns-import 'core)
(ns-import 'shell)
(ns-import 'add2)

(println (add2 8))

(ns-pop)
```


result:
```
./ns-test.lisp
{% comment %} (defq do (make-hash (list (join :type :eval) (join :files (list "ns-test.lisp" "add1.lisp" "add2.lisp"))))) {% endcomment %}
;; 10
==> (make-hash ((:ok . #<PID: 244954, EXIT STATUS: 0,  Complete>)))
```

[<-- back to the docs]( {{ site.url }} )
