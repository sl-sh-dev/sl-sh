---
layout: default
title: namespaces
permalink: sl-sh-namespaces.html
categories: [namespaces]
---
# namespaces usage and formatting library files properly
<hr>

### mkli
- explain mkli!

### writing library sl-sh code with proper namespaces


## An example

add1.lisp
{% comment %} (def directive (make-hash (list (join :type :lib) (join :name "add1.lisp")))) {% endcomment %}
```
(ns-push 'add1)

(ns-import 'shell)

(defn add1 (x)
	(+ x 1))

(ns-auto-export 'add1)
```

add2.lisp
{% comment %} (def directive (make-hash (list (join :type :lib) (join :name "add2.lisp")))) {% endcomment %}
```
(load "./add1.lisp")
(ns-push 'add2)
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
{% comment %} (def directive (make-hash (list (join :type :entrypoint) (join :name "ns-test.lisp")))) {% endcomment %}
```
#!/usr/bin/env sl-sh

(load "./add2.lisp")
(ns-push 'ns-test)
(ns-import 'shell)
(ns-import 'add2)

(println (add2 8))

(ns-pop)
```


result:
```
./ns-test.lisp
{% comment %} (def directive (make-hash (list (join :type :eval) (join :files (list "ns-test.lisp" "add1.lisp" "add2.lisp"))))) {% endcomment %}
```

[<-- back to the docs]( {{ site.url }} )

