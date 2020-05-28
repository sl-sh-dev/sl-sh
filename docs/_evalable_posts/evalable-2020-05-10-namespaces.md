---
layout: default
title: namespaces
categories: [namespaces]
---
# namespaces usage and formatting library files properly
<hr>

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
(ns-pop)
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
#!/usr/bin/env sl-sh

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
```

## Why?
Scripts exist to get things done and sometimes that means they need to be "quick and dirty."
Sl-sh can play that game. But when exists it can be used. And when it's used it
can be depended on. And if it's is depended on, then it may need to change.
When this happens loading and importing namespaces can be invaluable to create
abstractions and reusable code.

## But is it easy?
Yes! Enter [`mkli`]({{ site.url }}/{{ site.baseurl }}/#shell::mkli)

{% comment %} use comment to bracket this code so it does not appear anywhere in the final evaled post.

print-mkli-docs.lisp
{% comment %} (defq do (make-hash (list (join :type :entrypoint) (join :name "print-mkli-docs.lisp")))) {% endcomment %}
```
#!/usr/bin/env sl-sh

(core::ns-import 'core)
(ns-import 'shell)

(println (doc 'mkli))

```
end block comment to hide impl of sl-sh code that prints mkli docs so I
do not have to copy and paste it below. it just... appears!
{% endcomment %} 

mkli documentation:
```
{% comment %} (defq do (make-hash (list (join :type :eval) (join :files (list "print-mkli-docs.lisp"))))) {% endcomment %}
```


## Could it be... even easier?
Maybe?! The function [`ns-auto-export`]({{ site.url }}/{{ site.baseurl }}/#core::ns-auto-export)
tries to make it easy to export all the symbols created in the current namespace
importable (so as to avoid enumeration), while also avoiding making symbols not
meant to be importable
...
TODO
- show some examples of using ns-auto-exporta do the thing where you write the docs in
like in mkli!

## Other uses for namespaces
When the slshrc file runs on startup it does so implicitly in the `'user` namespace.
So if you were to, in your sl-sh shell, right now, run `ns-symbols-only 'user`
you would see only the symbols you created in your slshrc file (or the [default
slshrc file](https://github.com/sstanfield/slsh/blob/master/slshrc.example)) if
you have not created your own. If you were to call `ns-symbols 'user` instead,
you would see all the symbols created in the slshrc file as well as any symbols
imported into slshrc from other namespaces.

TODO
- This pattern can be overriden. talk about ns-create on the cli to create new environments.

[<-- back to the docs]( {{ site.url }} )


# TODO
- CHANGE_WAY_posts-are-organized-on slsh main documentation page it's redundant
currently... maybe... just list posts.
- docs builds stuff (so, both serve local docs and .githooks) should be
evalable post aware. so all posts should live in NOT _posts and _posts should
be entirely derived/created by those scripts.


