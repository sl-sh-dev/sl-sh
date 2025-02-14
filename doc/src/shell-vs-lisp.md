# Using shell commands in lisp

Slosh can easily be used to launch processes in lisp mode (e.g. in a script).
Interacting with the input and output of those programs can be done in a variety
of ways.


## Two main flavors of starting shell processes

1. *The $sh variety* which is a little clunky as you directly pass it a string, as seen in the let binding.
2. *The $ reader macro* which treats everything as a string but allows escaping slosh variables
directly in the shell with macro-like syntax `~`
```
(defn parse-git-branch () (let (branch ($sh "git rev-parse --abbrev-ref HEAD 2>/dev/null"))
    (if (= branch "")
            (str "")
                    (do $(export BRANCH_NAME=~branch)
                                (str "(" branch ")")))))
```
~ under construction ~

instead of (sh "wc" "-l")
we have $(wc -l)
(def v "foo")
$(echo ~foo)
$(echo ~v)


## calling shell commands in lisp code
### 3 ways
 - $() - reader-macro - convenient because it interprets everything as a string - can be used with macro like ~
  - (sh "") - regular function, but you have to pass it a string
  ** $() & (sh) are very similar.

   - ($sh ) - like backticks, call to shell, and get back a slosh string, e.g. run this command and give me the output
      but we do not have a reader macro yet.
           - this is not implemented w/ a reader macro but could be something like $(( )) except that might be ugly
           - ($sh "git rev-parse --abbrev-ref HEAD 2>/dev/null")
                - this returns a string, does more work for you
                - (sh "git rev-parse --abbrev-ref HEAD 2>/dev/null")
                  - Returns the exit status if foreground and the PID if background. Add the '&' to the string to background it.

```
NOTE:
on the CLI typing $() directly on the REPL, but if you wrapped it in say a (do $()) it would work, REPL isn't smart enough yet to
pass the $() to the LISP repl 
```

### bash precedent
- diff <(echo "1\n2\n3") <(echo "echo 1\n\1\n3")
- https://www.gnu.org/software/bash/manual/html_node/Process-Substitution.html#Process-Substitution
