# Slosh/Lisp syntax and macros

The core unit of a Lisp is a form:
```sloshignoreignore
42
```
```sloshignoreignore
+
```
```sloshignoreignore
(list 1 2)
```
```sloshignoreignore
;; The quote symbol means return the literal form after the character
;; This is identical to the form above.
'(1 2)
```
are forms. All
lisp code is structured as a sequence of forms inside forms separated by whitespace.

Expressions are enclosed in parentheses and Lisp uses prefix notation to parse expressions.
A parenthesized expression is a list of forms: `(list 1 2 3 4)` where the first
form, the head, should generally be a function or a macro and dictates what the expression will do,
and the subsequent children are arguments to the head.

## Comments

Single-line comments start with a `;` character and continue till the end of the line
```sloshignoreignore
; (prn "hello")
(prn "world")
```
Multi-line comments start with the `#|` characters to start and ends with the sample symbols reversed `|#`.
```sloshignoreignore
#|
(prn "oh")
(prn "hello")
|#
(prn "world")
```

Single forms can be commented out with `#;` characters. This directive tells the reader to discard the form.
```sloshignoreignore
#;(prn "oh")
#;(prn "hello")
(prn "world")
```

## Documentation
Any symbol that is used with `def` or `defn` can also bind documentation. Documentation
is multiline and is delimited with `#%` characters to start and ends with the same symbols reversed `%#`.
```sloshignoreignore
#%
Usage: (login SECRET)

Returns true if logged in false if not. Must use `SECRET`

Section: secrets

Example:
(assert-false (login "foo"))
%#
(defn login (secret)
    (if (= secret "helloworld")
        #t
        #f))


#%
Usage: (login SECRET)

Returns documentation for given symbol as map. Keyword is a documentation fragment
(usage, section, description, example) and value is text describing given fragment.

Section: secrets

Example:

(assert-false (login SECRET))
%#
(def SECRET "helloworld")
(prn (login SECRET))
```

## Macros

Helper macros syntax exists like in other lips such as:
quote (`'`),
quasiquote (``` ` ```),
unquote (```~```),
unquote-splice (```~@```),
macro, and
defmacro.


```
(defmacro dotimes
    ;; This macro accepts times, some number, as well as some number of forms to be evaluated.
    (times & body)

    ;; To avoid creating a variable name in the existing scope (gensym helps
    ;; with the creation of hygenic macros in slosh) is used and
    ;; the symbol i-name is assigned some random name.
    (let (i-name (gensym))

    ;; The quasiquote is used within the body of the macro to specify that all forms
    ;; inside the parentheses should have an implicit quote in front of them. This
    ;; way, by default, the macro will return an ast with the literal forms, as opposed to the
    ;; evaulauted forms; forms can be evaluated in the resultant ast with the unquote.

    `(let (~i-name 0)

        ;; i-iname and times are unquoted so they are evaluated ("escaping the
        ;; quasiquote") in the resultant ast returning numerical values instead
        ;; of symbols that evaluate to themselves.
        (while (< ~i-name ~times)

            ;; Since body is a list, it utilizes the unquote-splice operator, to
            ;; expand each of its elements into the resultant ast, in the case
            ;; of dotimes `body` in intended to be some number of forms that
            ;; should be executed on each invocation.
            ~@body

            ;; because i-name is a symbol defined in the outer scope use unquote
            ;; so the resultant ast outputs `(inc! random-var-nmae)` rather than
            ;; (inc! i-name). `i-name` is not a symbol in scope in the resultant ast,
            ;; ~i-name evaluates to a symbol that is in scope in the resultant
            ;; ast.
            (inc! ~i-name)))))
```


NOTE: 
The quasiquote is a quality of life macro helper, that obviates the need to
add a single quote to every list and every form in the body of your macro. It
is particularly useful in macros because often times many of the symbols you
want returned in a macro are the literal symbols you want in the output ast.
