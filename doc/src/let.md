# LET Bindings

## Basic form

(let ([name value]*) forms*)

Let will create a new lexical scope and will bind names to local variables with provided values. After the binding it is
an implicit do form. The bound symbols will only be in scope within this implicit do. Afterwards they will be unbound
or will revert to their previous shadowed value.

Note all bindings are required to be pairs (name value), name is a symbol and value can be any form (it will be
evaluated).

Let will shadow any variable names from outer scopes (including globals), it does not interact with dynamic scopes at
all.
Let is similar to let-rec in scheme, the bindings are created in order and later binding can see the values of previous
bindings. It will also allow an early reference to "see" a later binding (like let-rec) allowing some recursive forms to
be bound easily. Note that when doing this the later form must NOT be something being shadowed or the existing binding
will be used not the new one in the let.

### Examples

```slosh
(let (a 1, b 2, c 3) `(~a ~b ~c))
```

Produces (1 2 3)

```slosh
(let (a 1, b 2, c 3) (let (b 20, c (+ b 10)) `(~a ~b ~c)))
```

Produces (1 20 30)

```slosh
(let (a 1, b 2, c 3) (let (x (+ b 1), b 20, c (+ b 10)) `(~a ~x ~b ~c)))
```

Produces (1 3 20 30)

```slosh
(let (fnx (fn (x) (if (= x 0) #t (fny (- x 1))))
      fny (fn (y) (if (= y 0) #t (fnx (- y 1)))))
    (fnx 10))
```

Example of recursive references (a dumb one). It will produce #t (true) after ping ponging between fnx and fny.

```slosh
(let (fny (fn (y) y))
    (let (fnx (fn (x) (if (= x 0) #t (fny (- x 1))))
          fny (fn (y) (if (= y 0) #t (fnx (- y 1)))))
        (fny 10)))
```

This example will produce 8 because fnx will use the outer fny instead of the next fny.

## Destructure bindings

Let supports destructure bindings of sequences (list, vector) and hashmaps. It can support optional bindings as well
as rest (&) for sequences.

For sequences use [name+], if & is before the last name then it will get all the leftover values. A % indicates that all
the names after are optional (default to nil) and you can use := to set the default value. Patterns must match exactly,
for instance [a b c] requires a sequence with exactly three elements.  [a b c & rest] requires a sequence with at least
three elements.  [% a b c & rest] will take any sequence and bind the first, second and third values to a b and c if
available.

For maps use {[name key]+} (i.e. a map of symbols to keys), if this map contains :or then it's value will be a map of
key to default value used for any missing keys. It requires all keys to be included in the destructured map or to have a
default.

Note that destructures can be applied recursively and sequence destructure can contain map destructures and vice versa.

### Examples

```slosh
(def x '(1 2 3))
(let ([a b c] x) `(~a ~b ~c))
```

Produces (1 2 3).

```slosh
(let ([a b % c d] '(1 2)) (list a b c d))
```

Produces (1 2 nil nil).

```slosh
(let ({a :one, b 'two, c \"three\" [d e] :vec} {:one 1, two 2, \"three\" 3, :vec (4 5)}) (list a b c d e))
```

Produces (1 2 3 4 5).
