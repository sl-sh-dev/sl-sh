Hashmaps as a collection benefit from the same syntactic sugar provided by the
reader macro with the added support of some generic collection functions, `get`,
`set!`, and `clear!`.

For retrieving items from a hashmap prefer the reader syntax (<object>.<keyword>) or use the `get`
function.
```slosh
(def m {:one "one", :two "two", "three" 3})
(assert-equal "one" m.:one)
(assert-equal "two" m.:two)
(assert-equal 3 (get m "three"))
```
For setting key/value pairs on an existing hashmap prefer the reader syntax
with the `set!` function or pass it the output of `get`.
```slosh
(def m {:one "one", :two "two", "three" 3, :four 4})
(set! m.:one "changed1")
(assert-equal "changed1" m.:one)
(set! m.:two "changed2")
(assert-equal "changed2" m.:two)
(set! (get m "three") "changed3")
(assert-equal "changed3" (get m "three"))

(def four :four)
(set! m.~four "changed4")
(assert-equal "changed4" (get m :four))
```



Clearing all keys is done with the `clear!` function.
```slosh
(def m {:one "one", :two "two", "three" 3})
(clear! m)
(assert-equal (make-hash) m)
```
** Hashmap keys that are not keyword types (not prefixed by a colon) can not be
used with the reader syntax, which is why in the example the key "three" was
used with the `get` function each time instead of using the reader syntax.
