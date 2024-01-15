# slosh container/complex data types

## String
A string.  Strings in slosh are UTF8 encoded and are composed of chars that are
UTF grapheme clusters.  Because of the encoding strings can be indexed but they
must be traversed to find an index (i.e. they are indexed by 'chars' not bytes).

Read only string constants can be created with double quotes in the Reader.
For instance: (def str-const "Some String Const")

A mutable string can be created with the 'str' form, for instance:
(def string (str "Some val: " val))

Characters at index can be accessed with dot notation (see Vector).

Other string functions:
- str: concats all the values provided (as strings) to produce a new string (mutable)
- str-replace
- str-trim
- str-trim!
- str-contains
- str-push!
- str-map
- str-empty?
- str-starts-with
- str-split
- char-whitespace?

## Vector
A vector (dynamic array) of values.  Typically a vector will be created with the
'[' reader macro (expands to (vec ...)) for instance:
(def vector[1 2 3])
Vectors can be indexed with 'dot' notation (dot is a reader macro that expends
to a (get x index) for x.index.  For example:
(let (v [1 2 3])
  (prn v.0 ", " v.1 ", " v.2))
Dot notation can also be used with set! to set elements:
(let (v [1 2 3])
  (set! v.0 10)
  (set! v.1 20)
  (set! v.2 30)
  (prn v.0 ", " v.1 ", " v.2))

Other vector functions:
- vec: longform for '[]' syntax, prefer using brackets
- make-vec: takes a capacity and default value, makes a vector of that size with all values set to default
- vec-push!: descructive, pushes a new value to the end of a vec (increases len by one)
- vec-pop!: descructive, pops the last value from a vector and returns it (decreses vecotr len by one)
- vec-slice: takes a vecotr, start inde and option end index (defaults to end of vector), returns a new vec of eleemnts start (inclusive) end (exclusive)


## HashMap
A map of key/value pairs.  Use the '{' reader macro to create hashmap:
(def hm {:x 1, :y 2, :z 3})
Use dot notation (see vectors) to access and set keys:
(let (hm {:x 1, :y 2, :z 3})
  (set! hm.:x 10)
  (set! hm.:y 20)
  (set! hm.:z 30)
  (prn hm.:x ", " hm.:y ", " hm.:z))

Other hashmap function:
- make-hash: longfor for '{}' reader macro, prefer '{}'


## Pair/ConsCell/List
This is a traditional Lisp conscell data type, pair of vaues (car, cdr).  It can
used to create a linked list of values.

Other Pair function:
- car
- cdr
- list
- list-append
- cons
- xar!
- xdr!

## Common functions
These should all work on any of the containers.
- len: return the length of the container
- clear!: destructive form to remove all elements from the container
- set!: with dot notation to set an element of a container (see Vector and HashMap)
- dot notation: this is a reader macro the expends to (get val index) for val.index

