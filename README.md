# slvm

Simple Lisp Compiler And Virtual Machine

This compiles Lisp to slvm bytecode.  It is intended to eventually
become part of sl-sh (simple lisp shell).

Contains three projects:
- vm: This is the bytecode VM that is target of the compiler.
- compiler: the core compiler code
- slosh: a REPL with debugger and extensions that use compiler

## Running
cargo run -p slosh

## Compiler
These are a subset of sl-sh forms and most work exactly the same.  See the
sl-sh docs at:
https://sl-sh-dev.github.io/sl-sh/mydoc_api.html

### Primitive types
- True
- False
- Nil
- String Constant
- Symbol
- Character (chars are grapheme clusters)
- Float
- Integer
- Byte

### Heap allocated objects (complex types)
- Pair
- Vector
- String

### Special Forms
The following special forms are currently in the compiler:
- def
- set!
- do
- fn
- macro
- if
- quote (')
- back-quote (` supports , ,@)
- and
- or
- err
- let
- let*
- call/cc
- defer
- on-error
- while

### Compiled Forms
Normal forms follow normal calling evaluation.
Note: These are all compiled to bytecode and once compiled are not dynamic anymore.
- not
- recur
- this-fn
- type
- \+
- \-
- \*
- /
- inc!
- dec!
- list
- list-append
- cons
- car
- cdr
- xar!
- xdr!
- vec
- make-vec
- vec-push!
- vec-pop!
- vec-nth
- vec-set!
- vec-len
- vec-clear!
- str
- =
- /=
- <
- <=
- \>
- \>=
- eq?
- equal?

### Features
- Lisp reader (no reader macros yet)
- Lisp lists (pair/concell based)
- Vectors
- Tail call optimization
- Continuations (call/cc)
- Lambda/Closures (supports optional and variadic arguments)
- Garbage collection (basic but should function)
- Lisp back quotes (including nested back quotes)
- Macros

## slosh
Slosh is the prototype language and REPL using compiler and vm.

### Built-in Forms
These forms (written in Rust but callable from Lisp) are supported.
- pr (print)
- prn (println)
- dasm (disassemble a lambda or closure)
- load (load a lisp file and execute it)
- vec-slice (new vec that is a slice of old vec)
- vec->list (turn a vec to a list)
- get-prop (get a property from an object- either a global variable or a heap object)
- set-prop (set a property on an object- either a global variable or a heap object)
- eval (eval an expression)

### Features
- Line editor with history
- Debug on error, currently useful for probing VM state only

## Links
- sl-sh shell: https://github.com/sl-sh-dev/sl-sh
