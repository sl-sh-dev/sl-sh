# sl-compiler

Simple Lisp Compiler for slvm

This compiles Lisp to slvm bytecode.  It is intended to eventually
become part of sl-sh (simple lisp shell).

Contains two projects:
- sl-compiler: the core compiler code
- slosh: a REPL with debugger and extensions that use sl-compiler

## Running
cargo run -p slosh

## Compiler
These are a subset of sl-sh forms and most work exactly the same.  See the
sl-sh docs at:
https://sl-sh-dev.github.io/sl-sh/mydoc_api.html

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
- Lisp reader
- Lisp lists (pair/concell based)
- Vectors
- Tail call optimization
- Continuations (call/cc)
- Lambda/Closures
- Garbage collection (still WIP)
- Lisp back quotes (including nested back quotes)
- Macros

## slosh
Slosh is the prototype language and REPL using sl-compiler and slvm.

### Built-in Forms
These forms (written in Rust but callable from Lisp) are supported.
- pr (print)
- prn (println)
- dasm (disassemble a lambda or closure)
- load (load a lisp file and execute it)

### Features
- Line editor with history
- Debug on error, currently useful for probing VM state only

## Links
- sl-sh shell: https://github.com/sl-sh-dev/sl-sh
- slvm: https://github.com/sstanfield/slvm
