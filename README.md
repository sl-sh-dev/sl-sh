# slvm

Simple Lisp Byte Code VM

This is intended to be a byte code virtual machine for the sl-sh shell.
Currently a work in progress.  When done might be useful as a VM for other
small lisp interpreters or other languages.

It takes inspiration from Crafting Interpreters (https://craftinginterpreters.com/).
This is just the VM and is for a lisp so will not include a full language but
may get an assembler for testing purposes.  A prototype lisp compiler is in process.

Note on tests, they are currently sparse.  The intension is to use a compiler to
exercise the bytecode when it is more complete as well well as run the Lisp test
suite from sl-sh.

Features:
- Supports a pair base type (cons cell) as well as basic opcodes for Lisp list operations
- Supports tail call optimization (opcodes for tail calls)
- Supports continuations
- Closures
- Garbage Collection (still WIP)

Links:
- sl-sh shell: https://github.com/sl-sh-dev/sl-sh
- prototype lisp compiler: https://github.com/sstanfield/sl-compiler
