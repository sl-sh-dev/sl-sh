# vm

Simple Lisp Byte Code VM

This is intended to be a byte code virtual machine for the sl-sh shell.
This is currently a work in progress.  When complete it might be useful as a VM
for other small Lisp interpreters or other languages.

It takes inspiration from the book Crafting Interpreters (https://craftinginterpreters.com/).
This project is just the VM and is for a Lisp so will not include the language but
may add an assembler for testing purposes.  A prototype Lisp compiler is in process.

Note on tests, they are currently sparse.  The intention is to use a compiler to
exercise the bytecode when it is more complete as well as run the Lisp test
suite from sl-sh.

Features:
- Pair base type (cons cell) as well as basic opcodes for Lisp list operations
- Tail call optimization (opcodes for tail calls)
- Continuations
- Closures
- Garbage collection (basic but functional)

Links:
- sl-sh shell: https://github.com/sl-sh-dev/sl-sh
