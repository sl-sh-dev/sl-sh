# Namespaces

## Description

Namespaces are compiler bookkeeping for organizing global symbols.  When in a namespace then any symbols that are defined will have the current NAMESPACE:: prepended to the symbol.  When symbols are resolved the compiler will also try to prepend the current namespace first in order to find the symbol.

## Entering a namespace

From code use the 'with-ns' form.
```slosh
(doc 'with-ns)
```

From the top-level REPL you can use 'ns'.
```slosh
(doc 'ns)
```

This is an open-ended namespace change intended for the repl, prefer with-ns for a scoped namespace in code.

## Imports

Other namespaces can be imported to allow it's symbols to be accessed in a shorter form.  Use the 'import' form for this .
```slosh
(doc 'import)
```
For instance using ```(import iter)``` will allow any symbols in the iter namespace to be used without prepending 'iter::'.  You can also use the ```(import iter :as i)```, the :as form allows the namespace to be given a different name.  In this case the iter namespace could be replaced with 'i', (i::for ...) instead of (iter::for ...) for example.  Imports are resolved in the order they are compiled in case of conflict (i.e. the first import that resolves a symbol wins).  Imports are attached to the current namespace, changing namespaces will clear imports (note that 'with-ns' saves and restores the previous namespace with imports).

## Loading code

To load new code into your environment use load or run-script.

### Load

The load form should generally be preferred.  It will compile the code at compile time (vs runtime) and execute it at runtime.  This means:
- The path parameter has to be known at compile time: a string const, defined global or form that does not need local inputs.
- Any symbols defined in the loaded code will be known to the compiler at compile time and available for use.

```slosh
(doc 'load)
```

### Run Script
The run-script form loads each form in the file, compiles and executes it at runtime.  This means:
- It can take any parameter since it is resolved at runtime.
- Globals it defines will NOT be known until after it runs at runtime.

```slosh
(doc 'run-script)
```
