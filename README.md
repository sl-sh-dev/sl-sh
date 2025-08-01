<img src="https://raw.githubusercontent.com/sl-sh-dev/sl-sh/gh-pages-legacy/images/sl-sh-ascii-logo.png" alt="sl-sh logo" style="max-width:100%;">

# Simple Lisp Shell (pronounced slosh)

[![Rust](https://github.com/sl-sh-dev/sl-sh/actions/workflows/rust.yml/badge.svg)](https://github.com/sl-sh-dev/sl-sh/actions/workflows/rust.yml)

## Note this is a new experimental version, see ./legacy/ for the original version (slush).

Simple Lisp SHell (slosh) is a lisp based shell written in Rust. It is not POSIX
compliant and makes no effort to be. Sl-sh should run on any *nix platform as
well as macOS (CI currently only tests against ubuntu and macOS).It is a Lisp-1
that is heavily inspired by Clojure and Common Lisp. It is a shell, it is a
scripting language, and it is a REPL.

Some of the more prominent features:

* The terminal contains both a shell and a lisp reader so familiar bash-isms like
    ```
    cat file | tr -s " " | cut -d " " -f 2,4
    ```
  "just work", but starting a command with a lisp form (an s-expression) allows:
    ```
    (dotimes 10 (prn "hello world"))
    ```
  and the "boundary" can be easily crossed, the `$()` syntax calls out to the shell from within slosh code.
    ```
    (let ([pid, out] $(ls :>)) (iter::for l in (iter::iter out) (pr l)))
    ```
* Support for an rc/init file, ```~/.config/slosh/init.slosh```, to set up environment and fully customize your prompt.
* Common Lisp style macro system with support for quote and backquote (with clojure style ~ and ~@ expansion).
* Dynamically Typed
* Note lacks many features from legacy sl-sh but catching up (std lib is currently tiny).

Contains these crates:

- slosh: a REPL with debugger and extensions that use compiler, includes shell functionality.
- compiler: the core compiler code
- compile_state: helper crate with state contained by a VM for use with compiler
- vm: this is the bytecode VM that is target of the compiler
- builtins: set of some core builtins
- shell: contains shell specific code, this includes a shell reader (parser), job control etc
- bridge_macros: macros for exported Rust functions as slosh functions
- bridge_types: helper types for code using bridge_macros
- slosh_test: everything in slosh plus some additional functionality and dependencies related to debugging.
- legacy (excluded): original sl-sh version, more complete but slower and worse core shell support

## Running

cargo run -p slosh

## Installation

### 1. Get sl-sh

- [Install git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
    ```
    git clone https://github.com/sl-sh-dev/sl-sh
    cd slsh
    ```

### 2. Build sl-sh

- [Install Rust](https://www.rust-lang.org/tools/install) and build from source:
    ```

    cargo build -p slosh --release
    ./target/release/slosh
    ```

### 3. Use sl-sh as primary shell

- install binary

```
mkdir -p /usr/local/bin
sudo install -m 755 target/release/slosh /usr/local/bin/
```

- add slosh to /etc/shells and change login shell to slosh

```
echo /usr/local/bin/slosh | sudo tee -a /etc/shells
chsh -s /usr/local/bin/slosh
```
### 4. (Optional) Configure slosh
The slosh configuration file lives at ~/.config/slosh/init.slosh. <br>
If you run slosh and the file does not exist, a default one will be created for you. <br>
Review your existing shell configuration files like ~/.bashrc and ~/.bash_profile and manually translate them to slosh syntax and add to your init.slosh file <br>
For example, `export JAVA_HOME="/usr/local/opt/openjdk@11/bin/java"` becomes `(sh "export JAVA_HOME='/usr/local/opt/openjdk@11/bin/java'")`

## Compiler

These are a subset of sl-sh forms and most work exactly the same. See the
sl-sh docs at:
https://sl-sh-dev.github.io/sl-sh/

### Primitive types

- True
- False
- Nil
- String Constant
- Symbol
- Keyword
- Character (chars are grapheme clusters)
- Float (56 bit float)
- Integer (56 bit signed integer)
- Byte

### Heap allocated objects (complex types)

- Pair/ConsCell
- Vector
- HashMap
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
- back-quote (` supports ~ ~@)
- and
- or
- err
- let
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
- Lisp lists (pair/conscell based)
- Vectors
- Tail call optimization
- Continuations (call/cc)
- Lambda/Closures (supports optional and variadic arguments)
- Garbage collection (basic but should function)
- Lisp back quotes (including nested back quotes)
- Macros

## slosh

Slosh is the shell and scripting language REPL using the compiler, vm and shell crates.

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

- sl-sh legacy shell: https://github.com/sl-sh-dev/sl-sh/legacy

### Benchmarking

- Install [bencher](https://bencher.dev/docs/tutorial/quick-start/)
- To run benchmarks locally: `cargo bench`
- To upload benchmarks to bencher.dev: `bencher run "cargo bench"`
- Consider using iai in cloud: https://bencher.dev/learn/benchmarking/rust/iai/

### Generated documentation

The [documentation site](https://sl-sh-dev.github.io/sl-sh/) is served from the `doc` directory
based on the `doc/mk-site.sh` script referenced in the github action `.github/workflows/gh_pages.yml`.

Documentation can be built and viewed locally as the documentation site is a static set of html files
generated from the source code.

All of this is covered in `doc/README.md`

Must be compiled in lisp-test mode for appropriate documentation functions to be present (`cargo build --features lisp-test`).

# Dev Notes
## Development Tools
### Just and the Justfile
[Just](https://github.com/casey/just) is a command runner much like make, but with a simpler syntax.

Optionally, you can [install Just](https://github.com/casey/just?tab=readme-ov-file#packages) and use it to help easily test or build slosh.

You can also read the Justfile in the root directory to see the common commands that we use to test and build the project.

<br>

To verify you've installed Just, run `just` in the project root which should list all available commands.

Now you can run `just test` instead of the verbose `sh scripts/check-clippy-version.sh && cargo fmt && cargo clippy && cargo test --workspace`.

It's also possible to [set up shell completion scripts](https://github.com/casey/just?tab=readme-ov-file#shell-completion-scripts) which makes it quite quick to find the right command.
