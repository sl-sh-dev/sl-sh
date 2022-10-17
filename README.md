<img src="https://sl-sh-dev.github.io/sl-sh/images/sl-sh-ascii-logo.png" alt="sl-sh logo" style="max-width:100%;">

# Simple Lisp Shell (pronounced slush)

![Rust](https://github.com/sl-sh-dev/sl-sh/workflows/Rust/badge.svg?branch=master)

Simple Lisp SHell (sl-sh) is a lisp based shell written in Rust. It is not POSIX
compliant and makes no effort to be. Sl-sh should run on any *nix platform as
well as macOS (CI currently only tests against ubuntu and macOS).It is a Lisp-1
that is heavily inspired by Clojure and Common Lisp. It is a shell, it is a
scripting language, and it is a REPL.

Some of the more prominent features:

* The [shell reader](https://sl-sh-dev.github.io/sl-sh/mydoc_shellreader.html) supports endfix notation so familiar bash-isms like
    ```bash
    cat file | tr -s " " | cut -d " " -f 2,4
    ```
    "just work"
* Support for an rc file, [slshrc](https://sl-sh-dev.github.io/sl-sh/mydoc_slshrc_config.html), to set up environment and fully customize your prompt.
* Commpon Lisp style macro system with support for quote and backquote (with , and ,@ expansion).
* Rich set of types and cohesive standard library built around them: vectors, lists, iterators, file forms, hash maps, pairs, strings, integers, chars, and booleans.
* OO functionality with lisp style [defstruct](https://sl-sh-dev.github.io/sl-sh/mydoc_api.html#struct::defstruct) and [deftrait](https://sl-sh-dev.github.io/sl-sh/mydoc_api.html#struct::deftrait).
* Dynamically Typed
* Mutable state (it's a shell!) but support for functional idioms is built into the standard lib, e.g. filter, reduce, apply, map, etc.
* Import system with [namespaces](https://sl-sh-dev.github.io/sl-sh/mydoc_namespaces.html) to make writing modular scripts and library code easier.
* Common Lisp style keyword symbols with colon, `:a-keyword`.
* Clojure style [threading macros](https://clojure.org/guides/threading_macros) and scheme style [pipeline operators](https://srfi.schemers.org/srfi-197/srfi-197.html).

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

    cargo build --release
    ./target/release/sl-sh
    ```
    OR
- [Install docker](https://docs.docker.com/get-docker/) and build in a container:
```
docker run --rm --net host --user "$(id -u):$(id -g)" -v "$PWD:/usr/src/sl-sh" -w /usr/src/sl-sh rust:alpine cargo build --release
```

Either method will leave you with a binary target/release/sl-sh that will run the shell. The above docker command will produce a completely static binary while compiling with rust will be linked to you system's libc. You can use the musl target with cargo to produce a static binary with an installed rust.

sl-sh will load with the default slshrc file located in `lisp/slshrc`. To override see [the documentation on slshrc](https://sl-sh-dev.github.io/sl-sh/mydoc_slshrc_config.html).


### 3. Use sl-sh as primary shell
- install binary
```
sudo install -D -m 755 target/release/sl-sh /usr/local/bin/
```
- add sl-sh to /etc/shells and change login shell to sl-sh
```
echo /usr/local/bin/sl-sh | sudo tee -a /etc/shells
chsh -s /usr/local/bin/sl-sh
```
## Documentation

An [API](https://sl-sh-dev.github.io/sl-sh/mydoc_api.html) reference, various guides, and more on the [documentation site](https://sl-sh-dev.github.io/sl-sh/)
Be warned! Documentation is a first class citizen in sl-sh. Docstrings for functions
can be viewed directly in the terminal with the [doc](AP://sl-sh-dev.github.io/sl-sh/mydoc_api.html#root::doc)
form (e.g. `doc reduce`, `doc car` or `doc getopts`) and of the 357 sl-sh forms in existence at the time of this writing, 98.04% of them
include Example sections with actual test cases that are run as part of CI. It
is even possible to run a custom documentation site locally for all forms in
all namespaces loaded at startup in the user's slshrc, see [this](https://sl-sh-dev.github.io/sl-sh/mydoc_documentation.html) FAQ page.

## Status

Sl-sh is still in a beta stage and an official 1.0 release is targeted for 2023.
The current tree based interpreter is being re  implemented as a register-based
virtual machine called slosh. The sl_sh_proc_macro library is being written to
smooth the transition between the current implementation and slosh by allowing
the standard library to be implemented using native rust functions. The macro
takes care of converting the lisp types to rust types and back again for the
return value(s) at runtime. While the project is in beta, the API is not
stable (although it is fairly stable) and documentation on certain features is
lacking (but improving!). Finding examples on how to do things can best be
achieved by searching in the contrib directory and consulting the
[documentation](https://sl-sh-dev.github.io/sl-sh/).

