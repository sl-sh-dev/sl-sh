---
title: Introduction
sidebar: mydoc_sidebar
permalink: mydoc_introduction.html
folder: mydoc
---

## Overview

Simple Lisp SHell (sl-sh) is a lisp based shell written in Rust. It is not POSIX
compliant and makes no effort to be. It runs on the Linux and MacOS platforms.
It is a Lisp-1 that is heavily inspired by Clojure and Common Lisp. It is a 
shell, it is a scripting language, and it is a REPL.

Some of the more prominent features of this theme include the following:

* Supports endfix notation so familiar bash-isms like
    ```bash
    cat file | tr -s " " | cut -d " " -f 2,4
    ```
    "just work" and writing said operations when using the shell is unnecessary.
* Commpon Lisp style macro system with support for quote and backquote (with , and ,@ expansion).
* Common Lisp style keyword symbols with colon, `:a-keyword`.
* Dynamically Typed
* OO functionality with lisp style [defstruct](mydoc_api.html#struct::defstruct) and [deftrait](mydoc_api.html#struct::deftrait).
* Mutable state (it's a shell!) but support for functional idioms is built into the standard lib, e.g. filter, reduce, apply, map, etc.
* Support for an rc file to set up environment and fully customize your prompt.
* Clojure style [threading macros](https://clojure.org/guides/threading_macros) and scheme style [pipeline operators](https://srfi.schemers.org/srfi-197/srfi-197.html).
* Rich set of types and cohesive standard library built around them: vectors, lists, iterators, file forms, hash maps, pairs, strings, integers, chars, and booleans.
* Import system with namespaces to make writing modular scripts and/or library code easily.

## Getting started

To get started, see [Getting Started][index].

{% include links.html %}
