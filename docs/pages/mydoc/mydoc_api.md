---
title: Sl-sh Standard Library
tags: [documentation]
keywords: forms, sl-sh, examples, api, standard library
last_updated: March 1, 2021
sidebar: mydoc_sidebar
permalink: mydoc_api.html
toc: false
---

# Sl-sh


## Documentation structure for each form



| <b>form name</b> | <b>type</b> (see: [Type forms](#Type forms-contents)) |
| <b>namespace</b> (fully qualified names are of format namespace::symbol) | <b>usage</b> |

```
example code if exists
```

## Table of Contents

### <a id="Threading-macros forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Threading-macros forms](#Threading-macros forms-body)


<a id="Namespace: root::->-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``->``](#Namespace: root::->), <a id="Namespace: root::->>-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``->>``](#Namespace: root::->>), <a id="Namespace: root::chain-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``chain``](#Namespace: root::chain), <a id="Namespace: root::chain-and-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``chain-and``](#Namespace: root::chain-and), <a id="Namespace: root::chain-when-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``chain-when``](#Namespace: root::chain-when)
### <a id="Char forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Char forms](#Char forms-body)


<a id="Namespace: root::char-lower-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char-lower``](#Namespace: root::char-lower), <a id="Namespace: root::char-upper-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char-upper``](#Namespace: root::char-upper), <a id="Namespace: root::char-whitespace?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char-whitespace?``](#Namespace: root::char-whitespace?)
### <a id="Conditional forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Conditional forms](#Conditional forms-body)


<a id="Namespace: root::<-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``<``](#Namespace: root::<), <a id="Namespace: root::<=-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``<=``](#Namespace: root::<=), <a id="Namespace: root::=-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``=``](#Namespace: root::=), <a id="Namespace: root::>-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``>``](#Namespace: root::>), <a id="Namespace: root::>=-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``>=``](#Namespace: root::>=), <a id="Namespace: root::and-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``and``](#Namespace: root::and), <a id="Namespace: root::cond-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cond``](#Namespace: root::cond), <a id="Namespace: root::if-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``if``](#Namespace: root::if), <a id="Namespace: root::match-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``match``](#Namespace: root::match), <a id="Namespace: root::not-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``not``](#Namespace: root::not), <a id="Namespace: root::null-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``null``](#Namespace: root::null), <a id="Namespace: root::or-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``or``](#Namespace: root::or), <a id="Namespace: root::when-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``when``](#Namespace: root::when)
### <a id="Core forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Core forms](#Core forms-body)


<a id="Namespace: root::*collection-src*-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*collection-src*``](#Namespace: root::*collection-src*), <a id="Namespace: root::*core-src*-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*core-src*``](#Namespace: root::*core-src*), <a id="Namespace: root::*endfix-src*-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*endfix-src*``](#Namespace: root::*endfix-src*), <a id="Namespace: root::*getopts-src*-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*getopts-src*``](#Namespace: root::*getopts-src*), <a id="Namespace: root::*iterator-src*-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*iterator-src*``](#Namespace: root::*iterator-src*), <a id="Namespace: root::*lib-src*-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*lib-src*``](#Namespace: root::*lib-src*), <a id="Namespace: root::*seq-src*-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*seq-src*``](#Namespace: root::*seq-src*), <a id="Namespace: root::*shell-src*-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*shell-src*``](#Namespace: root::*shell-src*), <a id="Namespace: root::*slsh-std-src*-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*slsh-std-src*``](#Namespace: root::*slsh-std-src*), <a id="Namespace: root::*slshrc-src*-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*slshrc-src*``](#Namespace: root::*slshrc-src*), <a id="Namespace: root::*struct-src*-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*struct-src*``](#Namespace: root::*struct-src*), <a id="Namespace: root::*test-src*-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*test-src*``](#Namespace: root::*test-src*), <a id="Namespace: root::and-let*-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``and-let*``](#Namespace: root::and-let*), <a id="Namespace: root::apply-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``apply``](#Namespace: root::apply), <a id="Namespace: root::back-quote-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``back-quote``](#Namespace: root::back-quote), <a id="Namespace: root::block-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``block``](#Namespace: root::block), <a id="Namespace: root::def-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``def``](#Namespace: root::def), <a id="Namespace: root::def?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``def?``](#Namespace: root::def?), <a id="Namespace: root::defmacro-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``defmacro``](#Namespace: root::defmacro), <a id="Namespace: root::defn-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``defn``](#Namespace: root::defn), <a id="Namespace: root::do-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``do``](#Namespace: root::do), <a id="Namespace: root::doc-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``doc``](#Namespace: root::doc), <a id="Namespace: root::doc-raw-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``doc-raw``](#Namespace: root::doc-raw), <a id="Namespace: root::dotimes-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``dotimes``](#Namespace: root::dotimes), <a id="Namespace: root::dotimes-i-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``dotimes-i``](#Namespace: root::dotimes-i), <a id="Namespace: root::dyn-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``dyn``](#Namespace: root::dyn), <a id="Namespace: root::eprint-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``eprint``](#Namespace: root::eprint), <a id="Namespace: root::eprintln-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``eprintln``](#Namespace: root::eprintln), <a id="Namespace: root::err-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``err``](#Namespace: root::err), <a id="Namespace: root::error-stack-off-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``error-stack-off``](#Namespace: root::error-stack-off), <a id="Namespace: root::error-stack-on-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``error-stack-on``](#Namespace: root::error-stack-on), <a id="Namespace: root::eval-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``eval``](#Namespace: root::eval), <a id="Namespace: root::expand-macro-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``expand-macro``](#Namespace: root::expand-macro), <a id="Namespace: root::expand-macro-all-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``expand-macro-all``](#Namespace: root::expand-macro-all), <a id="Namespace: root::expand-macro1-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``expand-macro1``](#Namespace: root::expand-macro1), <a id="Namespace: root::fn-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fn``](#Namespace: root::fn), <a id="Namespace: root::format-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``format``](#Namespace: root::format), <a id="Namespace: root::gensym-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``gensym``](#Namespace: root::gensym), <a id="Namespace: root::get-error-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``get-error``](#Namespace: root::get-error), <a id="Namespace: root::identity-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``identity``](#Namespace: root::identity), <a id="Namespace: root::intern-stats-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``intern-stats``](#Namespace: root::intern-stats), <a id="Namespace: root::length-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``length``](#Namespace: root::length), <a id="Namespace: root::let-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``let``](#Namespace: root::let), <a id="Namespace: root::let*-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``let*``](#Namespace: root::let*), <a id="Namespace: root::lex-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``lex``](#Namespace: root::lex), <a id="Namespace: root::loop-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``loop``](#Namespace: root::loop), <a id="Namespace: root::macro-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``macro``](#Namespace: root::macro), <a id="Namespace: root::meta-add-tags-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``meta-add-tags``](#Namespace: root::meta-add-tags), <a id="Namespace: root::meta-column-no-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``meta-column-no``](#Namespace: root::meta-column-no), <a id="Namespace: root::meta-file-name-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``meta-file-name``](#Namespace: root::meta-file-name), <a id="Namespace: root::meta-line-no-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``meta-line-no``](#Namespace: root::meta-line-no), <a id="Namespace: root::meta-tag?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``meta-tag?``](#Namespace: root::meta-tag?), <a id="Namespace: root::nsubstitute!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``nsubstitute!``](#Namespace: root::nsubstitute!), <a id="Namespace: root::occurs-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``occurs``](#Namespace: root::occurs), <a id="Namespace: root::print-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``print``](#Namespace: root::print), <a id="Namespace: root::println-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``println``](#Namespace: root::println), <a id="Namespace: root::progn-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``progn``](#Namespace: root::progn), <a id="Namespace: root::quote-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``quote``](#Namespace: root::quote), <a id="Namespace: root::reader-macro-dot-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``reader-macro-dot``](#Namespace: root::reader-macro-dot), <a id="Namespace: root::recur-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``recur``](#Namespace: root::recur), <a id="Namespace: root::ref-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ref``](#Namespace: root::ref), <a id="Namespace: root::return-from-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``return-from``](#Namespace: root::return-from), <a id="Namespace: root::set!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``set!``](#Namespace: root::set!), <a id="Namespace: root::substitute-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``substitute``](#Namespace: root::substitute), <a id="Namespace: root::syscall-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``syscall``](#Namespace: root::syscall), <a id="Namespace: root::undef-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``undef``](#Namespace: root::undef), <a id="Namespace: root::unwind-protect-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``unwind-protect``](#Namespace: root::unwind-protect), <a id="Namespace: root::values-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``values``](#Namespace: root::values), <a id="Namespace: root::values-length-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``values-length``](#Namespace: root::values-length), <a id="Namespace: root::values-nth-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``values-nth``](#Namespace: root::values-nth), <a id="Namespace: root::var-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``var``](#Namespace: root::var), <a id="Namespace: root::varfn-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``varfn``](#Namespace: root::varfn)
### <a id="File forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[File forms](#File forms-body)


<a id="Namespace: root::close-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``close``](#Namespace: root::close), <a id="Namespace: root::flush-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``flush``](#Namespace: root::flush), <a id="Namespace: root::open-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``open``](#Namespace: root::open), <a id="Namespace: root::read-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``read``](#Namespace: root::read), <a id="Namespace: root::read-all-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``read-all``](#Namespace: root::read-all), <a id="Namespace: root::read-line-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``read-line``](#Namespace: root::read-line), <a id="Namespace: root::write-line-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``write-line``](#Namespace: root::write-line), <a id="Namespace: root::write-string-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``write-string``](#Namespace: root::write-string)
### <a id="Hashmap forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Hashmap forms](#Hashmap forms-body)


<a id="Namespace: root::hash-clear!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash-clear!``](#Namespace: root::hash-clear!), <a id="Namespace: root::hash-get-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash-get``](#Namespace: root::hash-get), <a id="Namespace: root::hash-haskey-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash-haskey``](#Namespace: root::hash-haskey), <a id="Namespace: root::hash-keys-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash-keys``](#Namespace: root::hash-keys), <a id="Namespace: root::hash-remove!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash-remove!``](#Namespace: root::hash-remove!), <a id="Namespace: root::hash-set!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash-set!``](#Namespace: root::hash-set!), <a id="Namespace: root::make-hash-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``make-hash``](#Namespace: root::make-hash)
### <a id="iterator forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[iterator forms](#iterator forms-body)


<a id="Namespace: iterator::append-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``append``](#Namespace: iterator::append), <a id="Namespace: iterator::append-iter-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``append-iter``](#Namespace: iterator::append-iter), <a id="Namespace: iterator::append-to!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``append-to!``](#Namespace: iterator::append-to!), <a id="Namespace: iterator::collect-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``collect``](#Namespace: iterator::collect), <a id="Namespace: iterator::collect-str-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``collect-str``](#Namespace: iterator::collect-str), <a id="Namespace: iterator::collect-vec-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``collect-vec``](#Namespace: iterator::collect-vec), <a id="Namespace: iterator::double-ended-iter?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``double-ended-iter?``](#Namespace: iterator::double-ended-iter?), <a id="Namespace: iterator::double-ended-iterator-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``double-ended-iterator``](#Namespace: iterator::double-ended-iterator), <a id="Namespace: iterator::empty?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``empty?``](#Namespace: iterator::empty?), <a id="Namespace: iterator::file-iter-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``file-iter``](#Namespace: iterator::file-iter), <a id="Namespace: iterator::filter-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``filter``](#Namespace: iterator::filter), <a id="Namespace: iterator::filter-iter-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``filter-iter``](#Namespace: iterator::filter-iter), <a id="Namespace: iterator::for-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``for``](#Namespace: iterator::for), <a id="Namespace: iterator::for-i-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``for-i``](#Namespace: iterator::for-i), <a id="Namespace: iterator::iter-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``iter``](#Namespace: iterator::iter), <a id="Namespace: iterator::iter?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``iter?``](#Namespace: iterator::iter?), <a id="Namespace: iterator::iterator-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``iterator``](#Namespace: iterator::iterator), <a id="Namespace: iterator::list-iter-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``list-iter``](#Namespace: iterator::list-iter), <a id="Namespace: iterator::map-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``map``](#Namespace: iterator::map), <a id="Namespace: iterator::map-iter-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``map-iter``](#Namespace: iterator::map-iter), <a id="Namespace: iterator::next!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``next!``](#Namespace: iterator::next!), <a id="Namespace: iterator::nth-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``nth``](#Namespace: iterator::nth), <a id="Namespace: iterator::range-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``range``](#Namespace: iterator::range), <a id="Namespace: iterator::range-iter-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``range-iter``](#Namespace: iterator::range-iter), <a id="Namespace: iterator::reduce-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``reduce``](#Namespace: iterator::reduce), <a id="Namespace: iterator::reduce-times-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``reduce-times``](#Namespace: iterator::reduce-times), <a id="Namespace: iterator::reverse-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``reverse``](#Namespace: iterator::reverse), <a id="Namespace: iterator::reverse-iter-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``reverse-iter``](#Namespace: iterator::reverse-iter), <a id="Namespace: iterator::slice-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``slice``](#Namespace: iterator::slice), <a id="Namespace: iterator::slice-iter-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``slice-iter``](#Namespace: iterator::slice-iter), <a id="Namespace: iterator::string-iter-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``string-iter``](#Namespace: iterator::string-iter), <a id="Namespace: iterator::vec-iter-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-iter``](#Namespace: iterator::vec-iter)
### <a id="Math forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Math forms](#Math forms-body)


<a id="Namespace: root::%-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``%``](#Namespace: root::%), <a id="Namespace: root::*-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*``](#Namespace: root::*), <a id="Namespace: root::+-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``+``](#Namespace: root::+), <a id="Namespace: root::--contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``-``](#Namespace: root::-), <a id="Namespace: root::/-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``/``](#Namespace: root::/)
### <a id="Namespace forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Namespace forms](#Namespace forms-body)


<a id="Namespace: root::ns-auto-export-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-auto-export``](#Namespace: root::ns-auto-export), <a id="Namespace: root::ns-create-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-create``](#Namespace: root::ns-create), <a id="Namespace: root::ns-enter-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-enter``](#Namespace: root::ns-enter), <a id="Namespace: root::ns-exists?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-exists?``](#Namespace: root::ns-exists?), <a id="Namespace: root::ns-export-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-export``](#Namespace: root::ns-export), <a id="Namespace: root::ns-import-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-import``](#Namespace: root::ns-import), <a id="Namespace: root::ns-list-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-list``](#Namespace: root::ns-list), <a id="Namespace: root::ns-pop-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-pop``](#Namespace: root::ns-pop), <a id="Namespace: root::ns-push-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-push``](#Namespace: root::ns-push), <a id="Namespace: root::ns-symbols-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-symbols``](#Namespace: root::ns-symbols)
### <a id="Pair forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Pair forms](#Pair forms-body)


<a id="Namespace: root::car-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``car``](#Namespace: root::car), <a id="Namespace: root::cdr-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cdr``](#Namespace: root::cdr), <a id="Namespace: root::join-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``join``](#Namespace: root::join), <a id="Namespace: root::list-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``list``](#Namespace: root::list), <a id="Namespace: root::xar!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``xar!``](#Namespace: root::xar!), <a id="Namespace: root::xdr!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``xdr!``](#Namespace: root::xdr!)
### <a id="pair-ext forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[pair-ext forms](#pair-ext forms-body)


<a id="Namespace: root::caaar-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``caaar``](#Namespace: root::caaar), <a id="Namespace: root::caadr-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``caadr``](#Namespace: root::caadr), <a id="Namespace: root::caar-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``caar``](#Namespace: root::caar), <a id="Namespace: root::cadar-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cadar``](#Namespace: root::cadar), <a id="Namespace: root::cadddr-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cadddr``](#Namespace: root::cadddr), <a id="Namespace: root::caddr-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``caddr``](#Namespace: root::caddr), <a id="Namespace: root::cadr-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cadr``](#Namespace: root::cadr), <a id="Namespace: root::cdaar-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cdaar``](#Namespace: root::cdaar), <a id="Namespace: root::cdadr-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cdadr``](#Namespace: root::cdadr), <a id="Namespace: root::cdar-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cdar``](#Namespace: root::cdar), <a id="Namespace: root::cddar-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cddar``](#Namespace: root::cddar), <a id="Namespace: root::cdddr-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cdddr``](#Namespace: root::cdddr), <a id="Namespace: root::cddr-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cddr``](#Namespace: root::cddr)
### <a id="Scripting forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Scripting forms](#Scripting forms-body)


<a id="Namespace: root::*load-path*-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*load-path*``](#Namespace: root::*load-path*), <a id="Namespace: root::load-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``load``](#Namespace: root::load), <a id="Namespace: shell::mkli-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``mkli``](#Namespace: shell::mkli)
### <a id="Sequence forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Sequence forms](#Sequence forms-body)


<a id="Namespace: root::butlast-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``butlast``](#Namespace: root::butlast), <a id="Namespace: root::collect-copy-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``collect-copy``](#Namespace: root::collect-copy), <a id="Namespace: root::empty-seq?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``empty-seq?``](#Namespace: root::empty-seq?), <a id="Namespace: root::first-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``first``](#Namespace: root::first), <a id="Namespace: root::in?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``in?``](#Namespace: root::in?), <a id="Namespace: root::last-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``last``](#Namespace: root::last), <a id="Namespace: root::non-empty-seq?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``non-empty-seq?``](#Namespace: root::non-empty-seq?), <a id="Namespace: root::qsort-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``qsort``](#Namespace: root::qsort), <a id="Namespace: root::rest-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``rest``](#Namespace: root::rest), <a id="Namespace: root::seq-for-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``seq-for``](#Namespace: root::seq-for), <a id="Namespace: root::seq?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``seq?``](#Namespace: root::seq?), <a id="Namespace: root::setnth!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``setnth!``](#Namespace: root::setnth!)
### <a id="Shell forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Shell forms](#Shell forms-body)


<a id="Namespace: root::*stderr*-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*stderr*``](#Namespace: root::*stderr*), <a id="Namespace: root::*stdin*-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*stdin*``](#Namespace: root::*stdin*), <a id="Namespace: root::*stdout*-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*stdout*``](#Namespace: root::*stdout*), <a id="Namespace: shell::alias-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``alias``](#Namespace: shell::alias), <a id="Namespace: shell::alias?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``alias?``](#Namespace: shell::alias?), <a id="Namespace: root::bg-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``bg``](#Namespace: root::bg), <a id="Namespace: shell::bg-color-rgb-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``bg-color-rgb``](#Namespace: shell::bg-color-rgb), <a id="Namespace: root::cd-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cd``](#Namespace: root::cd), <a id="Namespace: shell::clear-dirs-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``clear-dirs``](#Namespace: shell::clear-dirs), <a id="Namespace: root::command-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``command``](#Namespace: root::command), <a id="Namespace: shell::dirs-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``dirs``](#Namespace: shell::dirs), <a id="Namespace: shell::endfix-on-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``endfix-on``](#Namespace: shell::endfix-on), <a id="Namespace: shell::err>-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``err>``](#Namespace: shell::err>), <a id="Namespace: shell::err>>-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``err>>``](#Namespace: shell::err>>), <a id="Namespace: shell::err>null-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``err>null``](#Namespace: shell::err>null), <a id="Namespace: root::exit-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``exit``](#Namespace: root::exit), <a id="Namespace: root::export-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``export``](#Namespace: root::export), <a id="Namespace: shell::fc-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fc``](#Namespace: shell::fc), <a id="Namespace: root::fg-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fg``](#Namespace: root::fg), <a id="Namespace: shell::fg-color-rgb-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fg-color-rgb``](#Namespace: shell::fg-color-rgb), <a id="Namespace: root::form-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``form``](#Namespace: root::form), <a id="Namespace: root::fs-dir?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fs-dir?``](#Namespace: root::fs-dir?), <a id="Namespace: root::fs-exists?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fs-exists?``](#Namespace: root::fs-exists?), <a id="Namespace: root::fs-file?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fs-file?``](#Namespace: root::fs-file?), <a id="Namespace: shell::get-dirs-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``get-dirs``](#Namespace: shell::get-dirs), <a id="Namespace: shell::getopts-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``getopts``](#Namespace: shell::getopts), <a id="Namespace: root::glob-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``glob``](#Namespace: root::glob), <a id="Namespace: root::history-context-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``history-context``](#Namespace: root::history-context), <a id="Namespace: root::history-empty?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``history-empty?``](#Namespace: root::history-empty?), <a id="Namespace: root::history-length-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``history-length``](#Namespace: root::history-length), <a id="Namespace: root::history-nth-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``history-nth``](#Namespace: root::history-nth), <a id="Namespace: root::history-push-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``history-push``](#Namespace: root::history-push), <a id="Namespace: root::history-push-throwaway-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``history-push-throwaway``](#Namespace: root::history-push-throwaway), <a id="Namespace: root::jobs-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``jobs``](#Namespace: root::jobs), <a id="Namespace: shell::let-env-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``let-env``](#Namespace: shell::let-env), <a id="Namespace: root::loose-symbols-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``loose-symbols``](#Namespace: root::loose-symbols), <a id="Namespace: shell::out-err>-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``out-err>``](#Namespace: shell::out-err>), <a id="Namespace: shell::out-err>>-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``out-err>>``](#Namespace: shell::out-err>>), <a id="Namespace: shell::out-err>null-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``out-err>null``](#Namespace: shell::out-err>null), <a id="Namespace: shell::out>-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``out>``](#Namespace: shell::out>), <a id="Namespace: shell::out>>-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``out>>``](#Namespace: shell::out>>), <a id="Namespace: shell::out>null-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``out>null``](#Namespace: shell::out>null), <a id="Namespace: root::pid-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``pid``](#Namespace: root::pid), <a id="Namespace: root::pipe-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``pipe``](#Namespace: root::pipe), <a id="Namespace: shell::popd-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``popd``](#Namespace: shell::popd), <a id="Namespace: root::prompt-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``prompt``](#Namespace: root::prompt), <a id="Namespace: shell::pushd-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``pushd``](#Namespace: shell::pushd), <a id="Namespace: root::reap-jobs-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``reap-jobs``](#Namespace: root::reap-jobs), <a id="Namespace: shell::register-alias-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``register-alias``](#Namespace: shell::register-alias), <a id="Namespace: root::run-bg-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``run-bg``](#Namespace: root::run-bg), <a id="Namespace: shell::set-dirs-max-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``set-dirs-max``](#Namespace: shell::set-dirs-max), <a id="Namespace: root::sleep-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``sleep``](#Namespace: root::sleep), <a id="Namespace: shell::syntax-off-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``syntax-off``](#Namespace: shell::syntax-off), <a id="Namespace: shell::syntax-on-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``syntax-on``](#Namespace: shell::syntax-on), <a id="Namespace: shell::sys-command?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``sys-command?``](#Namespace: shell::sys-command?), <a id="Namespace: shell::temp-dir-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``temp-dir``](#Namespace: shell::temp-dir), <a id="Namespace: root::time-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``time``](#Namespace: root::time), <a id="Namespace: root::unexport-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``unexport``](#Namespace: root::unexport), <a id="Namespace: shell::unregister-alias-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``unregister-alias``](#Namespace: shell::unregister-alias), <a id="Namespace: root::version-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``version``](#Namespace: root::version), <a id="Namespace: root::wait-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``wait``](#Namespace: root::wait), <a id="Namespace: shell::|-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``|``](#Namespace: shell::|)
### <a id="String forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[String forms](#String forms-body)


<a id="Namespace: root::str-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str``](#Namespace: root::str), <a id="Namespace: root::str-append-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-append``](#Namespace: root::str-append), <a id="Namespace: root::str-bytes-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-bytes``](#Namespace: root::str-bytes), <a id="Namespace: root::str-cat-list-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-cat-list``](#Namespace: root::str-cat-list), <a id="Namespace: root::str-clear!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-clear!``](#Namespace: root::str-clear!), <a id="Namespace: root::str-contains-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-contains``](#Namespace: root::str-contains), <a id="Namespace: root::str-empty?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-empty?``](#Namespace: root::str-empty?), <a id="Namespace: root::str-ignore-expand-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-ignore-expand``](#Namespace: root::str-ignore-expand), <a id="Namespace: root::str-iter-empty?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-iter-empty?``](#Namespace: root::str-iter-empty?), <a id="Namespace: root::str-iter-next!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-iter-next!``](#Namespace: root::str-iter-next!), <a id="Namespace: root::str-iter-peek-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-iter-peek``](#Namespace: root::str-iter-peek), <a id="Namespace: root::str-iter-start-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-iter-start``](#Namespace: root::str-iter-start), <a id="Namespace: root::str-lower-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-lower``](#Namespace: root::str-lower), <a id="Namespace: root::str-ltrim-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-ltrim``](#Namespace: root::str-ltrim), <a id="Namespace: root::str-map-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-map``](#Namespace: root::str-map), <a id="Namespace: root::str-nth-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-nth``](#Namespace: root::str-nth), <a id="Namespace: root::str-push!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-push!``](#Namespace: root::str-push!), <a id="Namespace: root::str-replace-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-replace``](#Namespace: root::str-replace), <a id="Namespace: root::str-rsplit-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-rsplit``](#Namespace: root::str-rsplit), <a id="Namespace: root::str-rsplitn-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-rsplitn``](#Namespace: root::str-rsplitn), <a id="Namespace: root::str-rtrim-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-rtrim``](#Namespace: root::str-rtrim), <a id="Namespace: root::str-split-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-split``](#Namespace: root::str-split), <a id="Namespace: root::str-splitn-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-splitn``](#Namespace: root::str-splitn), <a id="Namespace: root::str-starts-with-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-starts-with``](#Namespace: root::str-starts-with), <a id="Namespace: root::str-sub-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-sub``](#Namespace: root::str-sub), <a id="Namespace: root::str-trim-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-trim``](#Namespace: root::str-trim), <a id="Namespace: root::str-upper-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-upper``](#Namespace: root::str-upper)
### <a id="struct forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[struct forms](#struct forms-body)


<a id="Namespace: struct::defstruct-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``defstruct``](#Namespace: struct::defstruct), <a id="Namespace: struct::deftrait-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``deftrait``](#Namespace: struct::deftrait)
### <a id="Type forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Type forms](#Type forms-body)


<a id="Namespace: root::builtin?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``builtin?``](#Namespace: root::builtin?), <a id="Namespace: root::char?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char?``](#Namespace: root::char?), <a id="Namespace: root::file?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``file?``](#Namespace: root::file?), <a id="Namespace: root::float->int-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``float->int``](#Namespace: root::float->int), <a id="Namespace: root::float?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``float?``](#Namespace: root::float?), <a id="Namespace: root::func?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``func?``](#Namespace: root::func?), <a id="Namespace: root::hash?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash?``](#Namespace: root::hash?), <a id="Namespace: root::int->float-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``int->float``](#Namespace: root::int->float), <a id="Namespace: root::int?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``int?``](#Namespace: root::int?), <a id="Namespace: root::lambda?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``lambda?``](#Namespace: root::lambda?), <a id="Namespace: root::list?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``list?``](#Namespace: root::list?), <a id="Namespace: root::macro?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``macro?``](#Namespace: root::macro?), <a id="Namespace: root::nil?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``nil?``](#Namespace: root::nil?), <a id="Namespace: root::pair?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``pair?``](#Namespace: root::pair?), <a id="Namespace: root::process?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``process?``](#Namespace: root::process?), <a id="Namespace: root::str->float-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str->float``](#Namespace: root::str->float), <a id="Namespace: root::str->int-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str->int``](#Namespace: root::str->int), <a id="Namespace: root::string?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``string?``](#Namespace: root::string?), <a id="Namespace: root::sym-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``sym``](#Namespace: root::sym), <a id="Namespace: root::sym->str-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``sym->str``](#Namespace: root::sym->str), <a id="Namespace: root::symbol?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``symbol?``](#Namespace: root::symbol?), <a id="Namespace: root::true?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``true?``](#Namespace: root::true?), <a id="Namespace: root::type-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``type``](#Namespace: root::type), <a id="Namespace: root::values?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``values?``](#Namespace: root::values?), <a id="Namespace: root::vec?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec?``](#Namespace: root::vec?)
### <a id="Vector forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Vector forms](#Vector forms-body)


<a id="Namespace: root::make-vec-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``make-vec``](#Namespace: root::make-vec), <a id="Namespace: root::vec-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec``](#Namespace: root::vec), <a id="Namespace: root::vec-clear!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-clear!``](#Namespace: root::vec-clear!), <a id="Namespace: root::vec-empty?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-empty?``](#Namespace: root::vec-empty?), <a id="Namespace: root::vec-insert!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-insert!``](#Namespace: root::vec-insert!), <a id="Namespace: root::vec-nth-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-nth``](#Namespace: root::vec-nth), <a id="Namespace: root::vec-pop!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-pop!``](#Namespace: root::vec-pop!), <a id="Namespace: root::vec-push!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-push!``](#Namespace: root::vec-push!), <a id="Namespace: root::vec-remove!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-remove!``](#Namespace: root::vec-remove!), <a id="Namespace: root::vec-set!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-set!``](#Namespace: root::vec-set!), <a id="Namespace: root::vec-slice-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-slice``](#Namespace: root::vec-slice)

## Documentation

### <a id="Threading-macros forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Threading-macros forms](#Threading-macros forms-contents)



| <a id="Namespace: root::->" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``->``](#Namespace: root::->-contents) | Type: Macro |
| ``Namespace: root::->`` | ``Usage: (-> &rest args)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-equal<br>
(str "I go at the beginning.I'll be stuck in the middle.I'll be at the end.")<br>
(-> "I go at the beginning."<br>
(str "I'll be stuck in the middle.")<br>
(str "I'll be at the end.")))<br>
<br>
</code>
</details>


| <a id="Namespace: root::->>" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``->>``](#Namespace: root::->>-contents) | Type: Macro |
| ``Namespace: root::->>`` | ``Usage: (->> &rest args)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-equal<br>
(str "I'll be at the beginning.I'll be more in the middle.I go at the end.")<br>
(->> "I go at the end."<br>
(str "I'll be more in the middle.")<br>
(str "I'll be at the beginning.")))<br>
<br>
</code>
</details>


| <a id="Namespace: root::chain" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``chain``](#Namespace: root::chain-contents) | Type: Macro |
| ``Namespace: root::chain`` | ``Usage: (chain init arg0 &rest args)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal "Ordered sentence: I will become a properly worded statement."<br>
(chain "a properly worded "<br>
(str "will " "become " _)<br>
(str "I " _ "statement.")<br>
(str "Ordered sentence: " _)))<br>
(test::assert-equal 3 (chain 7 (% _ 4)))<br>
(test::assert-equal 9 (chain 7 (% _ 4) (+ 1 8)))<br>
(test::assert-equal (list 6 12 18) (collect (chain 7<br>
(% _ 4)<br>
(range _ 10)<br>
(map (fn (x) (* x 2)) _)<br>
(filter (fn (x) (= 0 (% x 3))) _))))<br>
<br>
</code>
</details>


| <a id="Namespace: root::chain-and" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``chain-and``](#Namespace: root::chain-and-contents) | Type: Macro |
| ``Namespace: root::chain-and`` | ``Usage: (chain-and init arg0 &rest args)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-false (chain-and "howdy" (string? _) (= _ "howdy")))<br>
(test::assert-true  (chain-and "howdy" (str _ " partner") (= _ "howdy partner")))<br>
(defn formatted? (alleged-time-str)<br>
(let* ((do-fst (fn (tst start end)<br>
(fn (orig-string)<br>
(let ((result (get-error<br>
(chain orig-string<br>
(str-sub start (- end start) _)<br>
(str->int _)<br>
(tst _)))))<br>
(if (= :ok (car result))<br>
(if (cdr result) orig-string nil)<br>
nil)))))<br>
(valid-year? (do-fst (fn (x) (> x -1)) 0 4))<br>
(valid-month? (do-fst (fn (x) (and (> x 0) (< x 13))) 4 6))<br>
(valid-day? (do-fst (fn (x) (and (> x 0) (< x 32))) 6 8))<br>
(valid-hour? (do-fst (fn (x) (and (> x 0) (< x 24))) 9 11))<br>
(valid-minute? (do-fst (fn (x) (and (> x -1) (< x 60))) 11 13))<br>
(valid-second? (do-fst (fn (x) (and (> x -1) (< x 60))) 13 15))<br>
(is-zulu? (fn (orig-string) (= "Z" (str-sub 15 1 orig-string)))))<br>
(chain-and alleged-time-str<br>
(valid-year? _)<br>
(valid-month? _)<br>
(valid-day? _)<br>
(valid-hour? _)<br>
(valid-minute? _)<br>
(valid-second? _)<br>
(is-zulu? _)<br>
#t)))<br>
(test::assert-true (formatted? "20210227T154705Z"))<br>
(test::assert-false (formatted? "20210227T154705P"))<br>
(test::assert-false (formatted? "2021022TT154705Z"))<br>
(test::assert-false (formatted? "20210237T154705Z"))<br>
(test::assert-false (formatted? "2021222TT154705Z"))<br>
(test::assert-false (formatted? "20210227T254705Z"))<br>
(test::assert-false (formatted? "20210227T158705Z"))<br>
(test::assert-false (formatted? "20210227T154795Z"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::chain-when" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``chain-when``](#Namespace: root::chain-when-contents) | Type: Macro |
| ``Namespace: root::chain-when`` | ``Usage: (chain-when init arg0 &rest args)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(defn add-number-attributes (n)<br>
(chain-when (make-hash)<br>
((not (= (% n 2) 0)) (hash-set! _ :property :odd))<br>
((= (% n 2) 0) (hash-set! _ :property :even))<br>
((= 0 n) (hash-set! _ :zero :zero))<br>
((= 42 n) (err "N CAN NOT BE 42"))<br>
((> n 0) (hash-set! _ :symmetry :positive))<br>
((< n 0) (hash-set! _ :symmetry :negative))))<br>
(test::assert-equal :odd (hash-get (add-number-attributes 3) :property))<br>
(test::assert-equal :even (hash-get (add-number-attributes 4) :property))<br>
(test::assert-false (hash-get (add-number-attributes 4) :zero))<br>
(test::assert-equal :positive (hash-get (add-number-attributes 4) :symmetry))<br>
(test::assert-equal :negative (hash-get (add-number-attributes -4) :symmetry))<br>
(test::assert-equal :zero (hash-get (add-number-attributes 0) :zero))<br>
(test::assert-error-msg (add-number-attributes 42) "N CAN NOT BE 42")<br>
<br>
</code>
</details>
### <a id="Char forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Char forms](#Char forms-contents)



| <a id="Namespace: root::char-lower" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char-lower``](#Namespace: root::char-lower-contents) | Type: Function |
| ``Namespace: root::char-lower`` | ``Usage: (char-lower char) -> char`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal #\a (char-lower #\A))<br>
(test::assert-equal #\a (char-lower #\a))<br>
(test::assert-not-equal #\a (char-lower #\Z))<br>
(test::assert-equal #\λ (char-lower #\Λ))<br>
(test::assert-equal #\λ (char-lower #\λ))<br>
<br>
</code>
</details>


| <a id="Namespace: root::char-upper" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char-upper``](#Namespace: root::char-upper-contents) | Type: Function |
| ``Namespace: root::char-upper`` | ``Usage: (char-upper char) -> char`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal #\A (char-upper #\A))<br>
(test::assert-equal #\A (char-upper #\a))<br>
(test::assert-not-equal #\A (char-upper #\Z))<br>
(test::assert-equal #\Λ (char-upper #\λ))<br>
(test::assert-equal #\Λ (char-upper #\Λ))<br>
<br>
</code>
</details>


| <a id="Namespace: root::char-whitespace?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char-whitespace?``](#Namespace: root::char-whitespace?-contents) | Type: Function |
| ``Namespace: root::char-whitespace?`` | ``Usage: (char-whitespace? char) -> t/nil`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (char-whitespace? #\ ))<br>
(test::assert-true (char-whitespace? #\tab))<br>
(test::assert-false (char-whitespace? #\s))<br>
<br>
</code>
</details>
### <a id="Conditional forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Conditional forms](#Conditional forms-contents)



| <a id="Namespace: root::<" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``<``](#Namespace: root::<-contents) | Type: Function |
| ``Namespace: root::<`` | ``Usage: (< val0 ... valN)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (< 1 2))<br>
(test::assert-true (< 1 2 3 4))<br>
(test::assert-false (< 2 2))<br>
(test::assert-false (< 2 2 2))<br>
(test::assert-false (< 2 2 3))<br>
(test::assert-true (< 1.0 2.0))<br>
(test::assert-false (< 2.0 2.0))<br>
(test::assert-false (< 2.0 2.0 2.0))<br>
(test::assert-false (< 2.0 2.0 3.0))<br>
(test::assert-false (< 2.1 2.0 3.0))<br>
(test::assert-false (< 2 1))<br>
(test::assert-false (< 3 2 3))<br>
(test::assert-true (< "aaa" "aab"))<br>
(test::assert-false (< "aaa" "aaa"))<br>
(test::assert-true (< "aaa" "aab" "ccc"))<br>
(test::assert-false (< "baa" "aab"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::<=" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``<=``](#Namespace: root::<=-contents) | Type: Function |
| ``Namespace: root::<=`` | ``Usage: (<= val0 ... valN)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (<= 1 2))<br>
(test::assert-true (<= 2 2))<br>
(test::assert-true (<= 2 2 2))<br>
(test::assert-true (<= 2 2 3))<br>
(test::assert-true (<= 1.0 2.0))<br>
(test::assert-true (<= 2.0 2.0))<br>
(test::assert-true (<= 2.0 2.0 2.0))<br>
(test::assert-true (<= 2.0 2.0 3.0))<br>
(test::assert-false (<= 2.1 2.0 3.0))<br>
(test::assert-false (<= 2 1))<br>
(test::assert-false (<= 3 2 3))<br>
(test::assert-true (<= "aaa" "aab"))<br>
(test::assert-true (<= "aaa" "aaa"))<br>
(test::assert-true (<= "aaa" "aab" "ccc"))<br>
(test::assert-false (<= "baa" "aab"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::=" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``=``](#Namespace: root::=-contents) | Type: Function |
| ``Namespace: root::=`` | ``Usage: (= val0 ... valN)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-false (= 1 2))<br>
(test::assert-true (= 2 2))<br>
(test::assert-true (= 2 2 2))<br>
(test::assert-false (= 3 2 2))<br>
(test::assert-false (= 3.0 2.0))<br>
(test::assert-true (= 2.0 2.0))<br>
(test::assert-true (= 2.0 2.0 2.0))<br>
(test::assert-false (= 3.0 2.0 2.0))<br>
(test::assert-false (= 2.1 2.0 3.0))<br>
(test::assert-false (= 2 1))<br>
(test::assert-false (= 3 2 1))<br>
(test::assert-false (= 1.1 1.0))<br>
(test::assert-true (= 1.1 1.1))<br>
(test::assert-false (= 3 2 3))<br>
(test::assert-false (= "aab" "aaa"))<br>
(test::assert-true (= "aaa" "aaa"))<br>
(test::assert-true (= "aaa" "aaa" "aaa"))<br>
(test::assert-false (= "aaa" "aaaa" "aaa"))<br>
(test::assert-false (= "ccc" "aab" "aaa"))<br>
(test::assert-false (= "aaa" "aab"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::>" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``>``](#Namespace: root::>-contents) | Type: Function |
| ``Namespace: root::>`` | ``Usage: (> val0 ... valN)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-false (> 1 2))<br>
(test::assert-false (> 2 2))<br>
(test::assert-false (> 2 2 2))<br>
(test::assert-false (> 3 2 2))<br>
(test::assert-true (> 3.0 2.0))<br>
(test::assert-false (> 2.0 2.0))<br>
(test::assert-false (> 2.0 2.0 2.0))<br>
(test::assert-false (> 3.0 2.0 2.0))<br>
(test::assert-false (> 2.1 2.0 3.0))<br>
(test::assert-true (> 2 1))<br>
(test::assert-true (> 3 2 1))<br>
(test::assert-true (> 1.1 1.0))<br>
(test::assert-false (> 3 2 3))<br>
(test::assert-true (> "aab" "aaa"))<br>
(test::assert-false (> "aaa" "aaa"))<br>
(test::assert-true (> "ccc" "aab" "aaa"))<br>
(test::assert-false (> "aaa" "aab"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::>=" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``>=``](#Namespace: root::>=-contents) | Type: Function |
| ``Namespace: root::>=`` | ``Usage: (>= val0 ... valN)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-false (>= 1 2))<br>
(test::assert-true (>= 2 2))<br>
(test::assert-true (>= 2 2 2))<br>
(test::assert-true (>= 3 2 2))<br>
(test::assert-true (>= 3.0 2.0))<br>
(test::assert-true (>= 2.0 2.0))<br>
(test::assert-true (>= 2.0 2.0 2.0))<br>
(test::assert-true (>= 3.0 2.0 2.0))<br>
(test::assert-false (>= 2.1 2.0 3.0))<br>
(test::assert-true (>= 2 1))<br>
(test::assert-true (>= 1.1 1.0))<br>
(test::assert-false (>= 3 2 3))<br>
(test::assert-true (>= "aab" "aaa"))<br>
(test::assert-true (>= "aaa" "aaa"))<br>
(test::assert-true (>= "ccc" "aab" "aaa"))<br>
(test::assert-false (>= "aaa" "aab"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::and" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``and``](#Namespace: root::and-contents) | Type: SpecialForm |
| ``Namespace: root::and`` | ``Usage: (and exp0 ... expN) -> [nil or expN result]`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-false (and nil (err "and- can not happen")))<br>
(test::assert-equal "and- done" (and t "and- done"))<br>
(test::assert-equal "and- done" (and t t "and- done"))<br>
(test::assert-equal 6 (and t t (+ 1 2 3)))<br>
(test::assert-equal 6 (and (/ 10 5) (* 5 2) (+ 1 2 3)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::cond" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cond``](#Namespace: root::cond-contents) | Type: Macro |
| ``Namespace: root::cond`` | ``Usage: (cond ((test form*)*) -> result`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(defn select-option (a)<br>
(cond ((= a 1) "opt-one")<br>
((= a 2) (set! b 5) "opt-two")<br>
((= a 3) (str "opt" "-three"))))<br>
(defn select-option-def (a)<br>
(cond ((= a 1) "opt-one")<br>
((= a 2) "opt-two")<br>
((= a 3) (str "opt" "-three"))<br>
(t "default")))<br>
(def b 0)<br>
(assert-equal "opt-one" (select-option 1))<br>
(assert-equal b 0)<br>
(assert-equal "opt-two" (select-option 2))<br>
(assert-equal b 5)<br>
(assert-equal "opt-three" (select-option 3))<br>
(assert-equal nil (select-option 4))<br>
(assert-equal "opt-one" (select-option-def 1))<br>
(assert-equal "opt-two" (select-option-def 2))<br>
(assert-equal "opt-three" (select-option-def 3))<br>
(assert-equal "default" (select-option-def 4))<br>
<br>
</code>
</details>


| <a id="Namespace: root::if" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``if``](#Namespace: root::if-contents) | Type: SpecialForm |
| ``Namespace: root::if`` | ``Usage: (if p1 a1 p2 a2 ... pn an?) -> [evaled form result]`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-if-one<br>
(if t "ONE TRUE" "ONE FALSE"))<br>
(def test-if-two<br>
(if nil "TWO TRUE" "TWO FALSE"))<br>
(test::assert-equal "ONE TRUE" test-if-one)<br>
(test::assert-equal "TWO FALSE" test-if-two)<br>
(def test-if-one2<br>
(if t "ONE2 TRUE"))<br>
(def test-if-two2<br>
(if nil "TWO2 TRUE"))<br>
(test::assert-equal "ONE2 TRUE" test-if-one2)<br>
(test::assert-equal nil test-if-two2)<br>
(def test-if-one2<br>
(if nil "ONE FALSE" t "ONE TRUE" t "ONE TRUE2"))<br>
(def test-if-two2<br>
(if nil "TWO TRUE" nil "TWO FALSE" t "TWO TRUE2"))<br>
(def test-if-three2<br>
(if nil "THREE TRUE" nil "THREE FALSE" "THREE DEFAULT"))<br>
(test::assert-equal "ONE TRUE" test-if-one2)<br>
(test::assert-equal "TWO TRUE2" test-if-two2)<br>
(test::assert-equal "THREE DEFAULT" test-if-three2)<br>
(test::assert-false (if nil))<br>
(test::assert-false (if nil t nil t nil t))<br>
<br>
</code>
</details>


| <a id="Namespace: root::match" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``match``](#Namespace: root::match-contents) | Type: Macro |
| ``Namespace: root::match`` | ``Usage: (match condition (value form*)*) -> result`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(defn select-option (a)<br>
(match a (1 "opt-one")<br>
(2 (set! b 5) "opt-two")<br>
(3 (str "opt" "-three"))))<br>
(defn select-option-def (a)<br>
(match a (1 "opt-one")<br>
(2 "opt-two")<br>
(3 (str "opt" "-three"))<br>
(nil "default")))<br>
(def b 0)<br>
(assert-equal b 0)<br>
(assert-equal "opt-one" (select-option 1))<br>
(assert-equal "opt-two" (select-option 2))<br>
(assert-equal b 5)<br>
(assert-equal "opt-three" (select-option 3))<br>
(assert-equal nil (select-option 4))<br>
(assert-equal "opt-one" (select-option-def 1))<br>
(assert-equal "opt-two" (select-option-def 2))<br>
(assert-equal "opt-three" (select-option-def 3))<br>
(assert-equal "default" (select-option-def 4))<br>
<br>
</code>
</details>


| <a id="Namespace: root::not" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``not``](#Namespace: root::not-contents) | Type: Function |
| ``Namespace: root::not`` | ``Usage: (not expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (not nil))<br>
(test::assert-false (not 10))<br>
(test::assert-false (not t))<br>
(test::assert-false (not (+ 1 2 3)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::null" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``null``](#Namespace: root::null-contents) | Type: Function |
| ``Namespace: root::null`` | ``Usage: (null expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (null nil))<br>
(test::assert-false (null 10))<br>
(test::assert-false (null t))<br>
(test::assert-false (null (+ 1 2 3)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::or" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``or``](#Namespace: root::or-contents) | Type: SpecialForm |
| ``Namespace: root::or`` | ``Usage: (or exp0 ... expN) -> [nil or first non nil expression]`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (or nil nil t (err "and- can not happen")))<br>
(test::assert-false (or nil nil nil))<br>
(test::assert-equal "or- done" (or nil "or- done"))<br>
(test::assert-equal "or- done" (or nil nil "or- done"))<br>
(test::assert-equal 6 (or nil nil (+ 1 2 3)))<br>
(test::assert-equal 2 (or (/ 10 5) (* 5 2) (+ 1 2 3)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::when" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``when``](#Namespace: root::when-contents) | Type: Macro |
| ``Namespace: root::when`` | ``Usage: (when provided-condition if-true)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-true (when #t #t))<br>
(assert-false (when #t nil))<br>
(assert-false (when nil nil))<br>
<br>
</code>
</details>
### <a id="Core forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Core forms](#Core forms-contents)



| <a id="Namespace: root::*collection-src*" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*collection-src*``](#Namespace: root::*collection-src*-contents) | Type: String |
| ``Namespace: root::*collection-src*`` | ``Usage: (print *collection-src*)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(print *collection-src*)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::*core-src*" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*core-src*``](#Namespace: root::*core-src*-contents) | Type: String |
| ``Namespace: root::*core-src*`` | ``Usage: (print *core-src*)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(print *core-src*)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::*endfix-src*" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*endfix-src*``](#Namespace: root::*endfix-src*-contents) | Type: String |
| ``Namespace: root::*endfix-src*`` | ``Usage: (print *endfix-src*)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(print *endfix-src*)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::*getopts-src*" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*getopts-src*``](#Namespace: root::*getopts-src*-contents) | Type: String |
| ``Namespace: root::*getopts-src*`` | ``Usage: (print *getopts-src*)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(print *getopts-src*)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::*iterator-src*" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*iterator-src*``](#Namespace: root::*iterator-src*-contents) | Type: String |
| ``Namespace: root::*iterator-src*`` | ``Usage: (print *iterator-src*)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(print *iterator-src*)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::*lib-src*" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*lib-src*``](#Namespace: root::*lib-src*-contents) | Type: String |
| ``Namespace: root::*lib-src*`` | ``Usage: (print *lib-src*)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(print *lib-src*)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::*seq-src*" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*seq-src*``](#Namespace: root::*seq-src*-contents) | Type: String |
| ``Namespace: root::*seq-src*`` | ``Usage: (print *seq-src*)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(print *seq-src*)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::*shell-src*" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*shell-src*``](#Namespace: root::*shell-src*-contents) | Type: String |
| ``Namespace: root::*shell-src*`` | ``Usage: (print *shell-src*)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(print *shell-src*)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::*slsh-std-src*" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*slsh-std-src*``](#Namespace: root::*slsh-std-src*-contents) | Type: String |
| ``Namespace: root::*slsh-std-src*`` | ``Usage: (print *slsh-std-src*)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(print *slsh-std-src*)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::*slshrc-src*" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*slshrc-src*``](#Namespace: root::*slshrc-src*-contents) | Type: String |
| ``Namespace: root::*slshrc-src*`` | ``Usage: (print *slshrc-src*)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(print *slshrc-src*)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::*struct-src*" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*struct-src*``](#Namespace: root::*struct-src*-contents) | Type: String |
| ``Namespace: root::*struct-src*`` | ``Usage: (print *struct-src*)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(print *struct-src*)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::*test-src*" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*test-src*``](#Namespace: root::*test-src*-contents) | Type: String |
| ``Namespace: root::*test-src*`` | ``Usage: (print *test-src*)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(print *test-src*)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::and-let*" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``and-let*``](#Namespace: root::and-let*-contents) | Type: Macro |
| ``Namespace: root::and-let*`` | ``Usage: (and-let* vals &rest let-body)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal "charlie bravo alpha."<br>
(and-let* ((val (str "alpha."))<br>
(other-val (str "bravo " val)))<br>
(str "charlie " other-val)) "]")<br>
(test::assert-equal "alpha, bravo." (and-let* ((val (do<br>
(str "bravo.")))<br>
(other-val (do<br>
(str "alpha, " val))))))<br>
(test::assert-false (and-let* ((val (do (str "alpha, ") nil))<br>
(other-val (do (str "bravo " val))))))<br>
(test::assert-false (and-let* ((a-list (list 1 2 3 4 5 6 7 8))<br>
(evens (filter (fn (x) (= 0 (% x 2))) a-list))<br>
(doubled (map (fn (x) (* x 2)) evens))<br>
(odds (collect (filter (fn (x) (= 1 (% x 2))) doubled))))<br>
#t))<br>
<br>
</code>
</details>


| <a id="Namespace: root::apply" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``apply``](#Namespace: root::apply-contents) | Type: Function |
| ``Namespace: root::apply`` | ``Usage: (apply function arg* list)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-apply-one (apply str '("O" "NE")))<br>
(test::assert-equal "ONE" test-apply-one)<br>
(test::assert-equal 10 (apply + 1 '(2 7)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::back-quote" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``back-quote``](#Namespace: root::back-quote-contents) | Type: SpecialForm |
| ``Namespace: root::back-quote`` | ``Usage: `expression -> expression`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal (list 1 2 3) `(1 2 3))<br>
(test::assert-equal `(1 2 3) '(1 2 3))<br>
(def test-bquote-one 1)<br>
(def test-bquote-list '(1 2 3))<br>
(test::assert-equal (list 1 2 3) `(,test-bquote-one 2 3))<br>
(test::assert-equal (list 1 2 3) `(,@test-bquote-list))<br>
<br>
</code>
</details>


| <a id="Namespace: root::block" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``block``](#Namespace: root::block-contents) | Type: SpecialForm |
| ``Namespace: root::block`` | ``Usage: (block name form*)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal '(4 5) (block xxx '(1 2) (return-from xxx '(4 5)) '(a b) '(2 3)))<br>
(test::assert-equal '(4 5) (block xxx '(1 2) (return-from nil '(4 5)) '(a b) '(2 3)))<br>
(test::assert-equal '(5 6) (block xxx '(1 2) (block yyy (return-from xxx '(5 6)) '(a b)) '(2 3)))<br>
(test::assert-equal '(5 6) (block xxx '(1 2) (block yyy ((fn (p) (return-from xxx p)) '(5 6)) '(a b)) '(2 3)))<br>
(test::assert-equal '(2 3) (block xxx '(1 2) (block yyy (return-from yyy t) '(a b)) '(2 3)))<br>
(test::assert-equal '(5 6) (block yyy ((fn (p) (return-from yyy p)) '(5 6)) '(a b)) '(2 3))<br>
(test::assert-equal 2<br>
(block forloop<br>
(for item in '(1 2 3)<br>
(when (= 2 item)<br>
(return-from forloop item)))))<br>
<br>
</code>
</details>


| <a id="Namespace: root::def" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``def``](#Namespace: root::def-contents) | Type: SpecialForm |
| ``Namespace: root::def`` | ``Usage: (def symbol doc_string? expression) -> expression`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-do-one nil)<br>
(def test-do-two nil)<br>
(def test-do-three (do (set! test-do-one "One")(set! test-do-two "Two")"Three"))<br>
(test::assert-equal "One" test-do-one)<br>
(test::assert-equal "Two" test-do-two)<br>
(test::assert-equal "Three" test-do-three)<br>
(let ((test-do-one nil))<br>
; Add this to tthe let's scope (shadow the outer test-do-two).<br>
(test::assert-equal "Default" (def ns::test-do-four "Default"))<br>
; set the currently scoped value.<br>
(set! test-do-one "1111")<br>
(set! test-do-two "2222")<br>
(test::assert-equal "1111" test-do-one)<br>
(test::assert-equal "2222" test-do-two)<br>
(test::assert-equal "Default" test-do-four))<br>
; Original outer scope not changed.<br>
(test::assert-equal "One" test-do-one)<br>
(test::assert-equal "Default" test-do-four)<br>
;(def (sym "test-do-one") "do one")<br>
(test::assert-error (def (sym "test-do-one") "do one"))<br>
;(test::assert-equal "do one" test-do-one)<br>
(test::assert-error (def (sym->str test-do-one) "do one 2"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::def?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``def?``](#Namespace: root::def?-contents) | Type: SpecialForm |
| ``Namespace: root::def?`` | ``Usage: (def? expression) -> t\|nil`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-is-def t)<br>
(def test-is-def2 'test-is-def)<br>
(test::assert-true (def? test-is-def))<br>
(test::assert-true (def? (sym "test-is-def")))<br>
(test::assert-true (def? (ref test-is-def2)))<br>
(test::assert-false (def? test-is-def-not-defined))<br>
(test::assert-false (def? (sym "test-is-def-not-defined")))<br>
(test::assert-error (def? (ref test-is-def)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::defmacro" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``defmacro``](#Namespace: root::defmacro-contents) | Type: Macro |
| ``Namespace: root::defmacro`` | ``Usage: (defmacro name doc_string? argument_list body)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(defmacro test-mac (x) (var y (+ (ref (ref x)) 1)) `(set! ,x ,y))<br>
(def test-mac-x 2)<br>
(test-mac test-mac-x)<br>
(test::assert-equal 3 test-mac-x)<br>
(defmacro test-mac (x) `(set! ,x 15))<br>
(test-mac test-mac-x)<br>
(test::assert-equal 15 test-mac-x)<br>
<br>
</code>
</details>


| <a id="Namespace: root::defn" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``defn``](#Namespace: root::defn-contents) | Type: Macro |
| ``Namespace: root::defn`` | ``Usage: (defn name &rest args)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(defn defn-test (x y) (+ x y))<br>
(test::assert-equal 5 (defn-test 2 3))<br>
(defn defn-test (x y) (set! x (* x 2))(+ x y))<br>
(test::assert-equal 7 (defn-test 2 3))<br>
(defn defn-test (x y))<br>
(test::assert-false (defn-test 2 3))<br>
(defn defn-test (x y) t)<br>
(test::assert-true (defn-test 2 3))<br>
<br>
</code>
</details>


| <a id="Namespace: root::do" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``do``](#Namespace: root::do-contents) | Type: SpecialForm |
| ``Namespace: root::do`` | ``Usage: (do exp0 ... expN) -> expN`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-do-one nil)<br>
(def test-do-two nil)<br>
(def test-do-three (do (set! test-do-one "One")(set! test-do-two "Two")"Three"))<br>
(test::assert-equal "One" test-do-one)<br>
(test::assert-equal "Two" test-do-two)<br>
(test::assert-equal "Three" test-do-three)<br>
<br>
</code>
</details>


| <a id="Namespace: root::doc" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``doc``](#Namespace: root::doc-contents) | Type: Function |
| ``Namespace: root::doc`` | ``Usage: (doc symbol)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(doc 'car)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::doc-raw" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``doc-raw``](#Namespace: root::doc-raw-contents) | Type: Function |
| ``Namespace: root::doc-raw`` | ``Usage: (doc-raw symbol)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(doc-raw 'car)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::dotimes" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``dotimes``](#Namespace: root::dotimes-contents) | Type: Macro |
| ``Namespace: root::dotimes`` | ``Usage: (dotimes times body)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def i 0)<br>
(dotimes 11 (set! i (+ 1 i)))<br>
(assert-equal 11 i)<br>
<br>
</code>
</details>


| <a id="Namespace: root::dotimes-i" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``dotimes-i``](#Namespace: root::dotimes-i-contents) | Type: Macro |
| ``Namespace: root::dotimes-i`` | ``Usage: (dotimes-i idx-bind times body)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def i 0)<br>
(def i-tot 0)<br>
(dotimes-i idx 11 (do (set! i-tot (+ idx i-tot))(set! i (+ 1 i))))<br>
(assert-equal 11 i)<br>
(assert-equal 55 i-tot)<br>
<br>
</code>
</details>


| <a id="Namespace: root::dyn" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``dyn``](#Namespace: root::dyn-contents) | Type: SpecialForm |
| ``Namespace: root::dyn`` | ``Usage: (dyn key value expression) -> result_of_expression`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(defn test-dyn-fn () (print "Print dyn out"))<br>
(defn test-dyn-fn2 () (print "Print dyn out TWO"))<br>
(dyn *stdout* (open "/tmp/sl-sh.dyn.test" :create :truncate) (test-dyn-fn))<br>
(test::assert-equal "Print dyn out" (read-line (open "/tmp/sl-sh.dyn.test" :read)))<br>
(def test-dyn-indirect '*stdout*)<br>
(dyn (ref test-dyn-indirect) (open "/tmp/sl-sh.dyn.test" :create :truncate) (test-dyn-fn2))<br>
(test::assert-equal "Print dyn out TWO" (read-line (open "/tmp/sl-sh.dyn.test" :read)))<br>
(def test-dyn-true t)<br>
(test::assert-error (dyn (ref test-dyn-true) (open "/tmp/sl-sh.dyn.test" :create :truncate) (test-dyn-fn2)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::eprint" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``eprint``](#Namespace: root::eprint-contents) | Type: Function |
| ``Namespace: root::eprint`` | ``Usage: (eprint arg0 ... argN) -> nil`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
; Use a file for stderr for test.<br>
(dyn *stderr* (open "/tmp/sl-sh.eprint.test" :create :truncate) (do (eprint "eprint test out")(eprint " two") (close *stderr*)))<br>
(test::assert-equal "eprint test out two" (read-line (open "/tmp/sl-sh.eprint.test" :read)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::eprintln" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``eprintln``](#Namespace: root::eprintln-contents) | Type: Function |
| ``Namespace: root::eprintln`` | ``Usage: (eprintln arg0 ... argN) -> nil`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
; Use a file for stderr for test.<br>
(dyn *stderr* (open "/tmp/sl-sh.eprintln.test" :create :truncate) (do (eprintln "eprintln test out")(eprintln "line two") (close *stderr*)))<br>
(def topen (open "/tmp/sl-sh.eprintln.test" :read))<br>
(test::assert-equal "eprintln test out<br>
" (read-line topen))<br>
(test::assert-equal "line two<br>
" (read-line topen))<br>
<br>
</code>
</details>


| <a id="Namespace: root::err" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``err``](#Namespace: root::err-contents) | Type: Function |
| ``Namespace: root::err`` | ``Usage: (err string) -> raises an error`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-err-err (get-error (err "Test Error")))<br>
(test::assert-equal :error (car test-err-err))<br>
(test::assert-equal "Test Error" (cadr test-err-err))<br>
<br>
</code>
</details>


| <a id="Namespace: root::error-stack-off" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``error-stack-off``](#Namespace: root::error-stack-off-contents) | Type: Lambda |
| ``Namespace: root::error-stack-off`` | ``Usage: (error-stack-off)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
; no-op<br>
(error-stack-off)<br>
<br>
</code>
</details>


| <a id="Namespace: root::error-stack-on" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``error-stack-on``](#Namespace: root::error-stack-on-contents) | Type: Lambda |
| ``Namespace: root::error-stack-on`` | ``Usage: (error-stack-on)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
; no-op<br>
(error-stack-on)<br>
<br>
</code>
</details>


| <a id="Namespace: root::eval" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``eval``](#Namespace: root::eval-contents) | Type: Function |
| ``Namespace: root::eval`` | ``Usage: (eval expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-eval-one nil)<br>
(eval (read "(set! test-eval-one \"ONE\")"))<br>
(test::assert-equal "ONE" test-eval-one)<br>
(eval '(set! test-eval-one "TWO"))<br>
(test::assert-equal "TWO" test-eval-one)<br>
<br>
</code>
</details>


| <a id="Namespace: root::expand-macro" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``expand-macro``](#Namespace: root::expand-macro-contents) | Type: Function |
| ``Namespace: root::expand-macro`` | ``Usage: (expand-macro expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal '(def xx "value") (expand-macro '(def xx "value")))<br>
(defmacro mac-test-for<br>
(bind in in_list body) (do<br>
(if (not (= in 'in)) (err "Invalid test-mac-for: (test-mac-for [v] in [iterator] (body))"))<br>
`((fn (,bind)<br>
(if (> (length ,in_list) 0)<br>
(root::loop (plist) (,in_list) (do<br>
(set! ,bind (root::first plist))<br>
(,@body)<br>
(if (> (length plist) 1) (recur (root::rest plist)))))))nil)))<br>
(test::assert-equal '(<br>
(fn<br>
(i)<br>
(if<br>
(> (length '(1 2 3)) 0)<br>
(root::loop<br>
(plist)<br>
('(1 2 3))<br>
(do<br>
(set! i (root::first plist)) nil<br>
(if<br>
(> (length plist) 1)<br>
(recur (root::rest plist))))))) nil)<br>
(expand-macro '(mac-test-for i in '(1 2 3) ())))<br>
(test::assert-equal '(1 2 3) (expand-macro '(1 2 3)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::expand-macro-all" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``expand-macro-all``](#Namespace: root::expand-macro-all-contents) | Type: Function |
| ``Namespace: root::expand-macro-all`` | ``Usage: (expand-macro-all expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal '(def xx "value") (expand-macro-all '(def xx "value")))<br>
(defmacro mac-test-for<br>
(bind in in_list body) (do<br>
(if (not (= in 'in)) (err "Invalid test-mac-for: (test-mac-for [v] in [iterator] (body))"))<br>
`((fn (,bind)<br>
(if (> (length ,in_list) 0)<br>
(root::loop (plist) (,in_list) (do<br>
(set! ,bind (root::first plist))<br>
(,@body)<br>
(if (> (length plist) 1) (recur (root::rest plist)))))))nil)))<br>
(test::assert-equal '(<br>
(fn<br>
(i)<br>
(if<br>
(> (length '(1 2 3)) 0)<br>
(<br>
(fn<br>
(plist)<br>
(do<br>
(set! i (root::first plist)) nil<br>
(if<br>
(> (length plist) 1)<br>
(recur (root::rest plist)))))<br>
'(1 2 3)))) nil)<br>
(expand-macro-all '(mac-test-for i in '(1 2 3) ())))<br>
(test::assert-equal '(1 2 3) (expand-macro-all '(1 2 3)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::expand-macro1" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``expand-macro1``](#Namespace: root::expand-macro1-contents) | Type: Function |
| ``Namespace: root::expand-macro1`` | ``Usage: (expand-macro1 expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal '(def xx "value") (expand-macro1 '(def xx "value")))<br>
(defmacro mac-test-for<br>
(bind in in_list body) (do<br>
(if (not (= in 'in)) (err "Invalid test-mac-for: (test-mac-for [v] in [iterator] (body))"))<br>
`((fn (,bind)<br>
(if (> (length ,in_list) 0)<br>
(root::loop (plist) (,in_list) (do<br>
(set! ,bind (root::first plist))<br>
(,@body)<br>
(if (> (length plist) 1) (recur (root::rest plist)))))))nil)))<br>
(test::assert-equal '((fn<br>
(i)<br>
(if<br>
(> (length '(1 2 3)) 0)<br>
(root::loop<br>
(plist)<br>
('(1 2 3))<br>
(do<br>
(set! i (root::first plist)) nil<br>
(if<br>
(> (length plist) 1)<br>
(recur (root::rest plist)))))))nil)<br>
(expand-macro1 '(mac-test-for i in '(1 2 3) ())))<br>
(test::assert-equal '(1 2 3) (expand-macro1 '(1 2 3)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::fn" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fn``](#Namespace: root::fn-contents) | Type: SpecialForm |
| ``Namespace: root::fn`` | ``Usage: (fn (param*) expr*) -> exprN`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-fn1 nil)<br>
(def test-fn2 nil)<br>
(def test-fn3 nil)<br>
(def test-fn-empty ((fn ())))<br>
(test::assert-false test-fn-empty)<br>
((fn () (set! test-fn1 1)))<br>
(test::assert-equal 1 test-fn1)<br>
((fn () (set! test-fn1 10)(set! test-fn2 2)))<br>
(test::assert-equal 10 test-fn1)<br>
(test::assert-equal 2 test-fn2)<br>
((fn () (set! test-fn1 11)(set! test-fn2 20)(set! test-fn3 3)))<br>
(test::assert-equal 11 test-fn1)<br>
(test::assert-equal 20 test-fn2)<br>
(test::assert-equal 3 test-fn3)<br>
((fn (x y z) (set! test-fn1 x)(set! test-fn2 y)(set! test-fn3 z)) 12 21 30)<br>
(test::assert-equal 12 test-fn1)<br>
(test::assert-equal 21 test-fn2)<br>
(test::assert-equal 30 test-fn3)<br>
(test::assert-equal 63 ((fn (x y z) (set! test-fn1 x)(set! test-fn2 y)(set! test-fn3 z)(+ x y z)) 12 21 30))<br>
<br>
</code>
</details>


| <a id="Namespace: root::format" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``format``](#Namespace: root::format-contents) | Type: Function |
| ``Namespace: root::format`` | ``Usage: (format arg0 ... argN) -> string`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal "stringsome" (format "string" "some"))<br>
(test::assert-equal "string" (format "string" ""))<br>
(test::assert-equal "string 50" (format "string" " " 50))<br>
(test::assert-equal "string 50 100.5" (format "string" " " 50 " " 100.5))<br>
<br>
</code>
</details>


| <a id="Namespace: root::gensym" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``gensym``](#Namespace: root::gensym-contents) | Type: Function |
| ``Namespace: root::gensym`` | ``Usage: (gensym) -> symbol`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-gensym-one (gensym))<br>
(def test-gensym-two (gensym))<br>
(def test-gensym-three (gensym))<br>
(test::assert-true (str-starts-with "gs@@" (sym->str test-gensym-one)))<br>
(test::assert-true (str-starts-with "gs@@" (sym->str test-gensym-two)))<br>
(test::assert-true (str-starts-with "gs@@" (sym->str test-gensym-three)))<br>
(test::assert-true (symbol? (gensym)))<br>
(test::assert-true (symbol? test-gensym-one))<br>
(test::assert-true (symbol? test-gensym-two))<br>
(test::assert-true (symbol? test-gensym-three))<br>
<br>
</code>
</details>


| <a id="Namespace: root::get-error" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``get-error``](#Namespace: root::get-error-contents) | Type: Function |
| ``Namespace: root::get-error`` | ``Usage: (get-error exp0 ... expN) -> pair`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def get-error-t1 (get-error (err "Some Error")))<br>
(test::assert-equal :error (car get-error-t1))<br>
(test::assert-equal "Some Error" (cadr get-error-t1))<br>
(test::assert-true (vec? (caddr get-error-t1)))<br>
(test::assert-equal '(:ok . "Some String") (get-error "Some String"))<br>
(test::assert-equal '(:ok . "Some Other String") (get-error (def test-get-error "Some ") (str test-get-error "Other String")))<br>
<br>
</code>
</details>


| <a id="Namespace: root::identity" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``identity``](#Namespace: root::identity-contents) | Type: Lambda |
| ``Namespace: root::identity`` | ``Usage: (identity x)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-equal 0 (*identity* 0))<br>
<br>
</code>
</details>


| <a id="Namespace: root::intern-stats" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``intern-stats``](#Namespace: root::intern-stats-contents) | Type: SpecialForm |
| ``Namespace: root::intern-stats`` | ``Usage: (intern-stats)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(intern-stats)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::length" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``length``](#Namespace: root::length-contents) | Type: Function |
| ``Namespace: root::length`` | ``Usage: (length expression) -> int`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal 0 (length nil))<br>
(test::assert-equal 5 (length "12345"))<br>
; Note the unicode symbol is only one char even though it is more then one byte.<br>
(test::assert-equal 6 (length "12345Σ"))<br>
(test::assert-equal 3 (length '(1 2 3)))<br>
(test::assert-equal 3 (length '#(1 2 3)))<br>
(test::assert-equal 3 (length (list 1 2 3)))<br>
(test::assert-equal 3 (length (vec 1 2 3)))<br>
(test::assert-error (length 100))<br>
(test::assert-error (length 100.0))<br>
(test::assert-error (length #\x))<br>
<br>
</code>
</details>


| <a id="Namespace: root::let" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``let``](#Namespace: root::let-contents) | Type: Macro |
| ``Namespace: root::let`` | ``Usage: (let vals &rest let-body)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-do-one "One1")<br>
(def test-do-two "Two1")<br>
(def test-do-three (let ((test-do-one "One")) (set! test-do-two "Two")(test::assert-equal "One" test-do-one)"Three"))<br>
(test::assert-equal "One1" test-do-one)<br>
(test::assert-equal "Two" test-do-two)<br>
(test::assert-equal "Three" test-do-three)<br>
<br>
</code>
</details>


| <a id="Namespace: root::let*" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``let*``](#Namespace: root::let*-contents) | Type: Macro |
| ``Namespace: root::let*`` | ``Usage: (let* vals &rest let-body)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(let* ((add-one (fn (x) (+ 1 x)))<br>
(double-add (fn (x) (add-one (add-one x)))))<br>
(test::assert-equal 4 (add-one 3))<br>
(test::assert-equal 6 (double-add 4)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::lex" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``lex``](#Namespace: root::lex-contents) | Type: Macro |
| ``Namespace: root::lex`` | ``Usage: (lex exp0 ... expN) -> expN`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-do-one "One1")<br>
(def test-do-two "Two1")<br>
(def test-do-three (lex (var test-do-one "One")(set! test-do-two "Two")(test::assert-equal "One" test-do-one)"Three"))<br>
(test::assert-equal "One1" test-do-one)<br>
(test::assert-equal "Two" test-do-two)<br>
(test::assert-equal "Three" test-do-three)<br>
<br>
</code>
</details>


| <a id="Namespace: root::loop" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``loop``](#Namespace: root::loop-contents) | Type: Macro |
| ``Namespace: root::loop`` | ``Usage: (loop params bindings body)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def tot 0)<br>
(loop (idx) (3) (do<br>
(set! tot (+ tot 1))<br>
(if (> idx 1) (recur (- idx 1)))))<br>
(assert-equal 3 tot)<br>
<br>
</code>
</details>


| <a id="Namespace: root::macro" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``macro``](#Namespace: root::macro-contents) | Type: SpecialForm |
| ``Namespace: root::macro`` | ``Usage: (macro (args) `(apply + ,@args))`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-macro1 nil)<br>
(def test-macro2 nil)<br>
(def test-macro-empty (macro ()))<br>
(test::assert-false (test-macro-empty))<br>
(def test-mac nil)<br>
(def mac-var 2)<br>
(lex<br>
(var mac-var 3)<br>
(set! test-mac (macro (x) (set! test-macro2 100)(test::assert-equal 3 mac-var)`(* ,mac-var ,x))))<br>
(set! test-macro1 (test-mac 10))<br>
(test::assert-equal 30 test-macro1)<br>
(test::assert-equal 100 test-macro2)<br>
<br>
</code>
</details>


| <a id="Namespace: root::meta-add-tags" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``meta-add-tags``](#Namespace: root::meta-add-tags-contents) | Type: Function |
| ``Namespace: root::meta-add-tags`` | ``Usage: (meta-add-tags expression tag*)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def meta-add-tags-var '(1 2 3))<br>
(meta-add-tags meta-add-tags-var :tag1)<br>
(test::assert-true (meta-tag? meta-add-tags-var :tag1))<br>
(test::assert-false (meta-tag? meta-add-tags-var :tag2))<br>
(meta-add-tags meta-add-tags-var :tag2 '(:tag3 :tag4) '#(:tag5 :tag6) :tag7)<br>
(test::assert-true (meta-tag? meta-add-tags-var :tag2))<br>
(test::assert-true (meta-tag? meta-add-tags-var :tag3))<br>
(test::assert-true (meta-tag? meta-add-tags-var :tag4))<br>
(test::assert-true (meta-tag? meta-add-tags-var :tag5))<br>
(test::assert-true (meta-tag? meta-add-tags-var :tag6))<br>
(test::assert-true (meta-tag? meta-add-tags-var :tag7))<br>
<br>
</code>
</details>


| <a id="Namespace: root::meta-column-no" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``meta-column-no``](#Namespace: root::meta-column-no-contents) | Type: SpecialForm |
| ``Namespace: root::meta-column-no`` | ``Usage: (meta-column-no)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(meta-column-no)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::meta-file-name" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``meta-file-name``](#Namespace: root::meta-file-name-contents) | Type: SpecialForm |
| ``Namespace: root::meta-file-name`` | ``Usage: (meta-file-name)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(meta-file-name)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::meta-line-no" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``meta-line-no``](#Namespace: root::meta-line-no-contents) | Type: SpecialForm |
| ``Namespace: root::meta-line-no`` | ``Usage: (meta-line-no)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(meta-line-no)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::meta-tag?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``meta-tag?``](#Namespace: root::meta-tag?-contents) | Type: Function |
| ``Namespace: root::meta-tag?`` | ``Usage: (meta-tag? expression tag)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def meta-add-tag-var '(1 2 3))<br>
(meta-add-tags meta-add-tag-var :tag1)<br>
(test::assert-true (meta-tag? meta-add-tag-var :tag1))<br>
(test::assert-false (meta-tag? meta-add-tag-var :tag2))<br>
<br>
</code>
</details>


| <a id="Namespace: root::nsubstitute!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``nsubstitute!``](#Namespace: root::nsubstitute!-contents) | Type: Lambda |
| ``Namespace: root::nsubstitute!`` | ``Usage: (nsubstitute! new-item old-item lst &rest mods)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(let ((lst (list 1 2 3 4 5)))<br>
(test::assert-equal (list 1 2 10 4 5) (nsubstitute! 10 3 lst))<br>
(test::assert-equal (list 1 2 10 4 5) lst))<br>
<br>
</code>
</details>


| <a id="Namespace: root::occurs" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``occurs``](#Namespace: root::occurs-contents) | Type: Lambda |
| ``Namespace: root::occurs`` | ``Usage: (occurs (list 1 2 ...) 7) (occurs (list 1 2 ...) 0 (fn (x) (% x 2)))`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal 7 (occurs (list 1 3 5 2 4 10 2 4 88 2 1) 0 (fn (x) (% x 2))))<br>
(test::assert-equal 3 (occurs (list 1 3 5 2 4 10 2 4 88 2 1) 2))<br>
(test::assert-equal 0 (occurs (list 1 3 5 2 4 10 2 4 88 2 1) 42))<br>
(test::assert-equal 2 (occurs (list 1 3 5 2 4 10 2 4 88 2 1) 8 (fn (x) (* x 2))))<br>
<br>
</code>
</details>


| <a id="Namespace: root::print" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``print``](#Namespace: root::print-contents) | Type: Function |
| ``Namespace: root::print`` | ``Usage: (print arg0 ... argN) -> nil`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
; Use a file for stdout for test.<br>
(dyn *stdout* (open "/tmp/sl-sh.print.test" :create :truncate) (do (print "Print test out")(print " two") (close *stdout*)))<br>
(test::assert-equal "Print test out two" (read-line (open "/tmp/sl-sh.print.test" :read)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::println" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``println``](#Namespace: root::println-contents) | Type: Function |
| ``Namespace: root::println`` | ``Usage: (println arg0 ... argN) -> nil`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
; Use a file for stdout for test.<br>
(dyn *stdout* (open "/tmp/sl-sh.println.test" :create :truncate) (do (println "Println test out")(println "line two") (close *stdout*)))<br>
(def topen (open "/tmp/sl-sh.println.test" :read))<br>
(test::assert-equal "Println test out<br>
" (read-line topen))<br>
(test::assert-equal "line two<br>
" (read-line topen))<br>
<br>
</code>
</details>


| <a id="Namespace: root::progn" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``progn``](#Namespace: root::progn-contents) | Type: Macro |
| ``Namespace: root::progn`` | ``Usage: (progn &rest args)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;see do<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::quote" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``quote``](#Namespace: root::quote-contents) | Type: SpecialForm |
| ``Namespace: root::quote`` | ``Usage: 'expression -> expression`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal (list 1 2 3) (quote (1 2 3)))<br>
(test::assert-equal (list 1 2 3) '(1 2 3))<br>
(test::assert-equal '(1 2 3) (quote (1 2 3)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::reader-macro-dot" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``reader-macro-dot``](#Namespace: root::reader-macro-dot-contents) | Type: Lambda |
| ``Namespace: root::reader-macro-dot`` | ``Usage: (reader-macro-dot stream ch)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def dot-test (read "(1 2 #.(* 3 10) #.(str "o" "ne"))))<br>
(test::assert-equal '(1 2 30 "one"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::recur" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``recur``](#Namespace: root::recur-contents) | Type: Function |
| ``Namespace: root::recur`` | ``Usage: (recur &rest)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def tot 0)<br>
(loop (idx) (3) (do<br>
(set! tot (+ tot 1))<br>
(if (> idx 1) (recur (- idx 1)))))<br>
(assert-equal 3 tot)<br>
(set! tot 0)<br>
((fn (idx) (do<br>
(set! tot (+ tot 1))<br>
(if (> idx 1) (recur (- idx 1)))))5)<br>
(assert-equal 5 tot)<br>
<br>
</code>
</details>


| <a id="Namespace: root::ref" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ref``](#Namespace: root::ref-contents) | Type: SpecialForm |
| ``Namespace: root::ref`` | ``Usage: (ref symbol) -> expression`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-is-def t)<br>
(def test-is-def2 'test-is-def)<br>
(test::assert-true (ref test-is-def))<br>
(set! test-is-def '(1 2 3))<br>
(test::assert-equal '(1 2 3) (ref test-is-def))<br>
(test::assert-error (ref test-is-def-no-exist))<br>
(test::assert-error (ref (ref test-is-def)))<br>
(test::assert-equal '(1 2 3) (ref (ref test-is-def2)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::return-from" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``return-from``](#Namespace: root::return-from-contents) | Type: SpecialForm |
| ``Namespace: root::return-from`` | ``Usage: (return-from name expression?)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal '(4 5) (block xxx '(1 2) (return-from xxx '(4 5)) '(a b) '(2 3)))<br>
(test::assert-equal '(4 5) (block xxx '(1 2) (return-from nil '(4 5)) '(a b) '(2 3)))<br>
(test::assert-equal '(5 6) (block xxx '(1 2) (block yyy (return-from xxx '(5 6)) '(a b)) '(2 3)))<br>
(test::assert-equal '(5 6) (block xxx '(1 2) (block yyy ((fn (p) (return-from xxx p)) '(5 6)) '(a b)) '(2 3)))<br>
(test::assert-equal '(2 3) (block xxx '(1 2) (block yyy (return-from yyy t) '(a b)) '(2 3)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::set!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``set!``](#Namespace: root::set!-contents) | Type: SpecialForm |
| ``Namespace: root::set!`` | ``Usage: (set! symbol expression) -> expression`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-do-one nil)<br>
(def test-do-two nil)<br>
(def test-do-three (do (set! test-do-one "One")(set! test-do-two "Two")"Three"))<br>
(test::assert-equal "One" test-do-one)<br>
(test::assert-equal "Two" test-do-two)<br>
(test::assert-equal "Three" test-do-three)<br>
(let ((test-do-one nil))<br>
; set the currently scoped value.<br>
(test::assert-equal "1111" (set! test-do-one "1111"))<br>
(test::assert-equal "1111" test-do-one))<br>
; Original outer scope not changed.<br>
(test::assert-equal "One" test-do-one)<br>
;(set! (sym "test-do-one") "do one")<br>
;(test::assert-equal "do one" test-do-one)<br>
(test::assert-error (set! (sym->str test-do-one) "do one 2"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::substitute" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``substitute``](#Namespace: root::substitute-contents) | Type: Macro |
| ``Namespace: root::substitute`` | ``Usage: (substitute new-item old-item lst &rest mods)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(let ((lst (list 1 2 3 4 5))<br>
(lst2 (list 1 2 3 3 3 4 5)))<br>
(test::assert-equal (list 1 2 10 4 5) (substitute 10 3 lst))<br>
(test::assert-equal lst lst)<br>
(test::assert-equal (list 1 2 4 4 4 4 5) (substitute 4 3 lst2))<br>
(test::assert-equal (list 1 2 4 3 3 4 5) (substitute 4 3 lst2 :first)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::syscall" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``syscall``](#Namespace: root::syscall-contents) | Type: Function |
| ``Namespace: root::syscall`` | ``Usage: (syscall system-command arg0 ... argN)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-syscall-one (str (syscall "echo" -n "syscall-test")))<br>
(test::assert-equal "syscall-test" test-syscall-one)<br>
<br>
</code>
</details>


| <a id="Namespace: root::undef" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``undef``](#Namespace: root::undef-contents) | Type: SpecialForm |
| ``Namespace: root::undef`` | ``Usage: (undef symbol) -> expression`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-undef nil)<br>
(test::assert-true (def? test-undef))<br>
(undef test-undef)<br>
(test::assert-false (def? test-undef))<br>
(def test-undef "undef")<br>
(test::assert-equal "undef" (undef test-undef))<br>
(test::assert-false (def? test-undef))<br>
(test::assert-equal "undef: symbol test-undef not defined in current scope (can only undef symbols in current scope)." (cadr (get-error (undef test-undef))))<br>
<br>
</code>
</details>


| <a id="Namespace: root::unwind-protect" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``unwind-protect``](#Namespace: root::unwind-protect-contents) | Type: Function |
| ``Namespace: root::unwind-protect`` | ``Usage: (unwind-protect protected cleanup*) -> [protected result]`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-unwind-one nil)<br>
(def test-unwind-err (get-error<br>
(unwind-protect (err "Some protected error") (set! test-unwind-one "got it"))))<br>
(test::assert-equal :error (car test-unwind-err))<br>
(test::assert-equal "Some protected error" (cadr test-unwind-err))<br>
(test::assert-equal "got it" test-unwind-one)<br>
(def test-unwind-one nil)<br>
(def test-unwind-two nil)<br>
(def test-unwind-three nil)<br>
(def test-unwind-four nil)<br>
(def test-unwind-err (get-error<br>
(unwind-protect<br>
(do (set! test-unwind-one "set one")(err "Some protected error two")(set! test-unwind-two "set two"))<br>
(set! test-unwind-three "set three")(set! test-unwind-four "set four"))))<br>
(test::assert-equal :error (car test-unwind-err))<br>
(test::assert-equal "Some protected error two" (cadr test-unwind-err))<br>
(test::assert-equal "set one" test-unwind-one)<br>
(test::assert-equal nil test-unwind-two)<br>
(test::assert-equal "set three" test-unwind-three)<br>
(test::assert-equal "set four" test-unwind-four)<br>
<br>
</code>
</details>


| <a id="Namespace: root::values" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``values``](#Namespace: root::values-contents) | Type: Function |
| ``Namespace: root::values`` | ``Usage: (values expression*)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (values? (values 1 "str" 5.5)))<br>
(test::assert-equal 1 (values-nth 0 (values 1 "str" 5.5)))<br>
(test::assert-equal "str" (values-nth 1 (values 1 "str" 5.5)))<br>
(test::assert-equal 5.5 (values-nth 2 (values 1 "str" 5.5)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::values-length" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``values-length``](#Namespace: root::values-length-contents) | Type: Function |
| ``Namespace: root::values-length`` | ``Usage: (values-length expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal 3 (values-length (values 1 "str" 5.5)))<br>
(test::assert-equal 2 (values-length (values 1 "str")))<br>
(test::assert-equal 1 (values-length (values "str")))<br>
(test::assert-equal 0 (values-length (values)))<br>
(test::assert-equal "str" (values-nth 1 (values 1 "str" 5.5)))<br>
(test::assert-equal 5.5 (values-nth 2 (values 1 "str" 5.5)))<br>
(def test-vals-len (values 1 "str" 5.5))<br>
(test::assert-equal 3 (values-length test-vals-len))<br>
<br>
</code>
</details>


| <a id="Namespace: root::values-nth" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``values-nth``](#Namespace: root::values-nth-contents) | Type: Function |
| ``Namespace: root::values-nth`` | ``Usage: (values-nth idx expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal 1 (values-nth 0 (values 1 "str" 5.5)))<br>
(test::assert-equal "str" (values-nth 1 (values 1 "str" 5.5)))<br>
(test::assert-equal 5.5 (values-nth 2 (values 1 "str" 5.5)))<br>
(def test-vals-nth (values 1 "str" 5.5))<br>
(test::assert-equal 1 (values-nth 0 test-vals-nth))<br>
(test::assert-equal "str" (values-nth 1 test-vals-nth))<br>
(test::assert-equal 5.5 (values-nth 2 test-vals-nth))<br>
<br>
</code>
</details>


| <a id="Namespace: root::var" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``var``](#Namespace: root::var-contents) | Type: SpecialForm |
| ``Namespace: root::var`` | ``Usage: (var symbol expression) -> expression`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(lex<br>
(var test-do-one nil)<br>
(var test-do-two nil)<br>
(var test-do-three (do (set! test-do-one "One")(set! test-do-two "Two")"Three"))<br>
(test::assert-equal "One" test-do-one)<br>
(test::assert-equal "Two" test-do-two)<br>
(test::assert-equal "Three" test-do-three)<br>
(let ((test-do-one nil))<br>
; Add this to the let's scope (shadow the outer test-do-two).<br>
(test::assert-equal "Default" (var test-do-two "Default"))<br>
; set the currently scoped value.<br>
(set! test-do-one "1111")<br>
(set! test-do-two "2222")<br>
(test::assert-equal "1111" test-do-one)<br>
(test::assert-equal "2222" test-do-two))<br>
; Original outer scope not changed.<br>
(test::assert-equal "One" test-do-one)<br>
(test::assert-equal "Two" test-do-two))<br>
<br>
</code>
</details>


| <a id="Namespace: root::varfn" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``varfn``](#Namespace: root::varfn-contents) | Type: Macro |
| ``Namespace: root::varfn`` | ``Usage: (varfn name &rest args)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(lex<br>
(varfn varfn-test (x y) (+ x y))<br>
(test::assert-equal 5 (varfn-test 2 3))<br>
(varfn varfn-test2 (x y) (set! x (* x 2))(+ x y))<br>
(test::assert-equal 7 (varfn-test2 2 3))<br>
(test::assert-true (def? varfn-test))<br>
(test::assert-true (def? varfn-test2)))<br>
(test::assert-false (def? varfn-test))<br>
(test::assert-false (def? varfn-test2))<br>
<br>
</code>
</details>
### <a id="File forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[File forms](#File forms-contents)
 Options to open, one or more of these can be added to open after the filename.
A file can only be opened for reading or writing (read is default).

Option | Description
-------|-----------
:read | Open file for reading, this is the default.
:write | Open file for writing.
:append | Open file for writing and append new data to end.
:truncate | Open file for write and delete all existing data.
:create | Create the file if it does not exist and open for writing.
:create-new | Create if does not exist, error if it does and open for writing.
:on-error-nil | If open has an error then return nil instead of producing an error.

Notes on closing.  Files will close when they go out of scope.  Using close will
cause a reference to a file to be marked close (removes that reference).  If
there are more then one references to a file it will not actually close until
all are released.  Close will also flush the file even if it is not the final
reference.  If a reference to a file is captured in a closure that can also keep
it open (closures currently capture the entire scope not just used symbols).


| <a id="Namespace: root::close" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``close``](#Namespace: root::close-contents) | Type: Function |
| ``Namespace: root::close`` | ``Usage: (close file)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def tst-file (open "/tmp/slsh-tst-open.txt" :create :truncate))<br>
(write-line tst-file "Test Line Two")<br>
(close tst-file)<br>
(def tst-file (open "/tmp/slsh-tst-open.txt" :read))<br>
(test::assert-equal "Test Line Two<br>
" (read-line tst-file))<br>
(close tst-file)<br>
<br>
</code>
</details>


| <a id="Namespace: root::flush" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``flush``](#Namespace: root::flush-contents) | Type: Function |
| ``Namespace: root::flush`` | ``Usage: (flush file)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def tst-file (open "/tmp/slsh-tst-open.txt" :create :truncate))<br>
(write-line tst-file "Test Line Three")<br>
(flush tst-file)<br>
(def tst-file (open "/tmp/slsh-tst-open.txt" :read))<br>
(test::assert-equal "Test Line Three<br>
" (read-line tst-file))<br>
(close tst-file)<br>
<br>
</code>
</details>


| <a id="Namespace: root::open" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``open``](#Namespace: root::open-contents) | Type: Function |
| ``Namespace: root::open`` | ``Usage: (open filename option*)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-open-f (open "/tmp/slsh-tst-open.txt" :create :truncate))<br>
(write-line test-open-f "Test Line One")<br>
(close test-open-f)<br>
(test::assert-equal "Test Line One<br>
" (read-line (open "/tmp/slsh-tst-open.txt")))<br>
<br>
</code>
</details>


| <a id="Namespace: root::read" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``read``](#Namespace: root::read-contents) | Type: Function |
| ``Namespace: root::read`` | ``Usage: (read [file\|string]? end-exp?) -> expression`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def tst-file (open "/tmp/slsh-tst-open.txt" :create :truncate))<br>
(write-line tst-file "(1 2 3)(x y z)")<br>
;(write-string tst-file "Test Line Read Line Two")<br>
(flush tst-file)<br>
(def tst-file (open "/tmp/slsh-tst-open.txt" :read))<br>
(test::assert-equal '(1 2 3) (read tst-file))<br>
(test::assert-equal '(x y z) (read tst-file))<br>
(test::assert-error (read test-file))<br>
(close tst-file)<br>
(def tst-file (open "/tmp/slsh-tst-open.txt" :read))<br>
(test::assert-equal '(1 2 3) (read tst-file :done))<br>
(test::assert-equal '(x y z) (read tst-file :done))<br>
(test::assert-equal :done (read tst-file :done))<br>
(close tst-file)<br>
(test::assert-equal '(4 5 6) (read "(4 5 6)"))<br>
(def test-str "7 8 9")<br>
(test::assert-equal 7 (read test-str))<br>
(test::assert-equal 8 (read test-str))<br>
(test::assert-equal 9 (read test-str))<br>
(test::assert-error (read test-str))<br>
(def test-str "7 8 9")<br>
(test::assert-equal 7 (read test-str :done))<br>
(test::assert-equal 8 (read test-str :done))<br>
(test::assert-equal 9 (read test-str :done))<br>
(test::assert-equal :done (read test-str :done))<br>
(test::assert-equal '(x y z) (read "(x y z)"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::read-all" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``read-all``](#Namespace: root::read-all-contents) | Type: Function |
| ``Namespace: root::read-all`` | ``Usage: (read-all [file\|string]? empty-exp?) -> list\|vec\|empty-exp`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def tst-file (open "/tmp/slsh-tst-open.txt" :create :truncate))<br>
(write-line tst-file "(1 2 3)(x y z)")<br>
(flush tst-file)<br>
(def tst-file (open "/tmp/slsh-tst-open.txt" :read))<br>
(test::assert-equal '#((1 2 3)(x y z)) (read-all tst-file))<br>
(close tst-file)<br>
(test::assert-equal '(4 5 6) (read-all "(4 5 6)"))<br>
(test::assert-equal '(7 8 9) (read-all "7 8 9"))<br>
(test::assert-equal '(x y z) (read-all "(x y z)" :not-used))<br>
(test::assert-equal :empty (read-all ";(x y z)" :empty))<br>
<br>
</code>
</details>


| <a id="Namespace: root::read-line" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``read-line``](#Namespace: root::read-line-contents) | Type: Function |
| ``Namespace: root::read-line`` | ``Usage: (read-line file) -> string`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def tst-file (open "/tmp/slsh-tst-open.txt" :create :truncate))<br>
(write-line tst-file "Test Line Read Line One")<br>
(write-string tst-file "Test Line Read Line Two")<br>
(flush tst-file)<br>
(def tst-file (open "/tmp/slsh-tst-open.txt" :read))<br>
(test::assert-equal "Test Line Read Line One<br>
" (read-line tst-file))<br>
(test::assert-equal "Test Line Read Line Two" (read-line tst-file))<br>
(close tst-file)<br>
<br>
</code>
</details>


| <a id="Namespace: root::write-line" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``write-line``](#Namespace: root::write-line-contents) | Type: Function |
| ``Namespace: root::write-line`` | ``Usage: (write-line file string)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def tst-file (open "/tmp/slsh-tst-open.txt" :create :truncate))<br>
(write-line tst-file "Test Line Write Line")<br>
(flush tst-file)<br>
(def tst-file (open "/tmp/slsh-tst-open.txt" :read))<br>
(test::assert-equal "Test Line Write Line<br>
" (read-line tst-file))<br>
(close tst-file)<br>
<br>
</code>
</details>


| <a id="Namespace: root::write-string" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``write-string``](#Namespace: root::write-string-contents) | Type: Function |
| ``Namespace: root::write-string`` | ``Usage: (write-string file string)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def tst-file (open "/tmp/slsh-tst-open.txt" :create :truncate))<br>
(write-string tst-file "Test Line Write String")<br>
(flush tst-file)<br>
(def tst-file (open "/tmp/slsh-tst-open.txt" :read))<br>
(test::assert-equal "Test Line Write String" (read-line tst-file))<br>
(close tst-file)<br>
<br>
</code>
</details>
### <a id="Hashmap forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Hashmap forms](#Hashmap forms-contents)



| <a id="Namespace: root::hash-clear!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash-clear!``](#Namespace: root::hash-clear!-contents) | Type: Function |
| ``Namespace: root::hash-clear!`` | ``Usage: (hash-clear! hashmap)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def tst-hash (make-hash '((:key1 . "val one")(key2 . "val two")("key3" . "val three")(#\S . "val S"))))<br>
(test::assert-equal 4 (length (hash-keys tst-hash)))<br>
(test::assert-true (hash-haskey tst-hash :key1))<br>
(test::assert-true (hash-haskey tst-hash 'key2))<br>
(test::assert-true (hash-haskey tst-hash "key3"))<br>
(test::assert-true (hash-haskey tst-hash #\S))<br>
(hash-clear! tst-hash)<br>
(test::assert-equal 0 (length (hash-keys tst-hash)))<br>
(test::assert-false (hash-haskey tst-hash :key1))<br>
(test::assert-false (hash-haskey tst-hash 'key2))<br>
(test::assert-false (hash-haskey tst-hash "key3"))<br>
(test::assert-false (hash-haskey tst-hash #\S))<br>
<br>
</code>
</details>


| <a id="Namespace: root::hash-get" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash-get``](#Namespace: root::hash-get-contents) | Type: SpecialForm |
| ``Namespace: root::hash-get`` | ``Usage: (hash-get hashmap key default?) -> value`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def tst-hash (make-hash '((:key1 . "val one")(key2 . "val two")("key3" . "val three")(#\S . "val S"))))<br>
(test::assert-equal 4 (length (hash-keys tst-hash)))<br>
(test::assert-equal "val one" (hash-get tst-hash :key1))<br>
(test::assert-equal "val two" (hash-get tst-hash 'key2))<br>
(test::assert-equal "val three" (hash-get tst-hash "key3"))<br>
(test::assert-equal "val S" (hash-get tst-hash #\S))<br>
(test::assert-equal "default" (hash-get tst-hash :not-here "default"))<br>
(test::assert-equal "string default" (hash-get tst-hash :not-here (str "string " "default")))<br>
<br>
</code>
</details>


| <a id="Namespace: root::hash-haskey" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash-haskey``](#Namespace: root::hash-haskey-contents) | Type: Function |
| ``Namespace: root::hash-haskey`` | ``Usage: (hash-haskey hashmap key)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def tst-hash (make-hash '((:key1 . "val one")(key2 . "val two")("key3" . "val three")(#\S . "val S"))))<br>
(test::assert-equal 4 (length (hash-keys tst-hash)))<br>
(test::assert-true (hash-haskey tst-hash :key1))<br>
(test::assert-true (hash-haskey tst-hash 'key2))<br>
(test::assert-true (hash-haskey tst-hash "key3"))<br>
(test::assert-true (hash-haskey tst-hash #\S))<br>
(test::assert-false (hash-haskey tst-hash 'key1))<br>
(test::assert-false (hash-haskey tst-hash :key2))<br>
(test::assert-false (hash-haskey tst-hash "keynone"))<br>
(hash-remove! tst-hash :key1)<br>
(test::assert-false (hash-haskey tst-hash :key1))<br>
(hash-set! tst-hash :key1 "val one b")<br>
(test::assert-true (hash-haskey tst-hash :key1))<br>
<br>
</code>
</details>


| <a id="Namespace: root::hash-keys" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash-keys``](#Namespace: root::hash-keys-contents) | Type: Function |
| ``Namespace: root::hash-keys`` | ``Usage: (hash-keys hashmap)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def tst-hash (make-hash '((:key1 . "val one")(key2 . "val two")("key3" . "val three")(#\S . "val S"))))<br>
(test::assert-equal 4 (length (hash-keys tst-hash)))<br>
(test::assert-true (in? (hash-keys tst-hash) :key1) " Test :key1")<br>
(test::assert-true (in? (hash-keys tst-hash) 'key2) " Test key2")<br>
; Note string or char used as a key will be a symbol in the hash-keys list...<br>
(test::assert-true (in? (hash-keys tst-hash) 'S) " Test S")<br>
(test::assert-true (in? (hash-keys tst-hash) 'key3) " Test key3")<br>
(test::assert-false (in? (hash-keys tst-hash) :key4))<br>
<br>
</code>
</details>


| <a id="Namespace: root::hash-remove!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash-remove!``](#Namespace: root::hash-remove!-contents) | Type: Function |
| ``Namespace: root::hash-remove!`` | ``Usage: (hash-remove! hashmap key)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def tst-hash (make-hash '((:key1 . "val one")(key2 . "val two")("key3" . "val three")(#\S . "val S"))))<br>
(test::assert-equal 4 (length (hash-keys tst-hash)))<br>
(test::assert-equal "val one" (hash-get tst-hash :key1))<br>
(test::assert-equal "val two" (hash-get tst-hash 'key2))<br>
(test::assert-equal "val three" (hash-get tst-hash "key3"))<br>
(test::assert-equal "val S" (hash-get tst-hash #\S))<br>
(hash-remove! tst-hash 'key2)<br>
(test::assert-equal 3 (length (hash-keys tst-hash)))<br>
(test::assert-equal "val one" (hash-get tst-hash :key1))<br>
(test::assert-equal "val three" (hash-get tst-hash "key3"))<br>
(test::assert-equal "val S" (hash-get tst-hash #\S))<br>
(hash-remove! tst-hash :key1)<br>
(test::assert-equal 2 (length (hash-keys tst-hash)))<br>
(test::assert-equal "val three" (hash-get tst-hash "key3"))<br>
(test::assert-equal "val S" (hash-get tst-hash #\S))<br>
(hash-remove! tst-hash "key3")<br>
(test::assert-equal 1 (length (hash-keys tst-hash)))<br>
(test::assert-equal "val S" (hash-get tst-hash #\S))<br>
(hash-remove! tst-hash #\S)<br>
(test::assert-equal 0 (length (hash-keys tst-hash)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::hash-set!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash-set!``](#Namespace: root::hash-set!-contents) | Type: Function |
| ``Namespace: root::hash-set!`` | ``Usage: (hash-set! hashmap key value)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def tst-hash (make-hash))<br>
(test::assert-equal 0 (length (hash-keys tst-hash)))<br>
(hash-set! tst-hash :new-key '(1 2 3))<br>
(test::assert-equal 1 (length (hash-keys tst-hash)))<br>
(test::assert-equal '(1 2 3) (hash-get tst-hash :new-key))<br>
(def tst-hash (make-hash '((:key1 . "val one")(key2 . "val two")("key3" . "val three"))))<br>
(test::assert-equal 3 (length (hash-keys tst-hash)))<br>
(test::assert-equal "val one" (hash-get tst-hash :key1))<br>
(test::assert-equal "val two" (hash-get tst-hash 'key2))<br>
(test::assert-equal "val three" (hash-get tst-hash "key3"))<br>
(hash-set! tst-hash :new-key '(1 2 3))<br>
(test::assert-equal 4 (length (hash-keys tst-hash)))<br>
(test::assert-equal "val one" (hash-get tst-hash :key1))<br>
(test::assert-equal "val two" (hash-get tst-hash 'key2))<br>
(test::assert-equal "val three" (hash-get tst-hash "key3"))<br>
(test::assert-equal '(1 2 3) (hash-get tst-hash :new-key))<br>
(hash-set! tst-hash 'key2 "val two b")<br>
(hash-set! tst-hash :key1 "val one b")<br>
(hash-set! tst-hash "key3" "val three b")<br>
(test::assert-equal 4 (length (hash-keys tst-hash)))<br>
(test::assert-equal "val one b" (hash-get tst-hash :key1))<br>
(test::assert-equal "val two b" (hash-get tst-hash 'key2))<br>
(test::assert-equal "val three b" (hash-get tst-hash "key3"))<br>
(test::assert-equal '(1 2 3) (hash-get tst-hash :new-key))<br>
<br>
</code>
</details>


| <a id="Namespace: root::make-hash" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``make-hash``](#Namespace: root::make-hash-contents) | Type: Function |
| ``Namespace: root::make-hash`` | ``Usage: (make-hash associations?)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def tst-hash (make-hash))<br>
(test::assert-equal 0 (length (hash-keys tst-hash)))<br>
(def tst-hash (make-hash ()))<br>
(test::assert-equal 0 (length (hash-keys tst-hash)))<br>
(def tst-hash (make-hash nil))<br>
(test::assert-equal 0 (length (hash-keys tst-hash)))<br>
(def tst-hash (make-hash '((:key1 . "val one")(key2 . "val two")("key3" . "val three"))))<br>
(test::assert-equal 3 (length (hash-keys tst-hash)))<br>
(test::assert-equal "val one" (hash-get tst-hash :key1))<br>
(test::assert-equal "val two" (hash-get tst-hash 'key2))<br>
(test::assert-equal "val three" (hash-get tst-hash "key3"))<br>
(def tst-hash (make-hash '#((:keyv1 . "val one")(keyv2 . "val two")("keyv3" . "val three"))))<br>
(test::assert-equal 3 (length (hash-keys tst-hash)))<br>
(test::assert-equal "val one" (hash-get tst-hash :keyv1))<br>
(test::assert-equal "val two" (hash-get tst-hash 'keyv2))<br>
(test::assert-equal "val three" (hash-get tst-hash "keyv3"))<br>
; Not in test below that tst-hash-val is NOT evaluated so the symbol is the value.<br>
(def tst-hash-val "some val")<br>
(def tst-hash (make-hash '#((:keyv1 . "val one")(:keyv2 . "val two")(:keyv3 . tst-hash-val))))<br>
(test::assert-equal 3 (length (hash-keys tst-hash)))<br>
(test::assert-equal "val one" (hash-get tst-hash :keyv1))<br>
(test::assert-equal "val two" (hash-get tst-hash :keyv2))<br>
(test::assert-equal 'tst-hash-val (hash-get tst-hash :keyv3))<br>
<br>
</code>
</details>
### <a id="iterator forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[iterator forms](#iterator forms-contents)



| <a id="Namespace: iterator::append" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``append``](#Namespace: iterator::append-contents) | Type: Lambda |
| ``Namespace: iterator::append`` | ``Usage: (append first-iter &rest rest-iters)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-iter (append '(0 1 2) '#(3 4 5) '(6 7 8 9)))<br>
(assert-false (test-iter :empty?))<br>
(assert-equal 10 (test-iter :count))<br>
(assert-true (test-iter :empty?))<br>
(def test-iter (append '(0 1 2) 3 4 5 '(6 7 8 9)))<br>
(assert-false (test-iter :empty?))<br>
(assert-equal 10 (test-iter :count))<br>
(assert-true (test-iter :empty?))<br>
(def test-iter (append 0 1 2 '(3 4)))<br>
(assert-false (test-iter :empty?))<br>
(assert-equal 0 (test-iter :next!))<br>
(assert-equal 1 (test-iter :next!))<br>
(assert-equal 2 (test-iter :next!))<br>
(assert-equal 3 (test-iter :next!))<br>
(assert-equal 4 (test-iter :next!))<br>
(assert-true (test-iter :empty?))<br>
(def test-iter (append 0 1 2 nil))<br>
(assert-false (test-iter :empty?))<br>
(assert-equal 0 (test-iter :next!))<br>
(assert-equal 1 (test-iter :next!))<br>
(assert-equal 2 (test-iter :next!))<br>
(assert-true (test-iter :empty?))<br>
(def test-iter (append 0 1 2 '(nil)))<br>
(assert-false (test-iter :empty?))<br>
(assert-equal 0 (test-iter :next!))<br>
(assert-equal 1 (test-iter :next!))<br>
(assert-equal 2 (test-iter :next!))<br>
(assert-equal nil (test-iter :next!))<br>
(assert-true (test-iter :empty?))<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::append-iter" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``append-iter``](#Namespace: iterator::append-iter-contents) | Type: Lambda |
| ``Namespace: iterator::append-iter`` | ``Usage: (append-iter)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-iter ((iterator::append-iter) :init '(0 1 2) '#(3 4 5) '(6 7 8 9)))<br>
(def test-slice-iter (test-iter :slice 3 7))<br>
(assert-false (test-slice-iter :empty?))<br>
(assert-equal 3 (test-slice-iter :next!))<br>
(assert-equal 4 (test-slice-iter :next!))<br>
(assert-equal 5 (test-slice-iter :next!))<br>
(assert-equal 6 (test-slice-iter :next!))<br>
(assert-true (test-slice-iter :empty?))<br>
(def test-iter ((iterator::append-iter) :init '(0 1 2) nil '#(3 4 5) nil '(6 7 8 9)))<br>
(def test-slice-iter (test-iter :slice 0 4))<br>
(assert-false (test-slice-iter :empty?))<br>
(assert-equal 0 (test-slice-iter :next!))<br>
(assert-equal 1 (test-slice-iter :next!))<br>
(assert-equal 2 (test-slice-iter :next!))<br>
(assert-equal 3 (test-slice-iter :next!))<br>
(assert-true (test-slice-iter :empty?))<br>
(def test-iter ((iterator::append-iter) :init '(0 1 2) '#(3 4 5) '(6 7 8 9)))<br>
(def test-slice-iter (test-iter :slice 7))<br>
(assert-false (test-slice-iter :empty?))<br>
(assert-equal 7 (test-slice-iter :next!))<br>
(assert-equal 8 (test-slice-iter :next!))<br>
(assert-equal 9 (test-slice-iter :next!))<br>
(assert-true (test-slice-iter :empty?))<br>
(def test-iter ((iterator::append-iter) :init '(0 1 2) '#(3 4 5) '(6 7 8 9)))<br>
(assert-false (test-iter :empty?))<br>
(assert-equal 10 (test-iter :count))<br>
(assert-true (test-iter :empty?))<br>
(def test-iter ((iterator::append-iter) :init '(0 1 2) '() '#(3 4 5) nil '(6 7 8 9)))<br>
(assert-false (test-iter :empty?))<br>
(assert-equal 10 (test-iter :count))<br>
(assert-true (test-iter :empty?))<br>
(def test-iter ((iterator::append-iter) :init nil '(0 1 2) (vec) '#(3 4 5) '(6 7 8 9) nil))<br>
(assert-false (test-iter :empty?))<br>
(assert-equal 10 (test-iter :count))<br>
(assert-true (test-iter :empty?))<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::append-to!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``append-to!``](#Namespace: iterator::append-to!-contents) | Type: Lambda |
| ``Namespace: iterator::append-to!`` | ``Usage: (append-to! ret &rest others)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-iter '(0 1 2))<br>
(append-to! test-iter '#(3 4 5) '(6 7 8 9))<br>
(set! test-iter (iter test-iter))<br>
(def test-slice-iter (test-iter :slice 3 7))<br>
(assert-false (test-slice-iter :empty?))<br>
(assert-equal 3 (test-slice-iter :next!))<br>
(assert-equal 4 (test-slice-iter :next!))<br>
(assert-equal 5 (test-slice-iter :next!))<br>
(assert-equal 6 (test-slice-iter :next!))<br>
(assert-true (test-slice-iter :empty?))<br>
(def test-iter '(0 1 2))<br>
(append-to! test-iter '#(3 4 5) '(6 7 8 9))<br>
(set! test-iter (iter test-iter))<br>
(def test-slice-iter (test-iter :slice 0 4))<br>
(assert-false (test-slice-iter :empty?))<br>
(assert-equal 0 (test-slice-iter :next!))<br>
(assert-equal 1 (test-slice-iter :next!))<br>
(assert-equal 2 (test-slice-iter :next!))<br>
(assert-equal 3 (test-slice-iter :next!))<br>
(assert-true (test-slice-iter :empty?))<br>
(def test-iter '(0 1 2))<br>
(append-to! test-iter '#(3 4 5) '(6 7 8 9))<br>
(set! test-iter (iter test-iter))<br>
(def test-slice-iter (test-iter :slice 7))<br>
(assert-false (test-slice-iter :empty?))<br>
(assert-equal 7 (test-slice-iter :next!))<br>
(assert-equal 8 (test-slice-iter :next!))<br>
(assert-equal 9 (test-slice-iter :next!))<br>
(assert-true (test-slice-iter :empty?))<br>
(def test-iter '(0 1 2))<br>
(append-to! test-iter '#(3 4 5) '(6 7 8 9))<br>
(set! test-iter (iter test-iter))<br>
(assert-false (test-iter :empty?))<br>
(assert-equal 10 (test-iter :count))<br>
(assert-true (test-iter :empty?))<br>
(def test-iter nil)<br>
(append-to! test-iter nil '(0 1 2) nil '#(3 4 5) '(6 7 8 9) nil)<br>
(set! test-iter (iter test-iter))<br>
(assert-false (test-iter :empty?))<br>
(assert-equal 10 (test-iter :count))<br>
(assert-true (test-iter :empty?))<br>
(def test-iter (vec 0))<br>
(append-to! test-iter nil '#(1) '(2 3) nil)<br>
(set! test-iter (iter test-iter))<br>
(assert-false (test-iter :empty?))<br>
(assert-equal 0 (test-iter :next!))<br>
(assert-equal 1 (test-iter :next!))<br>
(assert-equal 2 (test-iter :next!))<br>
(assert-equal 3 (test-iter :next!))<br>
(assert-true (test-iter :empty?))<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::collect" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``collect``](#Namespace: iterator::collect-contents) | Type: Lambda |
| ``Namespace: iterator::collect`` | ``Usage: (collect s)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def collect-test (iterator::collect '#(1 2 3)))<br>
(assert-true (list? collect-test))<br>
(assert-equal '(1 2 3) collect-test)<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::collect-str" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``collect-str``](#Namespace: iterator::collect-str-contents) | Type: Lambda |
| ``Namespace: iterator::collect-str`` | ``Usage: (collect-str s)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def collect-str-test (iterator::collect-str (iterator::map (fn (ch) (char-upper ch)) "λabc σ")))<br>
(assert-true (string? collect-str-test))<br>
(assert-equal "ΛABC Σ" collect-str-test)<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::collect-vec" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``collect-vec``](#Namespace: iterator::collect-vec-contents) | Type: Lambda |
| ``Namespace: iterator::collect-vec`` | ``Usage: (collect-vec s)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def collect-vec-test (iterator::collect-vec '(1 2 3)))<br>
(assert-true (vec? collect-vec-test))<br>
(assert-equal '#(1 2 3) collect-vec-test)<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::double-ended-iter?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``double-ended-iter?``](#Namespace: iterator::double-ended-iter?-contents) | Type: Lambda |
| ``Namespace: iterator::double-ended-iter?`` | ``Usage: (double-ended-iter? thing)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(struct::defstruct test-iter<br>
; fields<br>
(current 0)<br>
; methods<br>
(:fn next! (self) (do (def val current)(set! current (+ 1 current)) val))<br>
(:fn empty? (self) (>= current 3))<br>
(:impl iterator::iterator))<br>
(assert-true (iterator::double-ended-iter? (iterator::iter '(1 2 3))))<br>
(assert-false (iterator::double-ended-iter? '(1 2 3)))<br>
(assert-false (iterator::double-ended-iter? (test-iter)))<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::double-ended-iterator" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``double-ended-iterator``](#Namespace: iterator::double-ended-iterator-contents) | Type: Lambda |
| ``Namespace: iterator::double-ended-iterator`` | ``Usage: (defstruct iter (:fn next! (self)...)(:fn next-back! (self)...)(:fn empty? (self)...)(:impl iterator::iterator iterator::double-ended-iterator))`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(ns-import 'struct)<br>
(ns-import 'iterator)<br>
(defstruct test-double-iter<br>
; fields<br>
(current 0)<br>
(current-end 2)<br>
; methods<br>
(:fn next! (self) (do (var val current)(set! current (+ 1 current)) val))<br>
(:fn next-back! (self) (do (var val current-end)(set! current-end (- current-end 1)) val))<br>
(:fn empty? (self) (> current current-end))<br>
(:impl iterator::iterator iterator::double-ended-iterator))<br>
(def tmap (test-double-iter))<br>
(assert-false (tmap :empty?))<br>
(assert-equal 0 (tmap :next!))<br>
(assert-equal 1 (tmap :next!))<br>
(assert-equal 2 (tmap :next!))<br>
(assert-true (tmap :empty?))<br>
(def tmap (test-double-iter))<br>
(assert-false (tmap :empty?))<br>
(assert-equal 2 (tmap :next-back!))<br>
(assert-equal 1 (tmap :next-back!))<br>
(assert-equal 0 (tmap :next-back!))<br>
(assert-true (tmap :empty?))<br>
(def tmap (test-double-iter))<br>
(assert-false (tmap :empty?))<br>
(assert-equal 0 (tmap :next!))<br>
(assert-equal 2 (tmap :next-back!))<br>
(assert-equal 1 (tmap :next-back!))<br>
(assert-true (tmap :empty?))<br>
; :nth-back! Example<br>
(def tmap ((vec-iter) :init '#(0 1 2 3 4) 0))<br>
(assert-false (tmap :empty?))<br>
(assert-equal 4 (tmap :nth-back! 0))<br>
(assert-equal 3 (tmap :nth-back! 0))<br>
(assert-equal 0 (tmap :nth-back! 2))<br>
(assert-true (tmap :empty?))<br>
; :reverse Example<br>
(def tmap ((test-double-iter) :reverse))<br>
(assert-false (tmap :empty?))<br>
(assert-equal 2 (tmap :next!))<br>
(assert-equal 1 (tmap :next!))<br>
(assert-equal 0 (tmap :next!))<br>
(assert-true (tmap :empty?))<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::empty?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``empty?``](#Namespace: iterator::empty?-contents) | Type: Lambda |
| ``Namespace: iterator::empty?`` | ``Usage: (empty? s)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-true (iterator::empty? nil))<br>
(assert-true (iterator::empty? '#()))<br>
(assert-false (iterator::empty? '#(1)))<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::file-iter" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``file-iter``](#Namespace: iterator::file-iter-contents) | Type: Lambda |
| ``Namespace: iterator::file-iter`` | ``Usage: (file-iter)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def tst-file (open "/tmp/file-iter-test.txt" :create :truncate))<br>
(write-line tst-file "line 1")<br>
(write-line tst-file "line 2")<br>
(write-line tst-file "line 3")<br>
(write-string tst-file "line 4")<br>
(close tst-file)<br>
(def test-iter ((iterator::file-iter) :init (open "/tmp/file-iter-test.txt")))<br>
(assert-false (test-iter :empty?))<br>
(assert-equal "line 1\n" (test-iter :next!))<br>
(assert-equal "line 2\n" (test-iter :next!))<br>
(assert-equal "line 3\n" (test-iter :next!))<br>
(assert-equal "line 4" (test-iter :next!))<br>
(assert-true (test-iter :empty?))<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::filter" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``filter``](#Namespace: iterator::filter-contents) | Type: Lambda |
| ``Namespace: iterator::filter`` | ``Usage: (filter predicate items)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-iter (iterator::filter (fn (x) (not (= x 2))) '(1 2 3)))<br>
(assert-false (test-iter :empty?))<br>
(assert-equal 1 (test-iter :next!))<br>
(assert-equal 3 (test-iter :next!))<br>
(assert-true (test-iter :empty?))<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::filter-iter" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``filter-iter``](#Namespace: iterator::filter-iter-contents) | Type: Lambda |
| ``Namespace: iterator::filter-iter`` | ``Usage: (filter-iter)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-iter (((iterator::list-iter) :init '(1 2 3)) :filter (fn (x) (not (= x 2)))))<br>
(assert-false (test-iter :empty?))<br>
(assert-equal 1 (test-iter :next!))<br>
(assert-equal 3 (test-iter :next!))<br>
(assert-true (test-iter :empty?))<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::for" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``for``](#Namespace: iterator::for-contents) | Type: Macro |
| ``Namespace: iterator::for`` | ``Usage: (for bind in items body)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def i 0)<br>
(iterator::for x in (iterator::range 11) (set! i (+ 1 i)))<br>
(assert-equal 11 i)<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::for-i" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``for-i``](#Namespace: iterator::for-i-contents) | Type: Macro |
| ``Namespace: iterator::for-i`` | ``Usage: (for-i idx-bind bind in items body)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def i 0)<br>
(def i-tot 0)<br>
(for-i idx x in '(1 2 3 4 5 6 7 8 9 10 11) (do (set! i-tot (+ idx i-tot))(set! i (+ 1 i))))<br>
(assert-equal 11 i)<br>
(assert-equal 55 i-tot)<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::iter" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``iter``](#Namespace: iterator::iter-contents) | Type: Lambda |
| ``Namespace: iterator::iter`` | ``Usage: (iter thing)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-true (iterator::iter? (iterator::iter '(1 2 3))))<br>
(assert-true (iterator::iter? (iterator::iter '#(1 2 3))))<br>
(assert-true (iterator::iter? (iterator::iter "abc")))<br>
(assert-true (iterator::iter? (iterator::iter (iterator::iter '(1 2 3)))))<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::iter?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``iter?``](#Namespace: iterator::iter?-contents) | Type: Lambda |
| ``Namespace: iterator::iter?`` | ``Usage: (iter? thing)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-true (iterator::iter? (iterator::iter '(1 2 3))))<br>
(assert-false (iterator::iter? '(1 2 3)))<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::iterator" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``iterator``](#Namespace: iterator::iterator-contents) | Type: Lambda |
| ``Namespace: iterator::iterator`` | ``Usage: (defstruct iter (:fn next! (self)...)(:fn empty? (self)...)(:impl iterator::iterator))`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(ns-import 'struct)<br>
(ns-import 'iterator)<br>
(defstruct test-iter<br>
; fields<br>
(current 0)<br>
; methods<br>
(:fn next! (self) (do (def val current)(set! current (+ 1 current)) val))<br>
(:fn empty? (self) (>= current 3))<br>
(:impl iterator::iterator))<br>
(def tmap (test-iter))<br>
(assert-false (tmap :empty?))<br>
(assert-equal 0 (tmap :next!))<br>
(assert-equal 1 (tmap :next!))<br>
(assert-equal 2 (tmap :next!))<br>
(assert-true (tmap :empty?))<br>
; :collect Example<br>
(def collect-test ((iter '#(1 2 3)) :collect))<br>
(assert-true (list? collect-test))<br>
(assert-equal '(1 2 3) collect-test)<br>
; :collect-vec Example<br>
(def collect-vec-test ((iter '(1 2 3)) :collect-vec))<br>
(assert-true (vec? collect-vec-test))<br>
(assert-equal '#(1 2 3) collect-vec-test)<br>
; :collect-str Example<br>
(def collect-str-test (((iter "λabc σ") :map (fn (ch) (char-upper ch))) :collect-str))<br>
(assert-true (string? collect-str-test))<br>
(assert-equal "ΛABC Σ" collect-str-test)<br>
; :map Example<br>
(def tmap ((test-iter) :map (fn (x) (+ 1 x))))<br>
(assert-false (tmap :empty?))<br>
(assert-equal 1 (tmap :next!))<br>
(assert-equal 2 (tmap :next!))<br>
(assert-equal 3 (tmap :next!))<br>
(assert-true (tmap :empty?))<br>
; :filter Example<br>
(def tmap ((test-iter) :filter (fn (x) (not (= x 1)))))<br>
(assert-false (tmap :empty?))<br>
(assert-equal 0 (tmap :next!))<br>
(assert-equal 2 (tmap :next!))<br>
(assert-true (tmap :empty?))<br>
; :count Example<br>
(def tmap (test-iter))<br>
(assert-false (tmap :empty?))<br>
(assert-equal 3 (tmap :count))<br>
(assert-true (tmap :empty?))<br>
; :nth! Example<br>
(def tmap ((list-iter) :init '(0 1 2 3 4)))<br>
(assert-false (tmap :empty?))<br>
(assert-equal 0 (tmap :nth! 0))<br>
(assert-equal 1 (tmap :nth! 0))<br>
(assert-equal 4 (tmap :nth! 2))<br>
(assert-true (tmap :empty?))<br>
; :double-ended? Example<br>
(ns-import 'struct)<br>
(ns-import 'iterator)<br>
(defstruct test-double-iter<br>
; fields<br>
(current 0)<br>
(current-end 2)<br>
; methods<br>
(:fn next! (self) (do (def val current)(set! current (+ 1 current)) val))<br>
(:fn next-back! (self) (do (def val current-end)(set! current-end (- current-end 1)) val))<br>
(:fn empty? (self) (> current current-end))<br>
(:impl iterator::iterator iterator::double-ended-iterator))<br>
(assert-false ((test-iter) :double-ended?))<br>
(assert-true ((test-double-iter) :double-ended?))<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::list-iter" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``list-iter``](#Namespace: iterator::list-iter-contents) | Type: Lambda |
| ``Namespace: iterator::list-iter`` | ``Usage: (list-iter)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-list-iter ((list-iter) :init '(1 2 3)))<br>
(assert-false (test-list-iter :empty?))<br>
(assert-equal 1 (test-list-iter :next!))<br>
(assert-equal 2 (test-list-iter :next!))<br>
(assert-equal 3 (test-list-iter :next!))<br>
(assert-true (test-list-iter :empty?))<br>
(def test-list-iter ((list-iter) :init '(1 2 3)))<br>
(assert-false (test-list-iter :empty?))<br>
(assert-equal 1 (test-list-iter :next!))<br>
(assert-equal 3 (test-list-iter :next-back!))<br>
(assert-equal 2 (test-list-iter :next!))<br>
(assert-true (test-list-iter :empty?))<br>
(def test-list-iter ((list-iter) :init '(1 2 3)))<br>
(assert-false (test-list-iter :empty?))<br>
(assert-equal 3 (test-list-iter :next-back!))<br>
(assert-equal 2 (test-list-iter :next-back!))<br>
(assert-equal 1 (test-list-iter :next-back!))<br>
(assert-true (test-list-iter :empty?))<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::map" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``map``](#Namespace: iterator::map-contents) | Type: Lambda |
| ``Namespace: iterator::map`` | ``Usage: (map map-fn items)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def tmap (iterator::map (fn (x) (+ 1 x)) '(0 1 2)))<br>
(assert-false (tmap :empty?))<br>
(assert-equal 1 (tmap :next!))<br>
(assert-equal 2 (tmap :next!))<br>
(assert-equal 3 (tmap :next!))<br>
(assert-true (tmap :empty?))<br>
(def tmap (iterator::reverse (iterator::map (fn (x) (+ 1 x)) '(0 1 2))))<br>
(assert-false (tmap :empty?))<br>
(assert-equal 3 (tmap :next!))<br>
(assert-equal 2 (tmap :next!))<br>
(assert-equal 1 (tmap :next!))<br>
(assert-true (tmap :empty?))<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::map-iter" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``map-iter``](#Namespace: iterator::map-iter-contents) | Type: Lambda |
| ``Namespace: iterator::map-iter`` | ``Usage: (map-iter)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-map-iter (((iterator::list-iter) :init '(1 2 3)) :map (fn (x) (* x 2))))<br>
(assert-false (test-map-iter :empty?))<br>
(assert-equal 2 (test-map-iter :next!))<br>
(assert-equal 4 (test-map-iter :next!))<br>
(assert-equal 6 (test-map-iter :next!))<br>
(assert-true (test-map-iter :empty?))<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::next!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``next!``](#Namespace: iterator::next!-contents) | Type: Lambda |
| ``Namespace: iterator::next!`` | ``Usage: (next! s)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-equal 1 (iterator::next! '(1 2 3)))<br>
(assert-equal 1 (iterator::next! '#(1 2 3)))<br>
(def next-test (iterator::iter '(4 5 6)))<br>
(assert-equal 4 (iterator::next! next-test))<br>
(assert-equal 5 (iterator::next! next-test))<br>
(assert-equal 6 (iterator::next! next-test))<br>
(assert-true (next-test :empty?))<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::nth" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``nth``](#Namespace: iterator::nth-contents) | Type: Lambda |
| ``Namespace: iterator::nth`` | ``Usage: (nth idx coll)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def tmap ((list-iter) :init '(0 1 2 3 4)))<br>
(assert-false (tmap :empty?))<br>
(assert-equal 0 (nth 0 tmap))<br>
(assert-equal 1 (nth 0 tmap))<br>
(assert-equal 4 (nth 2 tmap))<br>
(assert-true (tmap :empty?))<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::range" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``range``](#Namespace: iterator::range-contents) | Type: Lambda |
| ``Namespace: iterator::range`` | ``Usage: (range &rest i)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-iter (iterator::range 3 6))<br>
(assert-false (test-iter :empty?))<br>
(assert-equal 3 (test-iter :next!))<br>
(assert-equal 4 (test-iter :next!))<br>
(assert-equal 5 (test-iter :next!))<br>
(assert-equal 6 (test-iter :next!))<br>
(assert-true (test-iter :empty?))<br>
(def test-iter (iterator::range 3))<br>
(assert-false (test-iter :empty?))<br>
(assert-equal 0 (test-iter :next!))<br>
(assert-equal 1 (test-iter :next!))<br>
(assert-equal 2 (test-iter :next!))<br>
(assert-true (test-iter :empty?))<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::range-iter" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``range-iter``](#Namespace: iterator::range-iter-contents) | Type: Lambda |
| ``Namespace: iterator::range-iter`` | ``Usage: (range-iter)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-iter ((range-iter) :init 3 6))<br>
(assert-false (test-iter :empty?))<br>
(assert-equal 3 (test-iter :next!))<br>
(assert-equal 4 (test-iter :next!))<br>
(assert-equal 5 (test-iter :next!))<br>
(assert-equal 6 (test-iter :next!))<br>
(assert-true (test-iter :empty?))<br>
(def test-iter ((range-iter) :init 3 6))<br>
(assert-false (test-iter :empty?))<br>
(assert-equal 6 (test-iter :next-back!))<br>
(assert-equal 5 (test-iter :next-back!))<br>
(assert-equal 4 (test-iter :next-back!))<br>
(assert-equal 3 (test-iter :next-back!))<br>
(assert-true (test-iter :empty?))<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::reduce" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``reduce``](#Namespace: iterator::reduce-contents) | Type: Lambda |
| ``Namespace: iterator::reduce`` | ``Usage: (reduce reducing-fcn init-val coll)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-true (= 15 (reduce + 0 (list 1 2 3 4 5))))<br>
(assert-false (= 15 (reduce + 1 (list 1 2 3 4 5))))<br>
(assert-true (= "one hoopy frood" (reduce str "" (list "one " "hoopy " "frood"))))<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::reduce-times" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``reduce-times``](#Namespace: iterator::reduce-times-contents) | Type: Lambda |
| ``Namespace: iterator::reduce-times`` | ``Usage: (reduce-times value wrapping-fcn times)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-equal (list (list 3)) (reduce-times 3 list 2))<br>
(assert-equal 5 (reduce-times (reduce-times 5 list 5) first 5))<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::reverse" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``reverse``](#Namespace: iterator::reverse-contents) | Type: Lambda |
| ``Namespace: iterator::reverse`` | ``Usage: (reverse items)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def tmap (reverse ((vec-iter) :init '#(0 1 2) 0)))<br>
(assert-false (tmap :empty?))<br>
(assert-equal 2 (tmap :next!))<br>
(assert-equal 1 (tmap :next!))<br>
(assert-equal 0 (tmap :next!))<br>
(assert-true (tmap :empty?))<br>
(assert-error (reverse "string"))<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::reverse-iter" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``reverse-iter``](#Namespace: iterator::reverse-iter-contents) | Type: Lambda |
| ``Namespace: iterator::reverse-iter`` | ``Usage: (reverse-iter)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-iter (((vec-iter) :init '#(1 2 3) 0) :reverse))<br>
(assert-false (test-iter :empty?))<br>
(assert-equal 3 (test-iter :next!))<br>
(assert-equal 2 (test-iter :next!))<br>
(assert-equal 1 (test-iter :next!))<br>
(assert-true (test-iter :empty?))<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::slice" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``slice``](#Namespace: iterator::slice-contents) | Type: Lambda |
| ``Namespace: iterator::slice`` | ``Usage: (slice items start &rest end)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-slice-iter (slice '(0 1 2 3 4 5 6 7 8 9) 3 6))<br>
(assert-false (test-slice-iter :empty?))<br>
(assert-equal 3 (test-slice-iter :next!))<br>
(assert-equal 4 (test-slice-iter :next!))<br>
(assert-equal 5 (test-slice-iter :next!))<br>
(assert-true (test-slice-iter :empty?))<br>
(def test-slice-iter (slice '(0 1 2 3 4 5 6 7 8 9) 0 3))<br>
(assert-false (test-slice-iter :empty?))<br>
(assert-equal 0 (test-slice-iter :next!))<br>
(assert-equal 1 (test-slice-iter :next!))<br>
(assert-equal 2 (test-slice-iter :next!))<br>
(assert-true (test-slice-iter :empty?))<br>
(def test-slice-iter (slice '(0 1 2 3 4 5 6 7 8 9) 7))<br>
(assert-false (test-slice-iter :empty?))<br>
(assert-equal 7 (test-slice-iter :next!))<br>
(assert-equal 8 (test-slice-iter :next!))<br>
(assert-equal 9 (test-slice-iter :next!))<br>
(assert-true (test-slice-iter :empty?))<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::slice-iter" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``slice-iter``](#Namespace: iterator::slice-iter-contents) | Type: Lambda |
| ``Namespace: iterator::slice-iter`` | ``Usage: (slice-iter)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-slice-iter (((iterator::list-iter) :init '(0 1 2 3 4 5 6 7 8 9)) :slice 3 6))<br>
(assert-false (test-slice-iter :empty?))<br>
(assert-equal 3 (test-slice-iter :next!))<br>
(assert-equal 4 (test-slice-iter :next!))<br>
(assert-equal 5 (test-slice-iter :next!))<br>
(assert-true (test-slice-iter :empty?))<br>
(def test-slice-iter (((iterator::list-iter) :init '(0 1 2 3 4 5 6 7 8 9)) :slice 0 3))<br>
(assert-false (test-slice-iter :empty?))<br>
(assert-equal 0 (test-slice-iter :next!))<br>
(assert-equal 1 (test-slice-iter :next!))<br>
(assert-equal 2 (test-slice-iter :next!))<br>
(assert-true (test-slice-iter :empty?))<br>
(def test-slice-iter (((iterator::list-iter) :init '(0 1 2 3 4 5 6 7 8 9)) :slice 7))<br>
(assert-false (test-slice-iter :empty?))<br>
(assert-equal 7 (test-slice-iter :next!))<br>
(assert-equal 8 (test-slice-iter :next!))<br>
(assert-equal 9 (test-slice-iter :next!))<br>
(assert-true (test-slice-iter :empty?))<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::string-iter" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``string-iter``](#Namespace: iterator::string-iter-contents) | Type: Lambda |
| ``Namespace: iterator::string-iter`` | ``Usage: (string-iter)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-string-iter ((string-iter) :init "123"))<br>
(assert-false (test-string-iter :empty?))<br>
(assert-equal #\1 (test-string-iter :next!))<br>
(assert-equal #\2 (test-string-iter :next!))<br>
(assert-equal #\3 (test-string-iter :next!))<br>
(assert-true (test-string-iter :empty?))<br>
<br>
</code>
</details>


| <a id="Namespace: iterator::vec-iter" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-iter``](#Namespace: iterator::vec-iter-contents) | Type: Lambda |
| ``Namespace: iterator::vec-iter`` | ``Usage: (vec-iter)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-vec-iter ((vec-iter) :init '#(1 2 3) 0))<br>
(assert-false (test-vec-iter :empty?))<br>
(assert-equal 1 (test-vec-iter :next!))<br>
(assert-equal 2 (test-vec-iter :next!))<br>
(assert-equal 3 (test-vec-iter :next!))<br>
(assert-true (test-vec-iter :empty?))<br>
(def test-vec-iter ((vec-iter) :init (vec) 0))<br>
(assert-true (test-vec-iter :empty?))<br>
<br>
</code>
</details>
### <a id="Math forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Math forms](#Math forms-contents)



| <a id="Namespace: root::%" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``%``](#Namespace: root::%-contents) | Type: Function |
| ``Namespace: root::%`` | ``Usage: (% int int)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal 0 (% 50 10))<br>
(test::assert-equal 5 (% 55 10))<br>
(test::assert-equal 1 (% 1 2))<br>
<br>
</code>
</details>


| <a id="Namespace: root::*" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*``](#Namespace: root::*-contents) | Type: Function |
| ``Namespace: root::*`` | ``Usage: (* number+)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal 5 (* 5))<br>
(test::assert-equal 5 (* 1 5))<br>
(test::assert-equal 5.0 (* 1.0 5))<br>
(test::assert-equal 7.5 (* 1.5 5))<br>
(test::assert-equal 7.5 (* 1.5 5.0))<br>
(test::assert-equal 15 (* 3 5))<br>
(test::assert-equal 8 (* 1 2 4))<br>
(test::assert-equal 16 (* 2 2 4))<br>
(test::assert-equal 16.0 (* 2 2.0 4))<br>
(test::assert-equal 16.0 (* 2.0 2.0 4.0))<br>
<br>
</code>
</details>


| <a id="Namespace: root::+" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``+``](#Namespace: root::+-contents) | Type: Function |
| ``Namespace: root::+`` | ``Usage: (+ number+)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal 5 (+ 5))<br>
(test::assert-equal 5 (+ 5.0))<br>
(test::assert-equal 6 (+ 1 5))<br>
(test::assert-equal 6.5 (+ 1 5.5))<br>
(test::assert-equal 7 (+ 1 2 4))<br>
<br>
</code>
</details>


| <a id="Namespace: root::-" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``-``](#Namespace: root::--contents) | Type: Function |
| ``Namespace: root::-`` | ``Usage: (- number+)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal 5 (- 5))<br>
(test::assert-equal 5 (- 5.0))<br>
(test::assert-equal -4 (- 1 5))<br>
(test::assert-equal -4.5 (- 1 5.5))<br>
(test::assert-equal 4 (- 10 2 4))<br>
(test::assert-equal 4.9 (- 10.9 2 4))<br>
<br>
</code>
</details>


| <a id="Namespace: root::/" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``/``](#Namespace: root::/-contents) | Type: Function |
| ``Namespace: root::/`` | ``Usage: (/ number+)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal 5 (/ 50 10))<br>
(test::assert-equal 5 (/ 50.0 10.0))<br>
(test::assert-equal 0 (/ 1 5))<br>
(test::assert-equal .2 (/ 1.0 5))<br>
(test::assert-equal .2 (/ 1.0 5.0))<br>
(test::assert-equal 5.5 (/ 5.5 1))<br>
(test::assert-equal 2 (/ 16 2 4))<br>
(test::assert-equal 5 (/ 100 2 5 2))<br>
<br>
</code>
</details>
### <a id="Namespace forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Namespace forms](#Namespace forms-contents)



| <a id="Namespace: root::ns-auto-export" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-auto-export``](#Namespace: root::ns-auto-export-contents) | Type: Macro |
| ``Namespace: root::ns-auto-export`` | ``Usage: (ns-auto-export symbol)`` |

<br>


| <a id="Namespace: root::ns-create" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-create``](#Namespace: root::ns-create-contents) | Type: Function |
| ``Namespace: root::ns-create`` | ``Usage: (ns-create namespace)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(ns-push 'test-ns-create)<br>
(def test-ns-enter *ns*)<br>
(ns-create 'ns-create-test-namespace)<br>
(def test-symbol "testing")<br>
(test::assert-equal "testing" test-symbol)<br>
(ns-enter test-ns-create::test-ns-enter)<br>
(test::assert-false (def? test-symbol))<br>
(ns-pop)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::ns-enter" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-enter``](#Namespace: root::ns-enter-contents) | Type: Function |
| ``Namespace: root::ns-enter`` | ``Usage: (ns-enter namespace)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(ns-push 'test-ns-enter)<br>
(def test-ns-enter *ns*)<br>
(ns-create 'ns-enter-test-namespace)<br>
(def test-symbol "testing")<br>
(test::assert-equal "testing" test-symbol)<br>
(ns-enter test-ns-enter::test-ns-enter)<br>
(test::assert-false (def? test-symbol))<br>
(ns-enter 'ns-enter-test-namespace)<br>
(test::assert-true (def? test-symbol))<br>
(test::assert-equal "testing" test-symbol)<br>
(ns-enter test-ns-enter::test-ns-enter)<br>
(ns-pop)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::ns-exists?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-exists?``](#Namespace: root::ns-exists?-contents) | Type: Function |
| ``Namespace: root::ns-exists?`` | ``Usage: (ns-exists? namespace)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-false (ns-exists? 'ns-exists-test-namespace))<br>
(ns-push 'ns-exists-test-namespace)<br>
(ns-pop)<br>
(test::assert-true (ns-exists? 'ns-exists-test-namespace))<br>
<br>
</code>
</details>


| <a id="Namespace: root::ns-export" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-export``](#Namespace: root::ns-export-contents) | Type: Macro |
| ``Namespace: root::ns-export`` | ``Usage: (ns-export symbol_or_sequence)`` |

<br>


| <a id="Namespace: root::ns-import" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-import``](#Namespace: root::ns-import-contents) | Type: Macro |
| ``Namespace: root::ns-import`` | ``Usage: (ns-import namespace)`` |

<br>


| <a id="Namespace: root::ns-list" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-list``](#Namespace: root::ns-list-contents) | Type: Function |
| ``Namespace: root::ns-list`` | ``Usage: (ns-list)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-not-includes "ns-list-test-namespace" (ns-list))<br>
(ns-push 'ns-list-test-namespace)<br>
(ns-pop)<br>
(test::assert-includes "ns-list-test-namespace" (ns-list))<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::ns-pop" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-pop``](#Namespace: root::ns-pop-contents) | Type: Macro |
| ``Namespace: root::ns-pop`` | ``Usage: (ns-pop)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-ns-pop *ns*)<br>
(ns-push 'ns-pop-test-namespace)<br>
(test::assert-equal "ns-pop-test-namespace" *active-ns*)<br>
(ns-pop)<br>
(test::assert-equal test-ns-pop *ns*)<br>
<br>
</code>
</details>


| <a id="Namespace: root::ns-push" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-push``](#Namespace: root::ns-push-contents) | Type: Macro |
| ``Namespace: root::ns-push`` | ``Usage: (ns-push 'namespace)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-ns-push *ns*)<br>
(ns-push 'ns-pop-test-namespace)<br>
; *ns* will not be right...<br>
(test::assert-equal "ns-pop-test-namespace" *active-ns*)<br>
(ns-push 'ns-pop-test-namespace2)<br>
(test::assert-equal "ns-pop-test-namespace2" *active-ns*)<br>
(ns-pop)<br>
(test::assert-equal "ns-pop-test-namespace" *active-ns*)<br>
(ns-pop)<br>
(test::assert-equal test-ns-push *ns*)<br>
<br>
</code>
</details>


| <a id="Namespace: root::ns-symbols" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-symbols``](#Namespace: root::ns-symbols-contents) | Type: Function |
| ``Namespace: root::ns-symbols`` | ``Usage: (ns-symbols namespace)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-includes 'loop (ns-symbols 'root))<br>
(test::assert-not-includes 'dumb-symbol-xxx (ns-symbols 'root))<br>
(test::assert-includes 'car (ns-symbols 'root))<br>
t<br>
<br>
</code>
</details>
### <a id="Pair forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Pair forms](#Pair forms-contents)
Operations on the 'Pair' type (aka Cons Cell) that can be used to create
traditional Lisp list structures. These are the default list structure and
are produced with bare parentheses in code. These lists can also be created by
building them up with joins or with the list form.


| <a id="Namespace: root::car" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``car``](#Namespace: root::car-contents) | Type: Function |
| ``Namespace: root::car`` | ``Usage: (car pair)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def tst-pairs-two (list 'x 'y 'z))<br>
(test::assert-equal 'x (car tst-pairs-two))<br>
(test::assert-equal 10 (car '(10)))<br>
(test::assert-equal 9 (car '(9 11 13)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::cdr" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cdr``](#Namespace: root::cdr-contents) | Type: Function |
| ``Namespace: root::cdr`` | ``Usage: (cdr pair)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def tst-pairs-three (list 'x 'y 'z))<br>
(test::assert-equal '(y z) (cdr tst-pairs-three))<br>
(test::assert-equal nil (cdr '(10)))<br>
(test::assert-equal '(13) (cdr '(9 13)))<br>
(test::assert-equal '(11 13) (cdr '(9 11 13)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::join" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``join``](#Namespace: root::join-contents) | Type: Function |
| ``Namespace: root::join`` | ``Usage: (join car cdr)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def tst-pair-one (join 1 2))<br>
(test::assert-equal 1 (car tst-pair-one))<br>
(test::assert-equal 2 (cdr tst-pair-one))<br>
(test::assert-equal '(1 2 3) (join 1 (join 2 (join 3 nil))))<br>
<br>
</code>
</details>


| <a id="Namespace: root::list" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``list``](#Namespace: root::list-contents) | Type: Function |
| ``Namespace: root::list`` | ``Usage: (list item0 item1 .. itemN)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal '(1 2 3) (list 1 2 3))<br>
<br>
</code>
</details>


| <a id="Namespace: root::xar!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``xar!``](#Namespace: root::xar!-contents) | Type: Function |
| ``Namespace: root::xar!`` | ``Usage: (xar! pair expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def tst-pairs-three (list 'x 'y 'z))<br>
(test::assert-equal '(x y z) tst-pairs-three)<br>
(test::assert-equal '(s y z) (xar! tst-pairs-three 's))<br>
(test::assert-equal '(s y z) tst-pairs-three)<br>
(def tst-pairs-four nil)<br>
(test::assert-equal '() tst-pairs-four)<br>
(test::assert-equal '(t) (xar! tst-pairs-four 't))<br>
(test::assert-equal '(t) tst-pairs-four)<br>
<br>
</code>
</details>


| <a id="Namespace: root::xdr!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``xdr!``](#Namespace: root::xdr!-contents) | Type: Function |
| ``Namespace: root::xdr!`` | ``Usage: (xdr! pair expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def tst-pairs-five (list 'a 'b 'c))<br>
(test::assert-equal '(a b c) tst-pairs-five)<br>
(test::assert-equal '(a y z) (xdr! tst-pairs-five '(y z)))<br>
(test::assert-equal '(a y z) tst-pairs-five)<br>
(def tst-pairs-six nil)<br>
(test::assert-equal '() tst-pairs-six)<br>
(test::assert-equal '(nil . v) (xdr! tst-pairs-six 'v))<br>
(test::assert-equal '(nil . v) tst-pairs-six)<br>
<br>
</code>
</details>
### <a id="pair-ext forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[pair-ext forms](#pair-ext forms-contents)



| <a id="Namespace: root::caaar" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``caaar``](#Namespace: root::caaar-contents) | Type: Lambda |
| ``Namespace: root::caaar`` | ``Usage: (caaar lst)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-equal 1 (caaar '(((1 4) 5) (6 3) 2)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::caadr" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``caadr``](#Namespace: root::caadr-contents) | Type: Lambda |
| ``Namespace: root::caadr`` | ``Usage: (caadr lst)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-equal 6 (caadr '((1 4 5) (6 3) 2)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::caar" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``caar``](#Namespace: root::caar-contents) | Type: Lambda |
| ``Namespace: root::caar`` | ``Usage: (caar lst)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-equal 1 (caar '((1) 2 3)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::cadar" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cadar``](#Namespace: root::cadar-contents) | Type: Lambda |
| ``Namespace: root::cadar`` | ``Usage: (cadar lst)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-equal 4 (cadar '((1 4 5) (6 3) 2)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::cadddr" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cadddr``](#Namespace: root::cadddr-contents) | Type: Lambda |
| ``Namespace: root::cadddr`` | ``Usage: (cadddr lst)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-equal 6 (cadddr '((1 7 8) (4 5) 2 6 (3 9))))<br>
<br>
</code>
</details>


| <a id="Namespace: root::caddr" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``caddr``](#Namespace: root::caddr-contents) | Type: Lambda |
| ``Namespace: root::caddr`` | ``Usage: (caddr lst)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-equal 6 (caddr '((1 4 5) 2 6)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::cadr" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cadr``](#Namespace: root::cadr-contents) | Type: Lambda |
| ``Namespace: root::cadr`` | ``Usage: (cadr lst)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-equal 2 (cadr '(1 2 3)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::cdaar" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cdaar``](#Namespace: root::cdaar-contents) | Type: Lambda |
| ``Namespace: root::cdaar`` | ``Usage: (cdaar lst)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-equal '(7 8) (cdaar '(((1 7 8) 4 5) 2 (6 3))))<br>
<br>
</code>
</details>


| <a id="Namespace: root::cdadr" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cdadr``](#Namespace: root::cdadr-contents) | Type: Lambda |
| ``Namespace: root::cdadr`` | ``Usage: (cdadr lst)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-equal '(9) (cdadr '(((1 7 8) 4 5) (2 9) (6 3))))<br>
<br>
</code>
</details>


| <a id="Namespace: root::cdar" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cdar``](#Namespace: root::cdar-contents) | Type: Lambda |
| ``Namespace: root::cdar`` | ``Usage: (cdar lst)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-equal '(4 5) (cdar '((1 4 5) 2 3)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::cddar" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cddar``](#Namespace: root::cddar-contents) | Type: Lambda |
| ``Namespace: root::cddar`` | ``Usage: (cddar lst)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-equal '(5) (cddar '(((1 7 8) 4 5) 2 (6 3))))<br>
<br>
</code>
</details>


| <a id="Namespace: root::cdddr" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cdddr``](#Namespace: root::cdddr-contents) | Type: Lambda |
| ``Namespace: root::cdddr`` | ``Usage: (cdddr lst)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-equal '(3 9) (cdddr '(((1 7 8) 4 5) 2 6 3 9)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::cddr" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cddr``](#Namespace: root::cddr-contents) | Type: Lambda |
| ``Namespace: root::cddr`` | ``Usage: (cddr lst)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-equal '(3) (cddr '((1 4 5) 2 3)))<br>
<br>
</code>
</details>
### <a id="Scripting forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Scripting forms](#Scripting forms-contents)



| <a id="Namespace: root::*load-path*" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*load-path*``](#Namespace: root::*load-path*-contents) | Type: Vector |
| ``Namespace: root::*load-path*`` | ``Usage: (set '*load-path* '("/path/one" "/path/two"))`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(set '*load-path '("/path"))<br>
;(load "script-in-path")<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::load" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``load``](#Namespace: root::load-contents) | Type: Function |
| ``Namespace: root::load`` | ``Usage: (load path) -> [last form value]`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-load-one nil)<br>
(def test-load-two nil)<br>
(def test-load-fn (open "/tmp/slsh-test-load.testing" :create :truncate))<br>
(write-line test-load-fn "(set! test-load-one \"LOAD TEST\") '(1 2 3)")<br>
(close test-load-fn)<br>
(set! test-load-two (load "/tmp/slsh-test-load.testing"))<br>
(test::assert-equal "LOAD TEST" test-load-one)<br>
(test::assert-equal '(1 2 3) test-load-two)<br>
<br>
</code>
</details>


| <a id="Namespace: shell::mkli" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``mkli``](#Namespace: shell::mkli-contents) | Type: Lambda |
| ``Namespace: shell::mkli`` | ``Usage: (mkli filepath [namespace] [body])`` |

<br>
### <a id="Sequence forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Sequence forms](#Sequence forms-contents)
These macros will work on either a vector or a pair made into a proper list
(cons list).  Use these in preference to the vector/list specific versions when
possible (i.e. first vs car).
NOTE: list on this table can be a vector or a list.


| <a id="Namespace: root::butlast" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``butlast``](#Namespace: root::butlast-contents) | Type: Lambda |
| ``Namespace: root::butlast`` | ``Usage: (butlast obj)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-equal '(1 2) (butlast '(1 2 3)))<br>
(assert-equal '(1 2) (butlast '#(1 2 3)))<br>
(assert-equal nil (butlast '(1)))<br>
(assert-equal nil (butlast '#(1)))<br>
(assert-equal nil (butlast '()))<br>
(assert-equal nil (butlast nil))<br>
(assert-equal nil (butlast '#()))<br>
<br>
</code>
</details>


| <a id="Namespace: root::collect-copy" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``collect-copy``](#Namespace: root::collect-copy-contents) | Type: Lambda |
| ``Namespace: root::collect-copy`` | ``Usage: (collect-copy seq)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-colcl '(1 2 3))<br>
(assert-true (list? test-colcl))<br>
(def test-colcl2 (collect-copy test-colcl))<br>
(assert-true (list? test-colcl2))<br>
(assert-equal test-colcl test-colcl2)<br>
(def test-colcv '#(1 2 3))<br>
(assert-true (vec? test-colcv))<br>
(def test-colcv2 (collect-copy test-colcv))<br>
(assert-true (vec? test-colcv2))<br>
(assert-equal test-colcv test-colcv2)<br>
<br>
</code>
</details>


| <a id="Namespace: root::empty-seq?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``empty-seq?``](#Namespace: root::empty-seq?-contents) | Type: Lambda |
| ``Namespace: root::empty-seq?`` | ``Usage: (empty-seq? obj) -> t/nil`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-false (empty-seq? '(1 2 3)))<br>
(test::assert-false (empty-seq? '#(1 2 3)))<br>
(test::assert-true (empty-seq? '()))<br>
(test::assert-true (empty-seq? '#()))<br>
(test::assert-false (empty-seq? "aaa"))<br>
(test::assert-false (empty-seq? 1))<br>
<br>
</code>
</details>


| <a id="Namespace: root::first" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``first``](#Namespace: root::first-contents) | Type: Lambda |
| ``Namespace: root::first`` | ``Usage: (first obj)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-equal 1 (first '(1 2 3)))<br>
(assert-equal 1 (first '#(1 2 3)))<br>
(assert-equal nil (first '()))<br>
(assert-equal nil (first nil))<br>
(assert-equal nil (first '#()))<br>
<br>
</code>
</details>


| <a id="Namespace: root::in?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``in?``](#Namespace: root::in?-contents) | Type: Lambda |
| ``Namespace: root::in?`` | ``Usage: (in? seq-to-search item-to-match)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(let ((vowels-list (list 'a 'e 'i 'o 'u)))<br>
(assert-true (in? vowels-list 'u))<br>
(assert-false (in? vowels-list 'c))<br>
(assert-true (in? (list (list)) (list)))<br>
(assert-false (in? 8 18)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::last" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``last``](#Namespace: root::last-contents) | Type: Lambda |
| ``Namespace: root::last`` | ``Usage: (last obj)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-equal 3 (last '(1 2 3)))<br>
(assert-equal 3 (last '#(1 2 3)))<br>
(assert-equal nil (last '()))<br>
(assert-equal nil (last nil))<br>
(assert-equal nil (last '#()))<br>
<br>
</code>
</details>


| <a id="Namespace: root::non-empty-seq?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``non-empty-seq?``](#Namespace: root::non-empty-seq?-contents) | Type: Lambda |
| ``Namespace: root::non-empty-seq?`` | ``Usage: (non-empty-seq? obj) -> t/nil`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (non-empty-seq? '(1 2 3)))<br>
(test::assert-true (non-empty-seq? '#(1 2 3)))<br>
(test::assert-false (non-empty-seq? '()))<br>
(test::assert-false (non-empty-seq? '#()))<br>
(test::assert-false (non-empty-seq? "aaa"))<br>
(test::assert-false (non-empty-seq? 1))<br>
<br>
</code>
</details>


| <a id="Namespace: root::qsort" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``qsort``](#Namespace: root::qsort-contents) | Type: Lambda |
| ``Namespace: root::qsort`` | ``Usage: (qsort sequence comp-lambda?) -> [sorted vector]`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal '(1 2 3) (qsort '(2 3 1)))<br>
(test::assert-equal '(1 2 3) (qsort '#(2 3 1)))<br>
(test::assert-equal '(3 2 1) (qsort '(2 3 1) >))<br>
(test::assert-equal '(3 2 1) (qsort '#(2 3 1) (fn (a b) (< b a))))<br>
(test::assert-equal '("aaa" "aab" "aba" "baa" "bab" "ccc")<br>
(qsort '("aaa" "aab" "aba" "baa" "bab" "ccc")))<br>
(test::assert-equal '("aaa" "aab" "aba" "baa" "bab" "ccc")<br>
(qsort '("ccc" "bab" "baa" "aba" "aab" "aaa")))<br>
(test::assert-equal '("aaa" "aab" "aba" "baa" "bab" "ccc")<br>
(qsort '("aba" "bab" "aab" "ccc" "baa" "aaa")))<br>
(test::assert-equal '("ccc" "bab" "baa" "aba" "aab" "aaa")<br>
(qsort '("aba" "bab" "aab" "ccc" "baa" "aaa") >))<br>
(test::assert-equal '("ccc" "bab" "baa" "aba" "aab" "aaa")<br>
(qsort '("aba" "bab" "aab" "ccc" "baa" "aaa") (fn (a b) (> a b))))<br>
(test::assert-equal '() (qsort '()))<br>
(test::assert-equal '() (qsort '#()))<br>
(test::assert-equal '#() (qsort '()))<br>
(test::assert-equal '#() (qsort '#()))<br>
<br>
</code>
</details>


| <a id="Namespace: root::rest" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``rest``](#Namespace: root::rest-contents) | Type: Lambda |
| ``Namespace: root::rest`` | ``Usage: (rest obj)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-equal '(2 3) (rest '(1 2 3)))<br>
(assert-equal '(2 3) (rest '#(1 2 3)))<br>
(assert-equal nil (rest '(1)))<br>
(assert-equal nil (rest '#(1)))<br>
(assert-equal nil (rest '()))<br>
(assert-equal nil (rest nil))<br>
(assert-equal nil (rest '#()))<br>
<br>
</code>
</details>


| <a id="Namespace: root::seq-for" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``seq-for``](#Namespace: root::seq-for-contents) | Type: Macro |
| ``Namespace: root::seq-for`` | ``Usage: (seq-for bind in items body)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def i 0)<br>
(seq-for x in '(1 2 3 4 5 6) (set! i (+ 1 i)))<br>
(assert-equal 6 i)<br>
<br>
</code>
</details>


| <a id="Namespace: root::seq?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``seq?``](#Namespace: root::seq?-contents) | Type: Lambda |
| ``Namespace: root::seq?`` | ``Usage: (seq? expression) -> t/nil`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (seq? '(1 2 3)))<br>
(test::assert-true (seq? '#(1 2 3)))<br>
(test::assert-true (seq? '()))<br>
(test::assert-true (seq? '#()))<br>
(test::assert-false (seq? "aaa"))<br>
(test::assert-false (seq? 1))<br>
<br>
</code>
</details>


| <a id="Namespace: root::setnth!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``setnth!``](#Namespace: root::setnth!-contents) | Type: Lambda |
| ``Namespace: root::setnth!`` | ``Usage: (setnth! idx obj sequence)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def vctr (vec 0 1 2 3))<br>
(def vec-copy (collect-copy vctr))<br>
(setnth! 0 -5 vctr)<br>
(setnth! 1 -400000 vctr)<br>
(setnth! 2 -402202 vctr)<br>
(setnth! 3 -30000 vctr)<br>
(assert-not-equal vec-copy vctr)<br>
(setnth! 0 -4 vctr)<br>
(setnth! 1 -3 vctr)<br>
(setnth! 2 -2 vctr)<br>
(setnth! 3 -1 vctr)<br>
(assert-equal (list -4 -3 -2 -1) vctr)<br>
(assert-equal '(1 5 3) (setnth! 1 5 '#(1 2 3)) " Vector check")<br>
(assert-error (setnth! 0 1 '#()))<br>
(assert-error (setnth! 0 1 (vec)))<br>
(def lst (list 0 1 2 3))<br>
(def list-copy (collect-copy lst))<br>
(setnth! 0 -4 lst)<br>
(setnth! 1 -3 lst)<br>
(setnth! 2 -2 lst)<br>
(setnth! 3 -1 lst)<br>
(assert-not-equal list-copy lst)<br>
(setnth! 0 -4 lst)<br>
(setnth! 1 -3 lst)<br>
(setnth! 2 -2 lst)<br>
(setnth! 3 -1 lst)<br>
(assert-equal (list -4 -3 -2 -1) lst)<br>
(assert-equal '(1 5 3) (setnth! 1 5 '(1 2 3)) " List check")<br>
(assert-error (setnth! 0 1 '()))<br>
(assert-error (setnth! 0 1 (list)))<br>
<br>
</code>
</details>
### <a id="Shell forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Shell forms](#Shell forms-contents)
Forms to do shell operations like file tests, pipes, redirects, etc.


| <a id="Namespace: root::*stderr*" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*stderr*``](#Namespace: root::*stderr*-contents) | Type: File |
| ``Namespace: root::*stderr*`` | ``Usage: (write-line *stderr*)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
; Use a file for stderr for test.<br>
(dyn *stderr* (open "/tmp/sl-sh.stderr.test" :create :truncate) (do (write-line *stderr* "Test Error") (close *stderr*)))<br>
(test::assert-equal "Test Error<br>
" (read-line (open "/tmp/sl-sh.stderr.test" :read)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::*stdin*" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*stdin*``](#Namespace: root::*stdin*-contents) | Type: File |
| ``Namespace: root::*stdin*`` | ``Usage: (read-line *stdin*)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def stdin-test (open "/tmp/sl-sh.stdin.test" :create :truncate))<br>
(write-line stdin-test "Test line")<br>
(close stdin-test)<br>
; Use a file for stdin for test.<br>
(dyn *stdin* (open "/tmp/sl-sh.stdin.test" :read) (do (test::assert-equal "Test line<br>
" (read-line *stdin*)) (close *stdin*)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::*stdout*" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*stdout*``](#Namespace: root::*stdout*-contents) | Type: File |
| ``Namespace: root::*stdout*`` | ``Usage: (write-line *stdout*)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
; Use a file for stdout for test.<br>
(dyn *stdout* (open "/tmp/sl-sh.stdout.test" :create :truncate) (do (write-line *stdout* "Test out") (close *stdout*)))<br>
(test::assert-equal "Test out<br>
" (read-line (open "/tmp/sl-sh.stdout.test" :read)))<br>
<br>
</code>
</details>


| <a id="Namespace: shell::alias" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``alias``](#Namespace: shell::alias-contents) | Type: Macro |
| ``Namespace: shell::alias`` | ``Usage: (alias name body) or (alias name docstring body).`` |

<br>


| <a id="Namespace: shell::alias?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``alias?``](#Namespace: shell::alias?-contents) | Type: Lambda |
| ``Namespace: shell::alias?`` | ``Usage: (alias? name)`` |

<br>


| <a id="Namespace: root::bg" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``bg``](#Namespace: root::bg-contents) | Type: Function |
| ``Namespace: root::bg`` | ``Usage: (bg job-id?)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(bg)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: shell::bg-color-rgb" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``bg-color-rgb``](#Namespace: shell::bg-color-rgb-contents) | Type: Lambda |
| ``Namespace: shell::bg-color-rgb`` | ``Usage: (bg-color-rgb red-val green-val blue-val)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal "[48;2;128;128;128m" (bg-color-rgb 128 128 128))<br>
(test::assert-equal "[48;2;255;255;255m" (bg-color-rgb 255 255 255))<br>
(test::assert-equal "[48;2;255;0;0m" (bg-color-rgb 255 0 0))<br>
(test::assert-equal "[48;2;0;255;0m" (bg-color-rgb 0 255 0))<br>
(test::assert-equal "[48;2;0;0;255m" (bg-color-rgb 0 0 255))<br>
(test::assert-equal "[48;2;0;0;0m" (bg-color-rgb 0 0 0))<br>
<br>
</code>
</details>


| <a id="Namespace: root::cd" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cd``](#Namespace: root::cd-contents) | Type: Function |
| ``Namespace: root::cd`` | ``Usage: (cd dir-to-change-to)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(mkdir "/tmp/tst-fs-cd")<br>
(touch "/tmp/tst-fs-cd/fs-cd-marker")<br>
(test::assert-false (fs-exists? "fs-cd-marker"))<br>
(pushd "/tmp/tst-fs-cd")<br>
(root::cd "/tmp")<br>
(root::cd "/tmp/tst-fs-cd")<br>
(test::assert-true (fs-exists? "fs-cd-marker"))<br>
(rm "/tmp/tst-fs-cd/fs-cd-marker")<br>
(popd)<br>
(rmdir "/tmp/tst-fs-cd")<br>
<br>
</code>
</details>


| <a id="Namespace: shell::clear-dirs" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``clear-dirs``](#Namespace: shell::clear-dirs-contents) | Type: Lambda |
| ``Namespace: shell::clear-dirs`` | ``Usage: (clear-dirs)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(clear-dirs)<br>
(def cur-test-path (str-trim (str (pwd))))<br>
(test::assert-equal '() (get-dirs))<br>
(pushd "/tmp")<br>
(def cur-test-path2 (str-trim (str (pwd))))<br>
(test::assert-equal `(,cur-test-path) (get-dirs))<br>
(pushd (str-trim cur-test-path))<br>
(test::assert-equal `(,cur-test-path ,cur-test-path2) (get-dirs))<br>
(clear-dirs)<br>
(test::assert-equal '() (get-dirs))<br>
<br>
</code>
</details>


| <a id="Namespace: root::command" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``command``](#Namespace: root::command-contents) | Type: SpecialForm |
| ``Namespace: root::command`` | ``Usage: (command exp0 ... expN)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal "Failed to execute [str string]: No such file or directory (os error 2)" (cadr (get-error (command (str "string")))))<br>
(test::assert-equal "Some String<br>
" (str (command (echo "Some String"))))<br>
<br>
</code>
</details>


| <a id="Namespace: shell::dirs" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``dirs``](#Namespace: shell::dirs-contents) | Type: Lambda |
| ``Namespace: shell::dirs`` | ``Usage: (dirs)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(clear-dirs)<br>
(def cur-test-path (str (pwd)))<br>
(dyn *stdout* (open "/tmp/sl-sh.dirs.test" :create :truncate) (dirs))<br>
(test::assert-equal nil (read-line (open "/tmp/sl-sh.dirs.test" :read)))<br>
(pushd "/tmp")<br>
(def cur-test-path2 (str (pwd)))<br>
(dyn *stdout* (open "/tmp/sl-sh.dirs.test" :create :truncate) (dirs))<br>
(test::assert-equal cur-test-path (read-line (open "/tmp/sl-sh.dirs.test" :read)))<br>
(pushd (str-trim cur-test-path))<br>
(dyn *stdout* (open "/tmp/sl-sh.dirs.test" :create :truncate) (dirs))<br>
(def test-dirs-file (open "/tmp/sl-sh.dirs.test" :read))<br>
(test::assert-equal cur-test-path (read-line test-dirs-file))<br>
(test::assert-equal cur-test-path2 (read-line test-dirs-file))<br>
(close test-dirs-file)<br>
(popd)<br>
(dyn *stdout* (open "/tmp/sl-sh.dirs.test" :create :truncate) (dirs))<br>
(test::assert-equal cur-test-path (read-line (open "/tmp/sl-sh.dirs.test" :read)))<br>
(popd)<br>
(dyn *stdout* (open "/tmp/sl-sh.dirs.test" :create :truncate) (dirs))<br>
(test::assert-equal nil (read-line (open "/tmp/sl-sh.dirs.test" :read)))<br>
<br>
</code>
</details>


| <a id="Namespace: shell::endfix-on" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``endfix-on``](#Namespace: shell::endfix-on-contents) | Type: Macro |
| ``Namespace: shell::endfix-on`` | ``Usage: (endfix-on)`` |

<br>


| <a id="Namespace: shell::err>" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``err>``](#Namespace: shell::err>-contents) | Type: Macro |
| ``Namespace: shell::err>`` | ``Usage: (err> file body)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(err> "/tmp/sl-sh.err>.test" (eprintln "stderr redir one"))<br>
(def topen (open "/tmp/sl-sh.err>.test" :read))<br>
(test::assert-equal "stderr redir one<br>
" (read-line topen))<br>
(test::assert-false (read-line topen))<br>
(close topen)<br>
(err> "/tmp/sl-sh.err>.test" (eprintln "stderr redir two"))<br>
(def topen (open "/tmp/sl-sh.err>.test" :read))<br>
(test::assert-equal "stderr redir two<br>
" (read-line topen))<br>
(test::assert-false (read-line topen))<br>
(close topen)<br>
<br>
</code>
</details>


| <a id="Namespace: shell::err>>" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``err>>``](#Namespace: shell::err>>-contents) | Type: Macro |
| ``Namespace: shell::err>>`` | ``Usage: (err>> file body)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(err> "/tmp/sl-sh.err>>.test" (eprintln "stderr redir one"))<br>
(def topen (open "/tmp/sl-sh.err>>.test" :read))<br>
(test::assert-equal "stderr redir one<br>
" (read-line topen))<br>
(test::assert-false (read-line topen))<br>
(close topen)<br>
(err>> "/tmp/sl-sh.err>>.test" (eprintln "stderr redir two"))<br>
(def topen (open "/tmp/sl-sh.err>>.test" :read))<br>
(test::assert-equal "stderr redir one<br>
" (read-line topen))<br>
(test::assert-equal "stderr redir two<br>
" (read-line topen))<br>
(test::assert-false (read-line topen))<br>
(close topen)<br>
<br>
</code>
</details>


| <a id="Namespace: shell::err>null" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``err>null``](#Namespace: shell::err>null-contents) | Type: Macro |
| ``Namespace: shell::err>null`` | ``Usage: (err>null body)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(err> "/tmp/sl-sh.err>null.test" (do (eprintln "stderr redir one")(err>null (eprintln "stdnull redir one"))))<br>
(def topen (open "/tmp/sl-sh.err>null.test" :read))<br>
(test::assert-equal "stderr redir one<br>
" (read-line topen))<br>
(test::assert-false (read-line topen))<br>
(close topen)<br>
<br>
</code>
</details>


| <a id="Namespace: root::exit" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``exit``](#Namespace: root::exit-contents) | Type: Function |
| ``Namespace: root::exit`` | ``Usage: (exit code?)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(exit)<br>
;(exit 0)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::export" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``export``](#Namespace: root::export-contents) | Type: Function |
| ``Namespace: root::export`` | ``Usage: (export symbol string) -> string`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal "ONE" (export 'TEST_EXPORT_ONE "ONE"))<br>
(test::assert-equal "ONE" $TEST_EXPORT_ONE)<br>
<br>
</code>
</details>


| <a id="Namespace: shell::fc" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fc``](#Namespace: shell::fc-contents) | Type: Lambda |
| ``Namespace: shell::fc`` | ``Usage: (fc)`` |

<br>


| <a id="Namespace: root::fg" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fg``](#Namespace: root::fg-contents) | Type: Function |
| ``Namespace: root::fg`` | ``Usage: (fg job-id?)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(fg)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: shell::fg-color-rgb" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fg-color-rgb``](#Namespace: shell::fg-color-rgb-contents) | Type: Lambda |
| ``Namespace: shell::fg-color-rgb`` | ``Usage: (fg-color-rgb red-val green-val blue-val)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal "[38;2;128;128;128m" (fg-color-rgb 128 128 128))<br>
(test::assert-equal "[38;2;255;255;255m" (fg-color-rgb 255 255 255))<br>
(test::assert-equal "[38;2;255;0;0m" (fg-color-rgb 255 0 0))<br>
(test::assert-equal "[38;2;0;255;0m" (fg-color-rgb 0 255 0))<br>
(test::assert-equal "[38;2;0;0;255m" (fg-color-rgb 0 0 255))<br>
(test::assert-equal "[38;2;0;0;0m" (fg-color-rgb 0 0 0))<br>
<br>
</code>
</details>


| <a id="Namespace: root::form" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``form``](#Namespace: root::form-contents) | Type: SpecialForm |
| ``Namespace: root::form`` | ``Usage: (form exp0 ... expN)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal "Not a valid form true, not found." (cadr (get-error (form (true)))))<br>
(test::assert-equal "Some String" (form (str "Some String")))<br>
<br>
</code>
</details>


| <a id="Namespace: root::fs-dir?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fs-dir?``](#Namespace: root::fs-dir?-contents) | Type: Function |
| ``Namespace: root::fs-dir?`` | ``Usage: (fs-dir? path-to-test)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(mkdir "/tmp/tst-fs-dir")<br>
(touch "/tmp/tst-fs-dir/fs-dir-file")<br>
(test::assert-false (fs-dir? "/tmp/tst-fs-dir/fs-dir-file"))<br>
(test::assert-true (fs-dir? "/tmp/tst-fs-dir"))<br>
(test::assert-false (fs-dir? "/tmp/tst-fs-dir/fs-dir-nope"))<br>
(rm "/tmp/tst-fs-dir/fs-dir-file")<br>
(rmdir "/tmp/tst-fs-dir")<br>
<br>
</code>
</details>


| <a id="Namespace: root::fs-exists?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fs-exists?``](#Namespace: root::fs-exists?-contents) | Type: Function |
| ``Namespace: root::fs-exists?`` | ``Usage: (fs-exists? path-to-test)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(mkdir "/tmp/tst-fs-exists")<br>
(touch "/tmp/tst-fs-exists/fs-exists")<br>
(test::assert-true (fs-exists? "/tmp/tst-fs-exists/fs-exists"))<br>
(test::assert-true (fs-exists? "/tmp/tst-fs-exists"))<br>
(test::assert-false (fs-exists? "/tmp/tst-fs-exists/fs-exists-nope"))<br>
(rm "/tmp/tst-fs-exists/fs-exists")<br>
(rmdir "/tmp/tst-fs-exists")<br>
<br>
</code>
</details>


| <a id="Namespace: root::fs-file?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fs-file?``](#Namespace: root::fs-file?-contents) | Type: Function |
| ``Namespace: root::fs-file?`` | ``Usage: (fs-file? path-to-test)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(mkdir "/tmp/tst-fs-file")<br>
(touch "/tmp/tst-fs-file/fs-file")<br>
(test::assert-true (fs-file? "/tmp/tst-fs-file/fs-file"))<br>
(test::assert-false (fs-file? "/tmp/tst-fs-file"))<br>
(test::assert-false (fs-file? "/tmp/tst-fs-file/fs-file-nope"))<br>
(rm "/tmp/tst-fs-file/fs-file")<br>
(rmdir "/tmp/tst-fs-file")<br>
<br>
</code>
</details>


| <a id="Namespace: shell::get-dirs" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``get-dirs``](#Namespace: shell::get-dirs-contents) | Type: Lambda |
| ``Namespace: shell::get-dirs`` | ``Usage: (get-dirs)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(clear-dirs)<br>
(def cur-test-path (str-trim (str (pwd))))<br>
(test::assert-equal '() (get-dirs))<br>
(pushd "/tmp")<br>
(def cur-test-path2 (str-trim (str (pwd))))<br>
(test::assert-equal `(,cur-test-path) (get-dirs))<br>
(pushd (str-trim cur-test-path))<br>
(test::assert-equal `(,cur-test-path ,cur-test-path2) (get-dirs))<br>
(popd)<br>
(test::assert-equal `(,cur-test-path) (get-dirs))<br>
(popd)<br>
(test::assert-equal '() (get-dirs))<br>
<br>
</code>
</details>


| <a id="Namespace: shell::getopts" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``getopts``](#Namespace: shell::getopts-contents) | Type: Lambda |
| ``Namespace: shell::getopts`` | ``Usage: (getopts options-map args)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;See tests/getopts.lisp<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::glob" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``glob``](#Namespace: root::glob-contents) | Type: Function |
| ``Namespace: root::glob`` | ``Usage: (glob /path/with/*)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(mkdir "/tmp/tst-fs-glob")<br>
(touch "/tmp/tst-fs-glob/g1")<br>
(touch "/tmp/tst-fs-glob/g2")<br>
(touch "/tmp/tst-fs-glob/g3")<br>
(test::assert-equal '("/tmp/tst-fs-glob/g1" "/tmp/tst-fs-glob/g2" "/tmp/tst-fs-glob/g3") (glob "/tmp/tst-fs-glob/*"))<br>
(rm "/tmp/tst-fs-glob/g1")<br>
(rm "/tmp/tst-fs-glob/g2")<br>
(rm "/tmp/tst-fs-glob/g3")<br>
(rmdir "/tmp/tst-fs-glob")<br>
<br>
</code>
</details>


| <a id="Namespace: root::history-context" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``history-context``](#Namespace: root::history-context-contents) | Type: Function |
| ``Namespace: root::history-context`` | ``Usage: (history-context :context_id context-string) -> nil`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(history-context :repl "/home")<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::history-empty?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``history-empty?``](#Namespace: root::history-empty?-contents) | Type: Function |
| ``Namespace: root::history-empty?`` | ``Usage: (history-empty? :context_id) -> t/nil`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(history-empty? :repl)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::history-length" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``history-length``](#Namespace: root::history-length-contents) | Type: Function |
| ``Namespace: root::history-length`` | ``Usage: (history-length :context_id) -> int`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(history-length :repl)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::history-nth" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``history-nth``](#Namespace: root::history-nth-contents) | Type: Function |
| ``Namespace: root::history-nth`` | ``Usage: (history-nth :context_id nth) -> String`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(history-nth :repl 0)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::history-push" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``history-push``](#Namespace: root::history-push-contents) | Type: Function |
| ``Namespace: root::history-push`` | ``Usage: (history-push :context_id string) -> nil/t`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(history-push :repl "Some command")<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::history-push-throwaway" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``history-push-throwaway``](#Namespace: root::history-push-throwaway-contents) | Type: Function |
| ``Namespace: root::history-push-throwaway`` | ``Usage: (history-push-throwaway :context_id string) -> nil/t`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(history-push-throwaway :repl "Some broken command")<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: root::jobs" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``jobs``](#Namespace: root::jobs-contents) | Type: Function |
| ``Namespace: root::jobs`` | ``Usage: (jobs)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(jobs)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: shell::let-env" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``let-env``](#Namespace: shell::let-env-contents) | Type: Macro |
| ``Namespace: shell::let-env`` | ``Usage: (let-env vals &rest let_body)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-false $LET-ENV-TEST-VAR-NOT-HERE)<br>
(let-env ((LET-ENV-TEST-VAR-NOT-HERE "here"))<br>
(test::assert-equal "here" $LET-ENV-TEST-VAR-NOT-HERE))<br>
(test::assert-false $LET-ENV-TEST-VAR-NOT-HERE)<br>
<br>
</code>
</details>


| <a id="Namespace: root::loose-symbols" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``loose-symbols``](#Namespace: root::loose-symbols-contents) | Type: SpecialForm |
| ``Namespace: root::loose-symbols`` | ``Usage: (loose-symbols exp0 ... expN)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal "Some_Result" (loose-symbols Some_Result))<br>
(test::assert-equal "Some Result" (loose-symbols Some\ Result))<br>
<br>
</code>
</details>


| <a id="Namespace: shell::out-err>" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``out-err>``](#Namespace: shell::out-err>-contents) | Type: Macro |
| ``Namespace: shell::out-err>`` | ``Usage: (out-err> file body)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(out-err> "/tmp/sl-sh.out-err>.test" (do (println "stdout redir one")(eprintln "stderr redir one")))<br>
(def topen (open "/tmp/sl-sh.out-err>.test" :read))<br>
(test::assert-equal "stdout redir one<br>
" (read-line topen))<br>
(test::assert-equal "stderr redir one<br>
" (read-line topen))<br>
(test::assert-false (read-line topen))<br>
(close topen)<br>
(out-err> "/tmp/sl-sh.out-err>.test" (do (echo "stdout echo redir one")(eprintln "stderr redir one")))<br>
(def topen (open "/tmp/sl-sh.out-err>.test" :read))<br>
(test::assert-equal "stdout echo redir one<br>
" (read-line topen))<br>
(test::assert-equal "stderr redir one<br>
" (read-line topen))<br>
(test::assert-false (read-line topen))<br>
(close topen)<br>
(out-err> "/tmp/sl-sh.out-err>.test" (do (println "stdout redir two")(eprintln "stderr redir two")))<br>
(def topen (open "/tmp/sl-sh.out-err>.test" :read))<br>
(test::assert-equal "stdout redir two<br>
" (read-line topen))<br>
(test::assert-equal "stderr redir two<br>
" (read-line topen))<br>
(test::assert-false (read-line topen))<br>
(close topen)<br>
<br>
</code>
</details>


| <a id="Namespace: shell::out-err>>" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``out-err>>``](#Namespace: shell::out-err>>-contents) | Type: Macro |
| ``Namespace: shell::out-err>>`` | ``Usage: (out-err>> file body)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(out-err> "/tmp/sl-sh.out-err>>.test" (do (println "stdout redir one")(eprintln "stderr redir one")))<br>
(def topen (open "/tmp/sl-sh.out-err>>.test" :read))<br>
(test::assert-equal "stdout redir one<br>
" (read-line topen))<br>
(test::assert-equal "stderr redir one<br>
" (read-line topen))<br>
(test::assert-false (read-line topen))<br>
(close topen)<br>
(out-err>> "/tmp/sl-sh.out-err>>.test" (do (println "stdout redir two")(eprintln "stderr redir two")))<br>
(def topen (open "/tmp/sl-sh.out-err>>.test" :read))<br>
(test::assert-equal "stdout redir one<br>
" (read-line topen))<br>
(test::assert-equal "stderr redir one<br>
" (read-line topen))<br>
(test::assert-equal "stdout redir two<br>
" (read-line topen))<br>
(test::assert-equal "stderr redir two<br>
" (read-line topen))<br>
(test::assert-false (read-line topen))<br>
(close topen)<br>
<br>
</code>
</details>


| <a id="Namespace: shell::out-err>null" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``out-err>null``](#Namespace: shell::out-err>null-contents) | Type: Macro |
| ``Namespace: shell::out-err>null`` | ``Usage: (out-err>null body)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(out-err> "/tmp/sl-sh.out-err>null.test" (do<br>
(println "stdout redir one")<br>
(eprintln "stderr redir one")<br>
(out-err>null (do<br>
(println "stdnull redir one")<br>
(eprintln "stdnull redir one")))))<br>
(def topen (open "/tmp/sl-sh.out-err>null.test" :read))<br>
(test::assert-equal "stdout redir one<br>
" (read-line topen))<br>
(test::assert-equal "stderr redir one<br>
" (read-line topen))<br>
(test::assert-false (read-line topen))<br>
(close topen)<br>
<br>
</code>
</details>


| <a id="Namespace: shell::out>" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``out>``](#Namespace: shell::out>-contents) | Type: Macro |
| ``Namespace: shell::out>`` | ``Usage: (out> file body)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(out> "/tmp/sl-sh.out>.test" (echo "stdout redir one"))<br>
(def topen (open "/tmp/sl-sh.out>.test" :read))<br>
(test::assert-equal "stdout redir one<br>
" (read-line topen))<br>
(test::assert-false (read-line topen))<br>
(close topen)<br>
(out> "/tmp/sl-sh.out>.test" (echo "stdout redir two"))<br>
(def topen (open "/tmp/sl-sh.out>.test" :read))<br>
(test::assert-equal "stdout redir two<br>
" (read-line topen))<br>
(test::assert-false (read-line topen))<br>
(close topen)<br>
<br>
</code>
</details>


| <a id="Namespace: shell::out>>" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``out>>``](#Namespace: shell::out>>-contents) | Type: Macro |
| ``Namespace: shell::out>>`` | ``Usage: (out>> file body)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(out> "/tmp/sl-sh.out>>.test" (echo "stdout redir one"))<br>
(def topen (open "/tmp/sl-sh.out>>.test" :read))<br>
(test::assert-equal "stdout redir one<br>
" (read-line topen))<br>
(test::assert-false (read-line topen))<br>
(close topen)<br>
(out>> "/tmp/sl-sh.out>>.test" (echo "stdout redir two"))<br>
(def topen (open "/tmp/sl-sh.out>>.test" :read))<br>
(test::assert-equal "stdout redir one<br>
" (read-line topen))<br>
(test::assert-equal "stdout redir two<br>
" (read-line topen))<br>
(test::assert-false (read-line topen))<br>
(close topen)<br>
<br>
</code>
</details>


| <a id="Namespace: shell::out>null" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``out>null``](#Namespace: shell::out>null-contents) | Type: Macro |
| ``Namespace: shell::out>null`` | ``Usage: (out>null body)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(out> "/tmp/sl-sh.out>null.test" (do (println "stdout redir one")(out>null (println "stdnull redir one"))))<br>
(def topen (open "/tmp/sl-sh.out>null.test" :read))<br>
(test::assert-equal "stdout redir one<br>
" (read-line topen))<br>
(test::assert-false (read-line topen))<br>
(close topen)<br>
<br>
</code>
</details>


| <a id="Namespace: root::pid" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``pid``](#Namespace: root::pid-contents) | Type: Function |
| ``Namespace: root::pid`` | ``Usage: (pid proc)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def pid-test (echo -n))<br>
(test::assert-true (int? (pid pid-test)))<br>
(test::assert-error (pid 1))<br>
<br>
</code>
</details>


| <a id="Namespace: root::pipe" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``pipe``](#Namespace: root::pipe-contents) | Type: Function |
| ``Namespace: root::pipe`` | ``Usage: (pipe [expression]+)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def pipe-test (str (pipe (echo "one<br>
two<br>
three")(grep two))))<br>
(test::assert-equal "two<br>
" pipe-test)<br>
(def pipe-test (str (pipe (pipe (echo "one<br>
two<br>
twotwo<br>
three")(grep two))(grep twotwo))))<br>
(test::assert-equal "twotwo<br>
" pipe-test)<br>
(mkdir "/tmp/tst-pipe-dir")<br>
(def tsync (open "/tmp/tst-pipe-dir/test1" :create))<br>
(pipe (print "one<br>
two<br>
two2<br>
three") (grep two) tsync)<br>
(close tsync)<br>
(def topen (open "/tmp/tst-pipe-dir/test1" :read))<br>
(test::assert-equal "two<br>
" (read-line topen))<br>
(test::assert-equal "two2<br>
" (read-line topen))<br>
(test::assert-false (read-line topen))<br>
(close topen)<br>
(def topen (open "/tmp/tst-pipe-dir/test1" :read))<br>
(def pipe-test (str (pipe topen (grep two2))))<br>
(close topen)<br>
(test::assert-equal "two2<br>
" pipe-test)<br>
(rm "/tmp/tst-pipe-dir/test1")<br>
(rmdir "/tmp/tst-pipe-dir")<br>
<br>
</code>
</details>


| <a id="Namespace: shell::popd" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``popd``](#Namespace: shell::popd-contents) | Type: Lambda |
| ``Namespace: shell::popd`` | ``Usage: (popd)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def cur-test-path (str (pwd)))<br>
(pushd "/tmp")<br>
(def cur-test-path2 (str (pwd)))<br>
(assert-equal cur-test-path2 (str (pwd)))<br>
(popd)<br>
(assert-equal cur-test-path (str (pwd)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::prompt" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``prompt``](#Namespace: root::prompt-contents) | Type: Function |
| ``Namespace: root::prompt`` | ``Usage: (prompt string) -> string`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(def input-string (prompt "prompt> "))<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: shell::pushd" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``pushd``](#Namespace: shell::pushd-contents) | Type: Lambda |
| ``Namespace: shell::pushd`` | ``Usage: (pushd dir)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def cur-test-path (str (pwd)))<br>
(pushd "/tmp")<br>
(def cur-test-path2 (str (pwd)))<br>
(assert-equal cur-test-path2 (str (pwd)))<br>
(popd)<br>
(assert-equal cur-test-path (str (pwd)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::reap-jobs" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``reap-jobs``](#Namespace: root::reap-jobs-contents) | Type: Function |
| ``Namespace: root::reap-jobs`` | ``Usage: (reap-jobs) -> nil`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(reap-jobs)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: shell::register-alias" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``register-alias``](#Namespace: shell::register-alias-contents) | Type: Lambda |
| ``Namespace: shell::register-alias`` | ``Usage: (register-alias name)`` |

<br>


| <a id="Namespace: root::run-bg" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``run-bg``](#Namespace: root::run-bg-contents) | Type: SpecialForm |
| ``Namespace: root::run-bg`` | ``Usage: (run-bg exp0 ... expN)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
;(run-bg gitk)<br>
t<br>
<br>
</code>
</details>


| <a id="Namespace: shell::set-dirs-max" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``set-dirs-max``](#Namespace: shell::set-dirs-max-contents) | Type: Lambda |
| ``Namespace: shell::set-dirs-max`` | ``Usage: (set-dirs-max max)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(clear-dirs)<br>
(def cur-test-path (str-trim (str (pwd))))<br>
(pushd "/tmp")<br>
(def cur-test-path2 (str-trim (str (pwd))))<br>
(pushd (str-trim cur-test-path))<br>
(pushd "/tmp")<br>
(pushd (str-trim cur-test-path))<br>
(test::assert-equal `(,cur-test-path ,cur-test-path2 ,cur-test-path ,cur-test-path2) (get-dirs))<br>
(clear-dirs)<br>
(set-dirs-max 3)<br>
(pushd "/tmp")<br>
(pushd (str-trim cur-test-path))<br>
(pushd "/tmp")<br>
(pushd (str-trim cur-test-path))<br>
(test::assert-equal `(,cur-test-path2 ,cur-test-path ,cur-test-path2) (get-dirs))<br>
<br>
</code>
</details>


| <a id="Namespace: root::sleep" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``sleep``](#Namespace: root::sleep-contents) | Type: Function |
| ``Namespace: root::sleep`` | ``Usage: (sleep milliseconds) -> nil`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-sleep-var (time (sleep 1000)))<br>
(assert-true (> test-sleep-var 1.0))<br>
<br>
</code>
</details>


| <a id="Namespace: shell::syntax-off" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``syntax-off``](#Namespace: shell::syntax-off-contents) | Type: Macro |
| ``Namespace: shell::syntax-off`` | ``Usage: (syntax-off)`` |

<br>


| <a id="Namespace: shell::syntax-on" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``syntax-on``](#Namespace: shell::syntax-on-contents) | Type: Lambda |
| ``Namespace: shell::syntax-on`` | ``Usage: (syntax-on)`` |

<br>


| <a id="Namespace: shell::sys-command?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``sys-command?``](#Namespace: shell::sys-command?-contents) | Type: Lambda |
| ``Namespace: shell::sys-command?`` | ``Usage: (sys-command? com)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(assert-true (sys-command? "ls"))<br>
(assert-false (sys-command? "rst-not-a-comand-strsnt"))<br>
<br>
</code>
</details>


| <a id="Namespace: shell::temp-dir" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``temp-dir``](#Namespace: shell::temp-dir-contents) | Type: Lambda |
| ``Namespace: shell::temp-dir`` | ``Usage: (temp-dir)`` |

<br>


| <a id="Namespace: root::time" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``time``](#Namespace: root::time-contents) | Type: Function |
| ``Namespace: root::time`` | ``Usage: (time form) -> eval-time`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-sleep-var (time (sleep 1100)))<br>
(assert-true (> test-sleep-var 1.1))<br>
<br>
</code>
</details>


| <a id="Namespace: root::unexport" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``unexport``](#Namespace: root::unexport-contents) | Type: Function |
| ``Namespace: root::unexport`` | ``Usage: (unexport symbol)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal "ONE" (export 'TEST_EXPORT_ONE "ONE"))<br>
(test::assert-equal "ONE" $TEST_EXPORT_ONE)<br>
(unexport 'TEST_EXPORT_ONE)<br>
(test::assert-false $TEST_EXPORT_ONE)<br>
<br>
</code>
</details>


| <a id="Namespace: shell::unregister-alias" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``unregister-alias``](#Namespace: shell::unregister-alias-contents) | Type: Lambda |
| ``Namespace: shell::unregister-alias`` | ``Usage: (unregister-alias name)`` |

<br>


| <a id="Namespace: root::version" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``version``](#Namespace: root::version-contents) | Type: Function |
| ``Namespace: root::version`` | ``Usage: (version)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (string? (version)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::wait" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``wait``](#Namespace: root::wait-contents) | Type: Function |
| ``Namespace: root::wait`` | ``Usage: (wait proc-to-wait-for)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def wait-test (wait (err>null (ls /does/not/exist/123))))<br>
(test::assert-true (> wait-test 0))<br>
<br>
</code>
</details>


| <a id="Namespace: shell::|" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``\|``](#Namespace: shell::|-contents) | Type: Macro |
| ``Namespace: shell::\|`` | ``Usage: (\| &rest body)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def pipe-test (str (| (echo "one<br>
two<br>
three")(grep two))))<br>
(test::assert-equal "two<br>
" pipe-test)<br>
<br>
</code>
</details>
### <a id="String forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[String forms](#String forms-contents)



| <a id="Namespace: root::str" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str``](#Namespace: root::str-contents) | Type: Function |
| ``Namespace: root::str`` | ``Usage: (str arg0 ... argN) -> string`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal "stringsome" (str "string" "some"))<br>
(test::assert-equal "string" (str "string" ""))<br>
(test::assert-equal "string 50" (str "string" " " 50))<br>
(test::assert-equal "string 50 test<br>
" (str "string" " " 50 " " (echo "test")))<br>
<br>
</code>
</details>


| <a id="Namespace: root::str-append" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-append``](#Namespace: root::str-append-contents) | Type: Function |
| ``Namespace: root::str-append`` | ``Usage: (str-append string string) -> string`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal "stringsome" (str-append "string" "some"))<br>
(test::assert-equal "string" (str-append "string" ""))<br>
(test::assert-equal "string " (str-append "string" " "))<br>
<br>
</code>
</details>


| <a id="Namespace: root::str-bytes" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-bytes``](#Namespace: root::str-bytes-contents) | Type: Function |
| ``Namespace: root::str-bytes`` | ``Usage: (str-bytes string) -> int`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal 4 (str-bytes "Stau"))<br>
(test::assert-equal 0 (str-bytes ""))<br>
; Note 5 chars and 6 bytes because of the final char.<br>
(test::assert-equal 6 (str-bytes "StauΣ"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::str-cat-list" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-cat-list``](#Namespace: root::str-cat-list-contents) | Type: Function |
| ``Namespace: root::str-cat-list`` | ``Usage: (str-cat-list join-pattern sequence) -> string`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal "stringxxxyyyxxxsome" (str-cat-list "xxx" '("string" "yyy" "some")))<br>
(test::assert-equal "string yyy some" (str-cat-list " " '("string" "yyy" "some")))<br>
(test::assert-equal "stringyyysome" (str-cat-list "" '("string" "yyy" "some")))<br>
<br>
</code>
</details>


| <a id="Namespace: root::str-clear!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-clear!``](#Namespace: root::str-clear!-contents) | Type: Function |
| ``Namespace: root::str-clear!`` | ``Usage: (str-clear! string) -> string`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal "" (str-clear! (str "string")))<br>
(def test-str-clear (str "def-string"))<br>
(test::assert-equal "" (str-clear! test-str-clear))<br>
(test::assert-equal "" test-str-clear)<br>
<br>
</code>
</details>


| <a id="Namespace: root::str-contains" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-contains``](#Namespace: root::str-contains-contents) | Type: Function |
| ``Namespace: root::str-contains`` | ``Usage: (str-contains pattern string) -> t/nil`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (str-contains "Stau" "Stausomething"))<br>
(test::assert-false (str-contains "StaU" "Stausomething"))<br>
(test::assert-true (str-contains "some" "Stausomething"))<br>
(test::assert-false (str-contains "Some" "Stausomething"))<br>
(test::assert-true (str-contains "thing" "Stausomething"))<br>
(test::assert-false (str-contains "Thing" "Stausomething"))<br>
(test::assert-true (str-contains "someΣ" "StausomeΣthing"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::str-empty?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-empty?``](#Namespace: root::str-empty?-contents) | Type: Function |
| ``Namespace: root::str-empty?`` | ``Usage: (str-empty? string) -> t/nil`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (str-empty? ""))<br>
(test::assert-true (str-empty? (str-trim "   ")))<br>
(test::assert-false (str-empty? " "))<br>
(test::assert-false (str-empty? "string"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::str-ignore-expand" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-ignore-expand``](#Namespace: root::str-ignore-expand-contents) | Type: Function |
| ``Namespace: root::str-ignore-expand`` | ``Usage: (str-ignore-expand exp0 ... expN) -> [final expression]`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(export 'TST-IGNORE "TST")<br>
(test::assert-equal "some TST stuff" "some $TST-IGNORE stuff")<br>
(test::assert-equal "some \$TST-IGNORE stuff" (str-ignore-expand "some $TST-IGNORE stuff"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::str-iter-empty?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-iter-empty?``](#Namespace: root::str-iter-empty?-contents) | Type: Function |
| ``Namespace: root::str-iter-empty?`` | ``Usage: (str-iter-empty? string) -> t/nil`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-iter-start "test")<br>
(test::assert-true (str-iter-empty? test-iter-start))<br>
(str-iter-start test-iter-start)<br>
(test::assert-false (str-iter-empty? test-iter-start))<br>
(test::assert-equal #\t (str-iter-next! test-iter-start))<br>
(test::assert-equal #\e (str-iter-next! test-iter-start))<br>
(test::assert-equal #\s (str-iter-next! test-iter-start))<br>
(test::assert-equal #\t (str-iter-next! test-iter-start))<br>
(test::assert-true (str-iter-empty? test-iter-start))<br>
(def test-iter-start "test")<br>
(test::assert-true (str-iter-empty? test-iter-start))<br>
(str-iter-start test-iter-start)<br>
(test::assert-false (str-iter-empty? test-iter-start))<br>
(test::assert-equal #\t (str-iter-next! test-iter-start))<br>
(test::assert-equal #\e (str-iter-next! test-iter-start))<br>
(str-push! test-iter-start "one")<br>
(test::assert-true (str-iter-empty? test-iter-start))<br>
(str-iter-start test-iter-start)<br>
(test::assert-false (str-iter-empty? test-iter-start))<br>
(str-clear! test-iter-start)<br>
(test::assert-true (str-iter-empty? test-iter-start))<br>
<br>
</code>
</details>


| <a id="Namespace: root::str-iter-next!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-iter-next!``](#Namespace: root::str-iter-next!-contents) | Type: Function |
| ``Namespace: root::str-iter-next!`` | ``Usage: (str-iter-next! string) -> char`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-iter-start "y̆ΛλΣσ")<br>
(str-iter-start test-iter-start)<br>
(test::assert-false (str-iter-empty? test-iter-start))<br>
(def test-iter-one (str-iter-next! test-iter-start))<br>
(test::assert-equal #\y̆ test-iter-one)<br>
(test::assert-true (= #\y̆ test-iter-one))<br>
(test::assert-false (= #\y test-iter-one))<br>
(test::assert-equal #\Λ (str-iter-next! test-iter-start))<br>
(test::assert-equal #\λ (str-iter-next! test-iter-start))<br>
(test::assert-equal #\Σ (str-iter-next! test-iter-start))<br>
(test::assert-equal #\σ (str-iter-next! test-iter-start))<br>
(test::assert-true (str-iter-empty? test-iter-start))<br>
<br>
</code>
</details>


| <a id="Namespace: root::str-iter-peek" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-iter-peek``](#Namespace: root::str-iter-peek-contents) | Type: Function |
| ``Namespace: root::str-iter-peek`` | ``Usage: (str-iter-peek string) -> char`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-iter-start "y̆ΛλΣσ")<br>
(str-iter-start test-iter-start)<br>
(test::assert-false (str-iter-empty? test-iter-start))<br>
(def test-iter-one (str-iter-next! test-iter-start))<br>
(test::assert-equal #\y̆ test-iter-one)<br>
(test::assert-true (= #\y̆ test-iter-one))<br>
(test::assert-false (= #\y test-iter-one))<br>
(test::assert-equal #\Λ (str-iter-peek test-iter-start))<br>
(test::assert-equal #\Λ (str-iter-next! test-iter-start))<br>
(test::assert-equal #\λ (str-iter-peek test-iter-start))<br>
(test::assert-equal #\λ (str-iter-next! test-iter-start))<br>
(test::assert-equal #\Σ (str-iter-peek test-iter-start))<br>
(test::assert-equal #\Σ (str-iter-next! test-iter-start))<br>
(test::assert-equal #\σ (str-iter-peek test-iter-start))<br>
(test::assert-equal #\σ (str-iter-next! test-iter-start))<br>
(test::assert-true (str-iter-empty? test-iter-start))<br>
<br>
</code>
</details>


| <a id="Namespace: root::str-iter-start" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-iter-start``](#Namespace: root::str-iter-start-contents) | Type: Function |
| ``Namespace: root::str-iter-start`` | ``Usage: (str-iter-start string) -> string`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-iter-start "test")<br>
(test::assert-true (str-iter-empty? test-iter-start))<br>
(str-iter-start test-iter-start)<br>
(test::assert-false (str-iter-empty? test-iter-start))<br>
(test::assert-equal #\t (str-iter-next! test-iter-start))<br>
(test::assert-equal #\e (str-iter-next! test-iter-start))<br>
(test::assert-equal #\s (str-iter-next! test-iter-start))<br>
(test::assert-equal #\t (str-iter-next! test-iter-start))<br>
(test::assert-true (str-iter-empty? test-iter-start))<br>
(str-iter-start test-iter-start)<br>
(test::assert-false (str-iter-empty? test-iter-start))<br>
(test::assert-equal #\t (str-iter-next! test-iter-start))<br>
(test::assert-equal #\e (str-iter-next! test-iter-start))<br>
(str-iter-start test-iter-start)<br>
(test::assert-false (str-iter-empty? test-iter-start))<br>
(test::assert-equal #\t (str-iter-next! test-iter-start))<br>
(test::assert-equal #\e (str-iter-next! test-iter-start))<br>
(test::assert-equal #\s (str-iter-next! test-iter-start))<br>
(test::assert-equal #\t (str-iter-next! test-iter-start))<br>
(test::assert-true (str-iter-empty? test-iter-start))<br>
<br>
</code>
</details>


| <a id="Namespace: root::str-lower" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-lower``](#Namespace: root::str-lower-contents) | Type: Function |
| ``Namespace: root::str-lower`` | ``Usage: (str-lower string) -> string`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal "stau" (str-lower "STAU"))<br>
(test::assert-equal "stau" (str-lower "stau"))<br>
(test::assert-equal "stau" (str-lower "Stau"))<br>
(test::assert-equal "stau" (str-lower "StaU"))<br>
(test::assert-equal "stau" (str-lower "sTaU"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::str-ltrim" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-ltrim``](#Namespace: root::str-ltrim-contents) | Type: Function |
| ``Namespace: root::str-ltrim`` | ``Usage: (str-ltrim string) -> string`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal "some string" (str-ltrim "   some string"))<br>
(test::assert-equal "some string   " (str-ltrim "   some string   "))<br>
(test::assert-equal "some string   " (str-ltrim (str "   some string   ")))<br>
(test::assert-equal "some string   " (str-ltrim "some string   "))<br>
(test::assert-equal "some string" (str-ltrim "some string"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::str-map" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-map``](#Namespace: root::str-map-contents) | Type: Function |
| ``Namespace: root::str-map`` | ``Usage: (str-map lambda string) -> string`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal "XstringXstrX" (str-map (fn (ch) (if (= #\x ch) #\X ch)) "xstringxstrx"))<br>
(def test-str-map (str-map (fn (ch) (if (= #\x ch) #\X ch)) "xstringxstrx"))<br>
(test::assert-equal "XstringXstrX" test-str-map)<br>
(test::assert-true (string? test-str-map))<br>
(def test-str-map (str-map (fn (ch) (if (= #\x ch) #\X ch)) (str "xstringxstrx")))<br>
(test::assert-equal "XstringXstrX" test-str-map)<br>
(test::assert-true (string? test-str-map))<br>
<br>
</code>
</details>


| <a id="Namespace: root::str-nth" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-nth``](#Namespace: root::str-nth-contents) | Type: Function |
| ``Namespace: root::str-nth`` | ``Usage: (str-nth n string) -> char`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal #\a (str-nth 2 "stau"))<br>
(test::assert-equal #\s (str-nth 0 "stau"))<br>
(test::assert-equal #\u (str-nth 3 "stau"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::str-push!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-push!``](#Namespace: root::str-push!-contents) | Type: Function |
| ``Namespace: root::str-push!`` | ``Usage: (str-push! string arg0 ... argN) -> string`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal "stringsome" (str-push! (str "string") "some"))<br>
(def test-str-push (str "def-string"))<br>
(test::assert-equal "def-stringsome" (str-push! test-str-push "some"))<br>
(test::assert-equal "def-stringsome" test-str-push)<br>
<br>
</code>
</details>


| <a id="Namespace: root::str-replace" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-replace``](#Namespace: root::str-replace-contents) | Type: Function |
| ``Namespace: root::str-replace`` | ``Usage: (str-replace string old-pattern new-pattern) -> string`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal "some yyy string" (str-replace "some xxx string" "xxx" "yyy"))<br>
(test::assert-equal "some yyy string yyy" (str-replace "some xxx string xxx" "xxx" "yyy"))<br>
(test::assert-equal "yyy some yyy string yyy" (str-replace "xxx some xxx string xxx" "xxx" "yyy"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::str-rsplit" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-rsplit``](#Namespace: root::str-rsplit-contents) | Type: Function |
| ``Namespace: root::str-rsplit`` | ``Usage: (str-rsplit split-pattern string) -> vector`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal '("some" "yyy" "string") (str-rsplit "xxx" "stringxxxyyyxxxsome"))<br>
(test::assert-equal '("" "some" "yyy" "string") (str-rsplit "xxx" "stringxxxyyyxxxsomexxx"))<br>
(test::assert-equal '("some" "yyy" "string") (str-rsplit " " "string yyy some"))<br>
(test::assert-equal '("somexxxyyyxxxstring") (str-rsplit :whitespace "somexxxyyyxxxstring"))<br>
(test::assert-equal '("somexxxyyyxxxstring") (str-rsplit "zzz" "somexxxyyyxxxstring"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::str-rsplitn" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-rsplitn``](#Namespace: root::str-rsplitn-contents) | Type: Function |
| ``Namespace: root::str-rsplitn`` | ``Usage: (str-rsplitn n split-pattern string) -> vector`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal '("some" "yyy" "string") (str-rsplitn 3 "xxx" "stringxxxyyyxxxsome"))<br>
(test::assert-equal '("some" "yyy" "string") (str-rsplitn 4 "xxx" "stringxxxyyyxxxsome"))<br>
(test::assert-equal '("other" "string" "somexxxyyy") (str-rsplitn 3 "xxx" "somexxxyyyxxxstringxxxother"))<br>
(test::assert-equal '("somexxxyyyxxxstringxxxother") (str-rsplitn 1 "xxx" "somexxxyyyxxxstringxxxother"))<br>
(test::assert-equal '() (str-rsplitn 0 "xxx" "somexxxyyyxxxstringxxxzero"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::str-rtrim" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-rtrim``](#Namespace: root::str-rtrim-contents) | Type: Function |
| ``Namespace: root::str-rtrim`` | ``Usage: (str-rtrim string) -> string`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal "   some string" (str-rtrim "   some string"))<br>
(test::assert-equal "   some string" (str-rtrim "   some string   "))<br>
(test::assert-equal "   some string" (str-rtrim (str "   some string   ")))<br>
(test::assert-equal "some string" (str-rtrim "some string   "))<br>
(test::assert-equal "some string" (str-rtrim "some string"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::str-split" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-split``](#Namespace: root::str-split-contents) | Type: Function |
| ``Namespace: root::str-split`` | ``Usage: (str-split split-pattern string) -> vector`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal '("some" "yyy" "string") (str-split "xxx" "somexxxyyyxxxstring"))<br>
(test::assert-equal '("some" "yyy" "string" "") (str-split "xxx" "somexxxyyyxxxstringxxx"))<br>
(test::assert-equal '("" "some" "yyy" "string" "") (str-split "xxx" "xxxsomexxxyyyxxxstringxxx"))<br>
(test::assert-equal '("some" "yyy" "string") (str-split :whitespace "some yyy string"))<br>
(test::assert-equal '("somexxxyyyxxxstring") (str-split :whitespace "somexxxyyyxxxstring"))<br>
(test::assert-equal '("somexxxyyyxxxstring") (str-split "zzz" "somexxxyyyxxxstring"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::str-splitn" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-splitn``](#Namespace: root::str-splitn-contents) | Type: Function |
| ``Namespace: root::str-splitn`` | ``Usage: (str-splitn n split-pattern string) -> vector`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal '("some" "yyy" "string") (str-splitn 3 "xxx" "somexxxyyyxxxstring"))<br>
(test::assert-equal '("some" "yyy" "string") (str-splitn 4 "xxx" "somexxxyyyxxxstring"))<br>
(test::assert-equal '("some" "yyy" "stringxxxother") (str-splitn 3 "xxx" "somexxxyyyxxxstringxxxother"))<br>
(test::assert-equal '("somexxxyyyxxxstringxxxother") (str-splitn 1 "xxx" "somexxxyyyxxxstringxxxother"))<br>
(test::assert-equal '() (str-splitn 0 "xxx" "somexxxyyyxxxstringxxxzero"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::str-starts-with" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-starts-with``](#Namespace: root::str-starts-with-contents) | Type: Function |
| ``Namespace: root::str-starts-with`` | ``Usage: (str-starts-with pattern string) -> t/nil`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (str-starts-with "Stau" "Stausomething"))<br>
(test::assert-false (str-starts-with "StaU" "Stausomething"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::str-sub" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-sub``](#Namespace: root::str-sub-contents) | Type: Function |
| ``Namespace: root::str-sub`` | ``Usage: (str-sub start length string) -> string`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal "string" (str-sub 0 6 "stringxxxyyyxxxsome"))<br>
(test::assert-equal "some" (str-sub 15 4 "stringxxxyyyxxxsome"))<br>
(test::assert-equal "yyy" (str-sub 9 3 "stringxxxyyyxxxsome"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::str-trim" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-trim``](#Namespace: root::str-trim-contents) | Type: Function |
| ``Namespace: root::str-trim`` | ``Usage: (str-trim string) -> string`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal "some string" (str-trim "   some string"))<br>
(test::assert-equal "some string" (str-trim "   some string   "))<br>
(test::assert-equal "some string" (str-trim (str "   some string   ")))<br>
(test::assert-equal "some string" (str-trim "some string   "))<br>
(test::assert-equal "some string" (str-trim "some string"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::str-upper" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-upper``](#Namespace: root::str-upper-contents) | Type: Function |
| ``Namespace: root::str-upper`` | ``Usage: (str-upper string) -> string`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal "STAU" (str-upper "STAU"))<br>
(test::assert-equal "STAU" (str-upper "stau"))<br>
(test::assert-equal "STAU" (str-upper "Stau"))<br>
(test::assert-equal "STAU" (str-upper "StaU"))<br>
(test::assert-equal "STAU" (str-upper "sTaU"))<br>
<br>
</code>
</details>
### <a id="struct forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[struct forms](#struct forms-contents)



| <a id="Namespace: struct::defstruct" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``defstruct``](#Namespace: struct::defstruct-contents) | Type: Macro |
| ``Namespace: struct::defstruct`` | ``Usage: (defstruct name &rest fields)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(struct::defstruct test-struct<br>
; fields<br>
(a "a private attribute" nil)<br>
(b "a read/write attribute" "bee" :rw)<br>
(c "a read only attribute" "see" :ro)<br>
(d "a write only attribute" "dee" :wo)<br>
; methods<br>
(:fn what-d (self) d)<br>
(:fn what-a (self) a))<br>
(def ts (test-struct))<br>
(assert-equal nil (ts :what-a))<br>
(assert-equal "dee" (ts :what-d))<br>
(ts :set-d "something else")<br>
(assert-equal "something else" (ts :what-d))<br>
(assert-equal "bee" (ts :b))<br>
(assert-equal "see" (ts :c))<br>
(ts :set-b "queen")<br>
(assert-equal "queen" (ts :b))<br>
<br>
</code>
</details>


| <a id="Namespace: struct::deftrait" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``deftrait``](#Namespace: struct::deftrait-contents) | Type: Macro |
| ``Namespace: struct::deftrait`` | ``Usage: (deftrait name &rest fields)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(ns-push 'test-deftrait)<br>
(struct::deftrait test-trait<br>
; Require what-a and what-d in the structures that implement this trait.<br>
(:fn aaa (self) (self :what-a))<br>
(:fn ddd (self) (self :what-d)))<br>
(struct::defstruct test-struct<br>
; fields<br>
(a "a private attribute" nil)<br>
(b "a read/write attribute" "bee" :rw)<br>
(c "a read only attribute" "see" :ro)<br>
(d "a write only attribute" "dee" :wo)<br>
; methods<br>
(:fn what-d (self) d)<br>
(:fn what-a (self) a)<br>
(:impl test-deftrait::test-trait))<br>
(def ts (test-struct))<br>
(test::assert-equal nil (ts :aaa))<br>
(test::assert-equal "dee" (ts :ddd))<br>
(ts :set-d "something else")<br>
(test::assert-equal "something else" (ts :ddd))<br>
(test::assert-equal "bee" (ts :b))<br>
(test::assert-equal "see" (ts :c))<br>
(ts :set-b "queen")<br>
(test::assert-equal "queen" (ts :b))<br>
(ns-pop)<br>
<br>
</code>
</details>
### <a id="Type forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Type forms](#Type forms-contents)
These forms provide information/tests about an objects underlying type.


| <a id="Namespace: root::builtin?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``builtin?``](#Namespace: root::builtin?-contents) | Type: Function |
| ``Namespace: root::builtin?`` | ``Usage: (builtin? expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (builtin? type))<br>
(test::assert-true (builtin? if))<br>
(test::assert-false (builtin? (fn () ())))<br>
(test::assert-false (builtin? caar))<br>
(test::assert-false (builtin? 1))<br>
<br>
</code>
</details>


| <a id="Namespace: root::char?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char?``](#Namespace: root::char?-contents) | Type: Function |
| ``Namespace: root::char?`` | ``Usage: (char? expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (char? #\a))<br>
(test::assert-false (char? 1))<br>
(test::assert-false (char? "a"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::file?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``file?``](#Namespace: root::file?-contents) | Type: Function |
| ``Namespace: root::file?`` | ``Usage: (file? expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (file? (open :stdout)))<br>
(test::assert-false (file? (fn () ())))<br>
(test::assert-false (file? caar))<br>
(test::assert-false (file? 1))<br>
<br>
</code>
</details>


| <a id="Namespace: root::float->int" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``float->int``](#Namespace: root::float->int-contents) | Type: Function |
| ``Namespace: root::float->int`` | ``Usage: (float->int float) -> int`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal 0 (float->int 0.0))<br>
(test::assert-equal 10 (float->int 10.0))<br>
(test::assert-equal 10 (float->int 10.1))<br>
(test::assert-equal 10 (float->int 10.5))<br>
(test::assert-equal 10 (float->int 10.9))<br>
(test::assert-equal -101 (float->int -101.99))<br>
(test::assert-error (float->int "not int"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::float?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``float?``](#Namespace: root::float?-contents) | Type: Function |
| ``Namespace: root::float?`` | ``Usage: (float? expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (float? 1.5))<br>
(test::assert-false (float? 1))<br>
<br>
</code>
</details>


| <a id="Namespace: root::func?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``func?``](#Namespace: root::func?-contents) | Type: Macro |
| ``Namespace: root::func?`` | ``Usage: (func? to-test)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def func?-test 1)<br>
(test::assert-false (func? func?-test))<br>
(test::assert-true (func? car))<br>
(test::assert-true (func? first))<br>
(test::assert-true (func? let))<br>
<br>
</code>
</details>


| <a id="Namespace: root::hash?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash?``](#Namespace: root::hash?-contents) | Type: Function |
| ``Namespace: root::hash?`` | ``Usage: (hash? expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (hash? (make-hash)) "make-vec")<br>
(test::assert-false (hash? 1))<br>
(test::assert-false (hash? '(1 2 3)))<br>
(test::assert-false (hash? (list)))<br>
(test::assert-false (hash? (vec)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::int->float" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``int->float``](#Namespace: root::int->float-contents) | Type: Function |
| ``Namespace: root::int->float`` | ``Usage: (int->float int) -> float`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal 0 (int->float 0))<br>
(test::assert-equal 10 (int->float 10))<br>
(test::assert-equal -101 (int->float -101))<br>
(test::assert-error (int->float "not int"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::int?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``int?``](#Namespace: root::int?-contents) | Type: Function |
| ``Namespace: root::int?`` | ``Usage: (int? expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (int? 1))<br>
(test::assert-false (int? 1.5))<br>
<br>
</code>
</details>


| <a id="Namespace: root::lambda?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``lambda?``](#Namespace: root::lambda?-contents) | Type: Function |
| ``Namespace: root::lambda?`` | ``Usage: (lambda? expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (lambda? (fn () ())))<br>
(test::assert-true (lambda? caar))<br>
(test::assert-false (lambda? 1))<br>
(test::assert-false (lambda? if))<br>
<br>
</code>
</details>


| <a id="Namespace: root::list?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``list?``](#Namespace: root::list?-contents) | Type: Function |
| ``Namespace: root::list?`` | ``Usage: (list? expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (list? '(1 2 3)) "reader macro")<br>
(test::assert-true (list? (list 1 2 3)) "list")<br>
(test::assert-false (list? 1))<br>
(test::assert-false (list? '#(1 2 3)))<br>
(test::assert-false (list? (vec)))<br>
(test::assert-false (list? '(1 . 2)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::macro?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``macro?``](#Namespace: root::macro?-contents) | Type: Function |
| ``Namespace: root::macro?`` | ``Usage: (macro? expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (macro? (macro () ())))<br>
(test::assert-true (macro? defn))<br>
(test::assert-false (macro? 1))<br>
(test::assert-false (macro? if))<br>
<br>
</code>
</details>


| <a id="Namespace: root::nil?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``nil?``](#Namespace: root::nil?-contents) | Type: Function |
| ``Namespace: root::nil?`` | ``Usage: (nil? expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (nil? nil))<br>
(test::assert-false (nil? t))<br>
<br>
</code>
</details>


| <a id="Namespace: root::pair?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``pair?``](#Namespace: root::pair?-contents) | Type: Function |
| ``Namespace: root::pair?`` | ``Usage: (pair? expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (pair? '(1 . 2)) "reader macro")<br>
(test::assert-true (pair? (join 1 2)) "join")<br>
(test::assert-true (pair? '(1 2)))<br>
(test::assert-false (pair? 1))<br>
(test::assert-false (pair? '#(1 2 3)))<br>
(test::assert-false (pair? (vec)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::process?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``process?``](#Namespace: root::process?-contents) | Type: Function |
| ``Namespace: root::process?`` | ``Usage: (process? expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (process? (true)))<br>
(test::assert-false (process? (fn () ())))<br>
(test::assert-false (process? caar))<br>
(test::assert-false (process? 1))<br>
<br>
</code>
</details>


| <a id="Namespace: root::str->float" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str->float``](#Namespace: root::str->float-contents) | Type: Function |
| ``Namespace: root::str->float`` | ``Usage: (str->float string) -> float`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal 0 (str->float "0"))<br>
(test::assert-equal 10.0 (str->float "10.0"))<br>
(test::assert-equal 10.5 (str->float "10.5"))<br>
(test::assert-equal 101 (str->float "101"))<br>
(test::assert-equal -101.95 (str->float "-101.95"))<br>
(test::assert-error (str->float "not int"))<br>
(test::assert-error (str->float "--10"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::str->int" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str->int``](#Namespace: root::str->int-contents) | Type: Function |
| ``Namespace: root::str->int`` | ``Usage: (str->int string) -> int`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal 0 (str->int "0"))<br>
(test::assert-equal 101 (str->int "101"))<br>
(test::assert-equal -101 (str->int "-101"))<br>
(test::assert-error (str->int "not int"))<br>
(test::assert-error (str->int "10.0"))<br>
(test::assert-error (str->int "--10"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::string?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``string?``](#Namespace: root::string?-contents) | Type: Function |
| ``Namespace: root::string?`` | ``Usage: (string? expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (string? "string"))<br>
(test::assert-false (string? 1))<br>
<br>
</code>
</details>


| <a id="Namespace: root::sym" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``sym``](#Namespace: root::sym-contents) | Type: Function |
| ``Namespace: root::sym`` | ``Usage: (sym expression+) -> symbol`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-to-symbol-sym nil)<br>
(test::assert-true (symbol? (sym 55)))<br>
(test::assert-true (symbol? (sym 55.0)))<br>
(test::assert-true (symbol? (sym "to-symbol-test-new-symbol")))<br>
(test::assert-true (symbol? (sym (str "to-symbol-test-new-symbol-buf"))))<br>
(test::assert-true (symbol? (sym 'test-to-symbol-sym)))<br>
(set! test-to-symbol-sym "testing-sym")<br>
(test::assert-equal "testing-sym" (sym->str (sym test-to-symbol-sym)))<br>
(test::assert-true (symbol? (sym (sym->str 'test-to-symbol-sym))))<br>
<br>
</code>
</details>


| <a id="Namespace: root::sym->str" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``sym->str``](#Namespace: root::sym->str-contents) | Type: Function |
| ``Namespace: root::sym->str`` | ``Usage: (sym->str symbol) -> string`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-sym->str-sym nil)<br>
(test::assert-true (string? (sym->str 'test-sym->str-sym)))<br>
(test::assert-equal "test-sym->str-sym" (sym->str 'test-sym->str-sym))<br>
<br>
</code>
</details>


| <a id="Namespace: root::symbol?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``symbol?``](#Namespace: root::symbol?-contents) | Type: Function |
| ``Namespace: root::symbol?`` | ``Usage: (symbol? expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (symbol? 'symbol))<br>
(test::assert-false (symbol? 1))<br>
<br>
</code>
</details>


| <a id="Namespace: root::true?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``true?``](#Namespace: root::true?-contents) | Type: Function |
| ``Namespace: root::true?`` | ``Usage: (true? expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (true? t))<br>
(test::assert-false (true? nil))<br>
(test::assert-false (true? 1))<br>
(test::assert-false (true? "str"))<br>
<br>
</code>
</details>


| <a id="Namespace: root::type" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``type``](#Namespace: root::type-contents) | Type: Function |
| ``Namespace: root::type`` | ``Usage: (type expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal "True" (type t))<br>
(test::assert-equal "Float" (type 1.1))<br>
(test::assert-equal "Int" (type 1))<br>
(test::assert-equal "Symbol" (type 'symbol))<br>
(def type-sym 'symbol)<br>
(test::assert-equal "Symbol" (type type-sym))<br>
(test::assert-equal "String" (type "string"))<br>
(test::assert-equal "Char" (type #\a))<br>
(test::assert-equal "Lambda" (type (fn () ())))<br>
(test::assert-equal "Macro" (type (macro () ())))<br>
(test::assert-equal "Process" (type (true)))<br>
(test::assert-equal "SpecialForm" (type if))<br>
(test::assert-equal "Function" (type type))<br>
(test::assert-equal "Vector" (type '#(1 2 3)))<br>
(def type-vec '#(4 5 6))<br>
(test::assert-equal "Vector" (type type-vec))<br>
(test::assert-equal "Pair" (type '(1 . 2)))<br>
(test::assert-equal "Pair" (type '(1 2 3)))<br>
(test::assert-equal "Nil" (type nil))<br>
(test::assert-equal "Nil" (type '()))<br>
(test::assert-equal "HashMap" (type (make-hash)))<br>
(test::assert-equal "File" (type (open :stdin)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::values?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``values?``](#Namespace: root::values?-contents) | Type: Function |
| ``Namespace: root::values?`` | ``Usage: (values? expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (values? (values 1 "str" 5.5)))<br>
(test::assert-false (values? '(1 2 3)))<br>
(test::assert-false (values? '(1 . 3)))<br>
(test::assert-false (values? 1))<br>
(test::assert-true (int? (values 1 "str" 5.5)))<br>
(test::assert-false (string? (values 1 "str" 5.5)))<br>
(test::assert-false (float? (values 1 "str" 5.5)))<br>
(def test-is-values (values 1 2 3 "string" 1.5))<br>
(test::assert-true (values? test-is-values))<br>
(test::assert-true (int? test-is-values))<br>
(test::assert-false (string? test-is-values))<br>
(test::assert-false (float? test-is-values))<br>
<br>
</code>
</details>


| <a id="Namespace: root::vec?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec?``](#Namespace: root::vec?-contents) | Type: Function |
| ``Namespace: root::vec?`` | ``Usage: (vec? expression)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (vec? '#(1 2 3)) "reader macro")<br>
(test::assert-true (vec? (make-vec)) "make-vec")<br>
(test::assert-true (vec? (vec 1 2 3)) "vec")<br>
(test::assert-false (vec? 1))<br>
(test::assert-false (vec? '(1 2 3)))<br>
(test::assert-false (vec? (list)))<br>
<br>
</code>
</details>
### <a id="Vector forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Vector forms](#Vector forms-contents)
Forms ending in '!' are destructive and change the underlying vector, other forms
do not make changes to the the provided vector.  They are usable in place of a
list for purposes of lambda calls, parameters, etc (they work the same as a list
made from pairs but are vectors not linked lists).  Use #() to declare them in
code (i.e. '#(1 2 3) or #(+ 1 2)).


| <a id="Namespace: root::make-vec" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``make-vec``](#Namespace: root::make-vec-contents) | Type: Function |
| ``Namespace: root::make-vec`` | ``Usage: (make-vec capacity default)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-false (make-vec))<br>
(test::assert-equal '(x x x) (make-vec 3 'x))<br>
(test::assert-equal '(nil nil nil nil nil) (make-vec 5 nil))<br>
(test::assert-equal '() (make-vec 5))<br>
<br>
</code>
</details>


| <a id="Namespace: root::vec" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec``](#Namespace: root::vec-contents) | Type: Function |
| ``Namespace: root::vec`` | ``Usage: (vec item1 item2 .. itemN)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-false (vec))<br>
(test::assert-equal '(1 2 3) (vec 1 2 3))<br>
<br>
</code>
</details>


| <a id="Namespace: root::vec-clear!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-clear!``](#Namespace: root::vec-clear!-contents) | Type: Function |
| ``Namespace: root::vec-clear!`` | ``Usage: (vec-clear! vector)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-clear-vec (vec 1 2 3))<br>
(test::assert-false (vec-empty? test-clear-vec))<br>
(vec-clear! test-clear-vec)<br>
(test::assert-true (vec-empty? test-clear-vec))<br>
<br>
</code>
</details>


| <a id="Namespace: root::vec-empty?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-empty?``](#Namespace: root::vec-empty?-contents) | Type: Function |
| ``Namespace: root::vec-empty?`` | ``Usage: (vec-empty? vector)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-true (vec-empty? '#()))<br>
(test::assert-false (vec-empty? '#(1 2 3)))<br>
<br>
</code>
</details>


| <a id="Namespace: root::vec-insert!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-insert!``](#Namespace: root::vec-insert!-contents) | Type: Function |
| ``Namespace: root::vec-insert!`` | ``Usage: (vec-insert! vector index new-element) -> vector`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-insert-nth-vec (vec 1 2 3))<br>
(test::assert-equal '(1 2 3) test-insert-nth-vec)<br>
(vec-insert! test-insert-nth-vec 1 5)<br>
(test::assert-equal '(1 5 2 3) test-insert-nth-vec)<br>
(vec-insert! test-insert-nth-vec 2 6)<br>
(test::assert-equal '(1 5 6 2 3) test-insert-nth-vec)<br>
(vec-insert! test-insert-nth-vec 0 4)<br>
(test::assert-equal '(4 1 5 6 2 3) test-insert-nth-vec)<br>
<br>
</code>
</details>


| <a id="Namespace: root::vec-nth" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-nth``](#Namespace: root::vec-nth-contents) | Type: Function |
| ``Namespace: root::vec-nth`` | ``Usage: (vec-nth vector index) -> object`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal 5 (vec-nth '#(1 2 3 4 5 6) 4))<br>
(test::assert-equal 1 (vec-nth '#(1 2 3 4 5 6) 0))<br>
(test::assert-equal 3 (vec-nth '#(1 2 3 4 5 6) 2))<br>
(test::assert-equal 6 (vec-nth '#(1 2 3 4 5 6) 5))<br>
<br>
</code>
</details>


| <a id="Namespace: root::vec-pop!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-pop!``](#Namespace: root::vec-pop!-contents) | Type: Function |
| ``Namespace: root::vec-pop!`` | ``Usage: (vec-pop! vector) -> object`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-pop-vec (vec 1 2 3))<br>
(test::assert-equal 3 (vec-pop! test-pop-vec))<br>
(test::assert-equal '(1 2) test-pop-vec)<br>
(test::assert-equal 2 (vec-pop! test-pop-vec))<br>
(test::assert-equal '(1) test-pop-vec)<br>
(test::assert-equal 1 (vec-pop! test-pop-vec))<br>
(test::assert-equal '() test-pop-vec)<br>
<br>
</code>
</details>


| <a id="Namespace: root::vec-push!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-push!``](#Namespace: root::vec-push!-contents) | Type: Function |
| ``Namespace: root::vec-push!`` | ``Usage: (vec-push! vector object) -> vector`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-push-vec (vec))<br>
(test::assert-equal '(1) (vec-push! test-push-vec 1))<br>
(test::assert-equal '(1) test-push-vec)<br>
(test::assert-equal '(1 2) (vec-push! test-push-vec 2))<br>
(test::assert-equal '(1 2) test-push-vec)<br>
(test::assert-equal '(1 2 3) (vec-push! test-push-vec 3))<br>
(test::assert-equal '(1 2 3) test-push-vec)<br>
<br>
</code>
</details>


| <a id="Namespace: root::vec-remove!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-remove!``](#Namespace: root::vec-remove!-contents) | Type: Function |
| ``Namespace: root::vec-remove!`` | ``Usage: (vec-remove! vector index) -> vector`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-remove-nth-vec (vec 1 2 3))<br>
(test::assert-equal '(1 2 3) test-remove-nth-vec)<br>
(vec-remove! test-remove-nth-vec 1)<br>
(test::assert-equal '(1 3) test-remove-nth-vec)<br>
(vec-remove! test-remove-nth-vec 1)<br>
(test::assert-equal '(1) test-remove-nth-vec)<br>
(vec-remove! test-remove-nth-vec 0)<br>
(test::assert-equal '() test-remove-nth-vec)<br>
<br>
</code>
</details>


| <a id="Namespace: root::vec-set!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-set!``](#Namespace: root::vec-set!-contents) | Type: Function |
| ``Namespace: root::vec-set!`` | ``Usage: (vec-set! vector index value) -> vector`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(def test-setnth-vec (vec 1 2 3))<br>
(test::assert-equal '(1 5 3) (vec-set! test-setnth-vec 1 5))<br>
(test::assert-equal '(7 5 3) (vec-set! test-setnth-vec 0 7))<br>
(test::assert-equal '(7 5 9) (vec-set! test-setnth-vec 2 9))<br>
<br>
</code>
</details>


| <a id="Namespace: root::vec-slice" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-slice``](#Namespace: root::vec-slice-contents) | Type: Function |
| ``Namespace: root::vec-slice`` | ``Usage: (vec-slice vector start end?)`` |

<details style="padding-bottom: 5px;">
<summary>⮞</summary>
<code>
Example:<br>
(test::assert-equal '(5 6) (vec-slice '#(1 2 3 4 5 6) 4 6))<br>
(test::assert-equal '(1 2 3) (vec-slice '#(1 2 3 4 5 6) 0 3))<br>
(test::assert-equal '(3 4 5) (vec-slice '#(1 2 3 4 5 6) 2 5))<br>
(test::assert-equal '(3 4 5 6) (vec-slice '#(1 2 3 4 5 6) 2))<br>
<br>
</code>
</details>

version: sl-sh 0.9.27 (feature/documentationier-docs:df06119, release build, linux [x86_64], Mar 25 2021, 04:48:06 UTC [rustc 1.50.0 (cb75ad5db 2021-02-10)])

