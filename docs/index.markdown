---
layout: default
title: Sl-sh form documentation
---

# Sl-sh


## Documentation structure for each form



| <b>form name</b> | <b>type</b> (see: [Type forms](#Type forms-contents)) |
| <b>namespace</b> (fully qualified names are of format namespace::symbol) | <b>usage</b> |

```
example code if exists
```

## Table of Contents

### <a id="Char forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Char forms](#Char forms)


<a id="root::char!=-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char!=``](#root::char!=), <a id="root::char-lower-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char-lower``](#root::char-lower), <a id="root::char-upper-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char-upper``](#root::char-upper), <a id="root::char-whitespace?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char-whitespace?``](#root::char-whitespace?), <a id="root::char<-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char<``](#root::char<), <a id="root::char<=-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char<=``](#root::char<=), <a id="root::char=-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char=``](#root::char=), <a id="root::char>-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char>``](#root::char>), <a id="root::char>=-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char>=``](#root::char>=), 
### <a id="Conditional forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Conditional forms](#Conditional forms)


<a id="root::<-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``<``](#root::<), <a id="root::<=-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``<=``](#root::<=), <a id="root::=-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``=``](#root::=), <a id="root::>-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``>``](#root::>), <a id="root::>=-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``>=``](#root::>=), <a id="root::and-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``and``](#root::and), <a id="root::if-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``if``](#root::if), <a id="root::not-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``not``](#root::not), <a id="root::null-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``null``](#root::null), <a id="root::or-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``or``](#root::or), <a id="core::when-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``when``](#core::when), 
### <a id="Core forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Core forms](#Core forms)


<a id="root::apply-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``apply``](#root::apply), <a id="root::block-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``block``](#root::block), <a id="root::bquote-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``bquote``](#root::bquote), <a id="root::def-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``def``](#root::def), <a id="root::def?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``def?``](#root::def?), <a id="core::defmacro-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``defmacro``](#core::defmacro), <a id="core::defn-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``defn``](#core::defn), <a id="core::defq-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``defq``](#core::defq), <a id="root::doc-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``doc``](#root::doc), <a id="root::doc-raw-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``doc-raw``](#root::doc-raw), <a id="root::dyn-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``dyn``](#root::dyn), <a id="root::eprint-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``eprint``](#root::eprint), <a id="root::eprintln-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``eprintln``](#root::eprintln), <a id="root::err-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``err``](#root::err), <a id="root::error-stack-off-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``error-stack-off``](#root::error-stack-off), <a id="root::error-stack-on-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``error-stack-on``](#root::error-stack-on), <a id="root::eval-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``eval``](#root::eval), <a id="root::expand-macro-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``expand-macro``](#root::expand-macro), <a id="root::expand-macro-all-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``expand-macro-all``](#root::expand-macro-all), <a id="root::expand-macro1-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``expand-macro1``](#root::expand-macro1), <a id="root::fn-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fn``](#root::fn), <a id="root::fncall-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fncall``](#root::fncall), <a id="root::format-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``format``](#root::format), <a id="root::gensym-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``gensym``](#root::gensym), <a id="root::get-error-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``get-error``](#root::get-error), <a id="root::intern-stats-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``intern-stats``](#root::intern-stats), <a id="root::length-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``length``](#root::length), <a id="core::let-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``let``](#core::let), <a id="root::load-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``load``](#root::load), <a id="root::macro-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``macro``](#root::macro), <a id="core::match-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``match``](#core::match), <a id="root::meta-column-no-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``meta-column-no``](#root::meta-column-no), <a id="root::meta-file-name-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``meta-file-name``](#root::meta-file-name), <a id="root::meta-line-no-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``meta-line-no``](#root::meta-line-no), <a id="root::print-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``print``](#root::print), <a id="root::println-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``println``](#root::println), <a id="root::progn-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``progn``](#root::progn), <a id="root::quote-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``quote``](#root::quote), <a id="root::recur-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``recur``](#root::recur), <a id="root::return-from-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``return-from``](#root::return-from), <a id="root::set-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``set``](#root::set), <a id="core::setfn-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``setfn``](#core::setfn), <a id="core::setmacro-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``setmacro``](#core::setmacro), <a id="core::setq-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``setq``](#core::setq), <a id="root::symbol-name-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``symbol-name``](#root::symbol-name), <a id="root::to-symbol-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``to-symbol``](#root::to-symbol), <a id="root::undef-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``undef``](#root::undef), <a id="root::unwind-protect-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``unwind-protect``](#root::unwind-protect), 
### <a id="File forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[File forms](#File forms)


<a id="root::close-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``close``](#root::close), <a id="root::flush-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``flush``](#root::flush), <a id="root::open-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``open``](#root::open), <a id="root::read-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``read``](#root::read), <a id="root::read-line-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``read-line``](#root::read-line), <a id="root::write-line-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``write-line``](#root::write-line), <a id="root::write-string-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``write-string``](#root::write-string), 
### <a id="Hashmap forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Hashmap forms](#Hashmap forms)


<a id="root::hash-clear!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash-clear!``](#root::hash-clear!), <a id="root::hash-get-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash-get``](#root::hash-get), <a id="root::hash-haskey-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash-haskey``](#root::hash-haskey), <a id="root::hash-keys-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash-keys``](#root::hash-keys), <a id="root::hash-remove!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash-remove!``](#root::hash-remove!), <a id="root::hash-set!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash-set!``](#root::hash-set!), <a id="root::make-hash-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``make-hash``](#root::make-hash), 
### <a id="Math forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Math forms](#Math forms)


<a id="root::%-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``%``](#root::%), <a id="root::*-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*``](#root::*), <a id="root::+-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``+``](#root::+), <a id="root::--contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``-``](#root::-), <a id="root::/-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``/``](#root::/), 
### <a id="Namespace forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Namespace forms](#Namespace forms)


<a id="root::ns-create-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-create``](#root::ns-create), <a id="root::ns-enter-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-enter``](#root::ns-enter), <a id="root::ns-exists?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-exists?``](#root::ns-exists?), <a id="core::ns-export-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-export``](#core::ns-export), <a id="core::ns-import-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-import``](#core::ns-import), <a id="root::ns-list-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-list``](#root::ns-list), <a id="root::ns-pop-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-pop``](#root::ns-pop), <a id="root::ns-symbols-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-symbols``](#root::ns-symbols), 
### <a id="Pair forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Pair forms](#Pair forms)


<a id="root::car-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``car``](#root::car), <a id="root::cdr-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cdr``](#root::cdr), <a id="root::join-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``join``](#root::join), <a id="root::list-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``list``](#root::list), <a id="root::xar!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``xar!``](#root::xar!), <a id="root::xdr!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``xdr!``](#root::xdr!), 
### <a id="Unknown forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Unknown forms](#Unknown forms)


<a id="root::*load-path*-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*load-path*``](#root::*load-path*), 
### <a id="Sequence forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Sequence forms](#Sequence forms)


<a id="core::append-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``append``](#core::append), <a id="core::append!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``append!``](#core::append!), <a id="core::butlast-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``butlast``](#core::butlast), <a id="core::copy-seq-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``copy-seq``](#core::copy-seq), <a id="core::empty-seq?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``empty-seq?``](#core::empty-seq?), <a id="core::filter-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``filter``](#core::filter), <a id="core::first-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``first``](#core::first), <a id="core::in?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``in?``](#core::in?), <a id="core::last-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``last``](#core::last), <a id="core::loop-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``loop``](#core::loop), <a id="core::map-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``map``](#core::map), <a id="core::map!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``map!``](#core::map!), <a id="core::non-empty-seq?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``non-empty-seq?``](#core::non-empty-seq?), <a id="core::nth-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``nth``](#core::nth), <a id="core::qsort-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``qsort``](#core::qsort), <a id="core::reduce-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``reduce``](#core::reduce), <a id="core::rest-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``rest``](#core::rest), <a id="core::reverse-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``reverse``](#core::reverse), <a id="core::reverse!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``reverse!``](#core::reverse!), <a id="core::seq?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``seq?``](#core::seq?), <a id="core::setnth!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``setnth!``](#core::setnth!), 
### <a id="Shell forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Shell forms](#Shell forms)


<a id="root::*stderr*-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*stderr*``](#root::*stderr*), <a id="root::*stdin*-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*stdin*``](#root::*stdin*), <a id="root::*stdout*-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*stdout*``](#root::*stdout*), <a id="shell::alias-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``alias``](#shell::alias), <a id="shell::alias?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``alias?``](#shell::alias?), <a id="root::bg-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``bg``](#root::bg), <a id="shell::bg-color-rgb-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``bg-color-rgb``](#shell::bg-color-rgb), <a id="root::cd-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cd``](#root::cd), <a id="shell::clear-dirs-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``clear-dirs``](#shell::clear-dirs), <a id="root::command-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``command``](#root::command), <a id="shell::dirs-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``dirs``](#shell::dirs), <a id="core::dotimes-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``dotimes``](#core::dotimes), <a id="core::dotimesi-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``dotimesi``](#core::dotimesi), <a id="shell::endfix-on-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``endfix-on``](#shell::endfix-on), <a id="shell::err>-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``err>``](#shell::err>), <a id="shell::err>>-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``err>>``](#shell::err>>), <a id="shell::err>null-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``err>null``](#shell::err>null), <a id="root::exit-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``exit``](#root::exit), <a id="root::fg-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fg``](#root::fg), <a id="shell::fg-color-rgb-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fg-color-rgb``](#shell::fg-color-rgb), <a id="core::for-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``for``](#core::for), <a id="core::fori-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fori``](#core::fori), <a id="root::form-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``form``](#root::form), <a id="root::fs-dir?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fs-dir?``](#root::fs-dir?), <a id="root::fs-exists?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fs-exists?``](#root::fs-exists?), <a id="root::fs-file?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fs-file?``](#root::fs-file?), <a id="shell::get-dirs-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``get-dirs``](#shell::get-dirs), <a id="root::glob-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``glob``](#root::glob), <a id="root::jobs-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``jobs``](#root::jobs), <a id="shell::let-env-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``let-env``](#shell::let-env), <a id="root::loose-symbols-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``loose-symbols``](#root::loose-symbols), <a id="shell::out-err>-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``out-err>``](#shell::out-err>), <a id="shell::out-err>>-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``out-err>>``](#shell::out-err>>), <a id="shell::out-err>null-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``out-err>null``](#shell::out-err>null), <a id="shell::out>-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``out>``](#shell::out>), <a id="shell::out>>-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``out>>``](#shell::out>>), <a id="shell::out>null-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``out>null``](#shell::out>null), <a id="root::pid-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``pid``](#root::pid), <a id="root::pipe-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``pipe``](#root::pipe), <a id="shell::popd-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``popd``](#shell::popd), <a id="shell::pushd-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``pushd``](#shell::pushd), <a id="shell::register-alias-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``register-alias``](#shell::register-alias), <a id="root::run-bg-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``run-bg``](#root::run-bg), <a id="shell::set-dirs-max-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``set-dirs-max``](#shell::set-dirs-max), <a id="shell::syntax-off-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``syntax-off``](#shell::syntax-off), <a id="shell::syntax-on-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``syntax-on``](#shell::syntax-on), <a id="shell::sys-command?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``sys-command?``](#shell::sys-command?), <a id="root::unexport-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``unexport``](#root::unexport), <a id="shell::unregister-alias-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``unregister-alias``](#shell::unregister-alias), <a id="root::version-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``version``](#root::version), <a id="root::wait-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``wait``](#root::wait), <a id="shell::pipe-shorthand-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``|``](#shell::pipe-shorthand), 
### <a id="String forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[String forms](#String forms)


<a id="root::str-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str``](#root::str), <a id="root::str-append-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-append``](#root::str-append), <a id="root::str-buf-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-buf``](#root::str-buf), <a id="root::str-buf-clear!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-buf-clear!``](#root::str-buf-clear!), <a id="root::str-buf-map-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-buf-map``](#root::str-buf-map), <a id="root::str-buf-push!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-buf-push!``](#root::str-buf-push!), <a id="root::str-bytes-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-bytes``](#root::str-bytes), <a id="root::str-cat-list-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-cat-list``](#root::str-cat-list), <a id="root::str-contains-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-contains``](#root::str-contains), <a id="root::str-empty?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-empty?``](#root::str-empty?), <a id="root::str-ignore-expand-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-ignore-expand``](#root::str-ignore-expand), <a id="root::str-lower-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-lower``](#root::str-lower), <a id="root::str-ltrim-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-ltrim``](#root::str-ltrim), <a id="root::str-map-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-map``](#root::str-map), <a id="root::str-nth-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-nth``](#root::str-nth), <a id="root::str-replace-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-replace``](#root::str-replace), <a id="root::str-rsplit-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-rsplit``](#root::str-rsplit), <a id="root::str-rsplitn-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-rsplitn``](#root::str-rsplitn), <a id="root::str-rtrim-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-rtrim``](#root::str-rtrim), <a id="root::str-split-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-split``](#root::str-split), <a id="root::str-splitn-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-splitn``](#root::str-splitn), <a id="root::str-starts-with-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-starts-with``](#root::str-starts-with), <a id="root::str-sub-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-sub``](#root::str-sub), <a id="root::str-trim-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-trim``](#root::str-trim), <a id="root::str-upper-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-upper``](#root::str-upper), 
### <a id="Type forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Type forms](#Type forms)


<a id="root::builtin?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``builtin?``](#root::builtin?), <a id="root::char?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char?``](#root::char?), <a id="root::file?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``file?``](#root::file?), <a id="root::float?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``float?``](#root::float?), <a id="core::func?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``func?``](#core::func?), <a id="root::hash?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash?``](#root::hash?), <a id="root::int?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``int?``](#root::int?), <a id="root::lambda?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``lambda?``](#root::lambda?), <a id="root::list?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``list?``](#root::list?), <a id="root::macro?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``macro?``](#root::macro?), <a id="root::nil?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``nil?``](#root::nil?), <a id="root::pair?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``pair?``](#root::pair?), <a id="root::process?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``process?``](#root::process?), <a id="root::string-buf?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``string-buf?``](#root::string-buf?), <a id="root::string?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``string?``](#root::string?), <a id="root::symbol?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``symbol?``](#root::symbol?), <a id="root::true?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``true?``](#root::true?), <a id="root::type-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``type``](#root::type), <a id="root::vec?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec?``](#root::vec?), 
### <a id="Vector forms-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Vector forms](#Vector forms)


<a id="root::make-vec-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``make-vec``](#root::make-vec), <a id="root::vec-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec``](#root::vec), <a id="root::vec-clear!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-clear!``](#root::vec-clear!), <a id="root::vec-empty?-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-empty?``](#root::vec-empty?), <a id="root::vec-insert-nth!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-insert-nth!``](#root::vec-insert-nth!), <a id="root::vec-nth-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-nth``](#root::vec-nth), <a id="root::vec-pop!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-pop!``](#root::vec-pop!), <a id="root::vec-push!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-push!``](#root::vec-push!), <a id="root::vec-remove-nth!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-remove-nth!``](#root::vec-remove-nth!), <a id="root::vec-setnth!-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-setnth!``](#root::vec-setnth!), <a id="root::vec-slice-contents" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-slice``](#root::vec-slice), 

## Documentation

### <a id="Char forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Char forms](#Char forms-contents)



| <a id="root::char!=" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char!=``](#root::char!=-contents) | Function |
| root::char!= | ``(char!= char0 char1 ... charN) -> t/nil``<br><br>Test chars for in-equality. |

```
(test::assert-false (char!= #\  #\ ))
(test::assert-false (char!= #\a #\a))
(test::assert-false (char!= #\a #\a #\a))
(test::assert-true (char!= #\z #\a))
(test::assert-true (char!= #\z #\a #\a))
(test::assert-true (char!= #\z #\Z))
(test::assert-false (char!= #\a (char-lower #\A)))
```


| <a id="root::char-lower" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char-lower``](#root::char-lower-contents) | Function |
| root::char-lower | ``(char-lower char) -> char``<br><br>Get ascii lower case character for a character. |

```
(test::assert-equal #\a (char-lower #\A))
(test::assert-equal #\a (char-lower #\a))
(test::assert-not-equal #\a (char-lower #\Z))
```


| <a id="root::char-upper" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char-upper``](#root::char-upper-contents) | Function |
| root::char-upper | ``(char-upper char) -> char``<br><br>Get ascii upper case character for a character. |

```
(test::assert-equal #\A (char-upper #\A))
(test::assert-equal #\A (char-upper #\a))
(test::assert-not-equal #\A (char-upper #\Z))
```


| <a id="root::char-whitespace?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char-whitespace?``](#root::char-whitespace?-contents) | Function |
| root::char-whitespace? | ``(char-whitespace? char) -> t/nil``<br><br>Returns true if a character is whitespace, false/nil otherwise. |

```
(test::assert-true (char-whitespace? #\ ))
(test::assert-true (char-whitespace? #\tab))
(test::assert-false (char-whitespace? #\s))
```


| <a id="root::char<" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char<``](#root::char<-contents) | Function |
| root::char< | ``(char< char0 char1 ... charN) -> t/nil``<br><br>Test chars for less than. |

```
(test::assert-false (char< #\  #\ ))
(test::assert-false (char< #\b #\b))
(test::assert-false (char< #\a #\b #\a))
(test::assert-true (char< #\a #\z))
(test::assert-true (char< #\a #\b #\c))
(test::assert-true (char< #\Z #\z))
(test::assert-true (char< #\A (char-lower #\A)))
```


| <a id="root::char<=" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char<=``](#root::char<=-contents) | Function |
| root::char<= | ``(char<= char0 char1 ... charN) -> t/nil``<br><br>Test chars for less than or equal. |

```
(test::assert-true (char<= #\  #\ ))
(test::assert-true (char<= #\b #\b))
(test::assert-false (char<= #\a #\b #\a))
(test::assert-true (char<= #\a #\z))
(test::assert-false (char<= #\z #\a))
(test::assert-true (char<= #\a #\b #\c))
(test::assert-true (char<= #\Z #\z))
(test::assert-true (char<= #\A (char-lower #\A)))
```


| <a id="root::char=" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char=``](#root::char=-contents) | Function |
| root::char= | ``(char= char0 char1 ... charN) -> t/nil``<br><br>Test chars for equality. |

```
(test::assert-true (char= #\  #\ ))
(test::assert-true (char= #\a #\a))
(test::assert-true (char= #\a #\a #\a))
(test::assert-false (char= #\z #\a))
(test::assert-false (char= #\z #\a #\a))
(test::assert-false (char= #\z #\Z))
(test::assert-true (char= #\a (char-lower #\A)))
```


| <a id="root::char>" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char>``](#root::char>-contents) | Function |
| root::char> | ``(char> char0 char1 ... charN) -> t/nil``<br><br>Test chars for greater than. |

```
(test::assert-false (char> #\  #\ ))
(test::assert-false (char> #\a #\b))
(test::assert-false (char> #\a #\b #\a))
(test::assert-true (char> #\z #\a))
(test::assert-true (char> #\c #\b #\a))
(test::assert-true (char> #\z #\Z))
(test::assert-false (char> #\a (char-lower #\A)))
```


| <a id="root::char>=" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char>=``](#root::char>=-contents) | Function |
| root::char>= | ``(char>= char0 char1 ... charN) -> t/nil``<br><br>Test chars for greater than or equal. |

```
(test::assert-true (char>= #\  #\ ))
(test::assert-false (char>= #\a #\b))
(test::assert-false (char>= #\a #\b #\a))
(test::assert-true (char>= #\z #\a))
(test::assert-true (char>= #\c #\b #\a))
(test::assert-true (char>= #\z #\Z))
(test::assert-true (char>= #\a (char-lower #\A)))
```
### <a id="Conditional forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Conditional forms](#Conditional forms-contents)



| <a id="root::<" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``<``](#root::<-contents) | Function |
| root::< | ``(< val0 ... valN)``<br><br>Less than.  Works for int, float or string. |

```
(test::assert-true (< 1 2))
(test::assert-true (< 1 2 3 4))
(test::assert-false (< 2 2))
(test::assert-false (< 2 2 2))
(test::assert-false (< 2 2 3))
(test::assert-true (< 1.0 2.0))
(test::assert-false (< 2.0 2.0))
(test::assert-false (< 2.0 2.0 2.0))
(test::assert-false (< 2.0 2.0 3.0))
(test::assert-false (< 2.1 2.0 3.0))
(test::assert-false (< 2 1))
(test::assert-false (< 3 2 3))
(test::assert-true (< "aaa" "aab"))
(test::assert-false (< "aaa" "aaa"))
(test::assert-true (< "aaa" "aab" "ccc"))
(test::assert-false (< "baa" "aab"))
```


| <a id="root::<=" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``<=``](#root::<=-contents) | Function |
| root::<= | ``(<= val0 ... valN)``<br><br>Less than or equal.  Works for int, float or string. |

```
(test::assert-true (<= 1 2))
(test::assert-true (<= 2 2))
(test::assert-true (<= 2 2 2))
(test::assert-true (<= 2 2 3))
(test::assert-true (<= 1.0 2.0))
(test::assert-true (<= 2.0 2.0))
(test::assert-true (<= 2.0 2.0 2.0))
(test::assert-true (<= 2.0 2.0 3.0))
(test::assert-false (<= 2.1 2.0 3.0))
(test::assert-false (<= 2 1))
(test::assert-false (<= 3 2 3))
(test::assert-true (<= "aaa" "aab"))
(test::assert-true (<= "aaa" "aaa"))
(test::assert-true (<= "aaa" "aab" "ccc"))
(test::assert-false (<= "baa" "aab"))
```


| <a id="root::=" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``=``](#root::=-contents) | Function |
| root::= | ``(= val0 ... valN)``<br><br>Equals.  Works for int, float or string. |

```
(test::assert-false (= 1 2))
(test::assert-true (= 2 2))
(test::assert-true (= 2 2 2))
(test::assert-false (= 3 2 2))
(test::assert-false (= 3.0 2.0))
(test::assert-true (= 2.0 2.0))
(test::assert-true (= 2.0 2.0 2.0))
(test::assert-false (= 3.0 2.0 2.0))
(test::assert-false (= 2.1 2.0 3.0))
(test::assert-false (= 2 1))
(test::assert-false (= 3 2 1))
(test::assert-false (= 1.1 1.0))
(test::assert-true (= 1.1 1.1))
(test::assert-false (= 3 2 3))
(test::assert-false (= "aab" "aaa"))
(test::assert-true (= "aaa" "aaa"))
(test::assert-true (= "aaa" "aaa" "aaa"))
(test::assert-false (= "aaa" "aaaa" "aaa"))
(test::assert-false (= "ccc" "aab" "aaa"))
(test::assert-false (= "aaa" "aab"))
```


| <a id="root::>" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``>``](#root::>-contents) | Function |
| root::> | ``(> val0 ... valN)``<br><br>Greater than.  Works for int, float or string. |

```
(test::assert-false (> 1 2))
(test::assert-false (> 2 2))
(test::assert-false (> 2 2 2))
(test::assert-false (> 3 2 2))
(test::assert-true (> 3.0 2.0))
(test::assert-false (> 2.0 2.0))
(test::assert-false (> 2.0 2.0 2.0))
(test::assert-false (> 3.0 2.0 2.0))
(test::assert-false (> 2.1 2.0 3.0))
(test::assert-true (> 2 1))
(test::assert-true (> 3 2 1))
(test::assert-true (> 1.1 1.0))
(test::assert-false (> 3 2 3))
(test::assert-true (> "aab" "aaa"))
(test::assert-false (> "aaa" "aaa"))
(test::assert-true (> "ccc" "aab" "aaa"))
(test::assert-false (> "aaa" "aab"))
```


| <a id="root::>=" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``>=``](#root::>=-contents) | Function |
| root::>= | ``(>= val0 ... valN)``<br><br>Greater than or equal.  Works for int, float or string. |

```
(test::assert-false (>= 1 2))
(test::assert-true (>= 2 2))
(test::assert-true (>= 2 2 2))
(test::assert-true (>= 3 2 2))
(test::assert-true (>= 3.0 2.0))
(test::assert-true (>= 2.0 2.0))
(test::assert-true (>= 2.0 2.0 2.0))
(test::assert-true (>= 3.0 2.0 2.0))
(test::assert-false (>= 2.1 2.0 3.0))
(test::assert-true (>= 2 1))
(test::assert-true (>= 1.1 1.0))
(test::assert-false (>= 3 2 3))
(test::assert-true (>= "aab" "aaa"))
(test::assert-true (>= "aaa" "aaa"))
(test::assert-true (>= "ccc" "aab" "aaa"))
(test::assert-false (>= "aaa" "aab"))
```


| <a id="root::and" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``and``](#root::and-contents) | SpecialForm |
| root::and | ``(and exp0 ... expN) -> [nil or expN result]``<br><br>Evaluates each form until one produces nil (false), produces nil if any form is nil or the result of the last expression.<br><br>The and form will stop evaluting when the first expression produces nil. |

```
(test::assert-false (and nil (err "and- can not happen")))
(test::assert-equal "and- done" (and t "and- done"))
(test::assert-equal "and- done" (and t t "and- done"))
(test::assert-equal 6 (and t t (+ 1 2 3)))
(test::assert-equal 6 (and (/ 10 5) (* 5 2) (+ 1 2 3)))
```


| <a id="root::if" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``if``](#root::if-contents) | SpecialForm |
| root::if | ``(if condition then-form else-form?) -> [evaled form result]``<br><br>If then else conditional. |

```
(def 'test-if-one
    (if t "ONE TRUE" "ONE FALSE"))
(def 'test-if-two
    (if nil "TWO TRUE" "TWO FALSE"))
(test::assert-equal "ONE TRUE" test-if-one)
(test::assert-equal "TWO FALSE" test-if-two)

(def 'test-if-one2
    (if t "ONE2 TRUE"))
(def 'test-if-two2
    (if nil "TWO2 TRUE"))
(test::assert-equal "ONE2 TRUE" test-if-one2)
(test::assert-equal nil test-if-two2)
```


| <a id="root::not" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``not``](#root::not-contents) | Function |
| root::not | ``(not expression)``<br><br>Return true if expression is nil. |

```
(test::assert-true (not nil))
(test::assert-false (not 10))
(test::assert-false (not t))
(test::assert-false (not (+ 1 2 3)))
```


| <a id="root::null" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``null``](#root::null-contents) | Function |
| root::null | ``(null expression)``<br><br>Return true if expression is nil (null). |

```
(test::assert-true (null nil))
(test::assert-false (null 10))
(test::assert-false (null t))
(test::assert-false (null (+ 1 2 3)))
```


| <a id="root::or" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``or``](#root::or-contents) | SpecialForm |
| root::or | ``(or exp0 ... expN) -> [nil or first non nil expression]``<br><br>Evaluates each form until one produces a non-nil result, produces nil if all expressions are nil.<br><br>The or form will stop evaluting when the first expression produces non-nil. |

```
(test::assert-true (or nil nil t (err "and- can not happen")))
(test::assert-false (or nil nil nil))
(test::assert-equal "or- done" (or nil "or- done"))
(test::assert-equal "or- done" (or nil nil "or- done"))
(test::assert-equal 6 (or nil nil (+ 1 2 3)))
(test::assert-equal 2 (or (/ 10 5) (* 5 2) (+ 1 2 3)))
```


| <a id="core::when" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``when``](#core::when-contents) | Macro |
| core::when | ``(when provided-condition if-true)``<br><br>when is a convenience function used to check a form, provided-condition,<br>and run some form, if-true, if provided-condition evaluates to true. |

```
(assert-true (when #t #t))
(assert-false (when #t nil))
(assert-false (when nil nil))
```
### <a id="Core forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Core forms](#Core forms-contents)



| <a id="root::apply" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``apply``](#root::apply-contents) | Function |
| root::apply | ``(apply function arg* list)``<br><br>Call the provided function with the suplied arguments, last is a list that will be expanded. |

```
(def 'test-apply-one nil)
(apply set '('test-apply-one "ONE"))
(test::assert-equal "ONE" test-apply-one)
(test::assert-equal 10 (apply + 1 '(2 7)))
```


| <a id="root::block" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``block``](#root::block-contents) | SpecialForm |
| root::block | ``(block name form*)``<br><br>Create a block with name (name is not evaluted), if no return-from encountered then<br>return last expression (like progn). |

```
(test::assert-equal '(4 5) (block xxx '(1 2) (return-from xxx '(4 5)) '(a b) '(2 3)))
(test::assert-equal '(4 5) (block xxx '(1 2) (return-from nil '(4 5)) '(a b) '(2 3)))
(test::assert-equal '(5 6) (block xxx '(1 2) (block yyy (return-from xxx '(5 6)) '(a b)) '(2 3)))
(test::assert-equal '(5 6) (block xxx '(1 2) (block yyy ((fn (p) (return-from xxx p)) '(5 6)) '(a b)) '(2 3)))
(test::assert-equal '(2 3) (block xxx '(1 2) (block yyy (return-from yyy t) '(a b)) '(2 3)))
```


| <a id="root::bquote" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``bquote``](#root::bquote-contents) | SpecialForm |
| root::bquote | ``(bquote expression) -> expression``<br><br>Return expression without evaluation.  The reader macro ` will expand to a bquote form.<br><br>The bquote form (unlike quote) allows for symbol/form evaluation using , or ,@. |

```
(test::assert-equal (list 1 2 3) (bquote (1 2 3)))
(test::assert-equal (list 1 2 3) `(1 2 3))
(test::assert-equal `(1 2 3) (bquote (1 2 3)))
(test::assert-equal `(1 2 3) '(1 2 3))
(def 'test-bquote-one 1)
(def 'test-bquote-list '(1 2 3))
(test::assert-equal (list 1 2 3) (bquote (,test-bquote-one 2 3)))
(test::assert-equal (list 1 2 3) (bquote (,@test-bquote-list)))
```


| <a id="root::def" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``def``](#root::def-contents) | Function |
| root::def | ``(def symbol expression) -> expression``<br><br>Adds an expression to the current scope.  Return the expression that was defined. |

```
(def 'test-progn-one nil)
(def 'test-progn-two nil)
(def 'test-progn-three (progn (set 'test-progn-one "One")(set 'test-progn-two "Two")"Three"))
(test::assert-equal "One" test-progn-one)
(test::assert-equal "Two" test-progn-two)
(test::assert-equal "Three" test-progn-three)
(let ((test-progn-one nil))
    ; Add this to tthe let's scope (shadow the outer test-progn-two).
    (test::assert-equal "Default" (def 'test-progn-two "Default"))
    ; set the currently scoped value.
    (set 'test-progn-one "1111")
    (set 'test-progn-two "2222")
    (test::assert-equal "1111" test-progn-one)
    (test::assert-equal "2222" test-progn-two))
; Original outer scope not changed.
(test::assert-equal "One" test-progn-one)
```


| <a id="root::def?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``def?``](#root::def?-contents) | Function |
| root::def? | ``(def? expression) -> t\|nil``<br><br>Return true if symbol is defined.<br><br>Expression will be evaluated and if a symbol or string it will look up that<br>name in the symbol table and return true if it exists. |

```
(def 'test-is-def t)
(test::assert-true (def? 'test-is-def))
(test::assert-true (def? "test-is-def"))
(test::assert-true (def? (symbol-name 'test-is-def)))
(test::assert-false (def? 'test-is-def-not-defined))
(test::assert-false (def? "test-is-def-not-defined"))
```


| <a id="core::defmacro" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``defmacro``](#core::defmacro-contents) | Macro |
| core::defmacro | ``(defmacro name doc_string? argument_list body)``<br><br>Create a macro and bind it to a symbol in the current scope, |



| <a id="core::defn" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``defn``](#core::defn-contents) | Macro |
| core::defn | ``(defn name &rest args)``<br><br><br>Define a named function in the current namespace. |



| <a id="core::defq" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``defq``](#core::defq-contents) | Macro |
| core::defq | ``(defq sym doc-string? expression) -> expression``<br><br>Binds an expession to a quoted symbol (ie def 'sym bind) |



| <a id="root::doc" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``doc``](#root::doc-contents) | Function |
| root::doc | ``(doc symbol)``<br><br>Return the doc string for a symbol or nil if no string. |

```
;(doc 'car)
t
```


| <a id="root::doc-raw" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``doc-raw``](#root::doc-raw-contents) | Function |
| root::doc-raw | ``(doc-raw symbol)``<br><br>Return the raw (unexpanded) doc string for a symbol or nil if no string. |

```
;(doc-raw 'car)
t
```


| <a id="root::dyn" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``dyn``](#root::dyn-contents) | Function |
| root::dyn | ``(dyn key value expression) -> nil``<br><br>Creates a dynamic binding for key, assigns value to it and evals expression under it.<br><br>The binding is gone once the dyn form ends.  The binding will take precedent over<br>any other binding in any scope with that name for any form that evalute as a <br>result of the dynamic binging (for instance creating a dynamic binding for<br>*stdout* will cause all output to stdout to use the new binding in any print's<br>used indirectly). |

```
(defn test-dyn-fn () (print "Print dyn out"))
(dyn '*stdout* (open "/tmp/sl-sh.dyn.test" :create :truncate) (test-dyn-fn))
(test::assert-equal "Print dyn out" (read-line (open "/tmp/sl-sh.dyn.test" :read)))
```


| <a id="root::eprint" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``eprint``](#root::eprint-contents) | Function |
| root::eprint | ``(eprint arg0 ... argN) -> nil``<br><br>Print the arguments (as strings) to *stderr*. |

```
; Use a file for stderr for test.
(dyn '*stderr* (open "/tmp/sl-sh.eprint.test" :create :truncate) (eprint "eprint test out"))
(test::assert-equal "eprint test out" (read-line (open "/tmp/sl-sh.eprint.test" :read)))
```


| <a id="root::eprintln" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``eprintln``](#root::eprintln-contents) | Function |
| root::eprintln | ``(eprintln arg0 ... argN) -> nil``<br><br>Print the arguments (as strings) to *stderr* and then a newline. |

```
; Use a file for stderr for test.
(dyn '*stderr* (open "/tmp/sl-sh.eprintln.test" :create :truncate) (eprintln "eprintln test out"))
(test::assert-equal "eprintln test out
" (read-line (open "/tmp/sl-sh.eprintln.test" :read)))
```


| <a id="root::err" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``err``](#root::err-contents) | Function |
| root::err | ``(err string) -> raises an error``<br><br>Raise an error with the supplied string. |

```
(def 'test-err-err (get-error (err "Test Error")))
(test::assert-equal '#(:error "Test Error") test-err-err)
```


| <a id="root::error-stack-off" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``error-stack-off``](#root::error-stack-off-contents) | Function |
| root::error-stack-off | ``(error-stack-off)``<br><br>Do not print the eval stack on error. |

```
;(error-stack-off)
t
```


| <a id="root::error-stack-on" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``error-stack-on``](#root::error-stack-on-contents) | Function |
| root::error-stack-on | ``(error-stack-on)``<br><br>Print the eval stack on error. |

```
;(error-stack-on)
t
```


| <a id="root::eval" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``eval``](#root::eval-contents) | Function |
| root::eval | ``(eval expression)``<br><br>Evalute the provided expression.<br><br>If expression is a string read it to make an ast first to evaluate otherwise<br>evaluate the expression (note eval is a function not a special form, the<br>provided expression will be evaluated as part of call). |

```
(def 'test-eval-one nil)
(eval "(set 'test-eval-one \"ONE\")")
(test::assert-equal "ONE" test-eval-one)
(eval '(set 'test-eval-one "TWO"))
(test::assert-equal "TWO" test-eval-one)
```


| <a id="root::expand-macro" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``expand-macro``](#root::expand-macro-contents) | SpecialForm |
| root::expand-macro | ``(expand-macro expression)``<br><br>Expands a macro expression.  If that expansion is also a macro then expand it recursively.<br><br>Just returns the expression if not a macro. |

```
(test::assert-equal '(apply def 'xx '#("value")) (expand-macro (defq xx "value")))
(test::assert-equal '(
    (fn
        #(i)
        (progn
            (if
                (> (length '(1 2 3)) 0)
                (core::loop
                    (plist)
                    ('(1 2 3))
                    (progn
                        (core::setq i (core::first plist)) nil
                        (if
                            (> (length plist) 1)
                            (recur (core::rest plist)))))))) nil)
    (expand-macro (for i '(1 2 3) ())))
(test::assert-equal '(1 2 3) (expand-macro (1 2 3)))
```


| <a id="root::expand-macro-all" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``expand-macro-all``](#root::expand-macro-all-contents) | SpecialForm |
| root::expand-macro-all | ``(expand-macro-all expression)``<br><br>Expands a macro expression like expand-macro but also expand any embedded macros.  <br><br>Just returns the expression if not a macro. |

```
(test::assert-equal '(apply def 'xx '#("value")) (expand-macro-all (defq xx "value")))
(test::assert-equal '(
    (fn
        #(i)
        (progn
            (if
                (> (length '(1 2 3)) 0)
                (
                    (fn
                        (plist)
                        (progn
                            (apply set 'i '#((core::first plist))) nil
                            (if
                                (> (length plist) 1)
                                (recur (core::rest plist)))))
                    '(1 2 3))))) nil)
    (expand-macro-all (for i '(1 2 3) ())))
(test::assert-equal '(1 2 3) (expand-macro-all (1 2 3)))
```


| <a id="root::expand-macro1" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``expand-macro1``](#root::expand-macro1-contents) | SpecialForm |
| root::expand-macro1 | ``(expand-macro1 expression)``<br><br>Expands a macro expression.  Only expand the first macro.<br><br>Just returns the expression if not a macro. |

```
(test::assert-equal '(apply def 'xx '#("value")) (expand-macro1 (defq xx "value")))
(test::assert-equal '(core::let
    ((i))
    (if
        (> (length '(1 2 3)) 0)
        (core::loop
            (plist)
            ('(1 2 3))
            (progn
                (core::setq i (core::first plist)) nil
                (if
                    (> (length plist) 1)
                    (recur (core::rest plist)))))))
    (expand-macro1 (for i '(1 2 3) ())))
(test::assert-equal '(1 2 3) (expand-macro1 (1 2 3)))
```


| <a id="root::fn" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fn``](#root::fn-contents) | SpecialForm |
| root::fn | ``(fn (x) (x + 1))``<br><br>Create a function (lambda). |



| <a id="root::fncall" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fncall``](#root::fncall-contents) | Function |
| root::fncall | ``(fncall function arg0 ... argN)``<br><br>Call the provided function with the suplied arguments. |

```
(def 'test-fncall-one nil)
(fncall set 'test-fncall-one "ONE")
(test::assert-equal "ONE" test-fncall-one)
(test::assert-equal 10 (fncall + 1 2 7))
```


| <a id="root::format" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``format``](#root::format-contents) | Function |
| root::format | ``(format arg0 ... argN) -> string``<br><br>Build a formatted string from arguments.<br><br>Arguments will be turned into strings. |

```
(test::assert-equal "stringsome" (format "string" "some"))
(test::assert-equal "string" (format "string" ""))
(test::assert-equal "string 50" (format "string" " " 50))
(test::assert-equal "string 50 100.5" (format "string" " " 50 " " 100.5))
```


| <a id="root::gensym" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``gensym``](#root::gensym-contents) | Function |
| root::gensym | ``(gensym) -> symbol``<br><br>Generate a unique symbol.<br><br>Gensym uses a prefix of gs@@ followed by an incrementing counter.<br>It is useful to generate unique symbol names when writing macros (for instance). |

```
(def 'test-gensym-one (gensym))
(def 'test-gensym-two (gensym))
(def 'test-gensym-three (gensym))
(test::assert-equal "gs@@1" (symbol-name test-gensym-one))
(test::assert-equal "gs@@2" (symbol-name test-gensym-two))
(test::assert-equal "gs@@3" (symbol-name test-gensym-three))
(test::assert-true (symbol? (gensym)))
(test::assert-true (symbol? test-gensym-one))
(test::assert-true (symbol? test-gensym-two))
(test::assert-true (symbol? test-gensym-three))
```


| <a id="root::get-error" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``get-error``](#root::get-error-contents) | Function |
| root::get-error | ``(get-error exp0 ... expN)``<br><br>Evaluate each form (like progn) but on error return #(:error msg) instead of aborting.<br><br>If there is no error will return the value of the last expression. |

```
(test::assert-equal '#(:error "Some Error") (get-error (err "Some Error")))
(test::assert-equal "Some String" (get-error "Some String"))
(test::assert-equal "Some Other String" (get-error (def 'test-get-error "Some ") (str test-get-error "Other String")))
```


| <a id="root::intern-stats" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``intern-stats``](#root::intern-stats-contents) | SpecialForm |
| root::intern-stats | ``(intern-stats)``<br><br>Prints the stats for interned symbols. |

```
;(intern-stats)
t
```


| <a id="root::length" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``length``](#root::length-contents) | Function |
| root::length | ``(length expression) -> int``<br><br>Return length of suplied expression. |

```
(test::assert-equal 0 (length nil))
(test::assert-equal 5 (length "12345"))
; Note the unicode symbol is only one char even though it is more then one byte.
(test::assert-equal 6 (length "12345Σ"))
(test::assert-equal 3 (length '(1 2 3)))
(test::assert-equal 3 (length '#(1 2 3)))
(test::assert-equal 3 (length (list 1 2 3)))
(test::assert-equal 3 (length (vec 1 2 3)))
(test::assert-equal 1 (length 100))
(test::assert-equal 1 (length 100.0))
(test::assert-equal 1 (length #\x))
```


| <a id="core::let" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``let``](#core::let-contents) | Macro |
| core::let | ``(let vals &rest let_body)``<br><br><br>Takes list, vals, of form ((binding sexp) ...) and evaluates let_body with all<br>values of binding bound to the result of the evaluation of sexp. |



| <a id="root::load" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``load``](#root::load-contents) | Function |
| root::load | ``(load path) -> [last form value]``<br><br>Read and eval a file (from path- a string). |

```
(def 'test-load-one nil)
(def 'test-load-two nil)
(write-line (open "/tmp/slsh-test-load.testing" :create :truncate) "(set 'test-load-one \"LOAD TEST\") '(1 2 3)")
(set 'test-load-two (load "/tmp/slsh-test-load.testing"))
(test::assert-equal "LOAD TEST" test-load-one)
(test::assert-equal '(1 2 3) test-load-two)
```


| <a id="root::macro" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``macro``](#root::macro-contents) | Function |
| root::macro | ``(macro (&rest args) `(apply + ,@args))``<br><br>Define an anonymous macro. |



| <a id="core::match" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``match``](#core::match-contents) | Macro |
| core::match | ``(match condition &rest branches)``<br><br><br>Evaluate condition and look for matching value in each branch of type<br>(val action). Use nil to take action if no match (encouraged!). |



| <a id="root::meta-column-no" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``meta-column-no``](#root::meta-column-no-contents) | SpecialForm |
| root::meta-column-no | ``(meta-column-no)``<br><br>Column number from the file this came from. |

```
;(meta-column-no)
t
```


| <a id="root::meta-file-name" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``meta-file-name``](#root::meta-file-name-contents) | SpecialForm |
| root::meta-file-name | ``(meta-file-name)``<br><br>File name of the file this came from. |

```
;(meta-file-name)
t
```


| <a id="root::meta-line-no" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``meta-line-no``](#root::meta-line-no-contents) | SpecialForm |
| root::meta-line-no | ``(meta-line-no)``<br><br>Line number from the file this came from. |

```
;(meta-line-no)
t
```


| <a id="root::print" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``print``](#root::print-contents) | Function |
| root::print | ``(print arg0 ... argN) -> nil``<br><br>Print the arguments (as strings) to *stdout*. |

```
; Use a file for stdout for test.
(dyn '*stdout* (open "/tmp/sl-sh.print.test" :create :truncate) (print "Print test out"))
(test::assert-equal "Print test out" (read-line (open "/tmp/sl-sh.print.test" :read)))
```


| <a id="root::println" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``println``](#root::println-contents) | Function |
| root::println | ``(println arg0 ... argN) -> nil``<br><br>Print the arguments (as strings) to *stdout* and then a newline. |

```
; Use a file for stdout for test.
(dyn '*stdout* (open "/tmp/sl-sh.println.test" :create :truncate) (println "Println test out"))
(test::assert-equal "Println test out
" (read-line (open "/tmp/sl-sh.println.test" :read)))
```


| <a id="root::progn" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``progn``](#root::progn-contents) | SpecialForm |
| root::progn | ``(progn exp0 ... expN) -> expN``<br><br>Evalutate each form and return the last. |

```
(def 'test-progn-one nil)
(def 'test-progn-two nil)
(def 'test-progn-three (progn (set 'test-progn-one "One")(set 'test-progn-two "Two")"Three"))
(test::assert-equal "One" test-progn-one)
(test::assert-equal "Two" test-progn-two)
(test::assert-equal "Three" test-progn-three)
```


| <a id="root::quote" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``quote``](#root::quote-contents) | SpecialForm |
| root::quote | ``(quote expression) -> expression``<br><br>Return expression without evaluation.  The reader macro ' will expand to a quote form. |

```
(test::assert-equal (list 1 2 3) (quote (1 2 3)))
(test::assert-equal (list 1 2 3) '(1 2 3))
(test::assert-equal '(1 2 3) (quote (1 2 3)))
```


| <a id="root::recur" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``recur``](#root::recur-contents) | Function |
| root::recur | ``(recur &rest)`` |



| <a id="root::return-from" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``return-from``](#root::return-from-contents) | SpecialForm |
| root::return-from | ``(return-from name expression?)``<br><br>Causes enclosing block with name (name is not evaluted) to evaluate to expression. |

```
(test::assert-equal '(4 5) (block xxx '(1 2) (return-from xxx '(4 5)) '(a b) '(2 3)))
(test::assert-equal '(4 5) (block xxx '(1 2) (return-from nil '(4 5)) '(a b) '(2 3)))
(test::assert-equal '(5 6) (block xxx '(1 2) (block yyy (return-from xxx '(5 6)) '(a b)) '(2 3)))
(test::assert-equal '(5 6) (block xxx '(1 2) (block yyy ((fn (p) (return-from xxx p)) '(5 6)) '(a b)) '(2 3)))
(test::assert-equal '(2 3) (block xxx '(1 2) (block yyy (return-from yyy t) '(a b)) '(2 3)))
```


| <a id="root::set" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``set``](#root::set-contents) | Function |
| root::set | ``(set symbol expression) -> expression``<br><br>Sets an existing expression in the current scope(s).  Return the expression that was set. |

```
(def 'test-progn-one nil)
(def 'test-progn-two nil)
(def 'test-progn-three (progn (set 'test-progn-one "One")(set 'test-progn-two "Two")"Three"))
(test::assert-equal "One" test-progn-one)
(test::assert-equal "Two" test-progn-two)
(test::assert-equal "Three" test-progn-three)
(let ((test-progn-one nil))
    ; set the currently scoped value.
    (test::assert-equal "1111" (set 'test-progn-one "1111"))
    (test::assert-equal "1111" test-progn-one))
; Original outer scope not changed.
(test::assert-equal "One" test-progn-one)
```


| <a id="core::setfn" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``setfn``](#core::setfn-contents) | Macro |
| core::setfn | ``(setfn name &rest args)``<br><br><br>Binds name to function body in current namespace. |



| <a id="core::setmacro" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``setmacro``](#core::setmacro-contents) | Macro |
| core::setmacro | ``(setmacro name doc_string? argument_list body)``<br><br>Set a macro to an existing symbol. |



| <a id="core::setq" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``setq``](#core::setq-contents) | Macro |
| core::setq | ``(setq sym doc-string? expression) -> expression``<br><br>Set an expession to a quoted symbol (ie set 'sym bind) |



| <a id="root::symbol-name" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``symbol-name``](#root::symbol-name-contents) | Function |
| root::symbol-name | ``(symbol-name symbol) -> string``<br><br>Convert a symbol to its string representation.<br><br>The string will be the symbol name as a string. |

```
(def 'test-symbol-name-sym nil)
(test::assert-true (string? (symbol-name 'test-symbol-name-sym)))
(test::assert-equal "test-symbol-name-sym" (symbol-name 'test-symbol-name-sym))
```


| <a id="root::to-symbol" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``to-symbol``](#root::to-symbol-contents) | Function |
| root::to-symbol | ``(to-symbol expression) -> symbol``<br><br>Convert a string, symbol, int or float to a symbol.<br><br>If the symbol is new it will be interned. |

```
(def 'test-to-symbol-sym nil)
(test::assert-true (symbol? (to-symbol 55)))
(test::assert-true (symbol? (to-symbol 55.0)))
(test::assert-true (symbol? (to-symbol "to-symbol-test-new-symbol")))
(test::assert-true (symbol? (to-symbol (str-buf "to-symbol-test-new-symbol-buf"))))
(test::assert-true (symbol? (to-symbol 'test-to-symbol-sym)))
(test::assert-true (symbol? (to-symbol (symbol-name 'test-to-symbol-sym))))
```


| <a id="root::undef" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``undef``](#root::undef-contents) | Function |
| root::undef | ``(undef symbol)``<br><br>Remove a symbol from the current scope (if it exists). |

```
(def 'test-undef nil)
(test::assert-true (def? 'test-undef))
(undef 'test-undef)
(test::assert-false (def? 'test-undef))
```


| <a id="root::unwind-protect" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``unwind-protect``](#root::unwind-protect-contents) | Function |
| root::unwind-protect | ``(unwind-protect protected cleanup*) -> [protected result]``<br><br>After evaluation first form, make sure the following cleanup forms run (returns first form's result). |

```
(def 'test-unwind-one nil)
(def 'test-unwind-err (get-error
(unwind-protect (err "Some protected error") (set 'test-unwind-one "got it"))))
(test::assert-equal '#(:error "Some protected error") test-unwind-err)
(test::assert-equal "got it" test-unwind-one)

(def 'test-unwind-one nil)
(def 'test-unwind-two nil)
(def 'test-unwind-three nil)
(def 'test-unwind-four nil)
(def 'test-unwind-err (get-error
(unwind-protect
    (progn (set 'test-unwind-one "set one")(err "Some protected error two")(set 'test-unwind-two "set two"))
    (set 'test-unwind-three "set three")(set 'test-unwind-four "set four"))))
(test::assert-equal '#(:error "Some protected error two") test-unwind-err)
(test::assert-equal "set one" test-unwind-one)
(test::assert-equal nil test-unwind-two)
(test::assert-equal "set three" test-unwind-three)
(test::assert-equal "set four" test-unwind-four)
```
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


| <a id="root::close" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``close``](#root::close-contents) | Function |
| root::close | ``(close file)``<br><br>Close a file. |

```
(def 'tst-file (open "/tmp/slsh-tst-open.txt" :create :truncate))
(write-line tst-file "Test Line Two")
(close tst-file)
(def 'tst-file (open "/tmp/slsh-tst-open.txt" :read))
(test::assert-equal "Test Line Two
" (read-line tst-file))
(close tst-file)
```


| <a id="root::flush" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``flush``](#root::flush-contents) | Function |
| root::flush | ``(flush file)``<br><br>Flush a file. |

```
(def 'tst-file (open "/tmp/slsh-tst-open.txt" :create :truncate))
(write-line tst-file "Test Line Three")
(flush tst-file)
(def 'tst-file (open "/tmp/slsh-tst-open.txt" :read))
(test::assert-equal "Test Line Three
" (read-line tst-file))
(close tst-file)
```


| <a id="root::open" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``open``](#root::open-contents) | Function |
| root::open | ``(open filename option*)``<br><br>Open a file.<br><br>Options are:<br>    :read<br>    :write<br>    :append<br>    :truncate<br>    :create<br>    :create-new<br>    :on-error-nil |

```
(write-line (open "/tmp/slsh-tst-open.txt" :create :truncate) "Test Line One")
(test::assert-equal "Test Line One
" (read-line (open "/tmp/slsh-tst-open.txt")))
```


| <a id="root::read" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``read``](#root::read-contents) | Function |
| root::read | ``(read file\|string) -> list``<br><br>Read a file or string and return the list representation.<br><br>Unlike most lisp readers this one will put loose symbols in a list (i.e. you<br>enter things at the repl without the enclosing parens). |

```
(def 'tst-file (open "/tmp/slsh-tst-open.txt" :create :truncate))
(write-line tst-file "(1 2 3)")
;(write-string tst-file "Test Line Read Line Two")
(flush tst-file)
(def 'tst-file (open "/tmp/slsh-tst-open.txt" :read))
(test::assert-equal '(1 2 3) (read tst-file))
(close tst-file)
(test::assert-equal '(4 5 6) (read "(4 5 6)"))
(test::assert-equal '(7 8 9) (read "7 8 9"))
(test::assert-equal '(x y z) (read "(x y z)"))
```


| <a id="root::read-line" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``read-line``](#root::read-line-contents) | Function |
| root::read-line | ``(read-line file) -> string``<br><br>Read a line from a file. |

```
(def 'tst-file (open "/tmp/slsh-tst-open.txt" :create :truncate))
(write-line tst-file "Test Line Read Line One")
(write-string tst-file "Test Line Read Line Two")
(flush tst-file)
(def 'tst-file (open "/tmp/slsh-tst-open.txt" :read))
(test::assert-equal "Test Line Read Line One
" (read-line tst-file))
(test::assert-equal "Test Line Read Line Two" (read-line tst-file))
(close tst-file)
```


| <a id="root::write-line" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``write-line``](#root::write-line-contents) | Function |
| root::write-line | ``(write-line file string)``<br><br>Write a line to a file. |

```
(def 'tst-file (open "/tmp/slsh-tst-open.txt" :create :truncate))
(write-line tst-file "Test Line Write Line")
(flush tst-file)
(def 'tst-file (open "/tmp/slsh-tst-open.txt" :read))
(test::assert-equal "Test Line Write Line
" (read-line tst-file))
(close tst-file)
```


| <a id="root::write-string" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``write-string``](#root::write-string-contents) | Function |
| root::write-string | ``(write-string file string)``<br><br>Write a string to a file. |

```
(def 'tst-file (open "/tmp/slsh-tst-open.txt" :create :truncate))
(write-string tst-file "Test Line Write String")
(flush tst-file)
(def 'tst-file (open "/tmp/slsh-tst-open.txt" :read))
(test::assert-equal "Test Line Write String" (read-line tst-file))
(close tst-file)
```
### <a id="Hashmap forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Hashmap forms](#Hashmap forms-contents)



| <a id="root::hash-clear!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash-clear!``](#root::hash-clear!-contents) | Function |
| root::hash-clear! | ``(hash-clear! hashmap)``<br><br>Clears a hashmap.  This is a destructive form! |

```
(def 'tst-hash (make-hash '((:key1 . "val one")(key2 . "val two")("key3" . "val three"))))
(test::assert-equal 3 (length (hash-keys tst-hash)))
(test::assert-true (hash-haskey tst-hash :key1))
(test::assert-true (hash-haskey tst-hash 'key2))
(test::assert-true (hash-haskey tst-hash "key3"))
(hash-clear! tst-hash)
(test::assert-equal 0 (length (hash-keys tst-hash)))
(test::assert-false (hash-haskey tst-hash :key1))
(test::assert-false (hash-haskey tst-hash 'key2))
(test::assert-false (hash-haskey tst-hash "key3"))
```


| <a id="root::hash-get" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash-get``](#root::hash-get-contents) | Function |
| root::hash-get | ``(hash-get hashmap key)``<br><br>Get a value for a key from a hashmap. |

```
(def 'tst-hash (make-hash '((:key1 . "val one")(key2 . "val two")("key3" . "val three"))))
(test::assert-equal 3 (length (hash-keys tst-hash)))
(test::assert-equal "val one" (hash-get tst-hash :key1))
(test::assert-equal "val two" (hash-get tst-hash 'key2))
(test::assert-equal "val three" (hash-get tst-hash "key3"))
```


| <a id="root::hash-haskey" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash-haskey``](#root::hash-haskey-contents) | Function |
| root::hash-haskey | ``(hash-haskey hashmap key)``<br><br>Checks if a key is in a hashmap. |

```
(def 'tst-hash (make-hash '((:key1 . "val one")(key2 . "val two")("key3" . "val three"))))
(test::assert-equal 3 (length (hash-keys tst-hash)))
(test::assert-true (hash-haskey tst-hash :key1))
(test::assert-true (hash-haskey tst-hash 'key2))
(test::assert-true (hash-haskey tst-hash "key3"))
(test::assert-false (hash-haskey tst-hash 'key1))
(test::assert-false (hash-haskey tst-hash :key2))
(test::assert-false (hash-haskey tst-hash "keynone"))
(hash-remove! tst-hash :key1)
(test::assert-false (hash-haskey tst-hash :key1))
(hash-set! tst-hash :key1 "val one b")
(test::assert-true (hash-haskey tst-hash :key1))
```


| <a id="root::hash-keys" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash-keys``](#root::hash-keys-contents) | Function |
| root::hash-keys | ``(hash-keys hashmap)``<br><br>Returns a vector of all the hashmaps keys.  The keys will be unordered. |

```
(def 'tst-hash (make-hash '((:key1 . "val one")(key2 . "val two")("key3" . "val three"))))
(test::assert-equal 3 (length (hash-keys tst-hash)))
(test::assert-true (in? (hash-keys tst-hash) :key1) "Test :key1")
(test::assert-true (in? (hash-keys tst-hash) 'key2) "Test key2")
; Note string used as a key will be a symbol in the hash-keys list...
(test::assert-true (in? (hash-keys tst-hash) 'key3) "Test key3")
(test::assert-false (in? (hash-keys tst-hash) :key4))
```


| <a id="root::hash-remove!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash-remove!``](#root::hash-remove!-contents) | Function |
| root::hash-remove! | ``(hash-remove! hashmap key)``<br><br>Remove a key from a hashmap.  This is a destructive form! |

```
(def 'tst-hash (make-hash '((:key1 . "val one")(key2 . "val two")("key3" . "val three"))))
(test::assert-equal 3 (length (hash-keys tst-hash)))
(test::assert-equal "val one" (hash-get tst-hash :key1))
(test::assert-equal "val two" (hash-get tst-hash 'key2))
(test::assert-equal "val three" (hash-get tst-hash "key3"))
(hash-remove! tst-hash 'key2)
(test::assert-equal 2 (length (hash-keys tst-hash)))
(test::assert-equal "val one" (hash-get tst-hash :key1))
(test::assert-equal "val three" (hash-get tst-hash "key3"))
(hash-remove! tst-hash :key1)
(test::assert-equal 1 (length (hash-keys tst-hash)))
(test::assert-equal "val three" (hash-get tst-hash "key3"))
(hash-remove! tst-hash "key3")
(test::assert-equal 0 (length (hash-keys tst-hash)))
```


| <a id="root::hash-set!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash-set!``](#root::hash-set!-contents) | Function |
| root::hash-set! | ``(hash-set! hashmap key value)``<br><br>Add or update a hashmap key's value.  This is a destructive form! |

```
(def 'tst-hash (make-hash))
(test::assert-equal 0 (length (hash-keys tst-hash)))
(hash-set! tst-hash :new-key '(1 2 3))
(test::assert-equal 1 (length (hash-keys tst-hash)))
(test::assert-equal '(1 2 3) (hash-get tst-hash :new-key))
(def 'tst-hash (make-hash '((:key1 . "val one")(key2 . "val two")("key3" . "val three"))))
(test::assert-equal 3 (length (hash-keys tst-hash)))
(test::assert-equal "val one" (hash-get tst-hash :key1))
(test::assert-equal "val two" (hash-get tst-hash 'key2))
(test::assert-equal "val three" (hash-get tst-hash "key3"))
(hash-set! tst-hash :new-key '(1 2 3))
(test::assert-equal 4 (length (hash-keys tst-hash)))
(test::assert-equal "val one" (hash-get tst-hash :key1))
(test::assert-equal "val two" (hash-get tst-hash 'key2))
(test::assert-equal "val three" (hash-get tst-hash "key3"))
(test::assert-equal '(1 2 3) (hash-get tst-hash :new-key))
(hash-set! tst-hash 'key2 "val two b")
(hash-set! tst-hash :key1 "val one b")
(hash-set! tst-hash "key3" "val three b")
(test::assert-equal 4 (length (hash-keys tst-hash)))
(test::assert-equal "val one b" (hash-get tst-hash :key1))
(test::assert-equal "val two b" (hash-get tst-hash 'key2))
(test::assert-equal "val three b" (hash-get tst-hash "key3"))
(test::assert-equal '(1 2 3) (hash-get tst-hash :new-key))
```


| <a id="root::make-hash" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``make-hash``](#root::make-hash-contents) | Function |
| root::make-hash | ``(make-hash associations?)``<br><br>Make a new hash map.<br><br>If associations is provided (makes an empty map if not) then it is a list of<br>pairs (key . value) that populate the intial map. |

```
(def 'tst-hash (make-hash))
(test::assert-equal 0 (length (hash-keys tst-hash)))
(def 'tst-hash (make-hash ()))
(test::assert-equal 0 (length (hash-keys tst-hash)))
(def 'tst-hash (make-hash nil))
(test::assert-equal 0 (length (hash-keys tst-hash)))
(def 'tst-hash (make-hash '((:key1 . "val one")(key2 . "val two")("key3" . "val three"))))
(test::assert-equal 3 (length (hash-keys tst-hash)))
(test::assert-equal "val one" (hash-get tst-hash :key1))
(test::assert-equal "val two" (hash-get tst-hash 'key2))
(test::assert-equal "val three" (hash-get tst-hash "key3"))
(def 'tst-hash (make-hash '#((:keyv1 . "val one")(keyv2 . "val two")("keyv3" . "val three"))))
(test::assert-equal 3 (length (hash-keys tst-hash)))
(test::assert-equal "val one" (hash-get tst-hash :keyv1))
(test::assert-equal "val two" (hash-get tst-hash 'keyv2))
(test::assert-equal "val three" (hash-get tst-hash "keyv3"))
```
### <a id="Math forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Math forms](#Math forms-contents)



| <a id="root::%" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``%``](#root::%-contents) | Function |
| root::% | ``(% int int)``<br><br>Remainder from dividing first int by the second. |

```
(test::assert-equal 0 (% 50 10))
(test::assert-equal 5 (% 55 10))
(test::assert-equal 1 (% 1 2))
```


| <a id="root::*" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*``](#root::*-contents) | Function |
| root::* | ``(* number+)``<br><br>Multiply a sequence of numbers. |

```
(test::assert-equal 5 (* 5))
(test::assert-equal 5 (* 1 5))
(test::assert-equal 5.0 (* 1.0 5))
(test::assert-equal 7.5 (* 1.5 5))
(test::assert-equal 7.5 (* 1.5 5.0))
(test::assert-equal 15 (* 3 5))
(test::assert-equal 8 (* 1 2 4))
(test::assert-equal 16 (* 2 2 4))
(test::assert-equal 16.0 (* 2 2.0 4))
(test::assert-equal 16.0 (* 2.0 2.0 4.0))
```


| <a id="root::+" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``+``](#root::+-contents) | Function |
| root::+ | ``(+ number+)``<br><br>Add a sequence of numbers. |

```
(test::assert-equal 5 (+ 5))
(test::assert-equal 5 (+ 5.0))
(test::assert-equal 6 (+ 1 5))
(test::assert-equal 6.5 (+ 1 5.5))
(test::assert-equal 7 (+ 1 2 4))
```


| <a id="root::-" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``-``](#root::--contents) | Function |
| root::- | ``(- number+)``<br><br>Subtract a sequence of numbers. |

```
(test::assert-equal 5 (- 5))
(test::assert-equal 5 (- 5.0))
(test::assert-equal -4 (- 1 5))
(test::assert-equal -4.5 (- 1 5.5))
(test::assert-equal 4 (- 10 2 4))
(test::assert-equal 4.9 (- 10.9 2 4))
```


| <a id="root::/" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``/``](#root::/-contents) | Function |
| root::/ | ``(/ number+)``<br><br>Divide a sequence of numbers.  Requires at least two numbers. |

```
(test::assert-equal 5 (/ 50 10))
(test::assert-equal 5 (/ 50.0 10.0))
(test::assert-equal 0 (/ 1 5))
(test::assert-equal .2 (/ 1.0 5))
(test::assert-equal .2 (/ 1.0 5.0))
(test::assert-equal 5.5 (/ 5.5 1))
(test::assert-equal 2 (/ 16 2 4))
(test::assert-equal 5 (/ 100 2 5 2))
```
### <a id="Namespace forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Namespace forms](#Namespace forms-contents)



| <a id="root::ns-create" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-create``](#root::ns-create-contents) | Function |
| root::ns-create | ``(ns-create namespace)``<br><br>Creates and enters a new a namespace (must evaluate to a string or symbol). |

```
(ns-create 'ns-create-test-namespace)
(def 'test-symbol "testing")
(test::assert-equal "testing" test-symbol)
(ns-pop)
(test::assert-false (def? 'test-symbol))
```


| <a id="root::ns-enter" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-enter``](#root::ns-enter-contents) | Function |
| root::ns-enter | ``(ns-enter namespace)``<br><br>Enters an existing namespace (must evaluate to a string or symbol). |

```
(ns-create 'ns-enter-test-namespace)
(def 'test-symbol "testing")
(test::assert-equal "testing" test-symbol)
(ns-pop)
(test::assert-false (def? 'test-symbol))
(ns-enter 'ns-enter-test-namespace)
(test::assert-true (def? 'test-symbol))
(test::assert-equal "testing" test-symbol)
(ns-pop)
t
```


| <a id="root::ns-exists?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-exists?``](#root::ns-exists?-contents) | Function |
| root::ns-exists? | ``(ns-exists? namespace)``<br><br>True if the supplied namespace exists (must evaluate to a string or symbol). |

```
(test::assert-false (ns-exists? 'ns-exists-test-namespace))
(ns-create 'ns-exists-test-namespace)
(ns-pop)
(test::assert-true (ns-exists? 'ns-exists-test-namespace))
```


| <a id="core::ns-export" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-export``](#core::ns-export-contents) | Macro |
| core::ns-export | ``(ns-export symbol_or_sequence)``<br><br>Export a symbol or list of symbols to be imported into other namespaces. |



| <a id="core::ns-import" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-import``](#core::ns-import-contents) | Macro |
| core::ns-import | ``(ns-import namespace)``<br><br>Import any symbols exported from namespace into the current namespace. |



| <a id="root::ns-list" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-list``](#root::ns-list-contents) | Function |
| root::ns-list | ``(ns-list)``<br><br>Returns a vector of all namespaces. |

```
(test::assert-not-includes "ns-list-test-namespace" (ns-list))
(ns-create 'ns-list-test-namespace)
(ns-pop)
(test::assert-includes "ns-list-test-namespace" (ns-list))
t
```


| <a id="root::ns-pop" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-pop``](#root::ns-pop-contents) | Function |
| root::ns-pop | ``(ns-pop)``<br><br>Returns to the previous namespace. |

```
(ns-create 'ns-pop-test-namespace)
(test::assert-equal "ns-pop-test-namespace" *ns*)
(ns-pop)
(test::assert-not-equal "ns-pop-test-namespace" *ns*)
```


| <a id="root::ns-symbols" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``ns-symbols``](#root::ns-symbols-contents) | Function |
| root::ns-symbols | ``(ns-symbols namespace)``<br><br>Returns the list of all symbols in namespace (must evaluate to a string or symbol). |

```
(test::assert-not-includes 'dumb-symbol-xxx (ns-symbols 'core))
(test::assert-includes 'loop (ns-symbols 'core))
(test::assert-not-includes 'dumb-symbol-xxx (ns-symbols 'root))
(test::assert-includes 'car (ns-symbols 'root))
t
```
### <a id="Pair forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Pair forms](#Pair forms-contents)
Operations on the 'Pair' type (aka Cons Cell) that can be used to create
traditional Lisp list structures. These are the default list structure and
are produced with bare parentheses in code. These lists can also be created by
building them up with joins or with the list form.


| <a id="root::car" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``car``](#root::car-contents) | Function |
| root::car | ``(car pair)``<br><br>Return the car (first item) from a pair.  If used on a proper list this will be the first element. |

```
(def 'tst-pairs-two (list 'x 'y 'z))
(test::assert-equal 'x (car tst-pairs-two))
(test::assert-equal 10 (car '(10)))
(test::assert-equal 9 (car '(9 11 13)))
```


| <a id="root::cdr" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cdr``](#root::cdr-contents) | Function |
| root::cdr | ``(cdr pair)``<br><br>Return the cdr (second item) from a pair.  If used on a proper list this will be the list minus the first element. |

```
(def 'tst-pairs-three (list 'x 'y 'z))
(test::assert-equal '(y z) (cdr tst-pairs-three))
(test::assert-equal nil (cdr '(10)))
(test::assert-equal '(13) (cdr '(9 13)))
(test::assert-equal '(11 13) (cdr '(9 11 13)))
```


| <a id="root::join" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``join``](#root::join-contents) | Function |
| root::join | ``(join car cdr)``<br> <br>Create a pair with the provided car and cdr. |

```
(def 'tst-pair-one (join 1 2))
(test::assert-equal 1 (car tst-pair-one))
(test::assert-equal 2 (cdr tst-pair-one))
(test::assert-equal '(1 2 3) (join 1 (join 2 (join 3 nil))))
```


| <a id="root::list" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``list``](#root::list-contents) | Function |
| root::list | ``(list item0 item1 .. itemN)``<br><br>Create a proper list from pairs with items 0 - N. |

```
(test::assert-equal '(1 2 3) (list 1 2 3))
```


| <a id="root::xar!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``xar!``](#root::xar!-contents) | Function |
| root::xar! | ``(xar! pair expression)``<br><br>Destructive form thst replaces the car (first item) in a pair with a new expression.<br><br>If used on a proper list will replace the first item.  Can be used on nil to<br>create a pair (expression . nil). |

```
(def 'tst-pairs-three (list 'x 'y 'z))
(test::assert-equal '(x y z) tst-pairs-three)
(test::assert-equal '(s y z) (xar! tst-pairs-three 's))
(test::assert-equal '(s y z) tst-pairs-three)
(def 'tst-pairs-four nil)
(test::assert-equal '() tst-pairs-four)
(test::assert-equal '(t) (xar! tst-pairs-four 't))
(test::assert-equal '(t) tst-pairs-four)
```


| <a id="root::xdr!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``xdr!``](#root::xdr!-contents) | Function |
| root::xdr! | ``(xdr! pair expression)``<br><br>Destructive form that replaces the cdr (second item) in a pair with a new expression.<br><br>If used on a proper list will replace everthing after the first item.<br>Can be used on nil to create a pair (nil . expression). |

```
(def 'tst-pairs-five (list 'a 'b 'c))
(test::assert-equal '(a b c) tst-pairs-five)
(test::assert-equal '(a y z) (xdr! tst-pairs-five '(y z)))
(test::assert-equal '(a y z) tst-pairs-five)
(def 'tst-pairs-six nil)
(test::assert-equal '() tst-pairs-six)
(test::assert-equal '(nil . v) (xdr! tst-pairs-six 'v))
(test::assert-equal '(nil . v) tst-pairs-six)
```
### <a id="Unknown forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Unknown forms](#Unknown forms-contents)



| <a id="root::*load-path*" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*load-path*``](#root::*load-path*-contents) | Vector |
| root::*load-path* | ``(set '*load-path* '("/path/one" "/path/two"))``<br><br>Set the a list of paths to search for loading scripts with the load form. |

```
;(set '*load-path '("/path"))
;(load "script-in-path")
t
```
### <a id="Sequence forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Sequence forms](#Sequence forms-contents)
These macros will work on either a vector or a pair made into a proper list
(cons list).  Use these in preference to the vector/list specific versions when
possible (ie first vs car).
NOTE: list on this table can be a vector or a list.


| <a id="core::append" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``append``](#core::append-contents) | Lambda |
| core::append | ``(append l1 &rest others)``<br><br><br>Returns new list whose contents are a combined list (concatenated) of l1 and<br>every item in others. |



| <a id="core::append!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``append!``](#core::append!-contents) | Macro |
| core::append! | ``(append! ret &rest others)``<br><br><br>Modifies the first list by appending the other lists onto it. |



| <a id="core::butlast" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``butlast``](#core::butlast-contents) | Lambda |
| core::butlast | ``(butlast obj)``<br><br><br>Produces the provided list minus the last element.  Nil if the list is empty or one element. |



| <a id="core::copy-seq" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``copy-seq``](#core::copy-seq-contents) | Lambda |
| core::copy-seq | ``(copy-seq seq)``<br><br><br>Produces a copy of the provided list (copy has same type as the parameter). |



| <a id="core::empty-seq?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``empty-seq?``](#core::empty-seq?-contents) | Lambda |
| core::empty-seq? | ``(empty-seq? obj)``<br><br>`empty-seq?` returns true if a list or vector is empty and false<br>otherwise. If a non list or non vector is passed in it returns false. |



| <a id="core::filter" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``filter``](#core::filter-contents) | Lambda |
| core::filter | ``(filter pred coll)``<br><br><br>filter is used to strip a collection, coll, of all values that do not<br>pass the condition defined by the function, pred. Filter returns a new<br>collection. |

```
(assert-equal '(2 4) (filter (fn (x) (= (% x 2) 0)) (list 1 2 3 4 5)))
```


| <a id="core::first" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``first``](#core::first-contents) | Lambda |
| core::first | ``(first obj)``<br><br><br>Produces the first element of the provided list.  Nil if the list is empty. |



| <a id="core::in?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``in?``](#core::in?-contents) | Lambda |
| core::in? | ``(in? seq-to-search item-to-match)``<br><br><br>Takes a [seq?](#core::seq?) that is not an [empty-seq?](#core::empty-seq?) and returns true if the second argument is is in list, false otherwise. |

```
(let ((vowels-list (list 'a 'e 'i 'o 'u)))
    (assert-true (in? vowels-list 'u))
    (assert-false (in? vowels-list 'c))
    (assert-true (in? (list (list)) (list)))
    (assert-false (in? 8 18)))
```


| <a id="core::last" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``last``](#core::last-contents) | Lambda |
| core::last | ``(last obj)``<br><br><br>Produces the last element in the list.  Nil if the list is empty. |



| <a id="core::loop" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``loop``](#core::loop-contents) | Macro |
| core::loop | ``(loop params bindings body)``<br><br><br>Binds bindings to parameters in body. Use recur with desired bindings for<br>subsequent iteration. |



| <a id="core::map" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``map``](#core::map-contents) | Lambda |
| core::map | ``(map fun items)``<br><br><br>Returns a new list made by applying the lambda to each item in the provided list. |



| <a id="core::map!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``map!``](#core::map!-contents) | Lambda |
| core::map! | ``(map! fun items)``<br><br><br>Modifies a list by applying the lambda to each item in the list. |



| <a id="core::non-empty-seq?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``non-empty-seq?``](#core::non-empty-seq?-contents) | Lambda |
| core::non-empty-seq? | ``(non-empty-seq? obj)``<br><br>`non-empty-seq?` returns true if a list or vector is non-empty and false<br>otherwise. If a non list or non vector is passed in it returns false. |



| <a id="core::nth" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``nth``](#core::nth-contents) | Lambda |
| core::nth | ``(nth idx obj)``<br><br><br>Produces the element at the provided index (0 based), error if index is out of bounds. |



| <a id="core::qsort" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``qsort``](#core::qsort-contents) | Lambda |
| core::qsort | ``(qsort sequence comp-lambda?) -> [sorted vector]``<br><br>Sort a sequence using the quick sort algorithm.  Returns a vector of the sorted sequence.<br><br>The comp-lambda argument is optional, if provided it should be a lambda or<br>builtin that takes two arguments and return t or nil (it is the compare<br>function for the sort).  Defaults to < if not provided. |

```
(test::assert-equal '(1 2 3) (qsort '(2 3 1)))
(test::assert-equal '(1 2 3) (qsort '#(2 3 1)))
(test::assert-equal '(3 2 1) (qsort '(2 3 1) >))
(test::assert-equal '(3 2 1) (qsort '#(2 3 1) (fn (a b) (< b a))))
(test::assert-equal '("aaa" "aab" "aba" "baa" "bab" "ccc")
    (qsort '("aaa" "aab" "aba" "baa" "bab" "ccc")))
(test::assert-equal '("aaa" "aab" "aba" "baa" "bab" "ccc")
    (qsort '("ccc" "bab" "baa" "aba" "aab" "aaa")))
(test::assert-equal '("aaa" "aab" "aba" "baa" "bab" "ccc")
    (qsort '("aba" "bab" "aab" "ccc" "baa" "aaa")))
(test::assert-equal '("ccc" "bab" "baa" "aba" "aab" "aaa")
    (qsort '("aba" "bab" "aab" "ccc" "baa" "aaa") >))
(test::assert-equal '("ccc" "bab" "baa" "aba" "aab" "aaa")
    (qsort '("aba" "bab" "aab" "ccc" "baa" "aaa") (fn (a b) (> a b))))
(test::assert-equal '() (qsort '()))
(test::assert-equal '() (qsort '#()))
(test::assert-equal '#() (qsort '()))
(test::assert-equal '#() (qsort '#()))
```


| <a id="core::reduce" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``reduce``](#core::reduce-contents) | Lambda |
| core::reduce | ``(reduce reducing-fcn init-val coll)``<br><br><br>reduce is used to amalgamate a provided collection, coll, and an intitial value,<br>init-val, according to the reducing function, reducing-fcn, provided. The<br>reducing-fcn should be a function of two arguments. In the first iteration of<br>reduce, the init-val will be used as the first argument to the reducing-fcn and<br>(first coll) will be used as the second argument. For all subsequent iterations,<br>The result from the previous application of the reducing-fcn will be used as the<br>first argument to the reducing-fcn and the second argument will be the next item<br>in the collection when the collection is empty reduce will return the<br>amalgamated result. |

```
(assert-true (= 15 (reduce + 0 (list 1 2 3 4 5))))
(assert-false (= 15 (reduce + 1 (list 1 2 3 4 5))))
(assert-true (= "one hoopy frood" (reduce str "" (list "one " "hoopy " "frood"))))
```


| <a id="core::rest" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``rest``](#core::rest-contents) | Lambda |
| core::rest | ``(rest obj)``<br><br><br>Produces the provided list minus the first element.  Nil if the list is empty or one element. |



| <a id="core::reverse" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``reverse``](#core::reverse-contents) | Lambda |
| core::reverse | ``(reverse items)``<br><br><br>Returns a new list made by reversing the elements of the provided list. |



| <a id="core::reverse!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``reverse!``](#core::reverse!-contents) | Lambda |
| core::reverse! | ``(reverse! items)``<br><br><br>Modifies a list by reversing it's elements. |



| <a id="core::seq?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``seq?``](#core::seq?-contents) | Lambda |
| core::seq? | ``(seq? expression) -> t/nil``<br><br>True if expression is a sequence, nil otherwise. |

```
(test::assert-true (seq? '(1 2 3)))
(test::assert-true (seq? '#(1 2 3)))
(test::assert-true (seq? '()))
(test::assert-true (seq? '#()))
(test::assert-false (seq? "aaa"))
(test::assert-false (seq? 1))
```


| <a id="core::setnth!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``setnth!``](#core::setnth!-contents) | Lambda |
| core::setnth! | ``(setnth! idx obj l)``<br><br><br>Sets idx item in the vector or list to obj, produces nil or errors on invalid input. |

### <a id="Shell forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Shell forms](#Shell forms-contents)
Forms to do shell operations like file tests, pipes, redirects, etc.


| <a id="root::*stderr*" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*stderr*``](#root::*stderr*-contents) | File |
| root::*stderr* | ``(write-line *stderr*)``<br><br>File that connects to standard error by default.<br><br>Can be used in place of a write file object in any form that takes one.  Used<br>as the default for eprint and eprintln. |

```
; Use a file for stderr for test.
(dyn '*stderr* (open "/tmp/sl-sh.stderr.test" :create :truncate) (write-line *stderr* "Test Error"))
(test::assert-equal "Test Error
" (read-line (open "/tmp/sl-sh.stderr.test" :read)))
```


| <a id="root::*stdin*" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*stdin*``](#root::*stdin*-contents) | File |
| root::*stdin* | ``(read-line *stdin*)``<br><br>File that connects to standard in by default.<br><br>Can be used in place of a read file object in any form that takes one. |

```
(def 'stdin-test (open "/tmp/sl-sh.stdin.test" :create :truncate))
(write-line stdin-test "Test line")
(close stdin-test)
; Use a file for stdin for test.
(dyn '*stdin* (open "/tmp/sl-sh.stdin.test" :read) (test::assert-equal "Test line
" (read-line *stdin*)))
```


| <a id="root::*stdout*" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``*stdout*``](#root::*stdout*-contents) | File |
| root::*stdout* | ``(write-line *stdout*)``<br><br>File that connects to standard out by default.<br><br>Can be used in place of a write file object in any form that takes one.  Used<br>as the default for print and println. |

```
; Use a file for stdout for test.
(dyn '*stdout* (open "/tmp/sl-sh.stdout.test" :create :truncate) (write-line *stdout* "Test out"))
(test::assert-equal "Test out
" (read-line (open "/tmp/sl-sh.stdout.test" :read)))
```


| <a id="shell::alias" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``alias``](#shell::alias-contents) | Macro |
| shell::alias | ``(alias name body)``<br><br><br>Create an alias, intended to be used with executables not lisp code (use defn<br>for that). |



| <a id="shell::alias?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``alias?``](#shell::alias?-contents) | Lambda |
| shell::alias? | ``(alias? name)``<br><br><br>		Provides boolean value confirming or denying given alias' presence<br>		in set of registered aliases. |



| <a id="root::bg" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``bg``](#root::bg-contents) | Function |
| root::bg | ``(bg job-id?)``<br><br>Put a job in the background.<br><br>If no job id is specified use the last job. |

```
;(bg)
t
```


| <a id="shell::bg-color-rgb" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``bg-color-rgb``](#shell::bg-color-rgb-contents) | Lambda |
| shell::bg-color-rgb | ``(bg-color-rgb red-val green-val blue-val)``<br><br>Set the background color to the desired rgb where each arg is an integer between 0 and 255 inclusive. |



| <a id="root::cd" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``cd``](#root::cd-contents) | Function |
| root::cd | ``(cd dir-to-change-to)``<br><br>Change directory. |



| <a id="shell::clear-dirs" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``clear-dirs``](#shell::clear-dirs-contents) | Lambda |
| shell::clear-dirs | ``(clear-dirs)``<br><br><br>		Clears the directory stack. |



| <a id="root::command" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``command``](#root::command-contents) | SpecialForm |
| root::command | ``(command exp0 ... expN)``<br><br>Only execute system commands not forms within this form. |

```
(test::assert-equal '#(:error "Failed to execute [str string]: No such file or directory (os error 2)") (get-error (command (str "string"))))
(test::assert-equal "Some String
" (str (command (echo "Some String"))))
```


| <a id="shell::dirs" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``dirs``](#shell::dirs-contents) | Lambda |
| shell::dirs | ``(dirs)``<br><br><br>		List the directory stack. |



| <a id="core::dotimes" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``dotimes``](#core::dotimes-contents) | Macro |
| core::dotimes | ``(dotimes times body)``<br><br><br>Evaluate body a number of times equal to times' numerical value. |



| <a id="core::dotimesi" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``dotimesi``](#core::dotimesi-contents) | Macro |
| core::dotimesi | ``(dotimesi idx-bind times body)``<br><br><br>Evaluate body a number of times equal to times' numnrical value. Includes an <br>incrementing reference binding, idx-bind, accesible in body. |



| <a id="shell::endfix-on" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``endfix-on``](#shell::endfix-on-contents) | Macro |
| shell::endfix-on | ``(endfix-on)``<br><br><br>	Allows use of infix notation for common shell forms. The following is the<br>	complete mapping in lisp/endfix.lisp of all supported infix operators and<br>	the corresponding sl-sh function they map to:<br>		'\|\| 'or<br>		'\| '\|<br>		'@@ 'progn (@@ is used instead of ; because ; is a comment in lisp)<br>		'&& 'and<br>		'out> 'out><br>		'out>> 'out>><br>		'err> 'err><br>		'err>> 'err>><br>		'out>null 'out>null<br>		'out-err> 'out-err><br>		'out-err>> 'out-err>><br>		'out-err>null 'out-err>null |



| <a id="shell::err>" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``err>``](#shell::err>-contents) | Macro |
| shell::err> | ``(err> file body)``<br><br><br>Redirect stderr to file, truncate the file first. |



| <a id="shell::err>>" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``err>>``](#shell::err>>-contents) | Macro |
| shell::err>> | ``(err>> file body)``<br><br><br>Redirect stderr to file, append the output. |



| <a id="shell::err>null" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``err>null``](#shell::err>null-contents) | Macro |
| shell::err>null | ``(err>null body)``<br><br><br>Redirect stderr to null (/dev/null equivelent). |



| <a id="root::exit" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``exit``](#root::exit-contents) | Function |
| root::exit | ``(exit code?)``<br><br>Exit shell with optional status code. |

```
;(exit)
;(exit 0)
t
```


| <a id="root::fg" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fg``](#root::fg-contents) | Function |
| root::fg | ``(fg job-id?)``<br><br>Put a job in the foreground.<br><br>If no job id is specified use the last job. |

```
;(fg)
t
```


| <a id="shell::fg-color-rgb" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fg-color-rgb``](#shell::fg-color-rgb-contents) | Lambda |
| shell::fg-color-rgb | ``(fg-color-rgb red-val green-val blue-val)``<br><br>Set the foreground color to the desired rgb where each arg is an integer between 0 and 255 inclusive. |



| <a id="core::for" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``for``](#core::for-contents) | Macro |
| core::for | ``(for bind in_list body)``<br><br><br>bind is bound to the current element of in_list and is accesible<br>in body. body is evaluated a number of times equal to the the number of items<br>in in_list. |



| <a id="core::fori" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fori``](#core::fori-contents) | Macro |
| core::fori | ``(fori idx_bind bind in_list body)``<br><br><br>idx-bind is an incrementing reference bound to current elemtn in in_list.<br>accesible in body and a binding to the current element of in_list. bind is<br>bound to the current element of in_list as is also accesible in body.  body is<br>evaluated a number of times equal to the the number of items in in_list. |



| <a id="root::form" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``form``](#root::form-contents) | SpecialForm |
| root::form | ``(form exp0 ... expN)``<br><br>Like progn but do not execute system commands within this form. |

```
(test::assert-equal '#(:error "Not a valid form true, not found.") (get-error (form (true))))
(test::assert-equal "Some String" (form (str "Some String")))
```


| <a id="root::fs-dir?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fs-dir?``](#root::fs-dir?-contents) | Function |
| root::fs-dir? | ``(fs-dir? path-to-test)``<br><br>Is the given path a directory? |



| <a id="root::fs-exists?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fs-exists?``](#root::fs-exists?-contents) | Function |
| root::fs-exists? | ``(fs-exists? path-to-test)``<br><br>Does the given path exist? |



| <a id="root::fs-file?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``fs-file?``](#root::fs-file?-contents) | Function |
| root::fs-file? | ``(fs-file? path-to-test)``<br><br>Is the given path a file? |



| <a id="shell::get-dirs" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``get-dirs``](#shell::get-dirs-contents) | Lambda |
| shell::get-dirs | ``(get-dirs)``<br><br><br>		Return the vector of directories. |



| <a id="root::glob" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``glob``](#root::glob-contents) | Function |
| root::glob | ``(glob /path/with/*)``<br><br>Takes a list/varargs of globs and return the list of them expanded. |



| <a id="root::jobs" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``jobs``](#root::jobs-contents) | Function |
| root::jobs | ``(jobs)``<br><br>Print list of jobs with ids. |

```
;(jobs)
t
```


| <a id="shell::let-env" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``let-env``](#shell::let-env-contents) | Macro |
| shell::let-env | ``(let-env vals &rest let_body)``<br><br><br>Like let but sets environment variables that are reset after the macro finishes. |



| <a id="root::loose-symbols" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``loose-symbols``](#root::loose-symbols-contents) | SpecialForm |
| root::loose-symbols | ``(loose-symbols exp0 ... expN)``<br><br>Within this form any undefined symbols become strings. |

```
(test::assert-equal "Some_Result" (loose-symbols Some_Result))
```


| <a id="shell::out-err>" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``out-err>``](#shell::out-err>-contents) | Macro |
| shell::out-err> | ``(out-err> file body)``<br><br><br>Redirect both stdout and stderr to the same file, truncate the file first. |



| <a id="shell::out-err>>" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``out-err>>``](#shell::out-err>>-contents) | Macro |
| shell::out-err>> | ``(out-err>> file body)``<br><br><br>Redirect both stdout and stderr to the same file, append the output. |



| <a id="shell::out-err>null" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``out-err>null``](#shell::out-err>null-contents) | Macro |
| shell::out-err>null | ``(out-err>null body)``<br><br><br>Redirect both stdout and stderr to null (/dev/null equivelent). |



| <a id="shell::out>" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``out>``](#shell::out>-contents) | Macro |
| shell::out> | ``(out> file body)``<br><br><br>Redirect stdout to file, truncate the file first. |



| <a id="shell::out>>" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``out>>``](#shell::out>>-contents) | Macro |
| shell::out>> | ``(out>> file body)``<br><br><br>Redirect stdout to file, append the output. |



| <a id="shell::out>null" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``out>null``](#shell::out>null-contents) | Macro |
| shell::out>null | ``(out>null body)``<br><br><br>Redirect stdout to null (/dev/null equivelent). |



| <a id="root::pid" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``pid``](#root::pid-contents) | Function |
| root::pid | ``(pid proc)``<br><br>Return the pid of a process. |



| <a id="root::pipe" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``pipe``](#root::pipe-contents) | Function |
| root::pipe | ``(pipe (proc-whose-stdout) (is-inpup-here))``<br><br>Setup a pipe between processes. |



| <a id="shell::popd" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``popd``](#shell::popd-contents) | Lambda |
| shell::popd | ``(popd)``<br><br><br>		Pop first directory from directory stack and change to it. |



| <a id="shell::pushd" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``pushd``](#shell::pushd-contents) | Lambda |
| shell::pushd | ``(pushd dir)``<br><br><br>		Push current directory on the directory stack and change to new directory. |



| <a id="shell::register-alias" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``register-alias``](#shell::register-alias-contents) | Lambda |
| shell::register-alias | ``(register-alias name)``<br><br><br>		Registers an alias to the current scope. Useful if unregistering or<br>		ability to know whether an alias has been registered an alias is<br>		desirable. |



| <a id="root::run-bg" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``run-bg``](#root::run-bg-contents) | SpecialForm |
| root::run-bg | ``(run-bg exp0 ... expN)``<br><br>Like progn except any system commands started within form will be in the background. |

```
;(run-bg gitk)
t
```


| <a id="shell::set-dirs-max" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``set-dirs-max``](#shell::set-dirs-max-contents) | Lambda |
| shell::set-dirs-max | ``(set-dirs-max max)``<br><br><br>		Sets the max number of directories to save in the stack. |



| <a id="shell::syntax-off" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``syntax-off``](#shell::syntax-off-contents) | Macro |
| shell::syntax-off | ``(syntax-off)``<br><br><br>Turn off syntax highlighting at the repl. |



| <a id="shell::syntax-on" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``syntax-on``](#shell::syntax-on-contents) | Macro |
| shell::syntax-on | ``(syntax-on)``<br><br><br>Turn on syntax highlighting at the repl. |



| <a id="shell::sys-command?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``sys-command?``](#shell::sys-command?-contents) | Lambda |
| shell::sys-command? | ``(sys-command? com)``<br><br><br>True if the supplied command is a system command. |



| <a id="root::unexport" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``unexport``](#root::unexport-contents) | Function |
| root::unexport | ``(unexport symbol)``<br><br>Remove a var from the current shell environment. |

```
(test::assert-equal "ONE" (export 'TEST_EXPORT_ONE "ONE"))
(test::assert-equal "ONE" $TEST_EXPORT_ONE)
(unexport 'TEST_EXPORT_ONE)
(test::assert-false $TEST_EXPORT_ONE)
```


| <a id="shell::unregister-alias" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``unregister-alias``](#shell::unregister-alias-contents) | Lambda |
| shell::unregister-alias | ``(unregister-alias name)``<br><br><br>		Unregisters an alias, removing it from scope. |



| <a id="root::version" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``version``](#root::version-contents) | Function |
| root::version | ``(version)``<br><br>Produce executable version as string. |

```
(test::assert-true (string? (version)))
```


| <a id="root::wait" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``wait``](#root::wait-contents) | Function |
| root::wait | ``(wait proc-to-wait-for)``<br><br>Wait for a process to end and return it's exit status. |



| <a id="shell::pipe-shorthand" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``|``](#shell::pipe-shorthand-contents) | Macro |
| shell::| | ``(| &rest body)``<br><br><br>Shorthand for pipe builtin. |

### <a id="String forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[String forms](#String forms-contents)



| <a id="root::str" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str``](#root::str-contents) | Function |
| root::str | ``(str arg0 ... argN) -> string``<br><br>Make a new string with it's arguments.<br><br>Arguments will be turned into strings.  If an argument is a process then the<br>output of the process will be captured and put into the string. |

```
(test::assert-equal "stringsome" (str "string" "some"))
(test::assert-equal "string" (str "string" ""))
(test::assert-equal "string 50" (str "string" " " 50))
(test::assert-equal "string 50 test
" (str "string" " " 50 " " (echo "test")))
```


| <a id="root::str-append" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-append``](#root::str-append-contents) | Function |
| root::str-append | ``(str-append string string) -> string``<br><br>Make a new string by appending two strings. |

```
(test::assert-equal "stringsome" (str-append "string" "some"))
(test::assert-equal "string" (str-append "string" ""))
(test::assert-equal "string " (str-append "string" " "))
```


| <a id="root::str-buf" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-buf``](#root::str-buf-contents) | Function |
| root::str-buf | ``(str-buf arg0 ... argN) -> string-buffer``<br><br>Make a new string buffer with it's arguments.<br><br>Arguments will be turned into strings.  If an argument is a process then the<br>output of the process will be captured and put into the string buffer. |

```
(test::assert-equal "stringsome" (str-buf "string" "some"))
(test::assert-equal "StringBuf" (type (str-buf "string" "some")))
(test::assert-true (string-buf? (str-buf "string" "some")))
(test::assert-equal "string" (str-buf "string" ""))
(test::assert-equal "string 50" (str-buf "string" " " 50))
(test::assert-equal "string 50 test
" (str-buf "string" " " 50 " " (echo "test")))
```


| <a id="root::str-buf-clear!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-buf-clear!``](#root::str-buf-clear!-contents) | Function |
| root::str-buf-clear! | ``(str-buf-clear! string-buffer) -> string-buffer``<br><br>Clears a string-buffer.  This is a destructive form.<br><br>Returns the string-buffer it was given. |

```
(test::assert-equal "" (str-buf-clear! (str-buf "string")))
(def 'test-str-buf-clear (str-buf "def-string"))
(test::assert-equal "" (str-buf-clear! test-str-buf-clear))
(test::assert-equal "" test-str-buf-clear)
```


| <a id="root::str-buf-map" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-buf-map``](#root::str-buf-map-contents) | Function |
| root::str-buf-map | ``(str-buf-map lambda string) -> string-buffer``<br><br>Make a new string buffer by applying lambda to each char. |

```
(def 'test-str-buf-map (str-buf-map (fn (ch) (if (char= #\x ch) #\X ch)) "xstringxstrx"))
(test::assert-equal "XstringXstrX" test-str-buf-map)
(test::assert-true (string-buf? test-str-buf-map))
(def 'test-str-buf-map (str-buf-map (fn (ch) (if (char= #\x ch) #\X ch)) (str-buf "xstringxstrx")))
(test::assert-equal "XstringXstrX" test-str-buf-map)
(test::assert-true (string-buf? test-str-buf-map))
```


| <a id="root::str-buf-push!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-buf-push!``](#root::str-buf-push!-contents) | Function |
| root::str-buf-push! | ``(str-buf-push! string-buffer arg0 ... argN) -> string-buffer``<br><br>Push the args (as strings) onto the string-buffer.  This is a destructive form.<br><br>Arguments will be turned into strings.  Returns the string-buffer it was given. |

```
(test::assert-equal "stringsome" (str-buf-push! (str-buf "string") "some"))
(def 'test-str-buf-push (str-buf "def-string"))
(test::assert-equal "def-stringsome" (str-buf-push! test-str-buf-push "some"))
(test::assert-equal "def-stringsome" test-str-buf-push)
```


| <a id="root::str-bytes" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-bytes``](#root::str-bytes-contents) | Function |
| root::str-bytes | ``(str-bytes string) -> int``<br><br>Return number of bytes in a string (may be more then length).<br><br>Strings are utf8 so it chars and bytes may not be a one to one match. |

```
(test::assert-equal 4 (str-bytes "Stau"))
(test::assert-equal 0 (str-bytes ""))
; Note 5 chars and 6 bytes because of the final char.
(test::assert-equal 6 (str-bytes "StauΣ"))
```


| <a id="root::str-cat-list" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-cat-list``](#root::str-cat-list-contents) | Function |
| root::str-cat-list | ``(str-cat-list join-pattern sequence) -> string``<br><br>Build a string by concatting a sequence with a join string. |

```
(test::assert-equal "stringxxxyyyxxxsome" (str-cat-list "xxx" '("string" "yyy" "some")))
(test::assert-equal "string yyy some" (str-cat-list " " '("string" "yyy" "some")))
(test::assert-equal "stringyyysome" (str-cat-list "" '("string" "yyy" "some")))
```


| <a id="root::str-contains" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-contains``](#root::str-contains-contents) | Function |
| root::str-contains | ``(str-contains pattern string) -> t/nil``<br><br>True if string contains pattern (both strings). |

```
(test::assert-true (str-contains "Stau" "Stausomething"))
(test::assert-false (str-contains "StaU" "Stausomething"))
(test::assert-true (str-contains "some" "Stausomething"))
(test::assert-false (str-contains "Some" "Stausomething"))
(test::assert-true (str-contains "thing" "Stausomething"))
(test::assert-false (str-contains "Thing" "Stausomething"))
(test::assert-true (str-contains "someΣ" "StausomeΣthing"))
```


| <a id="root::str-empty?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-empty?``](#root::str-empty?-contents) | Function |
| root::str-empty? | ``(str-empty?) -> t/nil``<br><br>Is a string empty?  Let's find out... |

```
(test::assert-true (str-empty? ""))
(test::assert-true (str-empty? (str-trim "   ")))
(test::assert-false (str-empty? " "))
(test::assert-false (str-empty? "string"))
```


| <a id="root::str-ignore-expand" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-ignore-expand``](#root::str-ignore-expand-contents) | Function |
| root::str-ignore-expand | ``(str-ignore-expand exp0 ... expN) -> [final expression]``<br><br>Like progn but any strings in the form will not be expanded. |

```
(export 'TST-IGNORE "TST")
(test::assert-equal "some TST stuff" "some $TST-IGNORE stuff")
(test::assert-equal "some \$TST-IGNORE stuff" (str-ignore-expand "some $TST-IGNORE stuff"))
```


| <a id="root::str-lower" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-lower``](#root::str-lower-contents) | Function |
| root::str-lower | ``(str-lower string) -> string``<br><br>Get all lower case string from a string. |

```
(test::assert-equal "stau" (str-lower "STAU"))
(test::assert-equal "stau" (str-lower "stau"))
(test::assert-equal "stau" (str-lower "Stau"))
(test::assert-equal "stau" (str-lower "StaU"))
(test::assert-equal "stau" (str-lower "sTaU"))
```


| <a id="root::str-ltrim" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-ltrim``](#root::str-ltrim-contents) | Function |
| root::str-ltrim | ``(str-ltrim string) -> string``<br> <br>Trim left whitspace from string. |

```
(test::assert-equal "some string" (str-ltrim "   some string"))
(test::assert-equal "some string   " (str-ltrim "   some string   "))
(test::assert-equal "some string   " (str-ltrim (str-buf "   some string   ")))
(test::assert-equal "some string   " (str-ltrim "some string   "))
(test::assert-equal "some string" (str-ltrim "some string"))
```


| <a id="root::str-map" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-map``](#root::str-map-contents) | Function |
| root::str-map | ``(str-map lambda string) -> string``<br><br>Make a new string by applying lambda to each char. |

```
(test::assert-equal "XstringXstrX" (str-map (fn (ch) (if (char= #\x ch) #\X ch)) "xstringxstrx"))
(def 'test-str-map (str-map (fn (ch) (if (char= #\x ch) #\X ch)) "xstringxstrx"))
(test::assert-equal "XstringXstrX" test-str-map)
(test::assert-true (string? test-str-map))
(def 'test-str-map (str-map (fn (ch) (if (char= #\x ch) #\X ch)) (str-buf "xstringxstrx")))
(test::assert-equal "XstringXstrX" test-str-map)
(test::assert-true (string? test-str-map))
```


| <a id="root::str-nth" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-nth``](#root::str-nth-contents) | Function |
| root::str-nth | ``(str-nth n string) -> char``<br><br>Get the nth char of a string. |

```
(test::assert-equal #\a (str-nth 2 "stau"))
(test::assert-equal #\s (str-nth 0 "stau"))
(test::assert-equal #\u (str-nth 3 "stau"))
```


| <a id="root::str-replace" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-replace``](#root::str-replace-contents) | Function |
| root::str-replace | ``(str-replace string old-pattern new-pattern) -> string``<br> <br>Replace occurances of second string with third in the first string. |

```
(test::assert-equal "some yyy string" (str-replace "some xxx string" "xxx" "yyy"))
(test::assert-equal "some yyy string yyy" (str-replace "some xxx string xxx" "xxx" "yyy"))
(test::assert-equal "yyy some yyy string yyy" (str-replace "xxx some xxx string xxx" "xxx" "yyy"))
```


| <a id="root::str-rsplit" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-rsplit``](#root::str-rsplit-contents) | Function |
| root::str-rsplit | ``(str-rsplit split-pattern string) -> vector``<br> <br>Use a pattern to split a string into reverse order. |

```
(test::assert-equal '("some" "yyy" "string") (str-rsplit "xxx" "stringxxxyyyxxxsome"))
(test::assert-equal '("" "some" "yyy" "string") (str-rsplit "xxx" "stringxxxyyyxxxsomexxx"))
(test::assert-equal '("some" "yyy" "string") (str-rsplit " " "string yyy some"))
(test::assert-equal '("somexxxyyyxxxstring") (str-rsplit :whitespace "somexxxyyyxxxstring"))
(test::assert-equal '("somexxxyyyxxxstring") (str-rsplit "zzz" "somexxxyyyxxxstring"))
```


| <a id="root::str-rsplitn" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-rsplitn``](#root::str-rsplitn-contents) | Function |
| root::str-rsplitn | ``(str-rsplitn n split-pattern string) -> vector``<br> <br>Use a pattern to split a string with at most n items returned in reverse order. |

```
(test::assert-equal '("some" "yyy" "string") (str-rsplitn 3 "xxx" "stringxxxyyyxxxsome"))
(test::assert-equal '("some" "yyy" "string") (str-rsplitn 4 "xxx" "stringxxxyyyxxxsome"))
(test::assert-equal '("other" "string" "somexxxyyy") (str-rsplitn 3 "xxx" "somexxxyyyxxxstringxxxother"))
(test::assert-equal '("somexxxyyyxxxstringxxxother") (str-rsplitn 1 "xxx" "somexxxyyyxxxstringxxxother"))
(test::assert-equal '() (str-rsplitn 0 "xxx" "somexxxyyyxxxstringxxxzero"))
```


| <a id="root::str-rtrim" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-rtrim``](#root::str-rtrim-contents) | Function |
| root::str-rtrim | ``(str-rtrim string) -> string``<br> <br>Trim right whitespace from string. |

```
(test::assert-equal "   some string" (str-rtrim "   some string"))
(test::assert-equal "   some string" (str-rtrim "   some string   "))
(test::assert-equal "   some string" (str-rtrim (str-buf "   some string   ")))
(test::assert-equal "some string" (str-rtrim "some string   "))
(test::assert-equal "some string" (str-rtrim "some string"))
```


| <a id="root::str-split" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-split``](#root::str-split-contents) | Function |
| root::str-split | ``(str-split split-pattern string) -> vector``<br> <br>Use a pattern to split a string (:whitespace to split on whitespace). |

```
(test::assert-equal '("some" "yyy" "string") (str-split "xxx" "somexxxyyyxxxstring"))
(test::assert-equal '("some" "yyy" "string" "") (str-split "xxx" "somexxxyyyxxxstringxxx"))
(test::assert-equal '("" "some" "yyy" "string" "") (str-split "xxx" "xxxsomexxxyyyxxxstringxxx"))
(test::assert-equal '("some" "yyy" "string") (str-split :whitespace "some yyy string"))
(test::assert-equal '("somexxxyyyxxxstring") (str-split :whitespace "somexxxyyyxxxstring"))
(test::assert-equal '("somexxxyyyxxxstring") (str-split "zzz" "somexxxyyyxxxstring"))
```


| <a id="root::str-splitn" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-splitn``](#root::str-splitn-contents) | Function |
| root::str-splitn | ``(str-splitn n split-pattern string) -> vector``<br> <br>Use a pattern to split a string with at most n items. |

```
(test::assert-equal '("some" "yyy" "string") (str-splitn 3 "xxx" "somexxxyyyxxxstring"))
(test::assert-equal '("some" "yyy" "string") (str-splitn 4 "xxx" "somexxxyyyxxxstring"))
(test::assert-equal '("some" "yyy" "stringxxxother") (str-splitn 3 "xxx" "somexxxyyyxxxstringxxxother"))
(test::assert-equal '("somexxxyyyxxxstringxxxother") (str-splitn 1 "xxx" "somexxxyyyxxxstringxxxother"))
(test::assert-equal '() (str-splitn 0 "xxx" "somexxxyyyxxxstringxxxzero"))
```


| <a id="root::str-starts-with" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-starts-with``](#root::str-starts-with-contents) | Function |
| root::str-starts-with | ``(str-starts-with pattern string) -> t/nil``<br><br>True if string start with pattern (both strings). |

```
(test::assert-true (str-starts-with "Stau" "Stausomething"))
(test::assert-false (str-starts-with "StaU" "Stausomething"))
```


| <a id="root::str-sub" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-sub``](#root::str-sub-contents) | Function |
| root::str-sub | ``(str-sub start length string) -> string``<br><br>Return a substring from a string given start (0 based) and length. |

```
(test::assert-equal "string" (str-sub 0 6 "stringxxxyyyxxxsome"))
(test::assert-equal "some" (str-sub 15 4 "stringxxxyyyxxxsome"))
(test::assert-equal "yyy" (str-sub 9 3 "stringxxxyyyxxxsome"))
```


| <a id="root::str-trim" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-trim``](#root::str-trim-contents) | Function |
| root::str-trim | ``(str-trim string) -> string``<br> <br>Trim right and left whitespace from string. |

```
(test::assert-equal "some string" (str-trim "   some string"))
(test::assert-equal "some string" (str-trim "   some string   "))
(test::assert-equal "some string" (str-trim (str-buf "   some string   ")))
(test::assert-equal "some string" (str-trim "some string   "))
(test::assert-equal "some string" (str-trim "some string"))
```


| <a id="root::str-upper" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``str-upper``](#root::str-upper-contents) | Function |
| root::str-upper | ``(str-upper string) -> string``<br><br>Get all upper case string from a string. |

```
(test::assert-equal "STAU" (str-upper "STAU"))
(test::assert-equal "STAU" (str-upper "stau"))
(test::assert-equal "STAU" (str-upper "Stau"))
(test::assert-equal "STAU" (str-upper "StaU"))
(test::assert-equal "STAU" (str-upper "sTaU"))
```
### <a id="Type forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Type forms](#Type forms-contents)
These forms provide information/tests about an objects underlying type.


| <a id="root::builtin?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``builtin?``](#root::builtin?-contents) | Function |
| root::builtin? | ``(builtin? expression)``<br><br>True if the expression is a builtin function or special form, false otherwise. |

```
(test::assert-true (builtin? type))
(test::assert-true (builtin? if))
(test::assert-false (builtin? (fn () ())))
(test::assert-false (builtin? copy-seq))
(test::assert-false (builtin? 1))
```


| <a id="root::char?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``char?``](#root::char?-contents) | Function |
| root::char? | ``(char? expression)``<br><br>True if the expression is a char, false otherwise. |

```
(test::assert-true (char? #\a))
(test::assert-false (char? 1))
(test::assert-false (char? "a"))
```


| <a id="root::file?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``file?``](#root::file?-contents) | Function |
| root::file? | ``(file? expression)``<br><br>True if the expression is a file, false otherwise. |

```
(test::assert-true (file? (open :stdout)))
(test::assert-false (file? (fn () ())))
(test::assert-false (file? copy-seq))
(test::assert-false (file? 1))
```


| <a id="root::float?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``float?``](#root::float?-contents) | Function |
| root::float? | ``(float? expression)``<br><br>True if the expression is a float, false otherwise. |

```
(test::assert-true (float? 1.5))
(test::assert-false (float? 1))
```


| <a id="core::func?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``func?``](#core::func?-contents) | Macro |
| core::func? | ``(func? to-test)``<br><br><br>True if the expression is a [builtin?](#root::builtin?), a [lambda?](#root::lambda?), or a [macro?](#root::macro?) |



| <a id="root::hash?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``hash?``](#root::hash?-contents) | Function |
| root::hash? | ``(hash? expression)``<br><br>True if the expression is a hash map, false otherwise. |

```
(test::assert-true (hash? (make-hash)) "make-vec") 
(test::assert-false (hash? 1))
(test::assert-false (hash? '(1 2 3)))
(test::assert-false (hash? (list)))
(test::assert-false (hash? (vec)))
```


| <a id="root::int?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``int?``](#root::int?-contents) | Function |
| root::int? | ``(int? expression)``<br><br>True if the expression is an int, false otherwise. |

```
(test::assert-true (int? 1))
(test::assert-false (int? 1.5))
```


| <a id="root::lambda?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``lambda?``](#root::lambda?-contents) | Function |
| root::lambda? | ``(lambda? expression)``<br><br>True if the expression is a lambda, false otherwise. |

```
(test::assert-true (lambda? (fn () ())))
(test::assert-true (lambda? copy-seq))
(test::assert-false (lambda? 1))
(test::assert-false (lambda? if))
```


| <a id="root::list?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``list?``](#root::list?-contents) | Function |
| root::list? | ``(list? expression)``<br><br>True if the expression is a list, false otherwise. |

```
(test::assert-true (list? '(1 2 3)) "reader macro")
(test::assert-true (list? (list 1 2 3)) "list") 
(test::assert-false (list? 1))
(test::assert-false (list? '#(1 2 3)))
(test::assert-false (list? (vec)))
(test::assert-false (list? '(1 . 2)))
```


| <a id="root::macro?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``macro?``](#root::macro?-contents) | Function |
| root::macro? | ``(macro? expression)``<br><br>True if the expression is a macro, false otherwise. |

```
(test::assert-true (macro? (macro () ())))
(test::assert-true (macro? defn))
(test::assert-false (macro? 1))
(test::assert-false (macro? if))
```


| <a id="root::nil?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``nil?``](#root::nil?-contents) | Function |
| root::nil? | ``(nil? expression)``<br><br>True if the expression is nil, false otherwise. |

```
(test::assert-true (nil? nil))
(test::assert-false (nil? t))
```


| <a id="root::pair?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``pair?``](#root::pair?-contents) | Function |
| root::pair? | ``(pair? expression)``<br><br>True if the expression is a pair, false otherwise. |

```
(test::assert-true (pair? '(1 . 2)) "reader macro")
(test::assert-true (pair? (join 1 2)) "join") 
(test::assert-true (pair? '(1 2)))
(test::assert-false (pair? 1))
(test::assert-false (pair? '#(1 2 3)))
(test::assert-false (pair? (vec)))
```


| <a id="root::process?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``process?``](#root::process?-contents) | Function |
| root::process? | ``(process? expression)``<br><br>True if the expression is a process, false otherwise. |

```
(test::assert-true (process? (true)))
(test::assert-false (process? (fn () ())))
(test::assert-false (process? copy-seq))
(test::assert-false (process? 1))
```


| <a id="root::string-buf?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``string-buf?``](#root::string-buf?-contents) | Function |
| root::string-buf? | ``(string-buf? expression)``<br><br>True if the expression is a string buffer, false otherwise. |

```
(test::assert-true (string-buf? (str-buf "string")))
(test::assert-false (string-buf? "string"))
(test::assert-false (string-buf? 1))
```


| <a id="root::string?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``string?``](#root::string?-contents) | Function |
| root::string? | ``(string? expression)``<br><br>True if the expression is a string, false otherwise. |

```
(test::assert-true (string? "string"))
(test::assert-false (string? 1))
```


| <a id="root::symbol?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``symbol?``](#root::symbol?-contents) | Function |
| root::symbol? | ``(symbol? expression)``<br><br>True if the expression is a symbol, false otherwise. |

```
(test::assert-true (symbol? 'symbol))
(test::assert-false (symbol? 1))
```


| <a id="root::true?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``true?``](#root::true?-contents) | Function |
| root::true? | ``(true? expression)``<br><br>True if the expression is true (true type NOT non-null), false otherwise. |

```
(test::assert-true (true? t))
(test::assert-false (true? nil))
(test::assert-false (true? 1))
(test::assert-false (true? "str"))
```


| <a id="root::type" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``type``](#root::type-contents) | Function |
| root::type | ``(type expression)``<br><br>Return the type of the given expression as a string.<br><br>Types are:<br>    True<br>    Float<br>    Int<br>    Symbol<br>    String<br>    StringBuf<br>    Char<br>    Lambda<br>    Macro<br>    Process<br>    SpecialForm<br>    Function<br>    Vector<br>    Pair<br>    Nil<br>    HashMap<br>    File |

```
(test::assert-equal "True" (type t))
(test::assert-equal "Float" (type 1.1))
(test::assert-equal "Int" (type 1))
(test::assert-equal "Symbol" (type 'symbol))
(def 'type-sym 'symbol)
(test::assert-equal "Symbol" (type type-sym))
(test::assert-equal "String" (type "string"))
(test::assert-equal "StringBuf" (type (str-buf "buffer")))
(test::assert-equal "Char" (type #\a))
(test::assert-equal "Lambda" (type (fn () ())))
(test::assert-equal "Macro" (type (macro () ())))
(test::assert-equal "Process" (type (true)))
(test::assert-equal "SpecialForm" (type if))
(test::assert-equal "Function" (type type))
(test::assert-equal "Vector" (type '#(1 2 3)))
(def 'type-vec '#(4 5 6))
(test::assert-equal "Vector" (type type-vec))
(test::assert-equal "Pair" (type '(1 . 2)))
(test::assert-equal "Pair" (type '(1 2 3)))
(test::assert-equal "Nil" (type nil))
(test::assert-equal "Nil" (type '()))
(test::assert-equal "HashMap" (type (make-hash)))
(test::assert-equal "File" (type (open :stdin)))
```


| <a id="root::vec?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec?``](#root::vec?-contents) | Function |
| root::vec? | ``(vec? expression)``<br><br>True if the expression is a vector, false otherwise. |

```
(test::assert-true (vec? '#(1 2 3)) "reader macro")
(test::assert-true (vec? (make-vec)) "make-vec") 
(test::assert-true (vec? (vec 1 2 3)) "vec") 
(test::assert-false (vec? 1))
(test::assert-false (vec? '(1 2 3)))
(test::assert-false (vec? (list)))
```
### <a id="Vector forms-body" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[Vector forms](#Vector forms-contents)
Forms ending in '!' are destructive and change the underlying vector, other forms
do not make changes to the the provided vector.  They are usable in place of a
list for purposes of lambda calls, parameters, etc (they work the same as a list
made from pairs but are vectors not linked lists).  Use #() to declare them in
code (i.e. '#(1 2 3) or #(+ 1 2)).


| <a id="root::make-vec" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``make-vec``](#root::make-vec-contents) | Function |
| root::make-vec | ``(make-vec capacity default)``<br><br>Make a new vector with capacity and default item(s). |

```
(test::assert-false (make-vec))
(test::assert-equal '(x x x) (make-vec 3 'x))
(test::assert-equal '(nil nil nil nil nil) (make-vec 5 nil))
(test::assert-equal '() (make-vec 5))
```


| <a id="root::vec" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec``](#root::vec-contents) | Function |
| root::vec | ``(vec item1 item2 .. itemN)``<br><br>Make a new vector with items. |

```
(test::assert-false (vec))
(test::assert-equal '(1 2 3) (vec 1 2 3))
```


| <a id="root::vec-clear!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-clear!``](#root::vec-clear!-contents) | Function |
| root::vec-clear! | ``(vec-clear! vector)``<br><br>Clears a vector.  This is destructive! |

```
(def 'test-clear-vec (vec 1 2 3))
(test::assert-false (vec-empty? test-clear-vec))
(vec-clear! test-clear-vec)
(test::assert-true (vec-empty? test-clear-vec))
```


| <a id="root::vec-empty?" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-empty?``](#root::vec-empty?-contents) | Function |
| root::vec-empty? | ``(vec-empty? vector)``<br><br>True if the vector is empty. |

```
(test::assert-true (vec-empty? '#()))
(test::assert-false (vec-empty? '#(1 2 3)))
```


| <a id="root::vec-insert-nth!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-insert-nth!``](#root::vec-insert-nth!-contents) | Function |
| root::vec-insert-nth! | ``(vec-insert-nth! index new-element vector)``<br><br>Inserts new-element at index and moves following elements right in vector.  This is destructive! |

```
(def 'test-insert-nth-vec (vec 1 2 3))
(test::assert-equal '(1 2 3) test-insert-nth-vec)
(vec-insert-nth! 1 5 test-insert-nth-vec)
(test::assert-equal '(1 5 2 3) test-insert-nth-vec)
(vec-insert-nth! 2 6 test-insert-nth-vec)
(test::assert-equal '(1 5 6 2 3) test-insert-nth-vec)
(vec-insert-nth! 0 4 test-insert-nth-vec)
(test::assert-equal '(4 1 5 6 2 3) test-insert-nth-vec)
```


| <a id="root::vec-nth" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-nth``](#root::vec-nth-contents) | Function |
| root::vec-nth | ``(vec-nth index vector)``<br><br>Get the nth element (0 based) of a vector. |

```
(test::assert-equal 5 (vec-nth 4 '#(1 2 3 4 5 6)))
(test::assert-equal 1 (vec-nth 0 '#(1 2 3 4 5 6)))
(test::assert-equal 3 (vec-nth 2 '#(1 2 3 4 5 6)))
(test::assert-equal 6 (vec-nth 5 '#(1 2 3 4 5 6)))
```


| <a id="root::vec-pop!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-pop!``](#root::vec-pop!-contents) | Function |
| root::vec-pop! | ``(vec-pop! vector object)``<br><br>Pops the last object off of the end of the vector.  This is destructive! |

```
(def 'test-pop-vec (vec 1 2 3))
(test::assert-equal 3 (vec-pop! test-pop-vec))
(test::assert-equal '(1 2) test-pop-vec)
(test::assert-equal 2 (vec-pop! test-pop-vec))
(test::assert-equal '(1) test-pop-vec)
(test::assert-equal 1 (vec-pop! test-pop-vec))
(test::assert-equal '() test-pop-vec)
```


| <a id="root::vec-push!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-push!``](#root::vec-push!-contents) | Function |
| root::vec-push! | ``(vec-push! vector object)``<br><br>Pushes the provided object onto the end of the vector.  This is destructive! |

```
(def 'test-push-vec (vec))
(test::assert-equal '(1) (vec-push! test-push-vec 1))
(test::assert-equal '(1) test-push-vec)
(test::assert-equal '(1 2) (vec-push! test-push-vec 2))
(test::assert-equal '(1 2) test-push-vec)
(test::assert-equal '(1 2 3) (vec-push! test-push-vec 3))
(test::assert-equal '(1 2 3) test-push-vec)
```


| <a id="root::vec-remove-nth!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-remove-nth!``](#root::vec-remove-nth!-contents) | Function |
| root::vec-remove-nth! | ``(vec-remove-nth! index vector)``<br><br>Remove the element at index from vector.  This is destructive! |

```
(def 'test-remove-nth-vec (vec 1 2 3))
(test::assert-equal '(1 2 3) test-remove-nth-vec)
(vec-remove-nth! 1 test-remove-nth-vec)
(test::assert-equal '(1 3) test-remove-nth-vec)
(vec-remove-nth! 1 test-remove-nth-vec)
(test::assert-equal '(1) test-remove-nth-vec)
(vec-remove-nth! 0 test-remove-nth-vec)
(test::assert-equal '() test-remove-nth-vec)
```


| <a id="root::vec-setnth!" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-setnth!``](#root::vec-setnth!-contents) | Function |
| root::vec-setnth! | ``(vec-setnth! index value vector)``<br><br>Set the nth index (0 based) of a vector to value.  This is destructive! |

```
(def 'test-setnth-vec (vec 1 2 3))
(test::assert-equal '(1 5 3) (vec-setnth! 1 5 test-setnth-vec))
(test::assert-equal '(7 5 3) (vec-setnth! 0 7 test-setnth-vec))
(test::assert-equal '(7 5 9) (vec-setnth! 2 9 test-setnth-vec))
```


| <a id="root::vec-slice" class="anchor" aria-hidden="true" href="#sl-sh-form-documentation"></a>[``vec-slice``](#root::vec-slice-contents) | Function |
| root::vec-slice | ``(vec-slice vector start end?)``<br><br>Returns a slice of a vector (0 based indexes, end is exclusive). |

```
(test::assert-equal '(5 6) (vec-slice '#(1 2 3 4 5 6) 4 6))
(test::assert-equal '(1 2 3) (vec-slice '#(1 2 3 4 5 6) 0 3))
(test::assert-equal '(3 4 5) (vec-slice '#(1 2 3 4 5 6) 2 5))
(test::assert-equal '(3 4 5 6) (vec-slice '#(1 2 3 4 5 6) 2))
```
