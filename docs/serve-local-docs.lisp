#!/usr/bin/env sl-sh

(core::ns-import 'core)
(ns-import 'shell)

(pushd "~/development/slsh/docs")
(bundle exec jekyll serve --config _user_config.yml)

