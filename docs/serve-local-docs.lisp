#!/usr/bin/env sl-sh

(ns-import 'shell)

$(bundle exec jekyll serve --config (glob "~/development/sl-sh-dev/sl-sh/docs/_user_config.yml"))

