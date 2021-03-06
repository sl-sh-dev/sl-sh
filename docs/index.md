---
title: "Getting started with sl-sh"
keywords: sample homepage
tags: [getting_started]
sidebar: mydoc_sidebar
permalink: index.html
summary: These brief instructions will help you get started with sl-sh.
---


## Install sl-sh

Follow these instructions to install sl-sh.

### 1. Install Rust
-  Documentation [here](https://www.rust-lang.org/tools/install)

### 2. Build sl-sh
-  Get source and build
    ```
    git clone https://github.com/sstanfield/slsh
    cd slsh
    cargo build --release
    ./target/release/sl-sh
    ```
-  sl-sh will load with the default slshrc file.

{% include links.html %}

This site built from [Documentation Theme for Jekyll](https://github.com/tomjoht/documentation-theme-jekyll).
