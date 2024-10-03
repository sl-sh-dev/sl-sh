#!/bin/bash

# STEP 1. Extract the version of Clippy from the readme of GitHub's runner-images repository
remoteVersion=$(curl -s https://raw.githubusercontent.com/actions/runner-images/main/images/ubuntu/Ubuntu2204-Readme.md | grep "Cargo clippy" | awk '{print $4}')

# STEP 2. Extract the version of Clippy from the local installation
localVersion=$(cargo clippy --version | awk '{print $2}')

# STEP 3. Alert the user if they should update or not
if [ "$remoteVersion" != "$localVersion" ]; then
    echo "\033[31m⚠️  Outdated Clippy version (v$localVersion) is behind GitHub CI runner (v$remoteVersion). Consider \`rustup update\`\033[0m"
    exit 1
else
    echo "\033[32m✓ Up-to-date Clippy version (v$localVersion)\033[0m"
    exit 0
fi
