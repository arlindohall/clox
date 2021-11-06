#!/bin/bash

set -euxo pipefail

function setup-project {
    cd rust
}

function build-project {
    cargo clippy -- -Dwarnings
    cargo test
}

function run {
    setup-project
    build-project
}

run