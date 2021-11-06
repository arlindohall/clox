#!/bin/bash

set -euxo pipefail

function setup-directories {
    cd c
    mkdir build
    cd build
}

function configure-project {
    cmake ..
}

function build-project {
    make
}

function run {
    setup-directories
    configure-project
    build-project
}

run