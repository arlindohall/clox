#!/bin/bash

set -euxo pipefail

# # All tests
function all {
    for file in test/*.test.sh; do
        run_test $file
    done
}

function run_test {
    file=$1
    echo Testing $file...
    if bash $file ; then
        echo Done testing $file...
    else
        echo Failure during $file...
    fi
}

# # Build the project
function build {
    ./configure.py && ninja
}

function run {
    build
    all
}

CLOX=build/clox
export CLOX

run