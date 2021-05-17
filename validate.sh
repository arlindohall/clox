#!/bin/bash

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
        echo ❌ Failure during $file...
        exit 1
    fi
}

# # Build the project
function build {
    ./configure.py && ninja
}

function run {
    build
    all

    echo ✅  Done with all tests!
}

function run_file {
    file=$1

    build
    run_test $file

    echo ✅  Done with all tests!
}

CLOX=build/clox
export CLOX

if test $1 ; then
    run_file $1
else
    run
fi