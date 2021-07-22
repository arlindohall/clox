#!/bin/bash

# # All tests
function all {
    for file in test/*.test.lox; do
        run_lox $file
    done
}

function assert_fails {
    file=$1
    $CLOX $file || \
    if ! $CLOX $file ; then
        echo === Test $file exited ===
    else
        exit 1 # Failure
    fi
}

function failures {
    for file in test/failure-scripts/* ; do
        assert_fails $file
    done
}

function run_lox {
    file=$1
    echo === Testing lox script $file ===
    if $CLOX $1 ; then
        echo === Done testing $file ===
    else
        echo ❌ Failure during $file...
        exit 3
    fi
}

# # Build the project
function build {
    ./configure.py && ninja
}

function run {
    build
    failures
    all

    echo ✅  Done with all tests!
}

function run_file {
    file=$1

    if ! build ; then
        echo ❌ Failure during build...
        exit 1
    fi

    run_lox $file

    echo ✅  Done with all tests!
}

CLOX=build/clox
export CLOX

if test $1 ; then
    run_file $1
else
    run
fi