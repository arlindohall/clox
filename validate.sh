#!/bin/bash

# # All tests
function all {
    for file in test/*.test.sh; do
        run_test $file
    done

    for file in test/*.test.lox; do
        run_lox $file
    done
}

function run_test {
    file=$1
    echo === Testing $file ===
    if $file ; then
        echo === Done testing $file ===
    else
        echo ❌ Failure during $file...
        exit 2
    fi
}

function run_lox {
    file=$1
    echo === Testing lox script $file
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
    all

    echo ✅  Done with all tests!
}

function run_file {
    file=$1

    if ! build ; then
        echo ❌ Failure during build...
        exit 1
    fi

    if [[ $file =~ .*test.lox ]] ; then
        run_lox $file
    else
        run_test $file
    fi

    echo ✅  Done with all tests!
}

CLOX=build/clox
export CLOX

if test $1 ; then
    run_file $1
else
    run
fi