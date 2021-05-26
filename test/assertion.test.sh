#!/bin/bash

set -euxo pipefail

function test_assertions_fail {
    if $CLOX test/failure-scripts/assert-fail.lox ; then
        exit 1 # Failure
    else
        echo Assertions work as expected return=$?
    fi
}

test_assertions_fail