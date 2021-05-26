#!/bin/bash

set -euxo pipefail

. test/helpers.sh

function test_assertions_fail {
    assert_fails test/failure-scripts/assert-fail.lox "Assertions work as expected return=$?"
}

test_assertions_fail