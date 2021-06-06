#!/bin/bash

set -euxo pipefail

. test/helpers.sh

function test_reference_global_with_same_name_durining_initialize {
    assert_fails \
        test/failure-scripts/local-variable-scope.lox \
        74
}

test_reference_global_with_same_name_durining_initialize