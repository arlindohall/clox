#!/bin/bash

set -euxo pipefail

function test_very_simple_math_expression {
    echo '(1 + 2 == 3) != ("apple" == "pine" + "apple");' | $CLOX
}

test_very_simple_math_expression