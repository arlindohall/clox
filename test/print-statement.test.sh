#!/bin/bash

set -euxo pipefail

function test_print_statement {
    $CLOX <<EOF
print 1 + 2;
print "abc" + "def";
print 1 + 2 != "abc" + "def";
EOF
}

function test_smaller_print_statement {
    # Getting a segfault with this specific line from the above
    # Turns out it was some build consistency issue, not sure how
    echo 'print "abc" + "def";' | $CLOX
}

test_smaller_print_statement
test_print_statement