#!/bin/bash

function test_print_statement {
    $CLOX <<EOF
print 1 + 2;
print "abc" + "def";
print 1 + 2 != "abc" + "def";
EOF
}

test_print_statement