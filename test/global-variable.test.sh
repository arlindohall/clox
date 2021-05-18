#!/bin/bash

set -euxo pipefail

function test_create_and_read_global_variable {
    $CLOX <<EOF
var my_global1 = "hello";
var my_global2 = 1 + 2;
var my_global3;
EOF
}

test_create_and_read_global_variable