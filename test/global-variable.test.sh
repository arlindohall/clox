#!/bin/bash

set -euxo pipefail

function test_create_and_read_global_variable {
    $CLOX << EOF
var my_global1 = "hello";
var my_global2 = 1 + 2;
var my_global3;
EOF
}

function test_create_and_print_global_variable {
    $CLOX << EOF
var beverage = "cafe au lait";
var breakfast = "beignets with " + beverage;
print breakfast;
EOF
}

function test_create_set_and_print {
    $CLOX << EOF
var breakfast = "beignets";
var beverage = "cafe au lait";
breakfast = "beignets with " + beverage;

print breakfast + "!";
EOF
}

# test_create_and_read_global_variable
# test_create_and_print_global_variable
test_create_set_and_print