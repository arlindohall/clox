
function assert_fails {
    file=$1
    code=$2
    $CLOX $file || \
    if test $? = $code ; then
        echo === Test $file exited with expected code $code ===
    else
        exit 1 # Failure
    fi
}