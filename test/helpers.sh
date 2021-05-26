
function assert_fails {
    file=$1
    message=$2
    if $CLOX $1 ; then
        exit 1 # Failure
    else
        echo $2
    fi
}