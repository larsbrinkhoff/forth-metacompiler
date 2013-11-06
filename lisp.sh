#!/bin/sh

FORMS="$*"

try() {
    if type $1 > /dev/null 2>&1; then
	$1 $2 "(progn $FORMS)"
	exit $?
    fi
}

try sbcl "--noinform --eval"
try clisp "-q -x"
try ecl "-eval"
try ccl "--eval"

echo No Lisp found.
exit 1
