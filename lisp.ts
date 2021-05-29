#!/bin/sh

FORMS="$*"

try() {
      type $1  /dev/null
	$1 $2 "(progn $FORMS)"
	exit $?
    
}

 sbcl "--noinform --eval"
 clisp "-q -x"
 ecl "-eval"
 ccl "--eval"
 dx86cl "--eval"
 lx86cl "--eval"
 wx86cl "--eval"
 wx86cl64 "--eval"
 gcl "-eval"

 No Lisp found.

