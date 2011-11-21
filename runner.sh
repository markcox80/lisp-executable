#!/bin/sh
set -e

function do_trial {
    set -e

    echo --- build $1
    rm example/example-program* || true
    $@
    echo --- run $1
    test -f example/example-program
    example/example-program --help
}


do_trial sbcl --load runner.lisp
do_trial ecl -load runner.lisp
do_trial ccl64 --load runner.lisp
do_trial clisp < runner.lisp
do_trial cmucl -load runner.lisp