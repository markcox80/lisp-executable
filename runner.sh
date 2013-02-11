#!/bin/bash
set -e

function do_trial {
    set -e

    echo --- build $1
    rm example/example-program* || true
    rm example/control-c-tester* || true
    $@
    echo --- run example/example-program $1
    if ! [ -f example/example-program ] ; then
	echo example/example-program failed to build.
	exit 1
    fi

    example/example-program --help
}

function do_trial_both {
    set -e

    echo --- build $1
    rm example/example-program* || true
    rm example/control-c-tester* || true
    $@
    echo --- run example/example-program $1
    if ! [ -f example/example-program ] ; then
	echo example/example-program failed to build.
	exit 1
    fi

    example/example-program --help

    echo --- run example/control-c-tester $1
    if ! [ -f example/control-c-tester ] ; then
	echo example/control-c-tester failed to build.
	exit 1
    fi

    example/control-c-tester &
    pid="$!"
    sleep 3
    kill -INT $pid
    wait
    echo Terminated
}


do_trial_both sbcl --load runner.lisp
do_trial_both ecl -load runner.lisp
do_trial_both ccl64 --load runner.lisp
do_trial clisp < runner.lisp
do_trial_both cmucl -load runner.lisp
