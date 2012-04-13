#!/bin/sh

# clisp -c src/adv.lisp
#
#  XXX Current objective:
#      1) Make a script that compiles and runs everything
#         including regression tests.
#      2) Make it nice, ideomatically correct, using quicklisp and adv.
#

# Update the tags file
etags $(find . -name '*.lisp')

## This should have been sufficient but isn't
clisp -q -x '(asdf:compile-system :adv)'
clisp -q -x '(asdf:compile-system :adv-regression)'
clisp -q -x '(asdf:compile-system :adv-regression-scenario1)'

## So instead we do this
clisp -q -c "src/adv.lisp"
clisp -q -c "src/adv-regression.lisp"
clisp -q -c "src/adv-regression-scenario1.lisp"

## And run regression tests

for x in core-functionality scenario1; do
 clisp  -q -x "(load \"src/adv\")(load \"src/adv-regression.lisp\")(load \"src/adv-regression-$x.lisp\")(in-package :adv-regression-$x)(run-tests)"
done

# clisp  -q -x '(load "src/adv")(load "src/adv-regression.lisp")(load "src/adv-regression-core-functionality.lisp")(in-package :adv-regression-core-functionality)(run-tests)'

# clisp  -q -x '(load "src/adv")(load "src/adv-regression.lisp")(load "src/adv-regression-scenario1.lisp")(in-package :adv-regression-scenario1)(run-tests)'

