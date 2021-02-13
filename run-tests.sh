#!/bin/bash

sbcl \
        --noinform \
        --disable-debugger \
        --eval '(ql:quickload "cl-lambdacalc-test")' \
        --eval '(in-package :cl-lambdacalc)' \
        --eval '(defvar result (run-tests :all))' \
        --eval '(print-failures result)' \
        --eval '(print-errors result)' \
        --quit
