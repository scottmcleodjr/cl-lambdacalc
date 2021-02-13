;;;; package.lisp

(defpackage #:cl-lambdacalc
  (:use #:cl
        #:cl-ppcre)
  (:export #:lc-func
           #:define-lc-func
           #:lc-true
           #:lc-false
           #:lc-if
           #:lc-and
           #:lc-or
           #:lc-not
           #:lc-true-p
           #:lc-false-p
           #:lc-succ
           #:lc-add
           #:lc-mult
           #:lc-expt
           #:church-numeral-to-number
           #:number-to-church-numeral))
