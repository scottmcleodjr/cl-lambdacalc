;;;; cl-lambdacalc.asd

(asdf:defsystem #:cl-lambdacalc
  :description "Define Lisp functions using lambda calculus syntax"
  :author "S M McLeod"
  :license  "MIT"
  :version "0.0.1"
  :depends-on ("cl-ppcre") ;; BSD License
  :serial t
  :components ((:file "package")
               (:file "cl-lambdacalc")))
