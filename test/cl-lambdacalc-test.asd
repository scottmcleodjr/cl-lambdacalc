;;;; cl-lambdacalc.asd

(asdf:defsystem #:cl-lambdacalc-test
  :description "Test cases for the cl-lambdacalc package"
  :author "S M McLeod"
  :license  "MIT"
  :version "0.0.1"
  :depends-on ("cl-lambdacalc"
               "lisp-unit") ;; MIT License
  :serial t
  :components ((:file "package-test")
               (:file "cl-lambdacalc-test")))
