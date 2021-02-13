;;;; cl-lambdacalc.asd

(asdf:defsystem #:cl-lambdacalc
  :author "S M McLeod"
  :license  "MIT"
  :version "0.0.1"
  :depends-on ("cl-ppcre") ;; BSD License
  :serial t
  :components ((:file "package")
               (:file "cl-lambdacalc")))
