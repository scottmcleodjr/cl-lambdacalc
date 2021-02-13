;;;; cl-lambdacalc.lisp

(in-package #:cl-lambdacalc)

(define-test close-paren-position-should-return-nil-if-first-character-is-not-open-paren
  (assert-nil (close-paren-position ""))
  (assert-nil (close-paren-position "xy")))

(define-test close-paren-position-should-return-position-of-close-paren-matching-first-character
  (assert-eq 3 (close-paren-position "(xy)"))
  (assert-eq 8 (close-paren-position "(x(x(x)))y"))
  (assert-eq 3 (close-paren-position "(xy)(xy)")))

(define-test next-body-token-should-return-next-token-and-rest-of-body-string
  (multiple-value-bind (token rest) (next-body-token "a(bc)")
    (assert-eq #\a token)
    (assert-equal "(bc)" rest))
  (multiple-value-bind (token rest) (next-body-token "(Ax.x)yz")
    (assert-equal "Ax.x" token)
    (assert-equal "yz" rest)))

(define-test next-body-token-should-return-two-empty-string-values-if-arg-is-empty-string
  (multiple-value-bind (token rest) (next-body-token "")
    (assert-equal "" token)
    (assert-equal "" rest)))

(define-test unstructured-body-list-should-correctly-build-list-of-body-tokens
  (assert-equal '(x y z) (unstructured-body-list "xyz"))
  (assert-equal '(x (y z)) (unstructured-body-list "x(yz)"))
  (let ((ret (unstructured-body-list "(Ax.x)(yz)")))
    (assert-true (functionp (car ret)))
    (assert-equal '(y z) (cadr ret))))

(define-test parse-func-string-should-correctly-build-structured-list-of-tokens
  (assert-equal '(x y z (funcall (funcall x y) z)) (parse-func-string "Ax.Ay.Az.xyz"))
  (assert-equal '(x y z (funcall x (funcall y z))) (parse-func-string "Ax.Ay.Az.x(yz)"))
  (let ((ret (parse-func-string "Ax.Ay.Az.(Ax.x)(yz)")))
    (assert-true (functionp (cadr (nth 3 ret))))
    (setf (cadr (nth 3 ret)) :func)
    (assert-equal '(x y z (funcall :func (funcall y z))) ret)))

(define-test parse-func-string-should-support-both-single-and-sugared-arg-formats
  (assert-equal '(x y z (funcall (funcall x y) z)) (parse-func-string "Axyz.xyz"))
  (assert-equal '(x y z (funcall (funcall x y) z)) (parse-func-string "Ax.Ay.Az.xyz")))

(define-test lc-func-functions-should-return-expected-results-when-run-with-symbols
  (assert-eql :a (funcall (lc-func "Ax.x") :a))
  (assert-eql :b (funcall (funcall (lc-func "Ax.Ay.y") :a) :b))
  (assert-eql :a (funcall (funcall (lc-func "Af.Ax.f(fx)") (lc-func "Ax.x")) :a))
  (assert-eql :a (funcall (lc-func "Ax.(Ax.x)(Ay.y)x") :a)))

(define-test lc-true-should-return-first-argument
  (assert-eql :a (funcall (lc-true :a) :b)))

(define-test lc-false-should-return-second-argument
  (assert-eql :b (funcall (lc-false :a) :b)))

(define-test lc-if-should-return-second-argument-when-first-true-else-third
  (assert-eql :a (funcall (funcall (lc-if #'lc-true) :a) :b))
  (assert-eql :b (funcall (funcall (lc-if #'lc-false) :a) :b)))

(define-test lc-and-should-return-expected-value-per-truth-table
  (assert-true (lc-true-p (funcall (lc-and #'lc-true) #'lc-true)))
  (assert-nil (lc-true-p (funcall (lc-and #'lc-true) #'lc-false)))
  (assert-nil (lc-true-p (funcall (lc-and #'lc-false) #'lc-true)))
  (assert-nil (lc-true-p (funcall (lc-and #'lc-false) #'lc-false))))

(define-test lc-or-should-return-expected-value-per-truth-table
  (assert-true (lc-true-p (funcall (lc-or #'lc-true) #'lc-true)))
  (assert-true (lc-true-p (funcall (lc-or #'lc-true) #'lc-false)))
  (assert-true (lc-true-p (funcall (lc-or #'lc-false) #'lc-true)))
  (assert-nil (lc-true-p (funcall (lc-or #'lc-false) #'lc-false))))

(define-test lc-not-should-return-opposite-boolean
  (assert-true (lc-false-p (lc-not #'lc-true)))
  (assert-true (lc-true-p (lc-not #'lc-false))))

(define-test lc-true-p-should-return-t-if-arg-is-lc-true-function
  (assert-true (lc-true-p (lc-func "Ax.Ay.x"))))

(define-test lc-true-p-should-return-nil-if-arg-is-not-lc-true-function
  (assert-nil (lc-true-p (lc-func "Ax.x")))
  (assert-nil (lc-true-p (lc-func "Ax.Ay.y"))))

(define-test lc-false-p-should-return-t-if-arg-is-lc-false-function
  (assert-true (lc-false-p (lc-func "Ax.Ay.y"))))

(define-test lc-false-p-should-return-nil-if-arg-is-not-lc-false-function
  (assert-nil (lc-false-p (lc-func "Ax.x")))
  (assert-nil (lc-false-p (lc-func "Ax.Ay.x"))))

(define-test lc-succ-should-return-successor-church-numeral
  (assert-eq 1 (church-numeral-to-number (lc-succ (lc-func "Af.Ax.x"))))
  (assert-eq 2 (church-numeral-to-number (lc-succ (lc-func "Af.Ax.fx"))))
  (assert-eq 3 (church-numeral-to-number (lc-succ (lc-func "Af.Ax.f(fx)")))))

(define-test lc-add-should-return-sum-church-numeral
  (assert-eq 0 (church-numeral-to-number (funcall (lc-add (lc-func "Af.Ax.x"))
                                                  (lc-func "Af.Ax.x"))))
  (assert-eq 3 (church-numeral-to-number (funcall (lc-add (lc-func "Af.Ax.fx"))
                                                  (lc-func "Af.Ax.f(fx)"))))
  (assert-eq 8 (church-numeral-to-number (funcall (lc-add (lc-func "Af.Ax.f(f(f(fx)))"))
                                                  (lc-func "Af.Ax.f(f(f(fx)))")))))

(define-test lc-mult-should-return-product-church-numeral
  (assert-eq 0 (church-numeral-to-number (funcall (lc-mult (lc-func "Af.Ax.x"))
                                                  (lc-func "Af.Ax.fx"))))
  (assert-eq 6 (church-numeral-to-number (funcall (lc-mult (lc-func "Af.Ax.f(fx)"))
                                                  (lc-func "Af.Ax.f(f(fx))"))))
  (assert-eq 20 (church-numeral-to-number (funcall (lc-mult (lc-func "Af.Ax.f(f(f(f(fx))))"))
                                                   (lc-func "Af.Ax.f(f(f(fx)))")))))

(define-test lc-expt-should-return-power-church-numeral
  (assert-eq 16 (church-numeral-to-number (funcall (lc-expt (lc-func "Af.Ax.f(f(f(fx)))"))
                                                   (lc-func "Af.Ax.f(fx)"))))
  (assert-eq 27 (church-numeral-to-number (funcall (lc-expt (lc-func "Af.Ax.f(f(fx))"))
                                                   (lc-func "Af.Ax.f(f(fx))")))))

(define-test church-numeral-to-number-should-correctly-parse-church-numeral-to-number
  (assert-eq 0 (church-numeral-to-number (lc-func "Af.Ax.x")))
  (assert-eq 1 (church-numeral-to-number (lc-func "Af.Ax.fx")))
  (assert-eq 2 (church-numeral-to-number (lc-func "Af.Ax.f(fx)"))))

(define-test church-numeral-to-number-should-return-nil-if-function-is-not-church-numeral
  (assert-nil (church-numeral-to-number (lc-func "Ax.xx")))
  (assert-nil (church-numeral-to-number (lc-func "Af.Ax.(ff)x")))
  (assert-nil (church-numeral-to-number (lc-func "Ax.Ay.Az.xyz"))))

(define-test number-to-church-numeral-should-correctly-convert-number-to-church-numeral
  (assert-eq 0 (church-numeral-to-number (number-to-church-numeral 0)))
  (assert-eq 1 (church-numeral-to-number (number-to-church-numeral 1)))
  (assert-eq 2 (church-numeral-to-number (number-to-church-numeral 2))))
