;;;; cl-lambdacalc.lisp

(in-package #:cl-lambdacalc)

(defmacro try-or-nil (body)
  `(handler-case
       ,body
     (error (e)
       (declare (ignore e))
       nil)))

;; interpreter

(defun close-paren-position (s)
  (if (or (eq 0 (length s))
          (not (eql #\( (char s 0))))
      nil
      (do ((i 1 (+ 1 i))
           (count 0 (let ((c (char s i)))
                      (cond
                        ((eql c #\( ) (1+ count))
                        ((eql c #\) ) (1- count))
                        (t count)))))
          ((and (eql (char s i) #\) )
                (eq count 0))
           i))))

(defun next-body-token (body)
  (let ((next (if (eq 0 (length body))
                  nil
                  (char body 0))))
    (cond
      ((null next) (values "" ""))
      ((not (eql #\( next)) (values next (subseq body 1)))
      (t (let ((i (close-paren-position body)))
           (values (subseq body 1 i)
                   (subseq body (1+ i))))))))

(defun unstructured-body-list (body)
  (multiple-value-bind (token rest) (next-body-token body)
    (cond
      ((characterp token)   (cons (read-from-string (string token))
                                  (unstructured-body-list rest)))
      ((and (stringp token) (eq 0 (length token))) nil)
      ((and (stringp token) (eql #\A (char token 0)))
       (cons (lc-func token) (unstructured-body-list rest)))
      ((stringp token) (cons (unstructured-body-list token)
                             (unstructured-body-list rest))))))

(defun parse-func-string (func-string)
  (labels ((body-list (body)
             (cond
               ((not (listp body)) body)
               ((eq 1 (length body)) (body-list (car body)))
               ((eq 2 (length body)) (cons 'funcall
                                           (map 'list #'body-list
                                                body)))
               (t (body-list (cons (list (car body) (cadr body))
                                   (cddr body)))))))
  (let ((split-string (split "\\.(?!A)" func-string :limit 2)))
    (append (map 'list (lambda (c) (read-from-string (string c)))
                 (coerce (regex-replace-all "(A|\\.)" (car split-string) "") 'list))
            (list (body-list (unstructured-body-list (cadr split-string))))))))

(defun lc-func (func-string)
  (let* ((token-list (parse-func-string func-string))
         (len (length token-list))
         (output '()))
    (do ((i (1- len) (1- i)))
        ((< i 0) (eval output))
      (if (eq i (1- len))
          (setf output (nth i token-list))
          (let ((symbol (nth i token-list)))
            (setf output `(lambda (,symbol)
                            (declare (ignorable ,symbol))
                            ,output)))))))

(defmacro define-lc-func (name func-string)
  `(setf (symbol-function ',name) (lc-func ,func-string)))

;; built-ins - boolean logic

(define-lc-func lc-true "Axy.x")

(define-lc-func lc-false "Axy.y")

(define-lc-func lc-if "Axyz.xyz")

(define-lc-func lc-and "Axy.(Axyz.xyz)xy(Axy.y)")

(define-lc-func lc-or "Axy.(Axyz.xyz)x(Axy.x)y")

(define-lc-func lc-not "Ax.(Axyz.xyz)x(Axy.y)(Axy.x)")

(defun lc-boolean-test (func bool)
  (try-or-nil (eql (if bool :a :b)
                   (funcall (funcall func :a) :b))))

(defun lc-true-p (func)
  (lc-boolean-test func t))

(defun lc-false-p (func)
  (lc-boolean-test func nil))

;; built-ins - church numerals

(define-lc-func lc-succ "Anfx.f(nfx)")

(define-lc-func lc-add "Amnfx.mf(nfx)")

(define-lc-func lc-mult "Axyz.x(yz)")

(define-lc-func lc-expt "Axy.yx")

(defun church-numeral-to-number (func)
  (let ((result (try-or-nil (funcall (funcall func #'1+) 0))))
    (and (numberp result) ;; result could be a function
         result)))

(defun number-to-church-numeral (num)
  (labels ((apply-succ-times (n numeral)
             (if (eq 0 n)
                 numeral
                 (apply-succ-times (1- n) (lc-succ numeral)))))
    (apply-succ-times num #'lc-false)))
