(in-package #:noloop.assert-p)

(defun t-p (actual)
  "Check actual eq t"
  (assertion (eq t actual) actual t 'eq))

(defun not-t-p (actual)
  "Check actual not eq t"
  (assertion (not (eq t actual)) actual t 'not-eq))

(defun zero-p (actual)
  (assertion (eq 0 actual) actual 0 'eq))

(defun not-zero-p (actual)
  (assertion (not (eq 0 actual)) actual 0 'not-eq))

(defun nil-p (actual)
  (assertion (eq nil actual) actual nil 'eq))

(defun not-nil-p (actual)
  (assertion (not (eq nil actual)) actual nil 'not-eq))

(defun null-p (actual)
  (assertion (null actual) actual nil 'null))

(defun not-null-p (actual)
  (assertion (not (null actual)) actual nil 'not-null))

(defun eq-p (actual expected)
  (assertion (eq actual expected) actual expected 'eq))

(defun not-eq-p (actual expected)
  (assertion (not (eq actual expected)) actual expected 'not-eq))

(defun eql-p (actual expected)
  (assertion (eql actual expected) actual expected 'eql))

(defun not-eql-p (actual expected)
  (assertion (not (eql actual expected)) actual expected 'not-eql))

(defun equal-p (actual expected)
  (assertion (equal actual expected) actual expected 'equal))

(defun not-equal-p (actual expected)
  (assertion (not (equal actual expected)) actual expected 'not-equal))

(defun equalp-p (actual expected)
  (assertion (equalp actual expected) actual expected 'equalp))

(defun not-equalp-p (actual expected)
  (assertion (not (equalp actual expected)) actual expected 'not-equalp))

(defun typep-p (actual expected)
  (assertion (typep actual expected) actual expected 'typep))

(defun not-typep-p (actual expected)
  (assertion (not (typep actual expected)) actual expected 'not-typep))


(defun values-p (predicate actual expected)
  (assertion-values
   'every
   #'(lambda (i)
       (funcall (symbol-function predicate) expected i))
   actual
   expected
   (concatenate 'string "every" (string predicate))))

(defun not-values-p (predicate actual expected)
  (assertion-values
   'every
   #'(lambda (i)
       (not (funcall (symbol-function predicate) expected i)))
   actual
   expected
   (concatenate 'string "every" (string predicate))))

(defun custom-p (test-result actual expected operator)
  (assertion test-result actual expected operator))

(defun assertion (result actual expected operator)
  (unless result
    (error 'assertion-error
           :assertion-error-message (concatenate 'string (write-to-string actual) " " (write-to-string operator) " " (write-to-string expected))
           :assertion-error-result result
           :assertion-error-actual actual
           :assertion-error-expected expected
           :assertion-error-stack (get-stack-trace))))
