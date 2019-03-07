(in-package #:noloop.assert-p)

(defun t-p (actual)
  "Check actual eq t"
  (assertion (eq t actual) actual t 'eq))

(defun not-t-p (actual)
  "Check actual not eq t"
  (assertion (not (eq t actual)) actual t 'not-eq))

(defun zero-p (actual)
  "Check actual eq 0"
  (assertion (eq 0 actual) actual 0 'eq))

(defun not-zero-p (actual)
  "Check actual not eq 0"
  (assertion (not (eq 0 actual)) actual 0 'not-eq))

(defun nil-p (actual)
  "Check actual eq nil"
  (assertion (eq nil actual) actual nil 'eq))

(defun not-nil-p (actual)
  "Check actual not eq nil"
  (assertion (not (eq nil actual)) actual nil 'not-eq))

(defun null-p (actual)
  "Check null actual "
  (assertion (null actual) actual nil 'null))

(defun not-null-p (actual)
  "Check not null actual "
  (assertion (not (null actual)) actual nil 'not-null))

(defun eq-p (actual expected)
  "Check actual eq expected"
  (assertion (eq actual expected) actual expected 'eq))

(defun not-eq-p (actual expected)
  "Check actual not eq expected"
  (assertion (not (eq actual expected)) actual expected 'not-eq))

(defun eql-p (actual expected)
  "Check actual eql expected"
  (assertion (eql actual expected) actual expected 'eql))

(defun not-eql-p (actual expected)
  "Check actual not eql expected"
  (assertion (not (eql actual expected)) actual expected 'not-eql))

(defun equal-p (actual expected)
  "Check actual equal expected"
  (assertion (equal actual expected) actual expected 'equal))

(defun not-equal-p (actual expected)
  "Check actual not equal expected"
  (assertion (not (equal actual expected)) actual expected 'not-equal))

(defun equalp-p (actual expected)
  "Check actual equalp expected"
  (assertion (equalp actual expected) actual expected 'equalp))

(defun not-equalp-p (actual expected)
  "Check actual not equalp expected"
  (assertion (not (equalp actual expected)) actual expected 'not-equalp))

(defun typep-p (actual expected)
  "Check actual typep expected"
  (assertion (typep actual expected) actual expected 'typep))

(defun not-typep-p (actual expected)
  "Check actual not typep expected"
  (assertion (not (typep actual expected)) actual expected 'not-typep))

(defun values-p (predicate actual expected)
  "Check actual every predicate expected. Predicate is a symbol of function, actual is a list, and expected an atom. Example:
(values-p '= '(t t t) t) It's similar to (every #'(lambda (i) (= t i)) '(t t t))"
  (assertion-values
   'every
   #'(lambda (i)
       (funcall (symbol-function predicate) expected i))
   actual
   expected
   (concatenate 'string "every" (string predicate))))

(defun not-values-p (predicate actual expected)
  "Check actual every not predicate expected. Predicate is a symbol of function, actual is a list, and expected an atom. Example:
(values-p '= '(t t t) t) It's similar to (every #'(lambda (i) (not (= t i))) '(t t t))"
  (assertion-values
   'every
   #'(lambda (i)
       (not (funcall (symbol-function predicate) expected i)))
   actual
   expected
   (concatenate 'string "every" (string predicate))))

(defun error-p (form)
  "Check if form throw an error."
  (assertion (error-check form) form 'error 'catch-error))

(defun not-error-p (form)
  "Check if form not throw an error."
  (assertion (not (error-check form)) form 'error 'catch-error))

(defmacro condition-error-p (form condition)
  "Check if form throw an specified condition error."
  `(assertion
    (condition-error-check ,form ,condition)
    'form
    'error
    'catch-error))

(defmacro not-condition-error-p (form condition)
  "Check if form not throw an specified condition error."
  `(assertion
    (not (condition-error-check ,form ,condition))
    'form
    'error
    'catch-error))

(defun custom-p (test-result actual expected operator)
  "Custom check for special cases. Where test-result is a form that returns t or nil, actual is value actual used in test-result, expected is value expected used in test-result, operator is operator used in test-result. Return t when test-result is t, and throw assertion-error when test-result is nil."
  (assertion test-result actual expected operator))

(defun assertion (result actual expected operator)
  "When result is t return t. When result is nil throw assertion-error. One assertion need result(t or nil), one object actual, one object expected, and one operator(string or symbol). Assertion-error also loading one message of error(built with actual, expected and operator) and the stack trace of error throw moment."
  (if result
      t
      (error 'assertion-error
             :assertion-error-message (concatenate 'string (write-to-string actual) " " (write-to-string operator) " " (write-to-string expected))
             :assertion-error-result result
             :assertion-error-actual actual
             :assertion-error-expected expected
             :assertion-error-stack (get-stack-trace))))

