(defpackage #:noloop.assert-p-test
  (:use #:common-lisp
        #:simplet
        #:assert-p)
  (:nicknames #:assert-p-test)
  (:import-from #:assert-p
                #:assertion-error-check))
(in-package #:noloop.assert-p-test)

(defun test-t-p ()
  (let ((actual t))
    (assertion-error-check
     (eq (t-p actual) nil))))

(defun test-not-t-p ()
  (let ((actual nil))
    (assertion-error-check
     (eq (not-t-p actual) nil))))

(defun test-zero-p ()
  (let ((actual 0))
    (assertion-error-check
     (eq (zero-p actual) nil))))

(defun test-not-zero-p ()
  (let ((actual nil))
    (assertion-error-check
     (eq (not-zero-p actual) nil))))

(defun test-nil-p ()
  (let ((actual nil))
    (assertion-error-check
     (eq (nil-p actual) nil))))

(defun test-not-nil-p ()
  (let ((actual t))
    (assertion-error-check
     (eq (not-nil-p actual) nil))))

(defun test-null-p ()
  (let ((actual nil))
    (assertion-error-check
     (eq (null-p actual) nil))))

(defun test-not-null-p ()
  (let ((actual t))
    (assertion-error-check
     (eq (not-null-p actual) nil))))

(defun test-eq-p ()
  (let ((actual t)
        (expected t))
    (assertion-error-check
     (eq (eq-p actual expected) nil))))

(defun test-not-eq-p ()
  (let ((actual 3)
        (expected t))
    (assertion-error-check
     (eq (not-eq-p actual expected) nil))))

(defun test-eql-p ()
  (let ((actual t)
        (expected t))
    (assertion-error-check
     (eq (eql-p actual expected) nil))))

(defun test-not-eql-p ()
  (let ((actual 3)
        (expected t))
    (assertion-error-check
     (eq (not-eql-p actual expected) nil))))

(defun test-equal-p ()
  (let ((actual t)
        (expected t))
    (assertion-error-check
     (eq (equal-p actual expected) nil))))

(defun test-not-equal-p ()
  (let ((actual 3)
        (expected t))
    (assertion-error-check
     (eq (not-equal-p actual expected) nil))))

(defun test-equalp-p ()
  (let ((actual t)
        (expected t))
    (assertion-error-check
     (eq (equalp-p actual expected) nil))))

(defun test-not-equalp-p ()
  (let ((actual 3)
        (expected t))
    (assertion-error-check
     (eq (not-equalp-p actual expected) nil))))

(defun test-typep-p ()
  (let ((actual 1)
        (expected 'integer))
    (assertion-error-check
     (eq (typep-p actual expected) nil))))

(defun test-not-typep-p ()
  (let ((actual "stg")
        (expected 'integer))
    (assertion-error-check
     (eq (not-typep-p actual expected) nil))))

(defun test-values-p ()
  (let ((actual '("i" "i" "i"))
        (expected "i"))
    (assertion-error-check
     (eq (values-p 'string= actual expected) nil))))

(defun test-not-values-p ()
  (let ((actual '(1 1 1))
        (expected 2))
    (assertion-error-check
     (eq (not-values-p '= actual expected) nil))))

(defun test-error-p ()
  (assertion-error-check
   (eq (error-p #'(lambda () (error "Something"))) nil)))

(defun test-not-error-p ()
  (assertion-error-check
   (eq (not-error-p #'(lambda () ())) nil)))

(defun test-condition-error-p ()
  (assertion-error-check
   (eq nil (condition-error-p
            #'(lambda () (error 'division-by-zero))
             division-by-zero))))

(defun test-not-condition-error-p ()
  (assertion-error-check
   (eq nil (not-condition-error-p
            #'(lambda () (error "Something"))
            division-by-zero))))

(defun test-custom-p ()
  (let ((actual '(1 2 3))
        (expected '(1 2)))
    (assertion-error-check
     (eq nil (custom-p (every #'= actual expected)
                   actual
                   expected
                   "every =")))))

(suite "Suite assert-p"
       (test "Test t-p" #'test-t-p)
       (test "Test not-t-p" #'test-not-t-p)
       (test "Test zero-p" #'test-zero-p)
       (test "Test not-zero-p" #'test-not-zero-p)
       (test "Test nil-p" #'test-nil-p)
       (test "Test not-nil-p" #'test-not-nil-p)
       (test "Test null-p" #'test-null-p)
       (test "Test not-null-p" #'test-not-null-p)
       (test "Test eq-p" #'test-eq-p)
       (test "Test not-eq-p" #'test-not-eq-p)
       (test "Test eql-p" #'test-eql-p)
       (test "Test not-eql-p" #'test-not-eql-p)
       (test "Test equal-p" #'test-equal-p)
       (test "Test not-equal-p" #'test-not-equal-p)
       (test "Test equalp-p" #'test-equalp-p)
       (test "Test not-equalp-p" #'test-not-equalp-p)
       (test "Test typep-p" #'test-typep-p)
       (test "Test not-typep-p" #'test-not-typep-p)
       (test "Test values-p" #'test-values-p)
       (test "Test not-values-p" #'test-not-values-p)
       (test "Test error-p" #'test-error-p)
       (test "Test not-error-p" #'test-not-error-p)
       (test "Test error-p" #'test-error-p)
       (test "Test condition-error-p" #'test-condition-error-p)
       (test "Test not-condition-error-p" #'test-not-condition-error-p)
       (test "Test custom-p" #'test-custom-p))
