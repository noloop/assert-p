(defpackage #:noloop.assert-p
  (:use #:common-lisp)
  (:nicknames #:assert-p)
  (:import-from #:assertion-error
                #:assertion-error
                #:get-stack-trace
                #:assertion-error-actual
                #:assertion-error-expected
                #:assertion-error-message
                #:assertion-error-result
                #:assertion-error-stack)
  (:export #:t-p
	   #:not-t-p
           #:zero-p
           #:not-zero-p
           #:nil-p
           #:not-nil-p
           #:null-p
           #:not-null-p
           #:eq-p
           #:not-eq-p
           #:eql-p
           #:not-eql-p
           #:equal-p
           #:not-equal-p
           #:equalp-p
           #:not-equalp-p
           #:typep-p
           #:not-typep-p
           #:values-p
           #:not-values-p
           #:custom-p))
