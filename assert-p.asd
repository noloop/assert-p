;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defsystem :assert-p
  :author "noloop <noloop@zoho.com>"
  :maintainer "noloop <noloop@zoho.com>"
  :license "GPLv3"
  :version "1.0.0"
  :homepage "https://github.com/noloop/assert-p"
  :bug-tracker "https://github.com/noloop/assert-p/issues"
  :source-control (:git "git@github.com:noloop/assert-p.git")
  :description "A library of assertions written in Common Lisp."
  :depends-on (:assertion-error)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "macros" :depends-on ("package"))
                 (:file "assert-p" :depends-on ("macros")))))
  :in-order-to ((test-op (test-op "assert-p/test"))))

(defsystem :assert-p/test
  :author "noloop <noloop@zoho.com>"
  :maintainer "noloop <noloop@zoho.com>"
  :license "GPLv3"
  :description "assert-p Test."
  :depends-on (:assert-p :simplet)
  :defsystem-depends-on (:simplet-asdf)
  :components ((:module "test"
                :components
                ((:test-file "assert-p-test"))))
  :perform (test-op (op c) (symbol-call :simplet '#:run)))

