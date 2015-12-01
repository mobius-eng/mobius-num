(asdf:defsystem #:mobius-num-tests
  :serial t
  :description "Test for MOBIUS-NUM system"
  :author "Alexey Cherkaev <Alexey.Cherkaev@gmail.com>"
  :license "LGPLv.3"
  :version "0.0.1"
  :depends-on (#:mobius-utils #:alexandria #:optima #:fiveam #:mobius-num)
  :components ((:module
                "tests"
                :serial t
                :components ((:file "package")
                             (:file "ad-tests")
                             (:file "base-tests")
                             (:file "linear-tests")))))
