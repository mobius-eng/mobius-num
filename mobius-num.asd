(asdf:defsystem #:mobius-num
  :serial t
  :description "Some numerical algorithms"
  :author "Alexey Cherkaev <Alexey.Cherkaev@gmail.com>"
  :license "LGPLv.3"
  :version "0.0.1"
  :depends-on (#:mobius-utils #:alexandria #:cl-num-utils #:lla)
  :components ((:module
                "src"
                :serial t
                :components ((:module
                              "base"
                              :serial t
                              :components ((:file "package")
                                           (:file "utils")
                                           (:file "iterator")
                                           (:file "criteria")
                                           (:file "fixed-point")))
                             (:module
                              "linear"
                              :serial t
                              :components ((:file "package")
                                           (:file "linear-operations")
                                           (:file "linear-operations-numbers")
                                           (:file "linear-operations-arrays")
                                           (:file "bicg-stab")
                                           (:file "mvector")))
                             (:module
                              "nonlinear"
                              :serial t
                              :components ((:file "package")
                                           (:file "newton-raphson")
                                           (:file "diff")
                                           (:file "fsolve")))
                             (:module
                              "ode"
                              :serial t
                              :components ((:file "package")
                                           (:file "ode")
                                           (:file "crank-nicolson")))
                             (:file "package")))))

(asdf:defsystem #:mobius-num-tests
  :serial t
  :description "Tests and example of the use of numerical algorithms from MOBIUS-NUM"
  :author "mobius-eng <Alexey.Cherkaev@gmail.com>"
  :licence "LGPL v.3"
  :version "0.0.1"
  :depends-on (#:mobius-utils #:mobius-num #:fiveam)
  :components ((:module
                "tests"
                :serial t
                :components ((:file "package")
                             (:file "base")
                             (:file "linear")))))
