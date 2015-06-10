(asdf:defsystem #:mobius-num
  :serial t
  :description "Some numerical algorithms"
  :author "Alexey Cherkaev <Alexey.Cherkaev@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:mobius-utils #:alexandria #:cl-num-utils #:lla)
  :components ((:module
                "src"
                :serial t
                :components ((:module
                              "base"
                              :serial t
                              :components ((:file "package")
                                           (:file "iterator")
                                           (:file "criteria")
                                           (:file "fixed-point")))
                             (:module
                              "linear"
                              :serial t
                              :components ((:file "package")
                                           (:file "linear-operations")
                                           (:file "linear-operations-impl")
                                           (:file "mvector")))
                             (:module
                              "nonlinear"
                              :serial t
                              :components ((:file "package")
                                           (:file "newton-raphson")
                                           (:file "diff")))))))
