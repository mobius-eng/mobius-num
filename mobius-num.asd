(asdf:defsystem #:mobius-num
  :serial t
  :description "Some numerical algorithms"
  :author "Alexey Cherkaev <Alexey.Cherkaev@gmail.com>"
  :license "LGPLv.3"
  :version "0.0.1"
  :depends-on (#:mobius-utils #:alexandria #:optima)
  :components ((:module
                "src"
                :serial t
                :components ((:module
                              "base"
                              :serial t
                              :components ((:file "package")
                                           (:file "constants")
                                           (:file "helpers")
                                           (:file "iterator2")
                                           (:file "control2")
                                           (:file "fixed-point2")))
                             (:module
                              "ad"
                              :serial t
                              :components ((:file "package")
                                           (:file "symbolic")
                                           (:file "ad")))
                             (:module
                              "linear"
                              :serial t
                              :components ((:file "package")
                                           (:file "base")
                                           (:file "bicg-stab2")))
                             (:module
                              "nonlinear"
                              :serial t
                              :components ((:file "package")
                                           (:file "nonlinear")
                                           (:file "linsearch")
                                           (:file "newton")))
                             (:module
                              "ode"
                              :serial t
                              :components ((:file "package")
                                           (:file "ode")
                                           (:file "runge-kutta")))
                             ;;  (:file "package")
                             )
                )))
