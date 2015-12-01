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
                                           (:file "iterator")
                                           (:file "control")
                                           (:file "fixed-point")))
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
                                           (:file "bicg-stab2")
                                           ;; (:file "gsl-linear")
                                           ;; (:file "lingenerics")
                                           ;; (:file "impl-numbers")
                                           ;; (:file "impl-vectors")
                                           ;; (:file "impl-arrays")
                                           ;; (:file "linfunctions")
                                           ;; (:file "bicg-stab")
                                           ;; (:file "mvector")
                                           ))
                             (:module
                              "nonlinear"
                              :serial t
                              :components ((:file "package")
                                           (:file "linsearch")
                                           (:file "newton")
                                           ;; (:file "diff")
                                           ;; (:file "fsolve")
                                           ))
                             ;; (:module
                             ;;  "ode"
                             ;;  :serial t
                             ;;  :components ((:file "package")
                             ;;               (:file "ode")
                             ;;               (:file "crank-nicolson")))
                             ;;  (:file "package")
                             )
                ))
  )

;; (asdf:defsystem #:mobius-num-tests
;;   :serial t
;;   :description "Tests and example of the use of numerical algorithms from MOBIUS-NUM"
;;   :author "mobius-eng <Alexey.Cherkaev@gmail.com>"
;;   :licence "LGPL v.3"
;;   :version "0.0.1"
;;   :depends-on (#:mobius-utils #:mobius-num #:fiveam)
;;   :components ((:module
;;                 "tests"
;;                 :serial t
;;                 :components ((:file "package")
;;                              (:file "base")
;;                              (:file "linear")))))
