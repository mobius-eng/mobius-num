(in-package cl-user)

(defpackage #:mobius.numeric.tests.base
  (:nicknames #:num-tests-base)
  (:use #:cl
        #:numeric-constants
        #:criteria
        #:fixed-point
        #:mobius.numeric.fixed-point
        #:it.bese.fiveam)
  (:import-from #:mobius.utils #:average #:%)
  (:import-from #:cl-num-utils #:num=))


(defpackage #:mobius.numeric.tests.linear
  (:nicknames #:num-tests-linear)
  (:use #:cl
        #:numeric-constants
        #:numeric-linear
        #:criteria
        #:it.bese.fiveam)
  (:import-from #:mobius.utils #:%)
  (:import-from #:cl-num-utils #:num=))

;; (defpackage #:mobius.numeric.tests.nonlinear
;;   (:nicknames #:num-tests-nonlinear)
;;   (:use #:cl #:mobius.numeric #:it.bese.fiveam)
;;   (:import-from #:mobius.utils #:%))
