(in-package cl-user)

(defpackage #:mobius.numeric.tests.base
  (:nicknames #:num-tests-base)
  (:use #:cl #:mobius.numeric #:mobius.numeric.fixed-point #:it.bese.fiveam)
  (:import-from #:mobius.utils #:average #:%))


(defpackage #:mobius.numeric.tests.linear
  (:nicknames #:num-tests-linear)
  (:use #:cl #:mobius.numeric #:it.bese.fiveam)
  (:import-from #:mobius.utils #:%))

(defpackage #:mobius.numeric.tests.nonlinear
  (:nicknames #:num-tests-nonlinear)
  (:use #:cl #:mobius.numeric #:it.bese.fiveam)
  (:import-from #:mobius.utils #:%))
