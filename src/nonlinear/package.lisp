(in-package cl-user)

(defpackage #:mobius.numeric.newton-raphson
  (:nicknames #:newton)
  (:use #:cl #:mobius.utils #:linop #:mobius.numeric.fixed-point #:criteria)
  (:export #:newton-method))

(defpackage #:mobius.numeric.diff
  (:use #:cl #:mv #:linop #:cl-slice)
  (:export #:*diff-step*
           #:generic-d
           #:deriv))

(defpackage #:mobius.numeric.fsolve
  (:use #:cl
        #:newton
        #:linop
        #:mobius.numeric.diff
        #:criteria)
  (:import-from #:cl-num-utils #:num=)
  (:export #:fsolve))
