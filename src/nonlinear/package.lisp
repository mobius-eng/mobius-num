(in-package cl-user)

(defpackage #:mobius.numeric.nonlinear
  (:nicknames #:numeric-nonlinear)
  (:use #:cl #:optima #:numeric-linear-base)
  (:export #:nonlinear-value
           #:nonlinear-value-x #:nonlinear-value-f
           #:nonlinear-value-residual
           #:nonlinear-value-square-residual
           #:init-nonlinear-value
           #:fsolve #:solve-nonlinear)
  (:documentation
   "Generic solver of nonlinear equations"))

(defpackage #:mobius.numeric.linsearch
  (:nicknames #:numeric-linsearch)
  (:use #:cl #:numeric-helpers #:numeric-control #:numeric-fixed-point #:optima)
  (:export #:linsearch-value
           #:linsearch-value-lambda1
           #:linsearch-value-g1
           #:make-linsearch-value
           #:newton-step-gamma
           #:*linsearch-alpha*
           #:*linsearch-abs-lambda-min*
           #:*linsearch-max-iterations*
           #:linsearch-control
           #:make-linsearch-control
           #:linsearch
           #:make-linsearch
           #:linsearch-step)
  (:documentation
   "Line search algorithm to minimize function along a perdifined line"))

(defpackage #:mobius.numeric.newton-raphson
  (:nicknames #:numeric-newton)
  (:use #:cl #:optima #:mobius.utils #:numeric-linear-base
        #:numeric-bicg-stab
        #:numeric-fixed-point #:numeric-control #:numeric-helpers
        #:numeric-linsearch
        #:numeric-nonlinear)
  (:export #:newton-value
           #:newton-solution
           #:newton-residual
           #:make-newton-value
           #:newton #:make-newton
           #:linear-solver-failed #:linsearch-failed
           #:*newton-tolerance* #:*newton-max-iterations*
           #:make-newton-step
           #:newton-solve))
#|
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
|#
