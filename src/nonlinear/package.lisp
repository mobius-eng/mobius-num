(in-package cl-user)

(defpackage #:mobius.numeric.linsearch
  (:nicknames #:numeric-linsearch)
  (:use #:cl #:numeric-helpers #:control #:fixed-point #:optima)
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
           #:linsearch-step))


(defpackage #:mobius.numeric.newton-raphson
  (:nicknames #:newton)
  (:use #:cl #:mobius.utils #:linear-base #:bicg-stab
        #:fixed-point #:control #:numeric-helpers
        #:numeric-linsearch)
  (:shadowing-import-from #:ad
                          #:+ #:- #:* #:/
                          #:sin #:cos #:tan #:asin #:acos #:atan
                          #:exp #:log #:expt #:sqrt
                          #:sinh #:cosh #:tanh
                          #:= #:< #:> #:<= #:>=
                          #:zerop #:plusp #:minusp
                          #:literal-function
                          #:literal-vector
                          #:D #:diff #:gradient-f #:jacobian*vector #:jacobian*vector-save
                          #:directional-derivative-f
                          #:partial
                          #:comp)
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
