(in-package cl-user)

(defpackage #:mobius.numeric.linear-base
  (:nicknames #:numeric-linear-base)
  (:use #:cl #:optima #:numeric-helpers)
  (:export #:vector-length-mismatch
           #:vector-length-mismatch-length1
           #:vector-length-mismatch-length2
           #:make-double-float-vector #:make-vector #:vec
           #:reduce-vector
           #:copy-vector-to! #:set-vector-to-zero!
           #:dot #:l2-norm #:l2-norm-diff #:square-vector
           #:vector-almost-zero-p
           #:add-with-multipliers!
           #:linear-combination!
           #:assign-linear-combination!
           #:vector-average!
           #:scale-vector!
           #:negate-vector!
           #:matrix-mul #:matrix-mul-gen
           #:matrix-mul->function
           #:solve-linear
           #:check-vector-lengths)
  (:documentation
   "Basic function on vectors"))

(defpackage #:mobius.numeric.bicg-stab
  (:nicknames #:numeric-bicg-stab)
  (:use #:cl #:optima
        #:numeric-linear-base #:numeric-control
        #:numeric-fixed-point #:numeric-helpers)
  (:import-from #:mobius.utils #:%)
  (:export 
           #:bicg-stab-value #:bicg-stab-vector-length
           #:*bicg-stab-tolerance* #:*bicg-stab-max-iter-coeff*
           #:bicg-stab
           #:bicg-stab-solve
           #:bicg-stab-solution #:bicg-stab-residual)
  (:documentation "BiCG Stabilized method of solving set of linear equations"))
