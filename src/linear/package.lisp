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
           #:scale-vector!
           #:negate-vector!
           #:matrix-mul #:matrix-mul-gen
           #:matrix-mul->function
           #:solve-linear)
  (:documentation
   "Basic function on vectors"))

(defpackage #:mobius.numeric.bicg-stab
  (:nicknames #:numeric-bicg-stab)
  (:use #:cl #:optima
        #:numeric-linear-base #:numeric-control
        #:numeric-fixed-point #:numeric-helpers)
  (:export 
           #:rho-is-zero #:r0*v-is-zero
           #:bicg-stab-value #:bicg-stab-vector-length
           #:bicg-stab-control
           #:*bicg-stab-tolerance* #:*bicg-stab-max-iterations*
           #:bicg-stab
           #:make-bicg-stab-step
           #:bicg-stab-solve
           #:bicg-stab-solution #:bicg-stab-residual)
  (:documentation "BiCG Stabilized method of solving set of linear equations"))
