(in-package cl-user)

(defpackage #:mobius.numeric.linear-base
  (:nicknames #:linear-base)
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
           #:solve-linear))

(defpackage #:mobius.numeric.bicg-stab
  (:nicknames #:bicg-stab)
  (:use #:cl #:optima #:mobius.numeric.linear-base #:control #:fixed-point #:numeric-helpers)
  (:export 
           #:rho-is-zero #:r0*v-is-zero
           #:bicg-stab-value #:make-bicg-stab-value #:bicg-stab-vector-length
           #:bicg-stab-control #:make-bicg-stab-control
           #:*bicg-stab-tolerance* #:*bicg-stab-max-iterations*
           #:make-control-bicg-stab
           #:bicg-stab #:make-bicg-stab
           #:bicg-stab-control #:bicg-stab-value
           #:make-bicg-stab-step
           #:bicg-stab-solve
           #:bicg-stab-solution #:bicg-stab-residual))


;; (defpackage #:mobius.numeric.linear
;;   (:nicknames #:numeric-linear)
;;   (:use #:cl #:gsl #:numeric-constants #:criteria)
;;   (:import-from #:cl-num-utils #:num=)
;;   (:export #:zero-vector
;;            #:copy-vector
;;            #:make-vector
;;            #:list->vector
;;            #:vector-of
;;            #:dfvec
;;            #:sb64vec
;;            #:negate-vector
;;            #:vector-dim
;;            #:dot
;;            #:squared-l2-norm
;;            #:norm
;;            #:*norm-type*
;;            #:reduce-vector
;;            #:.abs  #:.sin  #:.cos  #:.tan  #:.exp  #:.log  #:.signum
;;            #:.abs! #:.sin! #:.cos! #:.tan! #:.exp! #:.log! #:.signum!
;;            #:elementwise!
;;            #:.=+! #:.=-! #:.=*! #:.=/!
;;            #:.+   #:.-   #:.*   #:./
;;            #:.+!  #:.-!  #:.*!  #:./!
;;            #:list->matrix
;;            #:make-matrix
;;            #:matrix-of
;;            #:identity-matrix
;;            #:m*
;;            #:m/))

