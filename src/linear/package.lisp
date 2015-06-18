(in-package cl-user)

(defpackage #:mobius.numeric.linear-operations
  (:nicknames #:linop)
  (:use #:cl #:mobius.utils #:mobius.numeric.utils)
  (:import-from #:alexandria #:with-gensyms)
  (:import-from #:cl-num-utils #:transpose)
  (:export #:zero-vector #:zero-vector!
           #:make-vector
           #:vector-dim
           #:map-vector #:map-vector!
           #:mapi-vector #:mapi-vector!
           #:reduce-vector
           #:dot
           #:outer-product
           #:m*
           #:m/
           #:transpose
           #:duplicate-vector
           #:negate-vector #:negate-vector!
           #:*norm-type*
           #:norm
           #:e+ #:e=+! #:e+!
           #:e- #:e=-! #:e-!
           #:e* #:e=*! #:e*!
           #:e/ #:e=/! #:e/!
           #:e-rev/ #:e=-rev/! #:e-rev/!
           #:inner-product
           #:identity-matrix
           #:eabs #:esin #:ecos #:etan #:esignum #:eexp
           #:eabs! #:esin! #:ecos! #:etan! #:esignum! :eexp!
           #:define-vector-function)
  (:documentation "Definitions of generic linear operations"))

(defpackage #:mobius.numeric.linear-operations-numbers
  (:use #:cl #:mobius.utils #:linop)
  (:import-from #:alexandria #:with-gensyms)
  (:documentation "Implementation of linear oprations for numbers"))


(defpackage #:mobius.numeric.linear-operations-arrays
  (:use #:cl #:mobius.utils #:linop)
  (:import-from #:alexandria #:with-gensyms)
  (:import-from #:cl-num-utils #:transpose)
  (:documentation "Implementations of linear operations for numbers and arrays"))

(defpackage #:mobius.numeric.bicg-stab
  (:nicknames #:bicgstab)
  (:use #:cl #:linop #:mobius.numeric.fixed-point)
  (:import-from #:alexandria #:with-gensyms)
  (:export #:*bicgstab-tolerance*
           #:*bicgstab-max-iterations*
           #:bicgstab)
  (:documentation "BiCGStab linear solver for a generalised linear operator"))

(defpackage #:mobius.numeric.mvector
  (:nicknames #:mv)
  (:use #:cl #:mobius.utils
        #:linop
        #:bicgstab)
  (:import-from #:alexandria #:with-gensyms)
  (:import-from #:cl-num-utils #:num= #:*num=-tolerance*)
  (:import-from #:lla #:mm)
  (:export #:mvector #:up #:down
           #:mvector-index-type #:mvector-datum
           #:array->mvector #:array->up #:array->down
           #:const-mvector  #:const-up  #:const-down
           #:zero-mvector   #:zero-up   #:zero-down
           #:build-mvector  #:build-up  #:build-down
           #:delta-mvector  #:delta-up  #:delta-down
           #:copy-mvector
           #:mvref
           #:mvector?
           #:column? #:up?
           #:row?    #:down?
           #:mvlength
           #:comaptible?
           #:incompatible-mvector-size
           #:throw-incompatible-mvector-size
           #:down-index #:down-index!
           #:up-index   #:up-index!)
  (:documentation "M(athematical) VECTOR implementation"))

