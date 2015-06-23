(in-package cl-user)

;; TODO: maybe should add cl-slice?
(defpackage #:mobius.numeric
  (:use #:cl
        #:mobius.numeric.utils
        #:criteria
        #:linop
        #:bicgstab
        #:mobius.numeric.fsolve
        #:mv
        #:ode
        #:mobius.numeric.crank-nicolson)
  (:import-from #:cl-num-utils #:num=  #:*num=-tolerance*)
  (:import-from #:mobius.numeric.diff #:deriv)
  (:export
   ;; CL-NUM-UTILS
   #:num=
   #:*num=-tolerance*
   #:transpose
   ;; Constants
   #:++INF+
   #:+-INF+
   #:+NAN+
   ;; Criteria
   #:make-criteria
   ;; Linear operations
   #:vector-dim
   #:norm
   #:*norm-type*
   #:negate-vector #:negate-vector!
   #:zero-vector   #:zero-vector!
   #:make-vector
   #:map-vector #:map-vector!
   #:mapi-vector #:mapi-vector!
   #:reduce-vector
   #:m*
   #:m/
   #:dot
   #:transpose
   #:duplicate-vector
   #:e+ #:e=+! #:e+!
   #:e- #:e=-! #:e-!
   #:e* #:e=*! #:e*!
   #:e/ #:e=/! #:e/!
   #:e-rev/ #:e=-rev/! #:e-rev/!
   #:outer-product #:inner-product
   #:eabs #:esin #:ecos #:etan #:eexp #:esignum
   #:eabs! #:esin! #:ecos! #:etan! #:eexp! #:esignum!
   #:define-vector-function
   ;; BICGSTAB
   #:bicgstab
   #:*bicgstab-criteria*
   ;; MVECTOR
   #:mvector #:up #:down
   #:mvector-index-type
   #:mvector-datum
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
   #:down-index  #:down-index!
   #:up-index    #:up-index!
   ;; DIFF
   #:deriv
   ;; FSOLVE
   #:fsolve
   ;; ODE
   #:ode-step
   #:make-crank-nicolson-method))
