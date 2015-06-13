(in-package cl-user)

;; TODO: maybe should add cl-slice?
(defpackage #:mobius.numeric
  (:use #:cl
        #:linop
        #:mobius.numeric.fsolve
        #:mv)
  (:import-from #:cl-num-utils #:num= #:transpose #:*num=-tolerance*)
  (:import-from #:mobius.numeric.diff #:deriv)
  (:export
   ;; CL-NUM-UTILS
   #:num=
   #:*num=-tolerance*
   #:transpose
   ;; Linear operations
   #:vector-dim
   #:norm
   #:elt-negate #:elt-negate!
   #:elt-zero   #:elt-zero!
   #:m*
   #:m/
   #:dot
   #:e+ #:e=+! #:e+!
   #:e- #:e=-! #:e-!
   #:e* #:e=*! #:e*!
   #:e/ #:e=/! #:e/!
   #:e-rev/ #:e=-rev/! #:e-rev/!
   #:outer-product
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
   #:mvector-map #:mvector-map!
   ;; DIFF
   #:deriv
   ;; FSOLVE
   #:fsolve))
