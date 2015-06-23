(in-package cl-user)

(defpackage #:mobius.numeric.linear-generics
  (:nicknames #:lingenerics)
  (:use #:cl)
  (:import-from #:cl-num-utils #:transpose)
  (:export #:zero-vector
           #:make-vector
           #:list->vector #:list->vector-method
           #:vector-dim
           #:map-vector
           #:reduce-vector
           #:outer-product
           #:inner-product
           #:m*
           #:m/
           #:transpose))

(defpackage #:mobius.numeric.linear-functions
  (:nicknames #:linfunctions)
  (:use #:cl #:lingenerics #:mobius.utils
        #:mobius.numeric.utils)
  (:export #:duplicate-vector
           #:negate-vector
           #:one-vector
           #:dot
           #:squared-l2-norm
           #:norm
           #:*norm-type*
           #:.+ #:.+! #:.=+!
           #:.- #:.-! #:.=-!
           #:.* #:.*! #:.=*!
           #:./ #:./! #:.=/!
           #:.rev/ #:.rev/! #:.=rev/!
           #:define-vector-function
           #:.abs  #:.sin  #:.cos  #:.tan  #:.exp  #:.signum
           #:.abs! #:.sin! #:.cos! #:.tan! #:.exp! #:.signum!))

(defpackage #:mobius.numeric.impl-numbers
  (:use #:cl #:mobius.utils #:lingenerics))

(defpackage #:mobius.numeric.impl-vectors
  (:use #:cl #:mobius.utils #:lingenerics))

(defpackage #:mobius.numeric.impl-arrays
  (:use #:cl #:mobius.utils #:lingenerics))


(defpackage #:mobius.numeric.bicg-stab
  (:nicknames #:bicgstab)
  (:use #:cl #:linop #:mobius.numeric.fixed-point #:criteria)
  (:export #:*bicgstab-criteria*
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

