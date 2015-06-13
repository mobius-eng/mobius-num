(in-package cl-user)

(defpackage #:mobius.numeric.linear-operations
  (:nicknames #:linop)
  (:use #:cl #:mobius.utils)
  (:import-from #:alexandria #:with-gensyms)
  (:export #:vector-dim
           #:*norm-type*
           #:norm
           #:elt+ #:elt=+! #:elt+!
           #:elt- #:elt=-! #:elt-!
           #:elt* #:elt=*! #:elt*!
           #:elt/ #:elt=/! #:elt/!
           #:elt=-rev/!
           #:elt-negate #:elt-negate!
           #:elt-zero #:elt-zero!
           #:m*
           #:m/
           #:dot
           #:e+ #:e=+! #:e+!
           #:e- #:e=-! #:e-!
           #:e* #:e=*! #:e*!
           #:e/ #:e=/! #:e/!
           #:e-rev/ #:e=-rev/! #:e-rev/!
           #:outer-product)
  (:documentation "Definitions of generic linear operations"))

(defpackage #:mobius.numeric.linear-operations-numbers
  (:use #:cl #:mobius.utils #:linop)
  (:import-from #:cl-num-utils #:transpose)
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
           #:bicgstab))


(defpackage #:mobius.numeric.mvector
  (:nicknames #:mv)
  (:use #:cl #:mobius.utils
        #:linop
        #:bicgstab)
  (:import-from #:alexandria #:with-gensyms)
  (:import-from #:cl-num-utils #:num= #:transpose #:*num=-tolerance*)
  (:import-from #:lla #:mm)
  (:export #:mvector #:up #:down
           #:mvector-index-type
           #:mvector-datum
           #:array->mvector
           #:array->up
           #:array->down
           #:const-mvector
           #:const-up
           #:const-down
           #:zero-mvector
           #:zero-up
           #:zero-down
           #:build-mvector
           #:build-up
           #:build-down
           #:delta-mvector
           #:delta-up
           #:delta-down
           #:copy-mvector
           #:mvref
           #:mvector?
           #:column?
           #:up?
           #:row?
           #:down?
           #:mvlength
           #:comaptible?
           #:incompatible-mvector-size
           #:throw-incompatible-mvector-size
           #:down-index
           #:down-index!
           #:up-index
           #:up-index!
           #:mvector-map
           #:mvector-map!))

