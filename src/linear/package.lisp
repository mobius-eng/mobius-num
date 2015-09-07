(in-package cl-user)

(defpackage #:mobius.numeric.matrix
  (:nicknames #:matrix)
  (:use #:cl)
  (:export #:matrix-dimensions
           #:matrix-row-num
           #:matrix-col-num
           #:make-matrix
           #:eye-matrix
           #:mref
           #:add
           #:sub
           #:scale
           #:negate
           #:zero
           #:mmul
           #:transpose
           #:domatrix))

(defpackage #:mobius.numeric.array-matrix
  (:use #:cl #:matrix)
  (:export #:array-matrix
           #:array-matrix-data
           #:make-array-matrix))

(defpackage #:mobius.numeric.permutation
  (:use #:cl #:matrix
        #:mobius.numeric.array-matrix)
  (:export #:permutation
           #:permutation-vector
           #:make-permutation
           #:inverse-permutation
           #:compose-permutation))

#|
(defpackage #:mobius.numeric.linear
  (:nicknames #:numeric-linear)
  (:use #:cl #:gsl #:numeric-constants #:criteria)
  (:import-from #:cl-num-utils #:num=)
  (:export #:zero-vector
           #:copy-vector
           #:make-vector
           #:list->vector
           #:vector-of
           #:dfvec
           #:sb64vec
           #:negate-vector
           #:vector-dim
           #:dot
           #:squared-l2-norm
           #:norm
           #:*norm-type*
           #:reduce-vector
           #:.abs  #:.sin  #:.cos  #:.tan  #:.exp  #:.log  #:.signum
           #:.abs! #:.sin! #:.cos! #:.tan! #:.exp! #:.log! #:.signum!
           #:elementwise!
           #:.=+! #:.=-! #:.=*! #:.=/!
           #:.+   #:.-   #:.*   #:./
           #:.+!  #:.-!  #:.*!  #:./!
           #:list->matrix
           #:make-matrix
           #:matrix-of
           #:identity-matrix
           #:m*
           #:m/))


|#