(in-package cl-user)

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

