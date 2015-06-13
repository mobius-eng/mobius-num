(in-package cl-user)

(defpackage #:mobius-num.newton-raphson
  (:nicknames #:newton)
  (:use #:cl #:mobius.utils #:linop #:mobius-num.fixed-point)
  (:export #:newton-method))

(defpackage #:mobius-num.diff
  (:use #:cl #:mv #:linop #:cl-slice)
  (:export #:*diff-step*
           #:generic-d
           #:deriv))
