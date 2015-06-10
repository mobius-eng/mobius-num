(in-package cl-user)

(defpackage #:mobius-num.fsolve
  (:use #:cl #:mobius-num.newton-raphson #:mobius-num.linear-operations #:mobius-num.diff)
  (:import-from #:cl-num-utils #:num= #:*num=-tolerance*))


(defpackage #:mobius-num
  (:use #:cl))
