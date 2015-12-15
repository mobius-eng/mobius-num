(in-package cl-user)

(defpackage #:mobius.numeric.symbolic
  (:nicknames #:numeric-symbolic #:sym)
  (:shadow #:+ #:- #:* #:/
           #:sin #:cos #:tan #:asin #:acos #:atan
           #:exp #:expt #:log #:sqrt
           #:sinh #:cosh #:tanh
           #:= #:< #:> #:<= #:>=
           #:zerop #:plusp #:minusp)
  (:use #:cl #:optima)
  (:export #:+ #:- #:* #:/
           #:bin+ #:bin- #:bin* #:bin/
           #:sin #:cos #:tan #:asin #:acos #:atan
           #:exp #:log #:expt #:sqrt
           #:sinh #:cosh #:tanh
           #:= #:< #:> #:<= #:>=
           #:zerop #:plusp #:minusp
           #:literal-vector #:literal-function)
  (:documentation
   "Symbolic extensions of CL arithmetic operations"))

(defpackage #:mobius.numeric.ad
  (:nicknames #:numeric-ad)
  (:shadow #:+ #:- #:* #:/
           #:sin #:cos #:tan #:asin #:acos #:atan
           #:exp #:expt #:log #:sqrt
           #:sinh #:cosh #:tanh
           #:= #:< #:> #:<= #:>=
           #:zerop #:plusp #:minusp #:numberp)
  (:import-from :alexandria #:iota)
  (:use #:cl #:optima)
  (:export #:+ #:- #:* #:/
           #:sin #:cos #:tan #:asin #:acos #:atan
           #:exp #:log #:expt #:sqrt
           #:sinh #:cosh #:tanh
           #:= #:< #:> #:<= #:>=
           #:zerop #:plusp #:minusp
           #:numberp #:variable-p
           #:literal-function
           #:D #:diff #:gradient-f #:jacobian*vector #:jacobian*vector-save
           #:directional-derivative-f
           #:partial)
  (:documentation
   "Automatic differentiation"))


;; (:shadowing-import-from #:numeric-symbolic
;;                           #:+ #:- #:* #:/
;;                           #:sin #:cos #:tan #:asin #:acos #:atan
;;                           #:exp #:expt #:log #:sqrt
;;                           #:sinh #:cosh #:tanh
;;                           #:= #:< #:<= #:> #:>=
;;                           #:zerop #:plusp #:minusp)
