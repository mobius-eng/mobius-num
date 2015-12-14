(in-package cl-user)


(defpackage #:mobius.numeric.tests.ad
  (:nicknames #:num-tests-ad)
  (:use #:cl #:fiveam #:ad)
  (:shadowing-import-from
   #:ad
   #:+ #:- #:* #:/
   #:= #:< #:> #:<= #:>=
   #:zerop #:plusp #:minusp
   #:sin #:cos #:tan #:asin #:acos #:atan
   #:sinh #:cosh #:tanh
   #:exp #:expt #:log #:sqrt))

(defpackage #:mobius.numeric.tests.base
  (:nicknames #:num-tests-base)
  (:use #:cl #:numeric-constants #:control #:fixed-point #:fiveam #:numeric-helpers)
  (:import-from #:mobius.utils #:average #:%)
  (:import-from #:cl-num-utils #:num=))

(defpackage #:mobius.numeric.tests.linear
  (:nicknames #:num-tests-linear)
  (:use #:cl #:numeric-control #:mobius.numeric.linear-base
        #:mobius.numeric.bicg-stab #:fiveam #:numeric-helpers)
  (:import-from #:cl-num-utils #:num=))




(defpackage #:mobius.numeric.tests.nonlinear
  (:nicknames #:num-tests-nonlinear)
  (:use #:cl #:numeric-ad #:numeric-linear-base
        #:numeric-newton #:fiveam
        #:numeric-helpers #:numeric-linsearch #:numeric-nonlinear)
  (:import-from #:mobius.utils #:% #:with-vector-items)
  (:shadowing-import-from
   #:numeric-ad
   #:+ #:- #:* #:/
   #:= #:< #:> #:<= #:>=
   #:zerop #:plusp #:minusp
   #:sin #:cos #:tan #:asin #:acos #:atan
   #:sinh #:cosh #:tanh
   #:exp #:expt #:log #:sqrt
   #:numberp))

(defpackage #:mobius.numeric.tests.ode
  (:nicknames #:num-tests-ode)
  (:use #:cl #:numeric-helpers #:numeric-linear-base
        #:numeric-runge-kutta #:numeric-ode #:fiveam))
