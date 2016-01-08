(in-package cl-user)

(defpackage #:mobius.numeric.ode
  (:nicknames #:numeric-ode)
  (:use #:cl #:numeric-control #:optima #:numeric-fixed-point #:numeric-linear-base)
  (:import-from #:mobius.utils #:%)
  (:export #:ode-state #:ode-state-time #:ode-state-value #:ode-state-rate
           #:ode-state-init-rate
           #:ode-error #:ode-error-scale #:ode-error-tolerance
           #:ode-function #:ode-function-jacobian
           #:ode-attempt-step #:ode-perform-step
           #:ode-step
           #:ode-evolve))


(defpackage #:mobius.numeric.runge-kutta
  (:nicknames #:numeric-runge-kutta)
  (:use #:cl #:numeric-linear-base #:numeric-control #:numeric-helpers #:numeric-ode)
  (:export #:make-tableau #:rk45ck-tableau #:tableau-order
           #:runge-kutta))


(defpackage #:mobius.numeric.trapezoid
  (:nicknames #:numeric-trapezoid)
  (:use #:cl #:numeric-linear-base #:numeric-control #:numeric-nonlinear
        #:numeric-newton #:numeric-ad #:numeric-helpers #:numeric-ode)
  (:shadowing-import-from #:numeric-ad
                          #:+ #:- #:* #:/
                          #:sin #:cos #:tan #:asin #:acos #:atan
                          #:exp #:log #:expt #:sqrt
                          #:sinh #:cosh #:tanh
                          #:= #:< #:> #:<= #:>=
                          #:zerop #:plusp #:minusp
                          #:numberp))

;; (defpackage mobius.numeric.ode
;;   (:nicknames #:ode)
;;   (:use #:cl #:mobius.utils #:linop #:mobius.numeric.utils #:criteria)           
;;   (:import-from #:cl-num-utils #:num=)
;;   (:import-from #:alexandria #:with-gensyms)
;;   (:export #:ode
;;            #:make-ode
;;            #:init-ode
;;            #:ode-starting-time
;;            #:ode-starting-value
;;            #:ode-starting-rate
;;            #:ode-requested-step
;;            #:ode-performed-step
;;            #:ode-recommended-step
;;            #:ode-next-value
;;            #:ode-next-rate
;;            #:ode-next-time
;;            #:ode-computation-error
;;            #:ode?
;;            #:ode-update
;;            #:duplicate-ode
;;            #:ode-request-step!
;;            #:ode-advance!
;;            #:*ode-successful-step-criteria*
;;            #:ode-step))

;; (defpackage mobius.numeric.crank-nicolson
;;   (:nicknames #:cn #:rk2imp)
;;   (:use #:cl #:mobius.utils #:linop #:mobius.numeric.fsolve
;;         #:mobius.numeric.utils #:ode
;;         #:mobius.numeric.fixed-point
;;         #:criteria)
;;   (:import-from #:cl-num-utils #:num=)
;;   (:export #:make-crank-nicolson-method
;;            #:*crank-nicolson-solver-criteria*))
