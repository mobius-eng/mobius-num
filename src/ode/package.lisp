(in-package cl-user)

(defpackage #:mobius.numeric.runge-kutta
  (:nicknames #:numeric-runge-kutta)
  (:use #:cl #:linear-base #:control #:numeric-helpers)
  (:export #:ode-state #:ode-state-time #:ode-state-value
           #:rk-tableau #:rk-a #:rk-b #:rk-c #:rk-c*
           #:intermediate-time #:intermediate-value #:tableau-order
           #:rk-data #:rk-k #:rk-tmp-value
           #:runge-kutta #:runge-kutta-data #:runge-kutta-tableau
           #:rk45ck-tableau
           #:runge-kutta-45-cash-karp #:rk45ck
           #:runge-kutta-update-k #:ode-accept-error-p))


(defpackage #:mobius.numeric.trapezoid
  (:nicknames #:numeric-trapezoid)
  (:use #:cl #:linear-base #:control #:numeric-nonlinear #:newton #:ad #:numeric-helpers)
  (:shadowing-import-from #:ad
                          #:+ #:- #:* #:/
                          #:sin #:cos #:tan #:asin #:acos #:atan
                          #:exp #:log #:expt #:sqrt
                          #:sinh #:cosh #:tanh
                          #:= #:< #:> #:<= #:>=
                          #:zerop #:plusp #:minusp
                          ;;#:literal-function
                          ;;#:literal-vector
                          ;; #:D #:diff #:gradient-f #:jacobian*vector #:jacobian*vector-save
                          ;;#:directional-derivative-f
                          ;;#:partial
                          ;;#:comp
                          ))

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
