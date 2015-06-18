(in-package cl-user)

(defpackage mobius.numeric.ode
  (:nicknames #:ode)
  (:use #:cl #:mobius.utils #:linop #:mobius.numeric.utils)
  (:import-from #:cl-num-utils #:num=)
  (:export #:ode
           #:make-ode
           #:init-ode
           #:ode-starting-time
           #:ode-starting-value
           #:ode-starting-rate
           #:ode-requested-step
           #:ode-performed-step
           #:ode-recommended-step
           #:ode-next-value
           #:ode-next-rate
           #:ode-next-time
           #:ode-computation-error
           #:ode?
           #:ode-update
           #:duplicate-ode
           #:ode-request-step!
           #:ode-advance!
           #:*ode-successful-step-criteria*
           #:ode-step))

(defpackage mobius.numeric.crank-nicolson
  (:nicknames #:cn #:rk2imp)
  (:use #:cl #:mobius.utils #:linop #:mobius.numeric.fsolve
        #:mobius.numeric.utils #:ode
        #:mobius.numeric.fixed-point)
  (:import-from #:cl-num-utils #:num=)
  (:export #:make-crank-nicolson-method
           #:*crank-nicolson-solver-criteria*))
