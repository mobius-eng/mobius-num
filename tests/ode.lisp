(in-package num-tests-ode)

(let ((rk45ck (rk45ck 1))
      (ode-function (lambda (state)
                      (vec 'double-float
                           (- (aref (ode-state-value state) 0))))))
  (runge-kutta-update-k (runge-kutta-data rk45ck)
                        (runge-kutta-tableau rk45ck)
                        ode-function
                        (ode-state 0d0 (vec 'double-float 1d0))
                        1d-1)
  (ode-accept-error-p rk45ck 1d-1 (vec 'double-float 1d0) 1d-5))
