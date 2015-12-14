(in-package num-tests-ode)

(let ((rk45ck (runge-kutta rk45ck-tableau 1))
      (ode-function (let ((v (make-vector 1))
                          (n 0))
                      (lambda (time value)
                        (incf n)
                        (format t "~&ODE-FUNCTION CALL #~D~%" n)
                        (format t "~Tt=~F v=~A~%" time value)
                        (setf (svref v 0) (- (aref value 0)))
                        v)))
      (state (ode-state 0d0 (vec 'double-float 1d0) (make-vector 1 'double-float)))
      (ode-error (ode-error (vec 'double-float 1d0) 1d-10)))
  (copy-vector-to! (funcall ode-function 0d0 (vec 'double-float 1d0))
                   (ode-state-rate state))
  ;; (format t "~&Init-state: ~A~%" state)
  (ode-evolve rk45ck
              ode-function
              state
              '(0.1d0 0.2d0 0.3d0 0.4d0 0.5d0)
              (lambda (state)
                (format t "~&New state = ~A~%" state))
              ode-error))
