(in-package num-tests-ode)

(def-suite ode-tests)

(in-suite ode-tests)

(test ode-rk45ck
  "Solve ODE
    D[y] (t) = - y(t)
using Embedded Runge-Kutta-45 method (Cash-Karp)
for t <- (0, 1)"
  (let (time
        ylist
        (time-list (loop for i from 1 upto 10 collect (* i 0.1d0)))
        (rk45ck (runge-kutta rk45ck-tableau 1))
        (v (make-vector 1 'double-float)))
    (flet ((ode-function (time value)
             (declare (ignore time))
             (setf (aref v 0) (- (aref value 0)))
             v))
      (let ((state (ode-state 0d0 (vec 'double-float 1d0) (make-vector 1 'double-float)))
            (ode-error (ode-error (vec 'double-float 1d0) 1d-9))
            (exact-solution (mapcar (comp #'exp #'-) time-list)))
        (ode-state-init-rate state #'ode-function)
        (ode-evolve rk45ck
                    #'ode-function
                    state
                    time-list
                    (lambda (state)
                      (push (ode-state-time state) time)
                      (push (aref (ode-state-value state) 0)
                            ylist))
                    ode-error)
        (setf time (nreverse time))
        (setf ylist (nreverse ylist))
        (format t "~&Collected values:~%Time: ~A~%Y: ~A~%" time ylist)
        (is-true (= (length time) (length ylist) (length time-list)))
        (is-true (every (lambda (x y) (almost-zero-p (abs (- x y)))) time time-list))
        (is-true (every (lambda (x y) (almost-zero-p (abs (- x y)) 1d-8))
                        exact-solution
                        ylist))))))

;; (run! 'ode-rk45ck)

(let ((rk45ck (runge-kutta rk45ck-tableau 1))
      (ode-function (let ((v (make-vector 1 'double-float))
                          (n 0))
                      (lambda (time value)
                        (incf n)
                        (format t "~&ODE-FUNCTION CALL #~D~%" n)
                        (format t "~Tt=~F v=~A~%" time value)
                        (setf (aref v 0) (- (aref value 0)))
                        v)))
      (state (ode-state 0d0 (vec 'double-float 1d0) (make-vector 1 'double-float)))
      (ode-error (ode-error (vec 'double-float 1d0) 1d-10)))
  (copy-vector-to! (funcall ode-function 0d0 (vec 'double-float 1d0))
                   (ode-state-rate state))
  (ode-evolve rk45ck
              ode-function
              state
              '(0.1d0 0.2d0 0.3d0 0.4d0 0.5d0)
              (lambda (state)
                (format t "~&New state = ~A~%" state))
              ode-error))
