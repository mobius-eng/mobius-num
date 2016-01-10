(in-package numeric-trapezoid)

(defclass ode-trap ()
  ((ode-trap-constant
    :initarg :constant
    :reader ode-trap-constant
    :documentation
    "Constant part of discretization:

           n   1    n   n
    C = - x  - - f(t , x )
               2
")
   (ode-trap-next-value
    :initarg :next-value
    :reader ode-trap-next-value
    :documentation
    "Space for the next value

     n+1       n+1
    x    ~= x(t   )
")
   (ode-trap-next-rate
    :initarg :next-rate
    :reader ode-trap-next-rate
    :documentation
    " Space to store the rate at the next time step

     n+1      n+1  n+1
    y    = f(t  , x   )
")
   (ode-trap-nonlinear-solver
    :initarg :nonlinear-solver
    :accessor ode-trap-nonlinear-solver
    :documentation
    "Nonlinear solver to solve for next value

     n+1
    x

in the discretized equation

     n+1   dt     n+1   n+1     n   1    n   n
    x   - ---- f(t   , x   ) - x  - - f(t , x ) = 0
           2                        2
")
   (ode-trap-tmp-value
    :initarg :tmp-value
    :accessor ode-trap-tmp-value
    :documentation
    "Temporary buffer to store intermediate values")
   (ode-trap-tmp-rate
    :initarg :tmp-rate
    :accessor ode-trap-tmp-rate
    :documentation
    "Temporary buffer to store value of intermediate rates"))
  (:documentation
   "Representation of the quality controlled trapezoid method
of solving initial value problem ODE:

                              0
    D[x](t) = f(t,x); x(0) = x

with step discretization

 n + 1    n                                     
x      - x    1    n  n         n + 1   n + 1   
----------- = -(f(t ,x )  +  f(t     , x     )) 
    dt        2                                 

"))

(defmethod ode-attempt-step ((method ode-trap) ode-function state time-step ode-error)
  (let ((new-value (direct-step method ode-function state time-step)))
    (let ((error-estimate (estimate-error (ode-state-value state) new-value ode-error)))
      (declare (type double-float error-estimate))
      (cond ((> error-estimate 1.0d0)
             (values nil (recommend-step time-step error-estimate)))
            (t (values t (recommend-step time-step error-estimate)))))))

(defmethod ode-perform-step ((method ode-trap) ode-function state time-step)
  (let ((next-value (ode-trap-next-value method)))
    (incf (ode-state-time state) time-step)
    (copy-vector-to! next-value (ode-state-value state))
    (ode-state-init-rate state ode-function)))


(defun direct-step (trap ode-function state time-step)
  "Performs direct step returning the value at the new time point"
  (with-accessors ((const ode-trap-constant)
                   (tmp ode-trap-tmp-value)
                   (nonlinear-method ode-trap-nonlinear-solver))
      trap
    (with-accessors ((value ode-state-value)
                     (rate ode-state-rate)
                     (current-time ode-state-time))
        state
      ;; Fill in constant
      (linear-combination! 0 const (cons -1d0 value) (cons (* -0.5d0 time-step) rate))
      (let ((next-time (+ current-time time-step)))
        (flet ((nonlinear-function (z result)
                 (copy-vector-to! (funcall ode-function next-time z) result)
                 (linear-combination! (* -0.5d0 time-step) result (cons 1d0 z) (cons 1d0 const)))
               (jacobian*vector (z v result)
                 (funcall (ode-function-jacobian ode-function) next-time z v result)
                 (linear-combination! (* -0.5d0 time-step) result (cons 1d0 v))))
          (copy-vector-to! value tmp)
          (multiple-value-bind (solution solved-p)
              (fsolve nonlinear-method #'nonlinear-function tmp #'jacobian*vector)
            (format t "~&Solved (time = ~A, step = ~A): ~A~%" solved-p next-time time-step)
            solution))))))

(defun estimate-error (trap ode-function state time-step ode-error)
  "Returns relative error estimate"
  ;; Set new rate
  (with-accessors ((curtime ode-state-time)
                   (curvalue ode-state-value)
                   (currate ode-state-rate))
      state
    (with-accessors ((nextvalue ode-trap-next-value)
                     (nextrate ode-trap-next-rate)
                     (tmpvalue ode-trap-tmp-value)
                     (tmprate ode-trap-tmp-rate))
        trap)
    (let ((half-step (* 0.5d0 time-step)))
      ;; Set new rate
      (copy-vector-to! (funcall ode-function (+ curtime time-step) nextvalue) nextrate)
      ;; Predictor for midpoint u(n+1/2)
      (vector-average! currate nextrate tmprate)
      (trapezoid-step curvalue half-step currate tmprate tmpvalue)
      ;; Updated midpoint rate
      (copy-vector-to! (funcall ode-function (+ curtime half-step) tmpvalue)
                       tmprate)
      ;; Corrected midpoint u(n+1/2)
      (trapezoid-step curvalue half-step currate tmprate tmpvalue)
      ;; Corrected midpoint rate (does it do much?)
      (copy-vector-to! (funcall ode-function (+ curtime half-step) tmpvalue)
                       tmprate)
      ;; Estimate of u(n+1) done in two steps
      (trapezoid-step tmpvalue half-step tmprate nextrate tmpvalue)
      )))


(defun trapezoid-step (init-value step init-rate final-rate final-value)
  (assign-linear-combination! final-value
                              (cons 1d0 init-value)
                              (cons (* 0.5d0 step) init-rate)
                              (cons (* 0.5d0 step) final-rate)))

(defun recommend-step (time-step error-estimate)
  "Calculates recommended step based on error estimate")
