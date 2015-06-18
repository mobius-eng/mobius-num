(in-package mobius.numeric.crank-nicolson)

(defun direct-step (u0 f0 f1 h dest)
  "Direct full step using Crank-Nicolson formula"
  (declare (type double-float h))
  (e=+! (e=*! (e+! dest f0 f1) (* 0.5d0 h)) u0))

(defvar *crank-nicolson-solver-criteria*
  (criteria:make (criteria:converged
                  #'(lambda (x y)
                      (<= (norm (e- (cadr x) (cadr y))) (* 1.0d-9 (norm (cadr x))))))
                 (criteria:limit-iterations 20))
  "Default nonlinear solver criteria for Crank-Nicolson method")

(defun simple-step (f jac ode)
  "Perform full Crank-Nicolson step without error control"
  (let ((h/2      (* 0.5d0 (ode-requested-step ode)))
        (new-time (+ (ode-starting-time ode) (ode-requested-step ode)))
        (n        (vector-dim (ode-starting-value ode))))
    (declare (type fixnum n) (type double-float h/2 new-time))
    (let ((identity-matrix (identity-matrix n)))
      (flet ((nonlinear-eq (u tmp)
               (let ((f (funcall f new-time u tmp)))
                 (e=+! (negate-vector! (e=+! (e=*! (e=+! f (ode-starting-rate ode)) h/2)
                                             (ode-starting-value ode)))
                       u)))
             (nonlinear-jac-num (u df-buffer)
               (+ (* (- h/2) (funcall jac new-time u df-buffer)) 1.0d0))
             (nonlinear-jac-vector (u df-buffer)
               (e=+! (e=*! (funcall jac new-time u df-buffer) (- h/2))
                     identity-matrix)))
        (if (zerop n)
            (fsolve #'nonlinear-eq
                    (ode-starting-value ode)
                    :simple-function nil
                    :criteria *crank-nicolson-solver-criteria*
                    :df #'nonlinear-jac-num)
            (fsolve #'nonlinear-eq
                    (ode-starting-value ode)
                    :simple-function nil
                    :criteria *crank-nicolson-solver-criteria*
                    :df #'nonlinear-jac-vector))))))

(declaim (inline avrg))
(defun avrg (a b dest)
  (e=*! (e+! dest a b) 0.5d0))

(defun two-half-steps (f ode tmpbuf)
  "Perform two half-steps using predictor-corrector method.
ODE must have a complete full Crank-Nicolson step.
TMPBUF is a vector of at least 8 temporary buffers for rates and values"
  (format t "TWO-HALF-STEPS: next-time = ~A~%" (ode-next-time ode))
  (let* ((h/2 (* 0.5d0 (ode-requested-step ode)))
         (t-0 (ode-starting-time ode))
         (u-0 (ode-starting-value ode))
         (f-0 (ode-starting-rate ode))
         (t-1 (ode-next-time ode))
         (f-1 (ode-next-rate ode))
         (f-1/2-estimate (avrg f-0 f-1 (aref tmpbuf 0)))
         (u-1/2-predictor (direct-step u-0 f-0 f-1/2-estimate h/2 (aref tmpbuf 1)))
         (f-1/2-predictor (funcall f (+ t-0 h/2) u-1/2-predictor (aref tmpbuf 2)))
         (u-1/2-corrector (direct-step u-0 f-0 f-1/2-predictor h/2 (aref tmpbuf 3)))
         (f-1/2-corrector (funcall f (+ t-0 h/2) u-1/2-corrector (aref tmpbuf 4)))
         (u-1-predictor (direct-step u-1/2-corrector
                                     f-1/2-corrector
                                     f-1
                                     h/2
                                     (aref tmpbuf 5)))
         (f-1-predictor (funcall f t-1 u-1-predictor (aref tmpbuf 6))))
    (direct-step u-1/2-corrector f-1/2-corrector f-1-predictor h/2 (aref tmpbuf 7))))

(declaim (inline cube))
(defun cube (x)
  (declare (type double-float x))
  (* x x x))

(defun make-tmp-buffers (vector size)
  (let ((v (make-array size :element-type (type-of vector))))
    (loop for i below size
       do (setf (aref v i) (duplicate-vector vector)))
    v))

(defun cn-step (f jac ode tolerance value-scale)
  "Crank-Nicolson time step with error control"
  (let ((safety-coefficient 1.2d0)
        (error-coefficient  (/ 4.0d0 3.0d0))
        (error-expt (/ -1.0d0 3.0d0))
        (error-minimum      (cube (/ 5.0d0)))
        (step-coefficient 0.9d0)
        (ode-fixed-point-buffer-1 (duplicate-ode ode))
        (ode-fixed-point-buffer-2 (duplicate-ode ode))
        (tmpbuf (make-tmp-buffers (ode-starting-value ode) 8))
        (delta-buffer (zero-vector (ode-starting-value ode)))
        (f-buffer (zero-vector (ode-starting-value ode))))
    (declare (type double-float
                   safety-coefficient
                   error-coefficient
                   error-minimum
                   step-coefficient))
    (flet ((improve-error (ode tmp)
             (multiple-value-bind (u-1 ign status) (simple-step f jac ode)
               (declare (ignore ign))
               (format t "~&Full-step ~A value ~A and status ~A~%"
                       (ode-requested-step ode) u-1 status)
               (case status
                 (:FINISHED (let* ((h (ode-requested-step ode))
                                   (t-1 (+ (ode-starting-time ode) h))
                                   (f-1 (funcall f t-1 u-1 f-buffer))
                                   (ode-full-step (ode-update
                                                   ode tmp
                                                   :performed-step h
                                                   :next-value u-1
                                                   :next-rate f-1))
                                   (u-1* (two-half-steps f ode-full-step tmpbuf))
                                   (computation-error
                                     (/ (* (norm (eabs!
                                                  (e=/! (e-! delta-buffer u-1 u-1*)
                                                        value-scale)))
                                           (* safety-coefficient
                                              error-coefficient))
                                       tolerance)))
                              (format t "Double-stepped u = ~A~%" u-1*)
                              (ode-update ode-full-step
                                          tmp
                                          :computation-error computation-error
                                          :performed-step
                                          (when (<= computation-error 1.0d0) h)
                                          :requested-step
                                          (when (> computation-error 1.0d0)
                                            (max (* h
                                                    step-coefficient
                                                    (expt computation-error error-expt))
                                                        (* 0.1d0 h)))
                                          :recommended-step
                                          (cond ((< computation-error error-minimum)
                                                 (* 5.0d0 h))
                                                ((<= computation-error 1.0d0)
                                                 (* h
                                                    step-coefficient
                                                    (expt computation-error
                                                          error-expt)))
                                                (t nil)))))
                 (otherwise (ode-update ode
                                        tmp
                                        :performed-step 0.0d0
                                        :requested-step (* 0.1d0
                                                         (ode-requested-step ode))
                                        :computation-error ++INF+))))))
      (fixed-point *ode-successful-step-criteria*
                   #'improve-error
                   ode
                   ode-fixed-point-buffer-1
                   ode-fixed-point-buffer-2))))

(defstruct crank-nicolson-method jac)

(defmethod ode-step ((method crank-nicolson-method) f ode tolerance u-scale)
  (cn-step f (crank-nicolson-method-jac method) ode tolerance u-scale))
