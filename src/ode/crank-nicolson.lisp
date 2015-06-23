(in-package mobius.numeric.crank-nicolson)

(defun direct-step (u0 f0 f1 h dest)
  "Direct full step using Crank-Nicolson formula"
  (declare (type double-float h))
  (e=+! (e=*! (e+! dest f0 f1) (* 0.5d0 h)) u0))

(defvar *crank-nicolson-solver-criteria*
  (make-criteria
   :converged (list #'(lambda (x y)
                        (<= (norm (e- (cadr x) (cadr y))) (* 1.0d-9 (norm (cadr x)))))
                    "Crank-Nicolson: convergence achieved")
   :limit-iterations '(20 "Crank-Nicolson: cannot solve nonlinear equation"))
  "Default nonlinear solver criteria for Crank-Nicolson method")


(defun simple-step (function jacobian init-state requested-step)
  "Perform full Crank-Nicolson step without error control"
  (let* ((h/2 (* 0.5d0 requested-step))
         (next-time (+ (ode-state-time init-state) requested-step))
         (n (vector-dim (ode-state-value init-state)))
         (identity-matrix (identity-matrix n)))
    (flet ((nonlinear-eq (u tmp)
             (let ((next-rate (funcall function new-time u tmp)))
               (e=+! (negate-vector! (e=+! (e=*! (e=+! next-rate
                                                       (ode-state-rate init-state))
                                                 h/2)
                                           (ode-state-value init-state)))
                     u)))
           (nonlinear-jacobian-numbers (u df-buffer)
             (+ (* (- h/2) (funcall jacobian new-time u df-buffer)) 1.0d0))
           (nonlinear-jacobian-vectors (u df-buffer)
             (e=+! (e=*! (funcall jacobian new-time u df-buffer) (- h/2))
                   identity-matrix)))
      (if (zerop n)
          (fsolve #'nonlinear-eq
                  (ode-state-value init-state)
                  :simple-function nil
                  :criteria *crank-nicolson-solver-criteria*
                  :df #'nonlinear-jacobian-numbers)
          (fsolve #'nonlinear-eq
                  (ode-state-value init-state)
                  :simple-function nil
                  :criteria *crank-nicolson-solver-criteria*
                  :df #'nonlinear-jacobian-vectors
                  :df-tmp )))))

(defun simple-step (f jac ode)
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


(eval-when (:compile-toplevel :execute :load-toplevel)
  (declaim (inline avrg))
  (defun avrg (a b dest)
    (e=*! (e+! dest a b) 0.5d0))

  (declaim (inline cube))
  (defun cube (x)
    (declare (type double-float x))
    (* x x x)))



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


(defun make-tmp-buffers (vector size)
  (let ((v (make-array size :element-type (type-of vector))))
    (loop for i below size
       do (setf (aref v i) (zero-vector vector)))
    v))

(defmethod ode-step-rules ((method crank-nicolson-method) ode computation-error)
  (declare (type double-float computation-error) (type ode ode))
  (let ((performed-step (ode-performed-step ode)))
    (declare (type double-float perfomed-step))
    (let ((error-expt #. (/ -1.0d0 3.0d0))
          (error-minimum #. (cube (/ 5.0d0)))
          (step-coefficient 0.9d0))
      (declare (type double-float error-expt error-minimum step-coefficient))
      (cond ((< computation-error error-minimum) (* performed-step 5.0d0))
            ((< computation-error 1.0d0) (* performed-step
                                            step-coefficient
                                            (expt computation-error error-expt)))
            ((> perfomed-step 0.0d0) (max (/ performed-step 10.0d0)
                                          (* perfomed-step (expt computation-error
                                                                 error-expt))))
            ((= performed-step 0.0d0) (ode-requested-step ode))
            (t (min (/ performed-step 10.0d0)
                    (* perfomed-step (expt computation-error error-expt))))))))

(defstruct crank-nicolson-method
  jacobian
  tmpbuf
  function-buffer)


(defmethod ode-advance ((method crank-nicolson-method) function)
  (let ((jac (crank-nicolson-method-jacobian method)))
    (cn-step function jac)))


(defun cn-step (function jacobian tmpbuf funciton-buffer)
  (let ((error-coefficient #. (* 1.2d0 (/ 4.0d0 3.0d0))))
    (declare (type double-float error-coefficient))
    #'(lambda (ode-in ode-out)
        (multiple-value-bind (u-1 ign status) (simple-step function
                                                           jacobian
                                                           ode-in)
          (case status
            (:finished
             (let* ((h (ode-step-requested (ode-step ode-in)))
                    (t-1 (+ (ode-state-time (ode-init-state ode-in)) h))
                    (ode-full-step (ode-update ode-in ode-out
                                               :performed-step h
                                               :next-value u-1
                                               :next-rate f-1))
                    (u-1* (two-half-steps function ode-full-step tmpbuf))
                    (computation-error (e=*! (eabs!
                                              (e=/! (e-! (ode-error-control-error
                                                          (ode-error ode-in))
                                                         u-1
                                                         u-1*)))
                                             error-coefficient)))
               (ode-update ode-full-step ode-out
                           :computation-error computation-error
                           :performed-step h)))
            (otherwise (ode-update ode-in ode-out
                                   :performed-step 0.0d0
                                   :requested-step (* 0.1d0 h)
                                   :computation-error +infinity)))))))

