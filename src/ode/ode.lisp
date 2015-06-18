(in-package mobius.numeric.ode)

(defstruct ode
  starting-time
  starting-value
  starting-rate
  (requested-step +NAN+)
  (performed-step 0.0d0)
  (recommended-step 0.d0)
  (next-time +NAN+)
  (next-value nil)
  (next-rate nil)
  (computation-error ++INF+))

(setf (symbol-function 'ode?) #'ode-p)

(defun init-ode (&key starting-time starting-value starting-rate)
  (make-ode :starting-time starting-time
            :starting-value starting-value
            :starting-rate starting-rate
            :next-value (duplicate-vector starting-value)
            :next-rate (duplicate-vector starting-rate)))


(defun ode-update (ode dest
                   &key performed-step recommended-step next-value next-rate
                   computation-error requested-step)
  (setf (ode-starting-time dest) (ode-starting-time ode))
  (setf (ode-starting-value dest) (e+! (ode-starting-value dest)
                                       (ode-starting-value ode)))
  (setf (ode-starting-rate dest) (e+! (ode-starting-rate dest)
                                      (ode-starting-rate ode)))
  (setf (ode-requested-step dest) (or requested-step (ode-requested-step ode)))
  (setf (ode-recommended-step dest) (or recommended-step
                                        (ode-recommended-step ode)))
  (setf (ode-performed-step dest) (or performed-step (ode-performed-step ode)))
  (setf (ode-next-time dest) (if performed-step
                                 (+ (ode-starting-time ode) performed-step)
                                 (ode-next-time ode)))
  (setf (ode-next-value dest) (e+! (ode-next-value dest)
                                   (or next-value
                                       (ode-next-value ode))))
  (setf (ode-next-rate dest) (e+! (ode-next-rate dest)
                                  (or next-rate (ode-next-rate ode))))
  (setf (ode-computation-error dest) (or computation-error
                                         (ode-computation-error ode)))
  dest)

(defun duplicate-ode (ode)
  (make-ode
   :starting-time (ode-starting-time ode)
   :starting-value (duplicate-vector (ode-starting-value ode))
   :starting-rate (duplicate-vector (ode-starting-rate ode))
   :requested-step (ode-requested-step ode)
   :performed-step (ode-performed-step ode)
   :recommended-step (ode-recommended-step ode)
   :next-time (ode-next-time ode)
   :next-value (if (ode-next-value ode)
                   (duplicate-vector (ode-next-value ode))
                   nil)
   :next-rate (if (ode-next-rate ode)
                  (duplicate-vector (ode-next-rate ode))
                  nil)
   :computation-error (ode-computation-error ode)))


(defun ode-request-step! (ode step)
  (setf (ode-requested-step ode) step)
  ;; (setf (ode-next-time ode) (+ (ode-starting-time ode) step)) don't
  ;; need it?
  ode)

(defun ode-advance! (ode)
  (setf (ode-starting-time ode) (ode-next-time ode))
  (setf (ode-starting-value ode) (ode-next-value ode))
  (setf (ode-starting-rate ode) (ode-next-rate ode))
  (setf (ode-requested-step ode) (ode-recommended-step ode))
  (setf (ode-performed-step ode) 0.0d0)
  (setf (ode-recommended-step ode) 0.d0)
  (setf (ode-next-time ode) +NAN+)
  (setf (ode-next-value ode) (ode-starting-value ode))
  (setf (ode-next-rate ode) (ode-starting-rate ode))
  (setf (ode-computation-error ode) ++INF+))

(defvar *ode-successful-step-criteria*
  (criteria:make
   (criteria:finished-value #f(<= (ode-computation-error %) 1.0d0)
                            "ODE: error <= tolerance achieved")
   (criteria:failed-value #f(num= (ode-starting-time %)
                                  (+ (ode-starting-time %)
                                     (ode-requested-step %)))
                          "ODE: Cannot control the error, step = 0")
   (criteria:limit-iterations 10)))

(defgeneric ode-step (method f ode tolerance u-scale)
  (:documentation "Generic ode stepper method"))
