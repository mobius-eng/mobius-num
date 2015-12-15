(in-package numeric-runge-kutta)

;; * Runge-Kutta methods

;; ** Tableau
(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun make-tableau (&rest args &key a b c c*)
   (cond ((null a) (error "Must provide A"))
         ((null b) (error "Must provide B"))
         ((null c) (error "Must provide C"))
         ((null c*) (error "Must provide C*"))
         (t args))))

;; Need to add this otherwise the constant migth be redefined
(eval-when (:compile-toplevel)
 (defconstant rk45ck-tableau
   (make-tableau
    :a (vec 'double-float
            0d0 0.2d0 0.3d0 0.6d0 1d0 0.875d0)
    :c (vec 'double-float
            (/ 37d0 378) 0d0 (/ 250d0 621) (/ 125d0 594) 0d0 (/ 512d0 1771))
    :c* (vec 'double-float
             (/ 2825d0 27648) 0d0 (/ 18575d0 48384) (/ 13525d0 55296) (/ 277d0 14336) 0.25d0)
    :b (make-array
        '(6 5)
        :initial-contents
        `((          0d0               0d0               0d0          0d0 0d0)
          (        0.2d0               0d0               0d0          0d0 0d0)
          (      0.075d0           0.225d0               0d0          0d0 0d0)
          (        0.3d0            -0.9d0             1.2d0          0d0 0d0)
          (,(/ -11d0 54)             2.5d0     ,(/ -70d0 27) ,(/ 35d0 27) 0d0)
          (,(/ 1631d0 55296) ,(/ 175d0 512) ,(/ 575d0 13824)
            ,(/ 44275d0 110592) ,(/ 253d0 4096)))))
   "Runge-Kutta embedded formula tableau of order 4-5 due to Cash-Karp"))

(defun tableau-order (tableau)
  "Highest order of the method"
  (1- (length (getf tableau :a))))


;; ** Runge-Kutta data
;; *** RK-tableau mixin
(defclass rk-tableau ()
  ((rk-a :initarg :a :reader rk-a)
   (rk-b :initarg :b :reader rk-b)
   (rk-c :initarg :c :reader rk-c)
   (rk-c* :initarg :c* :reader rk-c*)))

;; *** RK-data mixin
(defclass rk-data ()
  ((rk-k
    :initarg :k
    :reader rk-k)
   (rk-tmp-value
    :initarg :tmp-value
    :reader rk-tmp-value)))

(defun rk-intermediate-steps-number (rk)
  (length (rk-k rk)))

;; *** Full Runge-Kutta
(defclass runge-kutta (rk-tableau rk-data)
  ((rk-grow-exp :initarg :grow-exp
                :reader rk-grow-exp)
   (rk-shrink-exp :initarg :shrink-exp
                  :reader rk-shrink-exp)
   (rk-step-safety :initarg :step-safety
                   :reader rk-step-safety)
   (rk-grow-error-limit :initarg :grow-error-limit
                        :reader rk-grow-error-limit)))

(defun runge-kutta (tableau vector-size)
  (declare (type list tableau) (type fixnum vector-size))
  (let ((order (tableau-order tableau)))
    (let ((k (loop repeat (1+ order)
                collect (make-vector vector-size 'double-float)))
          (tmp-value (make-vector vector-size 'double-float))
          (grow-exp (- (/ 1d0 order)))
          (shrink-exp (- (/ 1d0 (1- order))))
          (step-safety 0.9d0))
      (let ((grow-error-limit (expt (/ 5d0 step-safety)
                                    (/ grow-exp))))
        (apply #'make-instance
               'runge-kutta
               :k (make-array (1+ order) :initial-contents k)
               :tmp-value tmp-value
               :grow-exp grow-exp
               :shrink-exp shrink-exp
               :step-safety step-safety
               :grow-error-limit grow-error-limit
               tableau)))))

(defun intermediate-time (rk i current-time time-step)
  "Get time for i-th intermediate step according the tableau"
  (+ current-time (* (aref (rk-a rk) i) time-step)))

(defun intermediate-value (rk y i)
  "Calculate intermediate value of Y according to tableau:
               __         
        y  +  \      b  k 
              /__ j<i ij j

K values are taken from DATA and B values are taken from TABLEAU"
  (let ((b (rk-b rk))
        (k (rk-k rk))
        (dest (rk-tmp-value rk)))
    (apply #'linear-combination!
           0d0 dest
           (cons 1d0 y)
           (loop for j from 0 below i
              collect (cons (aref b i j) (svref k j))))
    dest))

(defun rk-update-k! (rk ode-function initial-state time-step)
  "Update all vectors K for TIME-STEP according to tableau

        k   = hf(t + a h, y )
         i            i    i 

where y  is the i-th INTERMEDIATE-VALUE and h is TIME-STEP
       i
"
  (declare (type function ode-function)
           (type ode-state initial-state)
           (type double-float time-step)
           (type runge-kutta rk))
  (let ((k (rk-k rk))
        (t0 (ode-state-time initial-state))
        (y0 (ode-state-value initial-state))
        (f (ode-state-rate initial-state)))
    (declare (type (simple-vector *) k)
             (type (vector double-float *) y0)
             (type double-float t0))
    (scale-vector! time-step f (svref k 0))
    (loop for i from 1 below (rk-intermediate-steps-number rk)
       do (let ((y (intermediate-value rk y0 i))
                (time (intermediate-time rk i t0 time-step)))
            (scale-vector! time-step
                           (funcall ode-function time y)
                           (svref k i))))))

(defun component-difference (k c c* i)
  (loop for j below (length c)
     sum (* (- (aref c j) (aref c* j)) (aref (svref k j) i))))

(defun rk-adjust-step-coefficient (rk value-scale tolerance)
  (with-accessors ((safety rk-step-safety)
                   (grow-exp rk-grow-exp)
                   (shrink-exp rk-shrink-exp)
                   (grow-error-limit rk-grow-error-limit)
                   (k rk-k)
                   (c rk-c)
                   (c* rk-c*))
      rk
    (let* ((step-error (loop for i below (length value-scale)
                          maximizing (abs (/ (component-difference k c c* i)
                                             (* tolerance (aref value-scale i)))))))
      (cond ((> step-error 1d0) (* safety (expt step-error shrink-exp)))
            ((< step-error grow-error-limit) (* safety 5d0))
            (t (* safety (expt step-error grow-exp)))))))

(defmethod ode-attempt-step
    ((method runge-kutta) ode-function state time-step ode-error)
  (rk-update-k! method ode-function state time-step)
  (let ((recommended-step (* time-step
                             (rk-adjust-step-coefficient
                              method
                              (ode-error-scale ode-error)
                              (ode-error-tolerance ode-error)))))
    (values (>= recommended-step time-step) recommended-step)))


(defmethod ode-perform-step ((method runge-kutta) ode-function state time-step)
  (incf (ode-state-time state) time-step)
  (apply #'linear-combination!
         1d0 (ode-state-value state)
         (loop for i below (rk-intermediate-steps-number method)
            collect (cons (aref (rk-c method) i)
                          (svref (rk-k method) i))))
  (copy-vector-to! (funcall ode-function
                            (ode-state-time state)
                            (ode-state-value state))
                   (ode-state-rate state)))









