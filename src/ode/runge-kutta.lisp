(in-package numeric-runge-kutta)


(defclass ode-state ()
  ((ode-state-time
    :initarg :time
    :accessor ode-state-time)
   (ode-state-value
    :initarg :value
    :accessor ode-state-value)))

(defun ode-state (time value)
  (make-instance 'ode-state :time time :value value))


(defclass rk-tableau ()
  ((rk-a :initarg :a :reader rk-a)
   (rk-b :initarg :b :reader rk-b)
   (rk-c :initarg :c :reader rk-c)
   (rk-c* :initarg :c* :reader rk-c*)))

(defun intermediate-time (tableau i current-time time-step)
  (+ current-time (* (aref (rk-a tableau) i) time-step)))

(defun intermediate-value (data tableau y i)
  "Calculate intermediate value of Y according to tableau:
               __         
        y  +  \      b  k 
              /__ j<i ij j

K values are taken from DATA and B values are taken from TABLEAU"
  (let ((b (rk-b tableau))
        (k (rk-k data))
        (dest (rk-tmp-value data)))
    (apply #'linear-combination!
           0d0 dest
           (cons 1d0 y)
           (loop for j from 0 below i
              collect (cons (aref b i j) (svref k j))))
    dest))

(defun tableau-order (tableau)
  (1- (length (rk-a tableau))))

(defclass rk-data ()
  ((rk-k
    :initarg :k
    :reader rk-k)
   (rk-tmp-value
    :initarg :tmp-value
    :reader rk-tmp-value)))

(defun rk-data (method-order vector-size)
  (let ((k (loop for i from 0 upto method-order
              collect (make-vector vector-size 'double-float))))
    (make-instance 'rk-data
      :k (make-array (1+ method-order) :initial-contents k)
      :tmp-value (make-vector vector-size 'double-float))))


(defclass runge-kutta ()
  ((runge-kutta-data :initarg :data :reader runge-kutta-data)
   (runge-kutta-tableau :initarg :tableau :reader runge-kutta-tableau)))


(defconstant rk45ck-tableau
  (make-instance 'runge-kutta-tableau
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
  "Runge-Kutta embedded formula tableau of order 4-5 due to Cash-Karp")

(defclass runge-kutta-45-cash-karp (runge-kutta)
  ())

(defun rk45ck (size)
  "Make RK45 (Cash-Karp) method"
  (let ((method-order 5))
    (make-instance 'runge-kutta-45-cash-karp
      :data (rk-data method-order size)
      :tableau rk45ck-tableau)))

(defun runge-kutta-update-k (data tableau ode-function initial-state time-step)
  "Update all vectors K for TIME-STEP according to tableau

        k   = hf(t + a h, y )
         i            i    i 

where y  is the i-th INTERMEDIATE-VALUE and h is TIME-STEP
       i
"
  (declare (type function ode-function)
           (type ode-state initial-state)
           (type double-float time-step))
  (let ((k (rk-k data))
        (t0 (ode-state-time initial-state))
        (y0 (ode-state-value initial-state)))
    (declare (type (simple-vector *) k)
             (type (vector double-float *) y0)
             (type double-float t0))
    (dotimes (i (1+ (tableau-order tableau)))
      (let ((y (intermediate-value data tableau y0 i))
            (time (intermediate-time tableau i t0 time-step)))
        (linear-combination! 0d0 (svref k i)
                             (cons time-step
                                   (funcall ode-function (ode-state time y))))))))

(defun component-difference (k c c* i)
  (loop for j below (length c)
     sum (* (- (aref c j) (aref c* j)) (aref (svref k j) i))))


(defun rk-adjust-step-coefficient (data tableau value-scale tolerance)
  (let ((safety 0.9d0)
        (grow-exp -0.2d0)
        (shrink-exp -0.25d0)
        (grow-error-limit 1.89d-4)
        (k (rk-k data))
        (c (rk-c tableau))
        (c* (rk-c* tableau)))
    (let* ((step-error (loop for i below (length value-scale)
                          maximizing (abs (/ (component-difference k c c* i)
                                             (* tolerance (aref value-scale i)))))))
      (cond ((> step-error 1d0) (* safety (expt step-error shrink-exp)))
            ((< step-error grow-error-limit) (* safety 5d0))
            (t (* safety (expt step-error grow-exp)))))))

(defgeneric ode-accept-error-p (method time-step value-scale tolerance))

(defmethod ode-accept-error-p ((method runge-kutta-45-cash-karp)
                               time-step value-scale tolerance)
  (let ((next-step (* time-step
                      (rk-adjust-step-coefficient (runge-kutta-data method)
                                                  (runge-kutta-tableau method)
                                                  value-scale
                                                  tolerance))))
    (if (< next-step time-step)
        (values nil next-step)
        (values t next-step))))
