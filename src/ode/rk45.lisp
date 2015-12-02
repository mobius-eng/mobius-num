(in-package numeric-rk45)


(defclass ode-state ()
  ((ode-state-time
    :initarg :time
    :accessor ode-state-time)
   (ode-state-value
    :initarg :value
    :accessor ode-state-value)))

(defun make-ode-state (time value)
  (make-instance 'ode-state :time time :value value))

(defclass runge-kutta ()
  ((runge-kutta-k
    :initarg :k
    :reader runge-kutta-k)
   (runge-kutta-tmp-value
    :initarg :tmp-value
    :reader runge-kutta-tmp-value)))


(defclass runge-kutta-45-cash-karp (runge-kutta)
  ())


(defgeneric ode-step! (method ode-function initial-state time-step))


(defun new-tmp-value (tmp-y y b k i)
  (apply #'linear-combination!
         0d0 tmp-y
         (cons 1d0 y)
         (loop for j from 0 below i
            collect (cons (aref b i j) (svref k j)))))

(let ((a (vec 'double-float 0d0 0.2d0 0.3d0 0.6d0 1d0 0.875d0))
      (c (vec 'double-float (/ 37d0 378) 0d0 (/ 250d0 621) (/ 125d0 594) 0d0 (/ 512d0 1771)))
      (c* (vec 'double-float (/ 2825d0 27648) 0d0 (/ 18575d0 48384) (/ 13525d0 55296) (/ 277d0 14336) 0.25d0))
      (b (make-array '(6 5)
                     :element-type 'double-float
                     :initial-contents
                     `((             0d0            0d0              0d0            0d0                  0d0)
                       (           0.2d0            0d0              0d0            0d0                  0d0)
                       (         0.075d0        0.225d0              0d0            0d0                  0d0)
                       (           0.3d0         -0.9d0            1.2d0            0d0                  0d0)
                       (    ,(/ -11d0 54)         2.5d0     ,(/ -70d0 27)   ,(/ 35d0 27)                 0d0)
                       (,(/ 1631d0 55296) ,(/ 175d0 512) ,(/ 575d0 13824) ,(/ 44275d0 110592) ,(/ 253d0 4096))))))
 (defmethod ode-step! ((method runge-kutta-45-cash-karp) ode-function initial-state time-step)
   (with-accessors ((k runge-kutta-k)
                    (tmp-value runge-kutta-tmp-value))
       method
     (declare (type (simple-vector 6) k)
              (type (vector double-float *) k))
     (let ((t0 (ode-state-time initial-state))
           (y0 (ode-state-value initial-state))
           (h time-step))
       (dotimes (i 6)
         (new-tmp-value tmp-y y0 b k i)
         (linear-combination! 0d0 (svref k i)
                              (cons h (funcall ode-function (make-ode-state (+ t0 (* (aref a i) h))
                                                                            tmp-y)))))))))
