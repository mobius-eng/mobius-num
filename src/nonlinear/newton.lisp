(in-package #:newton)

;; * Newton's method
;; ** Value representation
(defclass newton-value ()
  ((newton-value-x
    :initarg :x
    :reader newton-value-x
    :documentation "Current approximation")
   (newton-value-f
    :initarg :f
    :reader newton-value-f
    :documentation "Value of function F on current approximation")
   (newton-value-dir
    :initarg :dir
    :reader newton-value-dir
    :documentation "Newton search direction (p). Only valid within newton-step")
   (newton-value-g0
    :initarg :g0
    :initform 0d0
    :accessor newton-value-g0
    :documentation "g(x)=1/2 f*f")
   (newton-value-Dg0
    :initarg :Dg0
    :initform 0d0
    :accessor newton-value-Dg0
    :documentation "Dg(x)= -2g(x) directional derivative")))

(defun make-newton-value (size)
  (make-instance 'newton-value
    :x (make-vector size 'double-float)
    :f (make-vector size 'double-float)
    :dir (make-vector size 'double-float)))

(defun newton-solution (val)
  (newton-value-x val))

(defun newton-residual (val)
  (newton-value-f val))

(defmethod print-object ((obj newton-value) out)
  (with-slots (newton-value-x newton-value-f) obj
    (print-unreadable-object (obj out :type t)
     (format out "x = ~A f(x) = ~A" newton-value-x newton-value-f))))

;; ** Control
(defvar *newton-tolerance* 1d-8)
(defvar *newton-max-iterations* 20)

(defun newton-finished-p (tolerance)
  (lambda (val)
    (with-accessors ((p newton-value-dir) (g0 newton-value-g0)) val
      (and (vector-almost-zero-p p tolerance)
           (almost-zero-p g0 tolerance)))))

(defun make-newton-control (tolerance max-iterations other-controls)
  (apply #'combine-controls
         (finished-value (newton-finished-p tolerance)
                         (lambda (value) (l2-norm (newton-value-f value))))
         (limit-iterations max-iterations)
         other-controls))

;; ** Method representation
(defclass newton ()
  ((newton-value
    :initarg :value
    :reader newton-value)
   (newton-linear-solver
    :initarg :linear-solver
    :accessor newton-linear-solver)
   (newton-df->x
    :initarg :df->x
    :reader newton-df->x)
   (newton-tmp-x
    :initarg :tmp-x
    :reader newton-tmp-x)
   (newton-control
    :initarg :control
    :reader newton-control)
   (newton-linsearch
    :initarg :linsearch
    :reader newton-linsearch)))

(defmethod print-object ((obj newton) out)
  (with-slots (newton-value newton-linear-solver) obj
    (print-unreadable-object (obj out :type t)
      (format out "~A ~A" newton-value newton-linear-solver))))

(defun make-newton (size &key
                           (linear-solver (make-bicg-stab size))
                           (tolerance *newton-tolerance*)
                           (max-iterations *newton-max-iterations*)
                           other-controls)
  (make-instance 'newton
    :value (make-newton-value size)
    :linear-solver linear-solver
    :df->x (make-vector size)
    :newton-tmp-x (make-vector size 'double-float)
    :control (make-newton-control tolerance max-iterations other-controls)
    :linsearch (make-linsearch)))


;; ** Conditions
(define-condition linear-solver-failed (error)
  ((linear-solver-failed-info
    :initarg :info
    :reader linear-solver-failed-info)))

;; TODO: might be different after LINSEARCH
(defun update-value! (val f)
  (add-with-multipliers! (newton-value-x val) (cons -1.0d0 (newton-value-dir val)))
  (copy-vector-to! (funcall f (newton-value-x val)) (newton-value-f val)))

(defun make-newton-step (method f)
  (declare (type newton method))
  (let (f-tmp-value)
    (lambda (value)
      (declare (type newton-value value))
      (flet ((linsearch-g (lmbda)
               (linear-combination!
                0d0
                (newton-tmp-x method)
                (cond 1d0 (newton-value-x value))
                (cons lmbda (newton-value-dir value)))
               (setf f-tmp-value (funcall f (newton-tmp-x method)))
               (* 0.5d0 (square-vector f-tmp-value)))
             (df-fun (y b)
               (jacobian*vector-save f
                                     (newton-value-x value)
                                     y
                                     (newton-df->x method)
                                     b)))
        (copy-vector-to! (newton-value-x value) (newton-value-dir value))
        (multiple-value-bind (solution successful-p info)
            (solve-linear (newton-linear-solver method)
                          #'df-fun
                          (newton-value-f value)
                          (newton-value-dir value))
          (if successful-p
              (let ((g1 (linsearch-g 1d0))
                    (linsearch-result (linsearch (newton-value-linsearch value)
                                                 #'linsearch-g
                                                 (newton-value-g0 value)
                                                 (newton-value-Dg0 value)
                                                 g1
                                                 #'newton-step-gamma)))
                (cond ((iterator:finished-p linsearch-result)
                       (let ((linsearch-value (iterator:value linsearch-result)))
                         (let ((lmbda (linsearch-value-lambda1 linsearch-value))
                               (g1 (linsearch-value-g1 linsearch-value)))
                           ;; NEWTON-TMP-X must contain last x + lambda * p
                           (copy-vector-to! (newton-tmp-x method) (newton-value-x value))
                           ;; F-TMP-VALUE must contain last time evaluated F on x + lambda p
                           (copy-vector-to! f-tmp-value (newton-value-f value))
                           (setf (newton-value-g0 value) g1)
                           (setf (newton-value-Dg0 value) (* -2d0 g1))
                           value)))
                      (t (error ""))))
              (error 'linear-solver-failed
                     :info (list :solution solution
                                 :info info))))))))

(defun init-method! (method f x)
  (let ((val (newton-method-value method)))
    (copy-vector-to! x (newton-value-x val))
    (copy-vector-to! (funcall f x) (newton-value-f val))))


(defun newton-method-solve (method f x &rest other-controls)
  (let ((stepper (make-newton-step method f))
        (control (make-control-newton *newton-tolerance*
                                      *newton-max-iterations*
                                      other-controls)))
    (init-method! method f x)
    (fixed-point control stepper (newton-method-value method))))
