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
  (with-slots (newton-value-x newton-value-f newton-value-g0) obj
    (print-unreadable-object (obj out :type t)
     (format out "x = ~A f(x) = ~A g(x) = ~F" newton-value-x newton-value-f newton-value-g0))))

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
    :tmp-x (make-vector size 'double-float)
    :control (make-newton-control tolerance max-iterations other-controls)
    :linsearch (make-linsearch)))


;; ** Conditions
(define-condition linear-solver-failed (error)
  ((linear-solver-failed-info
    :initarg :info
    :reader linear-solver-failed-info)))


(define-condition linsearch-failed (error)
  ((linsearch-failed-value
    :initarg :value
    :reader linsearch-failed-value)))


;; ** Newton step (globally convergent)
(defun make-newton-step (method f)
  (declare (type newton method))
  (with-accessors ((df->x newton-df->x)
                   (tmp-x newton-tmp-x)
                   (lsearch newton-linsearch)
                   (lin-solver newton-linear-solver))
      method
    (let (f-tmp-value
          (safety-coeff 0.9d0))
      (lambda (value)
        (declare (type newton-value value))
        (with-accessors ((x newton-value-x)
                         (p newton-value-dir)
                         (f-value newton-value-f)
                         (g0 newton-value-g0)
                         (Dg0 newton-value-Dg0))
            value
         (flet ((ls-g (lmbda)
                  (linear-combination! 0d0 tmp-x (cons 1d0 x) (cons lmbda p))
                  (setf f-tmp-value (funcall f tmp-x))
                  (* 0.5d0 (square-vector f-tmp-value)))
                (df-fun (y b)
                  (negate-vector! (jacobian*vector-save f x y df->x b))))
           (copy-vector-to! x p)
           (multiple-value-bind (solution successful-p info)
               (solve-linear lin-solver #'df-fun f-value p)
             (unless successful-p
               (error 'linear-solver-failed :info `(:solution ,solution :info ,info)))
             (let ((g1 (ls-g 1d0)))
               ;; (format t "~&NEWTON: ~A ~F~%" value g1)
               (unless (< g1 (* safety-coeff g0))
                   (let ((ls-result (linsearch lsearch #'ls-g g0 Dg0 g1 :gamma #'newton-step-gamma)))
                     (unless (iterator:finished-p ls-result)
                       (error 'linsearch-failed :value (iterator:value ls-result)))
                     (setf g1 (linsearch-value-g1 (iterator:value ls-result)))))
               (copy-vector-to! tmp-x x)
               (copy-vector-to! f-tmp-value f-value)
               (setf g0 g1)
               (setf Dg0 (* -2d0 g1))
               value))))))))


;; ** Solver

(defun init-method! (method f x)
  (let ((val (newton-value method)))
    (copy-vector-to! x (newton-value-x val))
    (copy-vector-to! (funcall f x) (newton-value-f val))
    (setf (newton-value-g0 val) (* 0.5 (square-vector (newton-value-f val))))
    (setf (newton-value-Dg0 val) (* -2d0 (newton-value-g0 val)))))


(defun newton-solve (method f x &rest other-controls)
  (let ((stepper (make-newton-step method f))
        (control (make-newton-control *newton-tolerance*
                                      *newton-max-iterations*
                                      other-controls)))
    (init-method! method f x)
    (fixed-point control stepper (newton-value method))))
