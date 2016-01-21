(in-package #:numeric-newton)

;; * Newton's method
;; ** Value representation
(defclass newton-value ()
  ((newton-value-approximation
    :initarg :approximation
    :reader newton-value-approximation
    :type nonlinear-value
    :documentation
    "Representation of the current approximation: a pair of (X F)
where F is (F X)")
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
    :approximation (nonlinear-value (make-vector size 'double-float)
                                    (make-vector size 'double-float))
    :dir (make-vector size 'double-float)))

(defmethod print-object ((obj newton-value) out)
  (with-slots (newton-value-approximation newton-value-g0) obj
    (print-unreadable-object (obj out :type t)
     (format out "~A g(x) = ~F" newton-value-approximation newton-value-g0))))

;; ** Control
(defvar *newton-tolerance* 1d-8
  "Default value for convergence tolerance. The convergence checks the value of the residual
and the value of the step to take. For convergence, both must fall below tolerance")
(defvar *newton-residual-abs-minimum* 1d-12
  "If residual falls below this value, the solution is considered converged even though
the method still suggests a large step to take")
(defvar *newton-max-iterations* 20)

(defun newton-finished-p (tolerance)
  (lambda (val)
    (with-accessors ((p newton-value-dir) (g0 newton-value-g0)) val
      (let ((residual (sqrt (* 2d0 g0))))
        (or (almost-zero-p residual *newton-residual-abs-minimum*)
            (and (vector-almost-zero-p p tolerance)
                 (almost-zero-p residual tolerance)))))))

(defun make-newton-control (tolerance max-iterations other-controls)
  (apply #'combine-controls
         (finished-value (newton-finished-p tolerance) #'newton-value-approximation)
         (limit-iterations max-iterations #'newton-value-approximation)
         other-controls))

;; ** Method representation
(defclass newton ()
  ((newton-value
    :initarg :value
    :reader newton-value)
   (newton-linear-solver
    :initarg :linear-solver
    :accessor newton-linear-solver)
   (newton-tmp-x
    :initarg :tmp-x
    :reader newton-tmp-x)
   (newton-tmp-f
    :initarg :tmp-f
    :reader newton-tmp-f)
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
                           (linear-solver (bicg-stab size))
                           (tolerance *newton-tolerance*)
                           (max-iterations *newton-max-iterations*)
                           other-controls)
  (make-instance 'newton
    :value (make-newton-value size)
    :linear-solver linear-solver
    :tmp-x (make-vector size 'double-float)
    :tmp-f (make-vector size 'double-float)
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
(defun make-newton-step (method function jacobian*vector)
  (declare (type newton method))
  (with-accessors ((tmp-x newton-tmp-x)
                   (tmp-f newton-tmp-f)
                   (lsearch newton-linsearch)
                   (lin-solver newton-linear-solver))
      method
    (lambda (value)
      (declare (type newton-value value))
      (with-accessors ((app newton-value-approximation)
                       (p newton-value-dir)
                       (g0 newton-value-g0)
                       (Dg0 newton-value-Dg0))
          value
        (with-accessors ((x nonlinear-value-x)
                         (f nonlinear-value-f))
            app
          (flet ((ls-g (lmbda)
                   (linear-combination! 0d0 tmp-x (cons 1d0 x) (cons lmbda p))
                   (funcall function tmp-x tmp-f)
                   (* 0.5d0 (square-vector tmp-f)))
                 (df-fun (y b)
                   (funcall jacobian*vector x y b)
                   (negate-vector! b)))
            (copy-vector-to! x p)
            (multiple-value-bind (solution successful-p info)
                (solve-linear lin-solver #'df-fun f p)
              (unless successful-p
                (error 'linear-solver-failed :info `(:solution ,solution :info ,info)))
              (let ((g1 (ls-g 1d0)))
                (multiple-value-bind (final-lambda successful-p final-g)
                    (linsearch lsearch #'ls-g #'newton-step-gamma g0 Dg0 g1)
                  (declare (ignore final-lambda))
                  (unless successful-p
                    (error 'linsearch-failed :value lsearch))
                  (copy-vector-to! tmp-x x)
                  (copy-vector-to! tmp-f f)
                  (setf g0 final-g)
                  (setf Dg0 (* -2d0 final-g))
                  value)))))))))


;; ** Solver

(defun init-newton-value! (value function x0)
  (with-accessors ((app newton-value-approximation)
                   (g0 newton-value-g0)
                   (Dg0 newton-value-Dg0))
      value
    (init-nonlinear-value app function x0)
    (setf g0 (* 0.5 (nonlinear-value-square-residual app)))
    (setf Dg0 (* -2d0 g0))))

;; ** Implementation of NONLINEAR-SOLVE
(defmethod solve-nonlinear ((method newton) f x jacobian*vector)
  (unless jacobian*vector
    (error 'nonlinear-no-jacobian :method 'newton))
  (let ((stepper (make-newton-step method f jacobian*vector)))
    (init-newton-value! (newton-value method) f x)
    (fixed-point (newton-control method) stepper (newton-value method))))

;; (sqrt (* 2d0 (newton-value-g0 value)))

