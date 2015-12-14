(in-package numeric-linsearch)

;; * Line search algorithm

;; ** Line search state representation
(defclass linsearch-value ()
  ((linsearch-value-lambda1
    :initarg :lambda1
    :type double-float
    :accessor linsearch-value-lambda1
    :documentation "Last lambda approximation")
   (linsearch-value-lambda2
    :initarg :lambda2
    :initform nil
    :type double-float
    :accessor linsearch-value-lambda2
    :documentation "Second last lambda approximation")
   (linsearch-value-g1
    :initarg :g1
    :type double-float
    :accessor linsearch-value-g1
    :documentation "g(lambda1)")
   (linsearch-value-g2
    :initarg :g2
    :initform nil
    :type double-float
    :accessor linsearch-value-g2
    :documentation "g(lambda2)")
   (linsearch-g0
    :initarg :g0
    :type double-float
    :accessor linsearch-value-g0
    :documentation "Initial value g(0)")
   (linsearch-dg0
    :initarg :dg0
    :type double-float
    :accessor linsearch-value-dg0
    :documentation "D[g](0) - derivative of g(lambda) @lambda=0"))
  (:documentation
   "All pieces of data to represent line search computation"))

(defun make-linsearch-value (g0 Dg0 g1 &optional (lambda1 0d0) lambda2 g2)
  "Construct the value for line search of the problem G(lambda) -> min"
  (make-instance 'linsearch-value
    :lambda1 lambda1
    :lambda2 lambda2
    :g1 g1
    :g2 g2
    :g0 g0
    :dg0 Dg0))

(defmethod print-object ((obj linsearch-value) out)
  (with-accessors ((lambda1 linsearch-value-lambda1)
                   (lambda2 linsearch-value-lambda2)
                   (g1 linsearch-value-g1)
                   (g2 linsearch-value-g2)
                   (g0 linsearch-value-g0)
                   (Dg0 linsearch-value-dg0))
      obj
    (print-unreadable-object (obj out :type t)
      (format out "G0 ~G DG0 ~G LAMBDA1 ~,3F G1 ~G LAMBDA2 ~,3F G2 ~G"
              g0 Dg0 lambda1 g1 lambda2 g2))))

(defun move-1->2! (value)
  "Move the latest approximation to the second latest"
  (with-accessors ((lambda1 linsearch-value-lambda1)
                   (lambda2 linsearch-value-lambda2)
                   (g1 linsearch-value-g1)
                   (g2 linsearch-value-g2))
      value
    (setf lambda2 lambda1)
    (setf g2 g1)))

(defun update-1! (value new-lambda1 new-g1)
  "Put new approximation"
  (with-accessors ((lambda1 linsearch-value-lambda1)
                   (g1 linsearch-value-g1))
      value
    (setf lambda1 new-lambda1)
    (setf g1 new-g1)))

(defun reinit-value! (value new-g0 new-dg0 new-g1)
  "Reinitialize the value"
  (with-accessors ((lambda1 linsearch-value-lambda1)
                   (lambda2 linsearch-value-lambda2)
                   (g1 linsearch-value-g1)
                   (g2 linsearch-value-g2)
                   (g0 linsearch-value-g0)
                   (Dg0 linsearch-value-dg0))
      value
    (setf g0 new-g0)
    (setf dg0 new-dg0)
    (setf lambda1 1.0d0)
    (setf g1 new-g1)
    (setf lambda2 nil)
    (setf g2 nil)))

(defun newton-step-gamma (value)
  "Newton step approximation for minimizing G(lambda)"
  (* -2.0d0 (linsearch-value-g0 value) (linsearch-value-lambda1 value)))

;; ** Control

(defvar *linsearch-alpha* 1d-4
  "Safety coefficient for minimization:
    g(lambda) < g0 + alpha * Dg(lambda)")
(defvar *linsearch-abs-lambda-min* 1d-3
  "Absolute minimum of lambda")
(defvar *linsearch-max-iterations* 10
  "Maximum number of iterations")

(defun linsearch-finished-p (alpha gamma)
  "Returns FINISHED-VALUE control for the problem with given ALPHA and GAMMA"
  (finished-value
   (lambda (value)
     (with-accessors ((g0 linsearch-value-g0)
                      (lambda1 linsearch-value-lambda1)
                      (g1 linsearch-value-g1))
         value
       (let ((gamma-value (funcall gamma value)))
         (< g1 (+ g0 (* alpha gamma-value))))))))


(defun new-lambda1 (g)
  (alter-value
   (lambda (value)
     (with-accessors ((lambda1 linsearch-value-lambda1)
                      (lambda2 linsearch-value-lambda2)
                      (g1 linsearch-value-g1)
                      (g2 linsearch-value-g2)
                      (g0 linsearch-value-g0)
                      (Dg0 linsearch-value-dg0))
         value
       (match lambda2
         (nil
          (move-1->2! value)
          (let ((new-lambda1 (quad-extremum (- g1 g0 Dg0) Dg0)))
            (update-1! value new-lambda1 (funcall g new-lambda1))
            value))
         (otherwise
          (multiple-value-bind (a b c)
              (cubic-approximation g0 Dg0 lambda1 g1 lambda2 g2)
            (let ((new-lambda1 (cubic-extremum-2 a b c)))
              (move-1->2! value)
              (update-1! value new-lambda1 (funcall g new-lambda1))
              value))))))))


(defun linsearch-lambda-keep-bound (g min-coeff max-coeff)
  "ALTER-VALUE control for keeping new lambda in bounds:

    MIN-COEFF*LAMBDA < NEW-LAMBDA < MAX-COEFF*LAMBDA
"
  (alter-value
   (lambda (value)
     (with-accessors ((lambda1 linsearch-value-lambda1)
                      (lambda2 linsearch-value-lambda2)
                      (g1 linsearch-value-g1))
         value
       (cond ((< lambda1 (* min-coeff lambda2))
              (setf lambda1 (* min-coeff lambda2))
              (setf g1 (funcall g lambda1))
              value)
             ((> lambda1 (* max-coeff lambda2))
              (setf lambda1 (* max-coeff lambda2))
              (setf g1 (funcall g lambda1))
              value)
             (t value))))))

(defun linsearch-abs-min-lambda (abs-min-lambda)
  "FAILED-VALUE control if lambda becomes too small"
  (failed-value 
   (lambda (value)
     (with-accessors ((lambda1 linsearch-value-lambda1)) value
       (< lambda1 abs-min-lambda)))))

(defun linsearch-minimize (value g gamma g0 Dg0 g1 &rest other-controls)
  (reinit-value! value g0 Dg0 g1)
  (let ((step (apply #'combine-controls
                     (linsearch-finished-p *linsearch-alpha* gamma)
                     (new-lambda1 g)
                     (linsearch-lambda-keep-bound g 0.1d0 0.5d0)
                     (linsearch-abs-min-lambda *linsearch-abs-lambda-min*)
                     (limit-iterations *linsearch-max-iterations*)
                     other-controls)))
    (iterate step (iterator:continue value))))

(defclass linsearch ()
  ((linsearch-value
    :initarg :value
    :reader linsearch-value)
   (linsearch-other-controls
    :initarg :other-controls
    :reader linsearch-other-controls))
  (:documentation
   "Line search method"))

(defun make-linsearch (&rest other-controls)
  (make-instance 'linsearch
    :value (make-linsearch-value 0d0 0d0 0d0)
    :other-controls other-controls))

(defun linsearch (linsearch g gamma g0 Dg0 g1)
  "Perform line search"
  (let ((result (apply #'linsearch-minimize
                       (linsearch-value linsearch)
                       g
                       gamma
                       g0
                       Dg0
                       g1
                       (linsearch-other-controls linsearch))))
    (match result
      ((iterator:iterator :status :finished :value value)
       (values (linsearch-value-lambda1 value)
               T
               (linsearch-value-g1 value)))
      ((iterator:iterator :value value)
       (values 0d0 nil value)))))
