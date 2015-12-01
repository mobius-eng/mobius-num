(in-package numeric-linsearch)

;; * Line search algorithm

;; ** Line search state representation
(defclass linsearch-value ()
  ((linsearch-value-lambda1
    :initarg :lambda1
    :type double-float
    :accessor linsearch-value-lambda1)
   (linsearch-value-lambda2
    :initarg :lambda2
    :initform nil
    :type double-float
    :accessor linsearch-value-lambda2)
   (linsearch-value-g1
    :initarg :g1
    :type double-float
    :accessor linsearch-value-g1)
   (linsearch-value-g2
    :initarg :g2
    :initform nil
    :type double-float
    :accessor linsearch-value-g2)
   (linsearch-g0
    :initarg :g0
    :type double-float
    :accessor linsearch-value-g0)
   (linsearch-dg0
    :initarg :dg0
    :type double-float
    :accessor linsearch-value-dg0)))

(defun make-linsearch-value (g0 Dg0 g1 &optional (lambda1 0d0) lambda2 g2)
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
  (with-accessors ((lambda1 linsearch-value-lambda1)
                   (lambda2 linsearch-value-lambda2)
                   (g1 linsearch-value-g1)
                   (g2 linsearch-value-g2))
      value
    (setf lambda2 lambda1)
    (setf g2 g1)))

(defun update-1! (value new-lambda1 new-g1)
  (with-accessors ((lambda1 linsearch-value-lambda1)
                   (g1 linsearch-value-g1))
      value
    (setf lambda1 new-lambda1)
    (setf g1 new-g1)))

(defun reinit-value! (value new-g0 new-dg0 new-g1)
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

;; ** Control

(defvar *linsearch-alpha* 1d-4)
(defvar *linsearch-abs-lambda-min* 1d-3)
(defvar *linsearch-max-iterations* 10)

(defun linsearch-finished-p (alpha gamma)
  (lambda (value)
    (with-accessors ((g0 linsearch-value-g0)
                     (lambda1 linsearch-value-lambda1)
                     (g1 linsearch-value-g1))
        value
      (let ((gamma-value (funcall gamma value)))
        (< g1 (+ g0 (* alpha gamma-value)))))))

(defun newton-step-gamma (value)
  (* -2.0d0 (linsearch-value-g0 value) (linsearch-value-lambda1 value)))

(defun linsearch-lambda-keep-bound (g min-coeff max-coeff)
  (lambda (value)
    (with-accessors ((lambda1 linsearch-value-lambda1)
                     (lambda2 linsearch-value-lambda2)
                     (g1 linsearch-value-g1))
        value
      (cond ((< lambda1 (* min-coeff lambda2))
             (setf lambda1 (* min-coeff lambda2))
             (setf g1 (funcall g lambda1))
             (iterator:continue value :limit-lambda :min))
            ((> lambda1 (* max-coeff lambda2))
             (setf lambda1 (* max-coeff lambda2))
             (setf g1 (funcall g lambda1))
             (iterator:continue value :limit-lambda :max))
            (t (iterator:continue value))))))

(defun linsearch-abs-min-lambda (abs-min-lambda)
  (lambda (value)
    (with-accessors ((lambda1 linsearch-value-lambda1)) value
      (< lambda1 abs-min-lambda))))

(defclass linsearch-control ()
  ((linsearch-control-g
    :initarg :g
    :accessor linsearch-control-g)
   (linsearch-control-gamma
    :initarg :gamma
    :accessor linsearch-control-gamma)
   (linsearch-control-others
    :initarg :others
    :accessor linsearch-control-others)
   (linsearch-control-compiled
    :initform nil
    :accessor linsearch-control-compiled)))

(defun compile-linsearch-control (linsearch-control)
  (with-slots ((g linsearch-control-g)
               (gamma linsearch-control-gamma)
               (others linsearch-control-others)
               (compiled linsearch-control-compiled))
      linsearch-control
    (unless (or (null g) (null gamma))
      (setf
       compiled
       (apply 
        #'combine-controls
        (finished-value (linsearch-finished-p *linsearch-alpha* gamma)
                        (lambda (value) (linsearch-value-lambda1 value)))
        (alter-value (linsearch-lambda-keep-bound g 0.1d0 0.5d0))
        (linsearch-abs-min-lambda *linsearch-abs-lambda-min*)
        (limit-iterations *linsearch-max-iterations*)
        others)))))

(defmethod initialize-instance :after ((obj linsearch-control) &key)
  (compile-linsearch-control obj))

(defmethod (setf linsearch-control-g) :after (new-g (obj linsearch-control))
  (declare (ignore new-g))
  (compile-linsearch-control obj))

(defmethod (setf linsearch-control-gamma) :after (new-gamma (obj linsearch-control))
  (declare (ignore new-gamma))
  (compile-linsearch-control obj))

(defmethod (setf linsearch-control-others) :after (new-others (obj linsearch-control))
  (declare (ignore new-others))
  (compile-linsearch-control obj))

(defun make-linsearch-control (&optional g gamma other-controls)
  (make-instance 'linsearch-control
    :g g
    :gamma gamma
    :others other-controls))

(defun reinit-control! (linsearch-control &key g gamma (other-controls nil others-p))
  (with-slots ((g-slot linsearch-control-g)
               (gamma-slot linsearch-control-gamma)
               (others-slot linsearch-control-others))
      linsearch-control
    (let (needs-compilation)
      (when (and g (not (eq g-slot g)))
        (setf g-slot g)
        (setf needs-compilation t))
      (when (and gamma (not (eq gamma-slot gamma)))
        (setf gamma-slot gamma)
        (setf needs-compilation t))
      (when (and others-p (not (equal others-slot other-controls)))
        (setf others-slot other-controls)
        (setf needs-compilation t))
      (when needs-compilation
        (compile-linsearch-control linsearch-control)))))

(defmethod init-control ((control linsearch-control) init-value)
  (init-control (linsearch-control-compiled control) init-value))

(defmethod apply-control ((control linsearch-control) value)
  (apply-control (linsearch-control-compiled control) value))

(defclass linsearch ()
  ((linsearch-value
    :initarg :value
    :reader linsearch-value)
   (linsearch-control
    :initarg :control
    :reader linsearch-control)))


(defun make-linsearch (&optional g gamma &rest other-controls)
  (make-instance 'linsearch
    :value (make-linsearch-value 0d0 0d0 0d0)
    :control (make-linsearch-control g gamma other-controls)))

(defun reinit! (linsearch g0 Dg0 g1 &key g gamma (other-controls nil others-p))
  (reinit-value! (linsearch-value linsearch) g0 Dg0 g1)
  (if others-p
      (reinit-control! (linsearch-control linsearch)
                       :g g
                       :gamma gamma
                       :other-controls other-controls)
      (reinit-control! (linsearch-control linsearch) :g g :gamma gamma)))

(defun linsearch-step (g)
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
         (multiple-value-bind (a b c) (cubic-approximation g0 dg0 lambda1 g1 lambda2 g2)
           (let ((new-lambda1 (cubic-extremum-2 a b c)))
             (move-1->2! value)
             (update-1! value new-lambda1 (funcall g new-lambda1))
             value)))))))

(defun linsearch (linsearch g g0 Dg0 g1 &key gamma (other-controls nil others-p))
  (apply #'reinit!
         linsearch g0 Dg0 g1
         (nconc `(:g ,g :gamma ,gamma)
                (and others-p `(:other-controls ,other-controls))))
  (let ((stepper (linsearch-step g)))
    (fixed-point (linsearch-control linsearch) stepper (linsearch-value linsearch))))





#|
(defclass linsearch-control ()
  ((linsearch-control-gamma
    :initarg :gamma
    :initform nil
    :reader linsearch-control-gamma)
   (linsearch-control-alpha
    :initarg :alpha
    :reader linsearch-control-alpha)))


(defun make-linsearch-control (gamma &optional (alpha *linsearch-alpha*))
  (make-instance 'linsearch-control
    :alpha alpha
    :gamma gamma))

(defclass linsearch-control-newton-step (linsearch-control)
  ())

(defun make-linsearch-control-newton-step (&optional (alpha *linsearch-alpha*))
  (make-instance 'linsearch-control-newton-step
    :alpha alpha
    :gamma nil))

(defmethod linsearch-control-gamma ((obj linsearch-control-newton-step))
  (lambda (x)
    (* -2d0 x g0)))

(defun reinit-linsearch-control! (control g0)
  (setf (linsearch-control-g0 control) g0))

(defmethod apply-control ((control linsearch-control) value)
  (with-accessors ((lambda1 linsearch-value-lambda1)
                   (g1 linsearch-value-g1))
      value
    (with-accessors ((g0 linsearch-control-g0)
                     (gamma linsearch-control-gamma)
                     (alpha linsearch-control-alpha))
        control
      (if (< g1 (+ g0 (* alpha (funcall gamma lambda1))))
          (iterator:finished value)
          (iterator:continue value)))))


(defun limit-lambda (g)
  (lambda (value)
    (with-accessors ((lambda1 linsearch-value-lambda1)
                     (lambda2 linsearch-value-lambda2)
                     (g1 linsearch-value-g1))
        value
      (cond ((< lambda1 (* 0.1d0 lambda2))
             (setf lambda1 (* 0.1d0 lambda2))
             (setf g1 (funcall g lambda1))
             (iterator:continue value :limit-lambda :min))
            ((> lambda1 (* 0.5d0 lambda2))
             (setf lambda1 (* 0.5d0 lambda2))
             (setf g1 (funcall g lambda1))
             (iterator:continue value :limit-lambda :max))
            (t (iterator:continue value))))))

(defun abs-limit-lambda (abs-lambda-min)
  (lambda (value)
    (with-accessors ((lambda1 linsearch-value-lambda1)) value
      (< lambda1 abs-lambda-min))))




|#
