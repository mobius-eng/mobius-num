(in-package control)
;; * Computation control

;; ** Control protocol
(defgeneric apply-control (control value)
  (:documentation
   "Apply CONTROL to a VALUE. CONTROL must return ITERATOR"))

(defgeneric init-control (control init-value)
  (:documentation
   "Initialize CONTROL using INIT-VALUE. Useful for stateful controls"))

;; ** Default implementations
(defmethod apply-control (control value)
  (iterator:continue value))

(defmethod init-control (control init-value)
  (declare (ignore control init-value))
  nil)

;; ** Control primitives
;; *** Macro helpers
(defmacro define-simple-constructor (class (&rest args))
  (let ((kw-args (mapcan (lambda (x) (list (make-keyword x) x)) args)))
    `(defun ,class (,@args)
       (make-instance ',class
         ,@kw-args))))

;; *** FINISHED-VALUE
(defclass finished-value ()
  ((finished-value-p :initarg :predicate :reader finished-value-p)
   (finished-value-info :initarg :info-function :reader finished-value-info)))

(define-simple-constructor finished-value (predicate info-function))

(defmethod apply-control ((control finished-value) value)
  (if (funcall (finished-value-p control) value)
      (iterator:finished value :finished-value (funcall (finished-value-info control) value))
      (iterator:continue value)))

;; *** FAILED-VALUE
(defclass failed-value ()
  ((failed-value-p :initarg :predicate :reader failed-value-p)
   (failed-value-info :initarg :info-function :reader failed-value-info)))

(define-simple-constructor failed-value (predicate info-function))

(defmethod apply-control ((control failed-value) value)
  (if (funcall (failed-value-p control) value)
      (iterator:failed value :failed-value (funcall (failed-value-info control) value))
      (iterator:continue value)))

;; *** LIMIT-ITERATIONS
(defclass limit-iterations ()
  ((limit-iterations-max :initarg :max :reader limit-iterations-max)
   (limit-iterations-performed :initform 0 :accessor limit-iterations-performed)))

(define-simple-constructor limit-iterations (max))

(defmethod init-control ((control limit-iterations) init-value)
  (declare (ignore init-value))
  (setf (limit-iterations-performed control) 0)
  nil)

(defmethod apply-control ((control limit-iterations) value)
  (if (>= (limit-iterations-performed control)
          (limit-iterations-max control))
      (iterator:failed
       value
       :exceeded-limit-iterations (limit-iterations-max control))
      (progn
        (incf (limit-iterations-performed control))
        (iterator:continue value))))

;; *** LOG-COMPUTATION
(defclass log-computation ()
  ((log-function :initarg :log-function :reader log-function)))

(define-simple-constructor log-computation (log-function))

(defmethod init-control ((control log-computation) init-value)
  (funcall (log-function control) :init init-value))

(defmethod apply-control ((control log-computation) value)
  (funcall (log-function control) :apply value)
  (iterator:continue value))


;; *** LOG-TO-INFO
(defclass log-to-info ()
  ((log-to-info-function :initarg :log-function :reader log-to-info-function)))


(define-simple-constructor log-to-info (log-function))

(defmethod apply-control ((control log-to-info) value)
  (iterator:continue value
                     :info-log (funcall (log-to-info-function control) value)))

;; *** CONVERGED-VALUE
(defclass converged-value ()
  ((converged-value-close-p :initarg :close-p :reader converged-value-close-p)
   (converged-value-copy :initarg :copy :reader converged-value-copy)
   (converged-value-last :initform nil :accessor converged-value-last)
   (converged-value-info :initarg :info-function :reader converged-value-info)))


(define-simple-constructor converged-value (close-p copy info-function))

(defmethod init-control ((control converged-value) init-value)
  (setf (converged-value-last control)
        (funcall (converged-value-copy control) init-value)))


(defmethod apply-control ((control converged-value) value)
  (if (funcall (converged-value-close-p control) value (converged-value-last control))
      (iterator:finished value
                         :converged-value (funcall (converged-value-info control) value))
      (progn
        (setf (converged-value-last control)
              (funcall (converged-value-copy control) value))
        (iterator:continue value))))


;; *** CONVERGED-NUMBER
(defclass converged-number ()
  ((converged-number-tolerance :initarg :tolerance :reader converged-number-tolerance)
   (converged-number-last :initform nil :accessor converged-number-last)))

(define-simple-constructor converged-number (tolerance))


(defmethod init-control ((control converged-number) init-value)
  (setf (converged-number-last control) init-value))


(defmethod apply-control ((control converged-number) value)
  (if (< (abs (- value (converged-number-last control)))
         (converged-number-tolerance control))
      (iterator:finished value)
      (progn
        (setf (converged-number-last control) value)
        (iterator:continue value))))

;; *** ALTER-VALUE
(defclass alter-value ()
  ((alter-value-function
    :initarg :function
    :reader alter-value-function)))

(define-simple-constructor alter-value (function))

(defmethod apply-control ((control alter-value) value)
  (funcall (alter-value-function control) value))

;; ** Combinators on controls
;; *** Most general control
(defclass control ()
  ((control-init-function :initarg :init :accessor control-init-function)
   (control-apply-function :initarg :apply :accessor control-apply-function)))

(define-simple-constructor control (init apply))

(defmethod init-control ((control control) init-value)
  (funcall (control-init-function control) init-value))

(defmethod apply-control ((control control) value)
  (funcall (control-apply-function control) value))
;; *** Combination

(defun reduced (x) (cons 'reduced x))
(defun reduced-p (x) (and (consp x) (eq (car x) 'reduced)))
(defun reduced-value (x) (cdr x))

(defun reduce-list (function init-state list)
  (if (reduced-p init-state)
      (reduced-value init-state)
      (match list
        (nil init-state)
        ((list* hd tl) (reduce-list function (funcall function init-state hd) tl)))))

(defun combine-controls (&rest controls)
  (flet ((init-function (init-value)
           (mapc (lambda (control)
                   (init-control control init-value))
                 controls))
         (apply-function (value)
           (reduce-list
            (lambda (iter-value control)
              (match iter-value
                ((iterator:iterator :status :continue)
                 (iterator:bind iter-value (lambda (x) (apply-control control x))))
                (otherwise (reduced iter-value))))
            (iterator:continue value)
            controls)))
    (make-instance 'control
      :init #'init-function
      :apply #'apply-function)))
