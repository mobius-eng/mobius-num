(in-package numeric-control)
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
  value)

(defmethod init-control (control init-value)
  (declare (ignore control init-value))
  nil)

;; ** Control primitives
;; *** Macro helpers
(defmacro define-simple-constructor (class (&rest args))
  "Make simple constuctor with boa arguments for CLASS"
  (let* ((only-args (remove-if (lambda (x) (eq x '&optional)) args))
         (kw-args (mapcan (lambda (x)
                            (if (consp x)
                                (list (make-keyword (car x)) (car x))
                                (list (make-keyword x) x)))
                          only-args)))
    `(defun ,class (,@args)
       (make-instance ',class
         ,@kw-args))))

;; *** FINISHED-VALUE
(defclass finished-value ()
  ((finished-value-p
    :initarg :predicate
    :reader finished-value-p
    :documentation "Value predicate to indicate that computation has finished")
   (finished-value-final
    :initarg :final
    :reader finished-value-final
    :documentation "Final function to apply to value once finished"))
  (:documentation
   "Controls if a computation has finished by testing the value"))

(define-simple-constructor finished-value (predicate &optional (final #'identity)))

(defmethod apply-control ((control finished-value) value)
  (with-accessors ((predicate finished-value-p)
                   (final finished-value-final))
      control
    (if (funcall predicate (iterator:value value))
        (iterator:->finished value final)
        value)))

;; *** FAILED-VALUE
(defclass failed-value ()
  ((failed-value-p
    :initarg :predicate
    :reader failed-value-p
    :documentation "Value predicate to indicate that computation has failed")
   (failed-value-final
    :initarg :final
    :reader failed-value-final
    :documentation "Final function to apply to value once failed"))
  (:documentation
   "Controls if a computations has failed by testing the value"))

(define-simple-constructor failed-value (predicate &optional (final #'identity)))

(defmethod apply-control ((control failed-value) value)
  (with-accessors ((predicate failed-value-p)
                   (final failed-value-final))
      control
    (if (funcall predicate (iterator:value value))
        (iterator:->failed value final)
        value)))

;; *** LIMIT-ITERATIONS
(defclass limit-iterations ()
  ((limit-iterations-max
    :initarg :max
    :reader limit-iterations-max
    :documentation "Max number of evaluations permitted")
   (limit-iterations-final
    :initarg :final
    :reader limit-iterations-final
    :documentation "Final function to apply to value once failed")
   (limit-iterations-performed
    :initform 0
    :accessor limit-iterations-performed
    :documentation
    "Internal storage of the number of evaluations already performed"))
  (:documentation
   "Control that fails computation if it exceeds max number of evaluations"))

(define-simple-constructor limit-iterations (max &optional (final #'identity)))

(defmethod init-control ((control limit-iterations) init-value)
  (declare (ignore init-value))
  (setf (limit-iterations-performed control) 0)
  nil)

(defmethod apply-control ((control limit-iterations) value)
  (with-accessors ((max limit-iterations-max)
                   (performed limit-iterations-performed)
                   (final limit-iterations-final))
      control
    (if (>= performed max)
        (iterator:->failed value final)
        (progn
          (incf performed)
          value))))

;; *** LOG-COMPUTATION
(defclass log-computation ()
  ((log-function
    :initarg :log-function
    :reader log-function
    :documentation "Function to log the computation process.
Accepts two arguments: (TAG VALUE)
TAG is either :INIT or :APPLY indicating at which point it was called
VALUE is ITERATOR's value passed."))
  (:documentation
   "Control that logs the progress of computation"))

(define-simple-constructor log-computation (log-function))

(defmethod init-control ((control log-computation) init-value)
  (funcall (log-function control) :init (iterator:value init-value)))

(defmethod apply-control ((control log-computation) value)
  (funcall (log-function control) :apply (iterator:value value))
  value)

;; *** CONVERGED-VALUE
(defclass converged-value ()
  ((converged-value-close-p
    :initarg :close-p
    :reader converged-value-close-p
    :documentation "Predicate that checks if two values are close enough
for the sequence to be considered convergent")
   (converged-value-copy
    :initarg :copy
    :reader converged-value-copy
    :documentation "Reliably copy the value: the value itself might be
detroyed during the computation, this function must be able to copy it
to reliably compare previus and current value in the sequence")
   (converged-value-final
    :initarg :final
    :reader converged-value-final
    :documentation "Final function to apply to value once converged")
   (converged-value-last
    :initform nil
    :accessor converged-value-last
    :documentation
    "Internal: stores the previous value (copied) of the sequence"))
  (:documentation
   "Controls if the sequence of values is converged (Cauchy criteria)
Since the value might change destructively, it requires COPY function
to store the copy of the previous value in the sequence"))

(define-simple-constructor converged-value (close-p copy &optional (final #'identity)))

(defmethod init-control ((control converged-value) init-value)
  (setf (converged-value-last control)
        (funcall (converged-value-copy control) (iterator:value init-value))))

(defmethod apply-control ((control converged-value) value)
  (with-accessors ((close-p converged-value-close-p)
                   (copy converged-value-copy)
                   (last-value converged-value-last)
                   (final converged-value-final))
      control
    (if (funcall close-p last-value (iterator:value value))
        (iterator:->finished value final)
        (progn
          (setf last-value (funcall copy (iterator:value value)))
          value))))

;; *** CONVERGED-NUMBER
(defclass converged-number ()
  ((converged-number-tolerance
    :initarg :tolerance
    :reader converged-number-tolerance
    :documentation
    "Absolute tolerance within which two numbers are considered to be the same")
   (converged-number-last
    :initform nil
    :accessor converged-number-last
    :documentation "Internal storage of the previous item in the sequence"))
  (:documentation
   "Control that will finish the computation if the number sequence converges.
This is a simplified (for numbers) version of CONVERGED-VALUE"))

(define-simple-constructor converged-number (tolerance))

(defmethod init-control ((control converged-number) init-value)
  (setf (converged-number-last control) (iterator:value init-value)))

(defmethod apply-control ((control converged-number) value)
  (if (< (abs (- (iterator:value value) (converged-number-last control)))
         (converged-number-tolerance control))
      (iterator:->finished value)
      (progn
        (setf (converged-number-last control) (iterator:value value))
        value)))

;; *** ALTER-VALUE
(defclass alter-value ()
  ((alter-value-function
    :initarg :function
    :reader alter-value-function
    :documentation
    "Function that alters the value. Must return ITERATOR"))
  (:documentation
   "Control that alters the value if, for example, it is outside
of some boundaries. The behaviour is control by FUNCTION."))

(define-simple-constructor alter-value (function))

(defmethod apply-control ((control alter-value) value)
  (iterator:update-value value (alter-value-function control)))

;; ** Combinators on controls
;; *** Most general control
(defclass control ()
  ((control-init-function
    :initarg :init
    :accessor control-init-function
    :documentation "A generic control init function")
   (control-apply-function
    :initarg :apply
    :accessor control-apply-function
    :documentation "A generic control apply function"))
  (:documentation
   "A generic control class. It is used to combine other controls."))

(define-simple-constructor control (init apply))

(defmethod init-control ((control control) init-value)
  (funcall (control-init-function control) init-value))

(defmethod apply-control ((control control) value)
  (funcall (control-apply-function control) value))
;; *** Combination
;; **** Reduction with short-circuiting
(defun reduced (x) (cons 'reduced x))
(defun reduced-p (x) (and (consp x) (eq (car x) 'reduced)))
(defun reduced-value (x) (cdr x))

(defun reduce-list (function init-state list)
  (if (reduced-p init-state)
      (reduced-value init-state)
      (match list
        (nil init-state)
        ((list* hd tl) (reduce-list function (funcall function init-state hd) tl)))))
;; **** Control combination
(defun combine-controls (&rest controls)
  "Combine controls forming a generic control. Controls are applied
in order they appear"
  (flet ((init-function (init-value)
           (dolist (c controls)
             (init-control c init-value)))
         (apply-function (value)
           (reduce-list
            (lambda (iter-value control)
              (if (iterator:continue-p iter-value)
                  (apply-control control iter-value)
                  (reduced iter-value)))
            value
            controls)))
    (make-instance 'control
      :init #'init-function
      :apply #'apply-function)))
