(in-package mobius.numeric.ode)

;; * General functionality for ODE solution

;; ** ODE solution state
(defclass ode-state ()
  ((ode-state-time
    :initarg :time
    :accessor ode-state-time)
   (ode-state-value
    :initarg :value
    :accessor ode-state-value)
   (ode-state-rate
    :initarg :rate
    :accessor ode-state-rate)))

(defun ode-state (time value rate)
  (make-instance 'ode-state :time time :value value :rate rate))

(defun ode-state-init-rate (ode-state ode-function)
  (with-accessors ((time ode-state-time)
                   (value ode-state-value)
                   (rate ode-state-rate))
      ode-state
   (copy-vector-to! (funcall ode-function time value) rate)))

(defmethod print-object ((obj ode-state) out)
  (print-unreadable-object (obj out :type t)
    (with-accessors ((time ode-state-time)
                     (value ode-state-value)
                     (rate ode-state-rate))
        obj
      (format out "~F ~A ~A" time value rate))))

(defclass ode-error ()
  ((ode-error-scale
    :initarg :scale
    :reader ode-error-scale)
   (ode-error-tolerance
    :initarg :tolerance
    :reader ode-error-tolerance)))

(defun ode-error (scale tolerance)
  (make-instance 'ode-error :tolerance tolerance :scale scale))


(defclass ode-function ()
  ((ode-function-f :initarg :function :reader ode-function-f)
   (ode-function-jacobian :initarg :jacobian :initform nil :reader ode-function-jacobian))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod initialize-instance :after ((odef ode-function) &key)
  (with-accessors ((fun ode-function-f)) odef
    (closer-mop:set-funcallable-instance-function odef fun)))

(defun ode-function (f &optional jacobian)
  (make-instance 'ode-function
    :function f :jacobian jacobian))


(defgeneric ode-attempt-step (method ode-function state time-step ode-error)
  (:documentation
   "Attempt step with TIME-STEP from initial STATE using METHOD.
Returns (VALUES SUCCESSFUL-P RECOMMENDED-STEP)
This operation should not modify the STATE!"))

(defgeneric ode-perform-step (method ode-function state time-step)
  (:documentation
   "Perform actual step (by modifying STATE). Is called if ODE-ATTEMPT-STEP
is successful. Return value is ignored"))

(defun ode-try-step-function (method ode-function state ode-error)
  (lambda (x)
    (let ((step (third x)))
      (format t "~&Trying step ~F~%" step)
      (multiple-value-bind (successful-p recommended-step)
          (ode-attempt-step method ode-function state step ode-error)
        (list successful-p step recommended-step)))))

(defun  ode-step (method ode-function state time-step ode-error)
  (let ((t0 (ode-state-time state)))
    (let ((init-attempt (list nil nil time-step))
          (control (combine-controls
                    (finished-value #f(first %))
                    (failed-value #f(= t0 (+ t0 (third %)))))))
      (match (fixed-point control
                          (ode-try-step-function method ode-function state ode-error)
                          init-attempt)
        ((iterator:iterator :status :finished
                            :value (list sccss-p tried-step recommended-step))
         (declare (ignore sccss-p))
         (ode-perform-step method ode-function state tried-step)
         (values state recommended-step))
        ((iterator:iterator :status :failed)
         (error "Step is too small"))
        (z (error "Unexpected result ~A" z))))))



(defun ode-next-step (method ode-function ode-error)
  (lambda (x)
    (destructuring-bind (state next-step recommended-step mt) x
      (multiple-value-bind (new-state new-recommended-step)
          (ode-step method ode-function state next-step ode-error)
        (cond ((> recommended-step next-step)
               (if (> new-recommended-step recommended-step)
                   (list new-state new-recommended-step new-recommended-step mt)
                   (list new-state recommended-step recommended-step mt)))
              (t (list new-state new-recommended-step new-recommended-step mt)))))))

(defun close-to-monitor-adjust-step ()
  (alter-value
   (lambda (x)
     (destructuring-bind (state next-step recommended-step monitor-time) x
         (let ((current-time (ode-state-time state)))
           (if (> (+ current-time next-step) (first monitor-time))
               (list state
                     (- (first monitor-time) current-time)
                     recommended-step
                     monitor-time)
               x))))))

(defun pop-monitor-time ()
  (alter-value
   (lambda (x)
     (destructuring-bind (state next-step recommended-step monitor-time) x
       (if (>= (ode-state-time state) (first monitor-time))
           (list state next-step recommended-step (rest monitor-time))
           x)))))

(defun perform-monitoring (monitor)
  (log-computation
   (lambda (tag x)
     (declare (ignore tag))
     (destructuring-bind (state x y monitor-time) x
         (declare (ignore x y))
       (when (>= (ode-state-time state) (first monitor-time))
         (funcall monitor state))))))

(defun ode-evolve-finished ()
  (finished-value
   (lambda (x)
     (destructuring-bind (p y z monitor-time) x
       (declare (ignore p y z))
       (null monitor-time)))))

(defun ode-evolve (method ode-function init-state monitor-time monitor ode-error)
  (let ((dt (- (first monitor-time) (ode-state-time init-state))))
    (let ((result (fixed-point (combine-controls (perform-monitoring monitor)
                                                 (pop-monitor-time)
                                                 (ode-evolve-finished)
                                                 (close-to-monitor-adjust-step))
                               (ode-next-step method ode-function ode-error)
                               (list init-state
                                     dt
                                     dt
                                     monitor-time))))
      (match result
        ((iterator:iterator :status :finished :value value)
         (first value))
        (otherwise
         (error "ODE-EVOLVE failed. Final state ~A" result))))))




#|


(defstruct ode-step
  (requested 0.0d0) (performed 0.0d0) (recommended 0.0d0))

(defstruct ode-error-control
  value-scale error)

(defun duplicate-error-control (err)
  (declare (type ode-step err))
  (make-ode-error-control
   :value-scale (duplicate-vector (slot-value err 'value-scale))
   :error (duplicate-vector (slot-value err 'error))))


(defstruct ode
  init-state
  next-state
  step
  error-control)

(defun init-ode (init-ode-state
                 &optional (value-scale (one-vector (ode-state-value init-ode-state))))
  (make-ode :init-state init-ode-state
            :next-state (duplicate-ode-state init-ode-state)
            :step (make-ode-step)
            :error-control (make-ode-error-control :value-scale value-scale
                                                   :error (duplicate-vector
                                                           value-scale))))

(defun duplicate-ode (ode)
  (declare (type ode ode))
  (make-ode :init-state (duplicate-ode-state (slot-value ode 'init-state))
            :next-state (duplicate-ode-state (slot-value ode 'next-state))
            :step (copy-ode-step (slot-value ode 'step))
            :error (duplicate-error-control (slot-value ode 'error))))

(defun ode-request-step! (ode step)
  (setf (ode-step-requested (ode-step ode)) step)
  ode)

(defgeneric ode-advance (method))

(defun ode-step (method function ode &optional
                                       (buffer1 (duplicate-ode ode))
                                       (buffer2 (duplicate-ode ode)))
  (fixed-point *ode-successful-step-criteria*
               (ode-advance method)
               (cons method ode)
               (cons method buffer1)
               (cond method buffer2)))










(defmacro setf-if-nil (object &body place-values)
  (with-gensyms (gobj)
    (let ((setf-expressions (mapcar #'(lambda (entry)
                                        `(unless (slot-value ,gobj ',(car entry))
                                           (setf (slot-value ,gobj ',(car entry))
                                                 ,(cadr entry))))
                                    place-values)))
     `(let ((,gobj ,object))
        ,@setf-expressions))))

(defmacro update-if-argument (object &body slot-arg-value-alt)
  (with-gensyms (gobj)
    (let ((setf-expressions
           (mapcar #'(lambda (entry)
                       (destructuring-bind (slot value alt) entry
                           `(if ,slot
                                (setf (slot-value ,gobj ',slot) ,value)
                                (setf (slot-value ,gobj ',slot) ,alt))))
                   slot-arg-value-alt)))
      `(let ((,gobj ,object))
         #+sbcl(declare (SB-PCL::%VARIABLE-REBINDING ,gobj ,object))
         ,@setf-expressions))))




(defmacro update-slots (object &body slot-values)
  (with-gensyms (gobj)
    (let ((setf-expressions (mapcar #'(lambda (entry)
                                        `(setf (slot-value ,gobj ',(car entry))
                                               ,(cadr entry)))
                                    slot-values)))
      `(let ((,gobj ,object))
         ,@setf-expressions))))

(defun init-ode (starting-time starting-value starting-rate)
  (make-instance 'ode
                 :t0 starting-time
                 :u0 starting-value
                 :f0 starting-rate))

(defun ode-update (ode dest &key performed-step recommended-step next-value
                              next-rate computation-error requested-step)
  (update-slots dest
    (starting-time (ode-starting-time ode))
    (starting-value (duplicate-vector (ode-starting-value ode)
                                      (ode-starting-value dest)))
    (starting-rate (duplicate-vector (ode-starting-rate ode)
                                     (ode-starting-rate dest)))
    (next-time (if performed-step
                   (+ performed-step (ode-starting-time ode))
                   (ode-starting-time ode)))
    (value-scale (ode-value-scale ode)))
  (update-if-argument dest
    (requested-step requested-step (ode-requested-step ode))
    (recommended-step recommended-step (ode-recommended-step ode))
    (performed-step performed-step (ode-performed-step ode))
    (next-value (duplicate-vector next-value (slot-value dest 'next-value))
                (duplicate-vector (ode-next-value ode) (slot-value dest 'next-value)))
    (next-rate (duplicate-vector next-rate (slot-value dest 'next-rate))
               (duplicate-vector (ode-next-rate ode) (slot-value dest 'next-rate)))
    (computation-error (duplicate-vector computation-error
                                         (slot-value dest 'computation-error))
                       (duplicate-vector (ode-computation-error ode)
                                         (slot-value dest 'computation-error))))
  (setf (ode-method dest) (ode-method ode))
  dest)

(defun duplicate-ode (ode)
  (with-slots (starting-time starting-value starting-rate
               requested-step performed-step recommended-step
               next-time next-value next-rate
               computation-error method value-scale) ode
    (make-instance 'ode
                   :t0 starting-time :h requested-step :h-did performed-step
                   :h-do recommended-step :t next-time :method method
                   :u0 starting-value :f0 starting-rate
                   :u (duplicate-vector next-value)
                   :f (duplicate-vector next-rate)
                   :error (duplicate-vector computation-error)
                   :u-scale (duplicate-vector value-scale))))

(defun ode-request-step! (ode step)
  (setf (slot-value ode 'requested-step) step)
  ode)

(defun ode-advance! (ode)
  (setf (ode-starting-time ode) (ode-next-time ode))
  (setf (ode-starting-value ode) (ode-next-value ode))
  (setf (ode-starting-rate ode) (ode-next-rate ode))
  (setf (ode-requested-step ode) (ode-recommended-step ode))
  (setf (ode-performed-step ode) 0.0d0)
  (setf (ode-recommended-step ode) 0.d0)
  (setf (ode-next-value ode) (ode-starting-value ode))
  (setf (ode-next-rate ode) (ode-starting-rate ode))
  ode)

(defmethod compile-criterium ((type (eql :ode-too-small-step)) &rest args)
  (let ((info (first args)))
    (compile-criterium :failed-value
                       #f(num= (ode-starting-time %) (+ (ode-starting-time %)
                                                        (ode-requested-step %)))
                       info)))

(defmethod compile-criterium ((type (eql :ode-error-control)) &rest args)
  (destructuring-bind (tolerance &optional info) args
    (in-criterium (x (buffer nil))
      (unless buffer
        (setf buffer (zero-vector (ode-starting-value (iterator:value x)))))
      (let* ((ode (iterator:value x))
             (computation-error (/ (norm (e=/! buffer
                                               (ode-computation-error ode)
                                               value-scale))
                                   tolerance))
             (new-step (ode-step-rules (ode-method ode) ode computation-error)))
        (cond ((< computation-error 1.0d0)
               (setf (ode-recommended-step ode) new-step)
               (iterator:add-info! (iterator:to-finished! x) :ode-error-control info))
              (t (setf (ode-requested-step ode) new-step)))))))

(defvar *ode-successful-step-criteria*
  (make-criteria
   :ode-error-control '(1.0d-7 "ODE: error <= tolerance achieved")
   :ode-too-small-step "ODE: cannot control the error, step = 0"
   :limit-iterations '(10 "ODE: max iterations reached")))

(defgeneric ode-step (method f ode)
  (:documentation "Generic ode stepper method"))

(defmethod ode-step :BEFORE (method f ode)
  (declare (type ode ode))
  (format t "Colling ODE-STEP :BEFORE")
  (setf (slot-value ode 'method) method))

(defgeneric ode-step-rules (method ode computation-error)
  (:documentation "Produces a new step for a particular METHOD based on
performed step, computation error etc. from ODE"))

(defun ode-solve (function starting-time starting-value output-time &optional method)
  )

|#
