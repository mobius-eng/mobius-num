(in-package mobius.numeric.criteria)

(defclass criteria ()
  ((list :initarg :list :accessor criteria-list)
   (function :accessor criteria-function)))

(defmethod print-object ((obj criteria) out)
  (print-unreadable-object (obj out :type t)
    (format out "~A" (slot-value obj 'list))))

(defgeneric compile-criterium (type &rest args)
  (:documentation "Make criterium function out of the criterium description"))

(defun build-criteria-function (criteria-list)
  (let ((all-criteria (mapcar #'(lambda (entry)
                                  (apply #'compile-criterium (car entry) (cdr entry)))
                              criteria-list)))
    #'(lambda (x)
        (let ((y (iterator:continue x)))
          (loop for c in all-criteria
             if (iterator:continue-p y)
             do (setf y (funcall c y))
             else
             return y
             end
             finally (return y))))))

(defun make-criteria (&rest entries)
  (let* ((list (mapcar #'(lambda (entry)
                           (cons (car entry)
                                 (if (listp (cdr entry))
                                     (cdr entry)
                                     (list (cdr entry)))))
                       (plist-alist entries)))
         (criteria (make-instance 'criteria :list list)))
    (declare (type criteria criteria))
    (setf (slot-value criteria 'function)
          (build-criteria-function (slot-value criteria 'list)))
    criteria))

(defun add-to-criteria (criteria id new-criterium)
  (push (cons id new-criterium) (slot-value criteria 'list))
  (setf (slot-value criteria 'function)
          (build-criteria-function (slot-value criteria 'list)))
  criteria)

(defun delete-from-criteria (criteria id)
  (setf (slot-value criteria 'list)
        (delete id (slot-value criteria 'list) :key #'car))
  (setf (slot-value criteria 'function)
          (build-criteria-function (slot-value criteria 'list)))
  criteria)

(defmacro in-criterium ((x &rest init) &body body)
  (if init
      `(let ,init
         (lambda (,x) ,@body ,x))
      `(lambda (,x) ,@body ,x)))

(defmethod compile-criterium ((type (eql  :finished-value)) &rest args)
  (let ((predicate (first args))
        (info (second args)))
    (in-criterium (x)
      (when (funcall predicate (iterator:value x))
        (iterator:add-info (iterator:->finished x) :finished-value info)))))

(defmethod compile-criterium ((type (eql :failed-value)) &rest args)
  (let ((predicate (first args))
        (info (second args)))
    (in-criterium (x)
      (when (funcall predicate (iterator:value x))
        (iterator:add-info (iterator:->failed x) :failed-value info)))))

(defmethod compile-criterium ((type (eql :log-value)) &rest args)
  (let ((msg-function (first args)))
    (in-criterium (x)
      (funcall msg-function (iterator:value x)))))

(defmethod compile-criterium ((type (eql :limit-iterations)) &rest args)
  (let ((max-count (first args))
        (info (second args)))
    (in-criterium (x (left max-count))
      (decf left)
      (when (<= left 0)
        (iterator:add-info (iterator:->failed x)
                           :limit-iterations
                           (if info
                               (format nil info)
                               (format nil "Max count reached ~D" max-count)))))))

(defmethod compile-criterium ((type (eql :modify-value)) &rest args)
  (let ((modifier (first args))
        (info (second args)))
    (in-criterium (x)
      (iterator:replace-value x (funcall modifier (iterator:value x)))
      (when info
        (iterator:add-info x :modify-value info)))))

