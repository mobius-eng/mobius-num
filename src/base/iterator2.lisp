(in-package mobius.numeric.iterator)

(defclass iterator ()
  ((status :initarg :status
           :accessor status
           :documentation "Iterator status: :continue :finished or :failed")
   (value :initarg :value
          :accessor value
          :documentation "Iterator value"))
  (:documentation "Computation flow status"))

(defmethod print-object ((obj iterator) out)
  (print-unreadable-object (obj out)
    (with-slots (status value) obj
      (format out "~A ~A" status value))))

(defun iterator-p (obj) (typep obj 'iterator))

(defun iterator (status value)
  (make-instance 'iterator :status status :value value))

(defun continue (val)
  "Make contine iterator"
  (iterator :continue val))

(defun failed (val)
  "Make failed iterator"
  (iterator :failed val))

(defun finished (val)
  "Make finished iterator"
  (iterator :finished val))

(declaim (inline status-p))
(defun status-p (obj status)
  "If OBJ is ITERATOR checks its STATUS"
  (and (iterator-p obj) (eq (slot-value obj 'status) status)))

(defun continue-p (iterator)
  "Is ITERATOR :continue?"
  (status-p iterator :continue))

(defun failed-p (iterator)
  "Is ITERATOR :failed?"
  (status-p iterator :failed))

(defun finished-p (iterator)
  "Is ITERATOR :finished?"
  (status-p iterator :finished))


(defun replace-value (iterator new-value)
  "Destrcutively change ITERATOR's value"
  (setf (value iterator) new-value)
  iterator)


(defun update-value (iterator update-function &rest args)
  "Destructively update ITERATOR's value"
  (setf (value iterator)
        (apply update-function (value iterator) args))
  iterator)


(defun change-status (iterator new-status &optional update-value-function)
  "Destructively change iterator's status"
  (setf (status iterator) new-status)
  (if update-value-function
      (update-value iterator update-value-function)
      iterator))

(defun ->continue (iterator &optional (update-value-function #'identity))
  (change-status iterator :continue update-value-function))

(defun ->failed (iterator &optional (update-value-function #'identity))
  (change-status iterator :failed update-value-function))

(defun ->finished (iterator &optional (update-value-function #'identity))
  (change-status iterator :finished update-value-function))

(defun bind (iterator function)
  "Propogate ITERATOR's value through the FUNCTION
if its status is :CONTINUE. Otherwise return ITERATOR
FUNCTION must return ITERATOR"
  (if (continue-p iterator)
      (funcall function (value iterator))
      iterator))

(defun fmap (iterator &rest functions)
  (if (continue-p iterator)
      (update-value iterator
                    (lambda (x)
                      (reduce (lambda (r f) (funcall f r))
                              functions
                              :initial-value x)))
      iterator))
