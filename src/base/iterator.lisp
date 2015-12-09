(in-package mobius.numeric.iterator)

(defclass iterator ()
  ((status :initarg :status
           :accessor status
           :documentation "Iterator status: :continue :finished or :failed")
   (value :initarg :value
          :accessor value
          :documentation "Iterator value")
   (info :initarg :info
         :initform nil
         :accessor info
         :documentation "Additional info (alist) of iterator status (meta-data)"))
  (:documentation "Computation flow status"))

(defmethod print-object ((obj iterator) out)
  (print-unreadable-object (obj out)
    (with-slots (status value info) obj
      (format out "~A ~A (INFO~%~4T~{~A~^~%~4T~})" status value info))))

(defun iterator-p (obj) (typep obj 'iterator))

(defun continue (val &optional key info)
  "Make contine iterator"
  (make-instance 'iterator
                 :status :continue
                 :value val
                 :info (if key (list (cons key info)) nil)))

(defun failed (val &optional key info)
  "Make failed iterator"
  (make-instance 'iterator
                 :status :failed
                 :value val
                 :info (if key (list (cons key info)) nil)))

(defun finished (val &optional key info)
  "Make finished iterator"
  (make-instance 'iterator
                 :status :finished
                 :value val
                 :info (if key (list (cons key info)) nil)))

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

(defun add-info (iterator key more-info)
  "Add extra info into ITERATOR"
  (push (cons key more-info) (info iterator))
  iterator)

(defun update-info (iterator update-function &rest args)
  "Destructively update info of ITERATOR"
  (setf (info iterator) (apply update-function (info iterator) args))
  iterator)


(defun change-status (iterator new-status)
  "Destructively change iterator's status"
  (setf (status iterator) new-status)
  iterator)

(defun ->continue (iterator) (change-status iterator :continue))
(defun ->failed (iterator) (change-status iterator :failed))
(defun ->finished (iterator) (change-status iterator :finished))

(defun replace-value (iterator new-value)
  "Destrcutively change ITERATOR's value"
  (setf (value iterator) new-value)
  iterator)


(defun update-value (iterator update-function &rest args)
  "Destructively update ITERATOR's value"
  (setf (value iterator)
        (apply update-function (value iterator) args))
  iterator)


(defun bind (iterator function)
  "Propogate ITERATOR's value through the FUNCTION
if its status is :CONTINUE. Otherwise return ITERATOR
FUNCTION must return ITERATOR
Preserves the INFO of ITERATOR"
  (if (continue-p iterator)
      (update-info (funcall function (value iterator))
                   (lambda (new-info) (nconc new-info (info iterator))))
      iterator))


