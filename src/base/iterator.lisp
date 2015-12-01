(in-package mobius.numeric.iterator)

(defclass iterator ()
  ((status :initarg :status :accessor status)
   (value :initarg :value :accessor value)
   (info :initarg :info :initform nil :accessor info)))

(defmethod print-object ((obj iterator) out)
  (print-unreadable-object (obj out)
    (with-slots (status value info) obj
      (format out "~A ~A (INFO~%~4T~{~A~^~%~4T~})" status value info))))



(defun iterator-p (obj) (typep obj 'iterator))

(defun continue (val &optional key info)
  (make-instance 'iterator
                 :status :continue
                 :value val
                 :info (if key (list (cons key info)) nil)))

(defun failed (val &optional key info)
  (make-instance 'iterator
                 :status :failed
                 :value val
                 :info (if key (list (cons key info)) nil)))

(defun finished (val &optional key info)
  (make-instance 'iterator
                 :status :finished
                 :value val
                 :info (if key (list (cons key info)) nil)))


(declaim (inline status-p))
(defun status-p (obj status)
  (and (iterator-p obj) (eq (slot-value obj 'status) status)))

(defun continue-p (iterator)
  (status-p iterator :continue))

(defun failed-p (iterator)
  (status-p iterator :failed))

(defun finished-p (iterator)
  (status-p iterator :finished))

(defun add-info (iterator key more-info)
  (push (cons key more-info) (info iterator))
  iterator)

(defun update-info (iterator update-function &rest args)
  (setf (info iterator) (apply update-function (info iterator) args))
  iterator)


(defun change-status (iterator new-status)
  (setf (status iterator) new-status)
  iterator)

(defun ->continue (iterator) (change-status iterator :continue))
(defun ->failed (iterator) (change-status iterator :failed))
(defun ->finished (iterator) (change-status iterator :finished))

(defun replace-value (iterator new-value)
  (setf (value iterator) new-value)
  iterator)


(defun update-value (iterator update-function &rest args)
  (setf (value iterator)
        (apply update-function (value iterator) args))
  iterator)


(defun bind (iterator function)
  (match iterator
    ((iterator (status :continue))
     (update-info (funcall function (value iterator))
                  (lambda (new-info)
                    (nconc new-info (info iterator)))))))


