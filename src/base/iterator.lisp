(in-package mobius.numeric.iterator)

(defclass iterator ()
  ((status :initarg :status :accessor status)
   (value :initarg :value :accessor value)
   (info :initarg :info :initform nil :accessor info)))

(defmethod print-object ((obj iterator) out)
  (print-unreadable-object (obj out)
    (with-slots (status value info) obj
     (format out "~A ~A  Info: ~A" status value info))))

(defun continue (val &optional info)
  (make-instance 'iterator
                 :status :continue
                 :value val
                 :info info))

(defun failed (val &optional info)
  (make-instance 'iterator
                 :status :failed
                 :value val
                 :info info))

(defun finished (val &optional info)
  (make-instance 'iterator
                 :status :finished
                 :value val
                 :info info))

(defun continue? (iterator)
  (eq (status iterator) :continue))

(defun failed? (iterator)
  (eq (status iterator) :failed))

(defun finished? (iterator)
  (eq (status iterator) :finished))

(defun update-info (iterator new-info)
  (make-instance 'iterator
                 :status (status iterator)
                 :value (value iterator)
                 :info new-info))

(defun add-info! (iterator key more-info)
  (push (cons key more-info) (info iterator))
  iterator)

(defun change-status! (iterator new-status)
  (setf (status iterator) new-status)
  iterator)

(defun to-continue! (iterator) (change-status! iterator :continue))
(defun to-failed! (iterator) (change-status! iterator :failed))
(defun to-finished! (iterator) (change-status! iterator :finished))

(defun replace-value! (iterator new-value)
  (setf (value iterator) new-value)
  iterator)
