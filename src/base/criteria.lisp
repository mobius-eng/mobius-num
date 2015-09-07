(in-package mobius.numeric.criteria)

;; BASE-CRITERIUM and generics
(defclass base-criterium ()
  ((info :initarg :info :reader criterium-info)))

(defgeneric criterium-function (criterium)
  (:documentation
   "Get criterium function"))

(defgeneric make-criterium (criterium &rest args)
  (:documentation
   "Generic factory function for criterium"))

(defmethod criterium-function ((criterium base-criterium))
  (lambda (x) x))

(defmethod make-criterium ((c (eql 'base-criterium)) &rest args)
  (make-instance 'base-criterium :info (first args)))

(defmethod print-object ((obj base-criterium) out)
  (print-unreadable-object (obj out :type t)
    (format out "~A" (criterium-info obj)))
  obj)

;; FINISHED-VALUE-CRITERIUM 
(defclass finished-value-criterium (base-criterium)
  ((predicate :initarg :predicate)))

(defmethod criterium-function ((criterium finished-value-criterium))
  (lambda (x)
    (with-slots (info predicate) criterium
      (if (funcall predicate (iterator:value x))
          (iterator:add-info (iterator:->finished x) :finished-value info)
          x))))

(defmethod make-criterium ((c (eql 'finished-value-criterium)) &rest args)
  (make-instance 'finished-value-criterium
      :predicate (first args)
      :info (second args)))

;; FAILED-VALUE-CRITERIUM
(defclass failed-value-criterium (base-criterium)
  ((predicate :initarg :predicate)))

(defmethod criterium-function ((criterium failed-value-criterium))
  (lambda (x)
    (with-slots (info predicate) criterium
      (if (funcall predicate (iterator:value x))
          (iterator:add-info (iterator:->failed x) :failed-value info)
          x))))

(defmethod make-criterium ((c (eql 'failed-value-criterium)) &rest args)
  (make-instance 'failed-value-criterium
      :predicate (first args)
      :info (second args)))


;; LOG-CRITERIUM
(defclass log-criterium (base-criterium)
  ((log-function :initarg :log-function)))

(defmethod criterium-function ((criterium log-criterium))
  (lambda (x)
    (funcall (slot-value criterium 'log-function) (iterator:value x))
    x))

(defmethod make-criterium ((c (eql 'log-criterium)) &rest args)
  (make-instance 'log-criterium
      :log-function (first args)
      :info (second args)))

;; LIMIT-ITER-CRITERIUM
(defclass limit-iter-criterium (base-criterium)
  ((max-iter-num :initarg :max)
   (iter-performed :initform 0)))

(defmethod criterium-function ((criterium limit-iter-criterium))
  (setf (slot-value criterium 'iter-performed) 0)
  (lambda (x)
    (with-slots (max-iter-num iter-performed info) criterium
      (if (> iter-performed max-iter-num)
          (iterator:add-info (iterator:->failed x) :limit-iter info)
          (progn
            (incf iter-performed)
            x)))))

(defmethod make-criterium ((c (eql 'limit-iter-criterium)) &rest args)
  (make-instance 'limit-iter-criterium
      :max (first args)
      :info (second args)))

(defmethod print-object ((obj limit-iter-criterium) out)
  (print-unreadable-object (obj out :type t)
    (with-slots (max-iter-num iter-performed) obj
      (format out "(MAX ~D) (PERFORMED ~D)" max-iter-num iter-performed)))
  obj)

;; MODIFY-VALUE-CRITERIUM
(defclass modify-value-criterium (base-criterium)
  ((modifying-function :initarg :modifying-function)))

(defmethod criterium-function ((c modify-value-criterium))
  (lambda (x)
    (iterator:replace-value x (funcall (slot-value c 'modifying-function)
                                       (iterator:value x)))))

(defmethod make-criterium ((c (eql 'modify-value-criterium)) &rest args)
  (make-instance 'modify-value-criterium
      :modifying-function (first args)
      :info (second args)))

;;; Criteria combination
(defclass criteria-seq ()
  ((criteria :initarg :criteria :accessor criteria-list)))

(defgeneric criteria-function (criteria-combination))

(defgeneric make-criteria (criteria-combination &rest args))

(defmethod criteria-function ((c criteria-seq))
  (with-slots (criteria) c
    (let ((funs (mapcar #'criterium-function criteria)))
      (lambda (x)
        (let ((y (iterator:continue x)))
          (loop for f in funs
             if (iterator:continue-p y)
             do (setf y (funcall f y))
             else
             return y
             end
             finally (return y)))))))

(defmethod make-criteria ((c (eql 'criteria-seq)) &rest args)
  (make-instance 'criteria-seq
      :criteria (mapcar #'(lambda (lst)
                            (apply #'make-criterium lst))
                        args)))

(defmethod print-object ((obj criteria-seq) out)
  (print-unreadable-object (obj out :type t)
    (format out "~{~A~^ ~}" (criteria-list obj)))
  obj)


(defmacro criteria (combination &body criterium-clauses)
  `(make-criteria
    ',combination
    ,@(mapcar (lambda (clause) `(list ',(first clause) ,@(rest clause)))
              criterium-clauses)))
