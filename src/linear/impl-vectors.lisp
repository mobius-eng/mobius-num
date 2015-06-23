(in-package mobius.numeric.impl-vectors)

;; * Constructors
(defmethod zero-vector ((original-vector vector) &optional destination)
  (if destination
      (loop for i below (array-dimension original-vector 0)
         do (setf (aref destination i) (coerce 0 (array-element-type destination)))
         finally (return destination))
      (make-array (array-dimensions original-vector)
                  :element-type (array-element-type original-vector)
                  :initial-element (coerce 0 (array-element-type original-vector)))))


(defmethod make-vector ((original-vector vector) dim)
  (let ((element-type (array-element-type original-vector)))
    (make-array dim
                :element-type element-type
                :initial-element (coerce 0 element-type))))

(defmethod list->vector-method ((vector-type (eql 'vector)) list)
  (make-array (length list)
              :element-type 'double-float
              :initial-contents (loop for x in list
                                   collecting (coerce x 'double-float))))

;; * Shape
(defmethod vector-dim ((u vector))
  (array-dimension u 0))

(defmethod transpose ((u vector))
  u)

;; * Iteration/mapping

(defmethod map-vector ((vector vector) function
                       &key destination other-vectors with-indices)
  (let ((w (or dest (make-array (array-dimension vector 0)
                                :element-type (array-element-type vector)))))
    (if with-indices
        (loop for i below (array-dimension vector 0)
           do (setf (aref w i)
                    (apply function
                           i
                           (aref vector i)
                           (mapcar #'(lambda (v) (aref v i)) other-vectors))))
        (loop for i below (array-dimension vector 0)
           do (setf (aref w i)
                    (apply function
                           (aref vector i)
                           (mapcar #'(lambda (v) (aref v i)) other-vectors)))))
    w))



;; * Reduction
(defmethod reduce-vector ((vector vector) reducing-function initial-value
                          &key mapping-function other-vectors)
  (let ((n (array-dimension vector 0))
        (result initial-value))
   (if mapping-function
       (loop for i below n
          do (setf result (funcall reducing-function
                                    result
                                    (apply mapping-function
                                           (aref vector i)
                                           (mapcar #f(aref % i) other-vectors))))
          finally (return result)))))


;; * Vector arithemtic

(defmethod m* ((u vector) (v vector) &optional destination)
  (declare (ignore destination))
  (let ((u-dim (array-dimension u 0))
        (v-dim (array-dimension v 0)))
    (if (= u-dim v-dim)
        (loop for i below u-dim
           summing (* (aref u i) (aref v i)))
        (error "M* (VECTOR VECTOR): dimensions do not match"))))

(defmethod inner-product ((u vector) (v vector)) (m* u v))

(defmethod outer-product ((u vector) (v vector) &optional buffer)
  (let* ((n (array-dimension u 0))
         (m (array-dimension v 0))
         (a (or buffer (make-array (list n m)
                                   :element-type (array-element-type u)))))
    (loop for i below n
       do (loop for j below m
             do (setf (aref a i j) (* (aref u i) (aref v j)))))
    a))
