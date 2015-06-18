(in-package mobius.numeric.linear-operations-arrays)

;; * Implementation of linear operations for arrays
;; ** Utility for arrays
(defun operate-on-arrays (op c a &rest b)
  "Apply elementwise function OP to elements of arrays A and B, saving the result in C"
  (let ((a-dims (array-dimensions a))
        (c-dims (array-dimensions c)))
    (if (equal a-dims c-dims)
        (labels ((process-dimensions (dims inds)
                   (if (null dims)
                       (let ((inds (reverse inds)))
                         (setf (apply #'aref c inds)
                               (apply op
                                      (apply #'aref a inds)
                                      (mapcar #f(apply #'aref % inds)
                                              b))))
                       (let ((n (car dims))
                             (other-dims (cdr dims)))
                         (loop for i from 0 below n
                            do (process-dimensions other-dims (cons i inds)))))))
          (process-dimensions a-dims nil)
          c)
        (error (format nil "OPERATE-ON-ARRAYS: arrays sizes do not match!")))))

;; ** Construction
(defmethod zero-vector ((u array))
  (let ((element-type (array-element-type u)))
    (make-array (array-dimensions u)
                :element-type element-type
               :initial-element (coerce 0 element-type))))

(defmethod zero-vector! ((u vector))
  (loop for i below (array-dimension u 0)
     do (setf (aref u i) 0.0d0))
  u)

(defmethod zero-vector! ((u array))
  (operate-on-arrays (lambda (&rest args)
                       (declare (ignore args))
                       (coerce 0 (array-element-type u)))
                     u
                     u))

(defmethod make-vector ((u vector) n)
  (let ((element-type (array-element-type u)))
    (make-array n
                :element-type element-type
               :initial-element (coerce 0 element-type))))

;; ** Shape
(defmethod vector-dim ((u vector))
  (array-dimension u 0))

(defmethod transpose ((u vector))
  u)
;; ** Mapping and reduction
;; *** Vectors
(defmethod map-vector ((u vector) f &key dest other-vectors)
  (let ((w (or dest (make-array (array-dimension u 0)
                                :element-type (array-element-type u)))))
   (loop for i below (array-dimension u 0)
      do (setf (aref w i)
               (apply f (aref u i) (mapcar #'(lambda (v) (aref v i)) other-vectors))))
   w))

(defmethod map-vector! ((u vector) f &key other-vectors)
  (loop for i below (array-dimension u 0)
     do (setf (aref u i)
              (apply f
                     (aref u i)
                     (mapcar #'(lambda (v) (aref v i)) other-vectors))))
  u)

(defmethod mapi-vector ((u vector) f &key dest other-vectors)
  (let ((w (or dest (make-array (array-dimension u 0)
                                :element-type (array-element-type u)))))
   (loop for i below (array-dimension u 0)
      do (setf (aref w i)
               (apply f i (aref u i) (mapcar #'(lambda (v) (aref v i)) other-vectors))))
   w))

(defmethod mapi-vector! ((u vector) f &key other-vectors)
  (loop for i below (array-dimension u 0)
     do (setf (aref u i)
              (apply f
                     i
                     (aref u i)
                     (mapcar #'(lambda (v) (aref v i)) other-vectors))))
  u)

(defmethod reduce-vector ((u vector) f initial-value)
  (reduce f u :initial-value initial-value))
;; *** Arrays
;; Not all methods are applicable to arrays in general
(defmethod map-vector ((u array) f &key dest other-vectors)
  (let ((w (or dest (zero-vector u))))
   (operate-on-arrays w u f other-vectors)))

(defmethod map-vector! ((u array) f &key other-vectors)
  (operate-on-arrays u u f other-vectors))


;; ** Structured multiplications
;; for vectors (simpler implementation)
(defmethod m* ((u vector) (v vector) &optional destination)
  (declare (ignore destination))
  (let ((u-dim (array-dimension u 0))
        (v-dim (array-dimension v 0)))
    (if (= u-dim v-dim)
        (loop for i below u-dim
           summing (* (aref u i) (aref v i)))
        (error "M* (VECTOR VECTOR): dimensions do not match"))))

;; for multidimensional arrays
(defmethod m* ((u array) (v array) &optional destination)
  (let* ((u-dims (array-dimensions u))
         (v-dims (array-dimensions v))
         (u-n (car (last u-dims)))
         (v-n (car v-dims)))
    (if (= u-n v-n)
        (let ((x (or destination (make-array (append (butlast u-dims) (cdr v-dims))
                                             :element-type (array-element-type u)))))
          (labels ((process-a (dims1 dims2 inds1 inds2)
                     (cond ((and (null dims1) (null dims2))
                            (setf (apply #'aref
                                         x
                                         (nconc (reverse inds1)
                                                (reverse inds2)))
                                  (loop for i from 0 below u-n
                                     summing (* (apply #'aref
                                                       u
                                                       (reverse (cons i inds1)))
                                                (apply #'aref
                                                       v
                                                       (cons i (reverse inds2)))))))
                           ((null dims1)                                              ; exhausted u-indices, loop over v-inidices
                            (loop for i from 0 below (car dims2)                      ; dims2 is not null at this point
                               do (process-a nil
                                             (cdr dims2)
                                             inds1
                                             (cons i inds2))))
                           (t (loop for i from 0 below (car dims1)
                                 do (process-a (cdr dims1) dims2 (cons i inds1) inds2))))
                     ))
            (process-a (butlast u-dims) (cdr v-dims) nil nil)
            x))
        (error "M* (ARRAY ARRAY): inner dimensions do not match"))))
;; DOT product for vectors is just M*
(defmethod dot ((u vector) (v vector))
  (m* u v))
;; OUTER-PRODUCT for two vectors
(defmethod outer-product ((u vector) (v vector) &optional buffer)
  (let* ((n (array-dimension u 0))
         (m (array-dimension v 0))
         (a (or buffer (make-array (list n m)
                                   :element-type (array-element-type u)))))
    (loop for i below n
       do (loop for j below m
             do (setf (aref a i j) (* (aref u i) (aref v j)))))
    a))

;; ** Structured division
(defmethod m/ ((a array) (b vector) &optional x0 dest)
  "Implementation using LLA:SOLVE"
  (declare (ignore x0 dest))
  (lla:solve a b))
