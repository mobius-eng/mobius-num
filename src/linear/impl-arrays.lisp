(in-package mobius.numeric.impl-arrays)

;; * Utility
(defun operate-on-arrays (function destination array &key with-indices other-arrays)
  "Apply elementwise FUNCTION to elements of the ARRAY and OTHER-ARRAYS,
saving the result in DESTINATION"
  (let ((array-dims (array-dimensions array))
        (dest-dims (array-dimensions destination)))
    (if (equal array-dims dest-dims)
        (labels ((process-dimensions-no-indices (dims inds)
                   (if (null dims)
                       (let ((inds (reverse inds)))
                         (setf (apply #'aref destination inds)
                               (apply function
                                      (apply #'aref array inds)
                                      (mapcar #f(apply #'aref % inds)
                                              other-arrays))))
                       (let ((n (car dims))
                             (other-dims (cdr dims)))
                         (loop for i from 0 below n
                            do (process-dimensions-no-indices other-dims
                                                              (cons i inds))))))
                 (process-dimensions-with-indices (dims inds)
                   (if (null dims)
                       (let ((inds (reverse inds)))
                         (setf (apply #'aref destination inds)
                               (apply function
                                      inds
                                      (apply #'aref array inds)
                                      (mapcar #f(apply #'aref % inds)
                                              other-arrays))))
                       (let ((n (car dims))
                             (other-dims (cdr dims)))
                         (loop for i from 0 below n
                            do (process-dimensions-with-indices other-dims
                                                                (cons i inds)))))))
          (if with-indices
              (process-dimensions-with-indices array-dims nil)
              (process-dimensions-no-indices array-dims nil))
          destination)
        (error
         "OPERATE-ON-ARRAYS - dimensions do not match: ARRAY ~A, DESTINATION ~A"
         array-dims dest-dims))))

(defun first-item-of-nested-list (list)
  (cond ((atom list) list)
        ((null list) nil)
        (t (first-item-of-nested-list (first list)))))

(defun nested-list-dimensions (list &optional dims-stack)
  (cond ((or (null list) (atom list)) (nreverse dims-stack))
        (t (nested-list-dimensions (first list) (cons (length list) dims-stack)))))

;; * Constructors
(defmethod zero-vector ((vector array) &optional destination)
  (let ((dims (array-dimensions vector))
        (element-type (array-element-type vector)))
    (if destination
        (operate-on-arrays (constantly (coerce 0 element-type))
                           destination
                           vector)
        (make-array dims
                    :element-type element-type
                    :initial-element (coerce 0 element-type)))))

(defmethod make-vector ((original-vector array) new-dims)
  (let ((element-type (array-element-type original-vector)))
    (make-array new-dims :element-type element-type)))

(defmethod list->vector-method ((vector-type (eql 'array)) list)
  (let ((element-type (type-of (first-item-of-nested-list list)))
        (dims (nested-list-dimensions list)))
    (make-array dims :element-type element-type :initial-contents list)))

;; * Shape
(defmethod vector-dim ((vector array)) (array-dimensions vector))

;; * Iteration
(defmethod map-vector ((vector array) function
                       &key destination other-vectors with-indices)
  (let ((destination (or destination (zero-vector vector))))
    (operate-on-arrays function
                       destination
                       vector
                       :with-indices with-indices
                       :other-arrays other-vectors)))

;; * Reduction is not really possible:
;; there are multiple ways how one can reduce an array. For example,
;; matrix can be reduced by rows or by columns. We also might want
;; rows or columns operations on them. With more dimensions it becomes
;; even trickier

;; * Operations
;; ** Most general
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
                           ((null dims1) ; exhausted u-indices, loop over v-inidices
                            (loop for i from 0 below (car dims2)
                               do (process-a nil
                                             (cdr dims2)
                                             inds1
                                             (cons i inds2))))
                           (t (loop for i from 0 below (car dims1)
                                 do (process-a (cdr dims1)
                                               dims2
                                               (cons i inds1)
                                               inds2))))))
            (process-a (butlast u-dims) (cdr v-dims) nil nil)
            x))
        (error "M* (ARRAY ARRAY): inner dimensions do not match"))))

;; ** More specialised
(defmethod m* ((u array) (v vector) &optional destination)
  (let* ((u-dims (array-dimensions u))
         (v-dims (array-dimension v 0))
         (u-n (car (last u-dims)))
         (v-n v-dims))
    (if (= u-n v-n)
        (let ((x (or destination (make-array (butlast u-dims)
                                             :element-type (array-element-type u)))))
          (labels ((process-a (dims1 inds1)
                     (cond ((null dims1)
                            (setf (apply #'aref
                                         x
                                         (reverse inds1))
                                  (loop for i from 0 below u-n
                                     summing (* (apply #'aref
                                                       u
                                                       (reverse (cons i inds1)))
                                                (aref v i)))))
                           (t (loop for i from 0 below (car dims1)
                                 do (process-a (cdr dims1) (cons i inds1)))))))
            (process-a (butlast u-dims) nil)
            x))
        (error "M* (ARRAY VECTOR): inner dimensions do not match"))))

;; ** Structured division
(defmethod m/ ((a array) (b vector) &optional x0)
  "Implementation using LLA:SOLVE"
  (declare (ignore x0))
  (lla:solve a b))

;; TODO: implement OUTER-PRODUCT
