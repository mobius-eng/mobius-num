(in-package mobius.numeric.linear-operations-arrays)

;; * Implementation of linear operations for arrays
;; ** Utility for arrays

;; ** Construction


;; *** Arrays
;; Not all methods are applicable to arrays in general
(defmethod map-vector ((u array) f &key dest other-vectors)
  (let ((w (or dest (zero-vector u))))
   (operate-on-arrays w u f other-vectors)))

(defmethod map-vector! ((u array) f &key other-vectors)
  (operate-on-arrays u u f other-vectors))


;; ** Structured multiplications
;; for vectors (simpler implementation)

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

;; ** Structured division
(defmethod m/ ((a array) (b vector) &optional x0 dest)
  "Implementation using LLA:SOLVE"
  (declare (ignore x0 dest))
  (lla:solve a b))
