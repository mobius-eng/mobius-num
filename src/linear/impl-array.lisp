(in-package mobius.numeric.impl-arrays)

;; * Utility
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

(defmethod map-vector ((vector array) function
                       &key destination other-vectors with-indices)
  (let ((w (or destination (zero-vector vector))))
    (if with-indices
        (operate-on-arrays #'(lambda (&rest args)
                               (apply function inds args))
                           w
                           vector
                           other-vectors)
        (operate-on-arrays function w vector other-vectors))))
