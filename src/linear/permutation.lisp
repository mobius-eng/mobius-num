;; Mode: Lisp; -*-
;;; Permutation
(in-package mobius.numeric.permutation)

(defclass permutation (array-matrix)
  ()
  (:documentation "Permutation on vectors"))

(defmethod print-object ((obj permutation) out)
  (print-unreadable-object (obj out :type t)
    (format out "~A" (permutation-vector obj)))
  nil)

(defgeneric permutation-data->vector (data))

(defun permutation-vector (p)
  "Vector representing permutation p(i)=j"
  (array-matrix-data p))

(defmethod permutation-data->vector ((data list))
  (let* ((max-index (loop for x in data
                      if (listp x)
                      maximize (apply #'max x)
                      else
                      maximize x
                      end))
         (vector (make-array (1+ max-index) :initial-element 0)))
    (loop for i upto max-index
      do (setf (svref vector i) i))
    (loop for x in data
      when (listp x)
      do (setf (svref vector (first x)) (second x))
      end
      finally (return vector))))

(defmethod permutation-data->vector ((data vector))
  (make-array (length data) :initial-contents data))

(defun make-permutation (data)
  "Make permutation based on DATA. DATA can be either
  list of permutations or a vector"
  (make-instance 'permutation
    :data (permutation-data->vector data)))

(defun inverse-permutation (permutation)
  (with-accessors ((p permutation-vector)) permutation
    (let ((new-vector (copy-seq p)))
      (loop for i from 0 below (length p)
        do (setf (svref new-vector (svref p i)) i))
      (make-permutation new-vector))))

(defun compose-permutation (p1 p2)
  (let* ((v1 (permutation-vector p1))
         (v2 (permutation-vector p2))
         (v (copy-seq v1)))
    (loop for i below (length v)
      do (setf (svref v i) (svref v2 (svref v1 i))))
    (make-permutation v)))

(defun inplace-compose-permutation (p1 p2)
  (let ((v1 (permutation-vector p1))
        (v2 (permutation-vector p2)))
    (loop for i below (length v1)
      do (setf (svref v1 i) (svref v2 (svref v1 i))))
    p1))

;; TODO: FIXME incorrect!!!
(defun permute (p &rest permutation-indices)
  (let ((v (permutation-vector p)))
    (loop for (i j) in permutation-indices
      if (and (= (svref v i) i) (= (svref v j) j))
      do (progn
           (setf (svref v i) j)
           (setf (svref v j) i))
      else do (error "Cannot cnahge existing permutation: compose instead")
      end))
  p)

(defun get-permutation (p i)
  (svref (permutation-vector p) i))

;;; Implementation of matrix protocol
(defmethod matrix-dimensions ((m permutation))
  (let ((n (length (permutation-vector m))))
    (list n n)))

(defmethod matrix-row-num ((m permutation))
  (length (permutation-vector m)))

(defmethod matrix-col-num ((m permutation))
  (length (permutation-vector m)))

(defmethod mref ((m permutation) i j)
  (if (= (get-permutation m i) j)
      1
      0))

(defmethod make-matrix ((type (eql 'permutation)) rows cols)
  (assert (= rows cols) ()
          "Permutation matrices are always square: rows != cols")
  (make-instance 'permutation
    :data (make-array rows
                      :initial-contents (loop for i below rows
                                          collect i))))

(defmethod eye-matrix ((type (eql 'permutation)) dim)
  (make-matrix 'permutation dim dim))

(defun permutation->array-matrix (p)
  (let* ((n (matrix-row-num p))
         (array (make-array (list n n) :initial-element 0)))
    (loop for i below n
      do (setf (aref array i (get-permutation p i)) 1))
    (make-array-matrix array)))

(defmethod (setf mref) (new-val (m permutation) i j)
  (let* ((mat (permutation->array-matrix m)))
    (call-next-method new-val mat i j)))


(defmethod add :around ((m1 permutation) (m2 permutation) &key dest allocate)
  (call-next-method (permutation->array-matrix m1)
                    (permutation->array-matrix m2)
                    :dest dest
                    :allocate allocate))

(defmethod sub :around ((m1 permutation) (m2 permutation) &key dest allocate)
  (call-next-method (permutation->array-matrix m1)
                   (permutation->array-matrix m2)
                   :dest dest
                   :allocate allocate))

(defmethod scale :around (factor (m permutation) &key dest allocate)
  (call-next-method factor
                    (permutation->array-matrix m)
                    :dest dest
                    :allocate allocate))

(defmethod negate :around ((m permutation) &key dest allocate)
  (call-next-method (permutation->array-matrix m)
                    :dest dest
                    :allocate allocate))

(defmethod zero :around ((m permutation) &key dest allocate)
  (call-next-method (permutation->array-matrix m)
                    :dest dest
                    :allocate allocate))

(defmethod mmul ((m1 permutation) (m2 permutation) &key dest)
  (let ((v1 (permutation-vector m1))
        (v2 (permutation-vector m2))
        (n (matrix-row-num m1)))
    (assert (equalp (matrix-dimensions m1) (matrix-dimensions m2)) ()
            "Permutation matrix dimensions are not equal")
    (unless dest
      (make-permutation (make-array n :initial-element 0)))
    (cond ((eq m1 dest)
           (inplace-compose-permutation m1 m2))
          ((eq m2 dest)
           (error "Cannot use the second argument as destination"))
          (t (let ((v (permutation-vector dest)))
               (loop for i below n
                 do (setf (svref v i)
                          (svref v2 (svref v1 i)))))))
    dest))
               
(defmethod transpose ((p permutation) &key dest)
  (if dest
      (cond ((eq p dest)
             (let ((p2 (permutation-vector (inverse-permutation p)))
                   (vd (permutation-vector dest)))
               (loop for i below (length vd)
                 do (setf (svref vd i) (svref p2 i)))
               dest))
            (t (let ((vd (permutation-vector dest))
                     (vp (permutation-vector p)))
                 (assert (equalp (matrix-dimensions p)
                                 (matrix-dimensions vd))
                         ()
                         "Permutation matrix dimensions are not equal")
                 (loop for i below (length vd)
                   do (setf (svref vd (svref vp i)) i))
                 dest)))
      (inverse-permutation p)))












