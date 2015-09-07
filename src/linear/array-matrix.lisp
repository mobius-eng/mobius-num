;; -*- Mode: Lisp; -*-
;;; Array based matrices
(in-package mobius.numeric.array-matrix)

;;; ARRAY based matrix
(defclass array-matrix ()
  ((data :initarg :data :accessor array-matrix-data))
  (:documentation "ARRAY based matrix"))
;; Constructor
(defun make-array-matrix (array)
  (make-instance 'array-matrix
    :data array))

(defmethod print-object ((obj array-matrix) out)
  (print-unreadable-object (obj out :type t)
    (format out "~A" (array-matrix-data obj)))
  nil)
;; Matrix operations 
(defmethod mref ((m array-matrix) i j)
  (aref (array-matrix-data m) i j))

(defmethod (setf mref) (val (m array-matrix) i j)
  (setf (aref (array-matrix-data m) i j) val))

(defmethod matrix-dimensions ((m array-matrix))
  (array-dimensions (array-matrix-data m)))

(defmethod matrix-row-num ((m array-matrix))
  (array-dimension (array-matrix-data m) 0))

(defmethod matrix-col-num ((m array-matrix))
  (array-dimension (array-matrix-data m) 1))
;; some helpers
(defun ensure-allocation (m dest allocate)
  (cond ((and (null dest) (null allocate)) m)
        (allocate 
         (make-array-matrix
          (make-array (matrix-dimensions m)
                      :element-type (array-element-type (array-matrix-data m)))))
        (t (assert (equalp (matrix-dimensions dest) (matrix-dimensions m)))
           dest)))

(defun array-matrix-elementwise-binary (m1 m2 op dest allocate)
  (assert (equalp (matrix-dimensions m1) (matrix-dimensions m2))
          ()
          "Matrices dimensions do not match ~A and ~A" m1 m2)
  (setf dest (ensure-allocation m1 dest allocate))
  (domatrix ((result dest) (x m1) (y m2))
    (setf result (funcall op x y)))
  dest)

(defun array-matrix-elementwise-unary (m op dest allocate)
  (setf dest (ensure-allocation m dest allocate))
  (domatrix ((result dest) (x m))
    (setf result (funcall op x)))
  dest)
;; linear space operations
(defmethod add ((m1 array-matrix) (m2 array-matrix) &key dest allocate)
  (array-matrix-elementwise-binary m1 m2 #'+ dest allocate))

(defmethod sub ((m1 array-matrix) (m2 array-matrix) &key dest allocate)
  (array-matrix-elementwise-binary m1 m2 #'- dest allocate))

(defmethod negate ((m array-matrix) &key dest allocate)
  (array-matrix-elementwise-unary m #'- dest allocate))

(defmethod scale (factor (m array-matrix) &key dest allocate)
  (array-matrix-elementwise-unary m #'(lambda (x) (* x factor)) dest allocate))

(defmethod zero ((m array-matrix) &key dest allocate)
  (array-matrix-elementwise-unary m (constantly 0) dest allocate))

;; matrix multiplication
(defmethod mmul ((m1 array-matrix) (m2 array-matrix) &key dest)
  (let ((m1-dims (matrix-dimensions m1))
        (m2-dims (matrix-dimensions m2)))
    (assert (= (second m1-dims) (first m2-dims)) ()
            "Matrix dimensions do not conform: ~A * ~A" m1 m2)
    (if dest
        (let ((dest-dims (matrix-dimensions dest)))
          (assert (and (= (first m1-dims) (first dest-dims))
                       (= (second m2-dims) (second dest-dims)))
                  ()
                  "Result (DEST) dimensions do not conform"))
        (setf dest (make-array-matrix (make-array `(,(first m1-dims) ,(second m2-dims))))))
    (loop for i below (first m1-dims)
      do (loop for j below (second m2-dims)
           do (setf (mref dest i j)
                    (loop for k below (second m1-dims) sum (* (mref m1 i k) (mref m2 k j))))))
    dest))
;; transpose
(defmethod transpose ((m array-matrix) &key dest)
  (destructuring-bind (nrows ncols) (matrix-dimensions m)
    (if dest
        (assert (equalp (list ncols nrows) (matrix-dimensions dest)) ()
                "Matrix dimensions are not equal")
        (setf dest (make-array-matrix (make-array (list ncols nrows)))))
    (cond ((eq m dest)
           (let (tmp)
             (loop for i below nrows
               do (loop for j from i below ncols
                    do (progn
                         (setf tmp (mref dest i j))
                         (setf (mref dest i j) (mref m j i))
                         (setf (mref dest j i) tmp))))))
          (t (loop for i below nrows
               do (loop for j below ncols
                    do (setf (mref dest j i) (mref m i j)))))))
  dest)
         
;; constructors:

(defmethod make-matrix ((type (eql 'array-matrix)) rows cols)
  (make-array-matrix (make-array (list rows cols) :initial-element 0)))

(defmethod eye-matrix ((type (eql 'array-matrix)) dim)
  (let ((m (make-matrix 'array-matrix dim dim)))
    (loop for i below (matrix-col-num m)
      do (setf (mref m i i ) 1))
    m))

