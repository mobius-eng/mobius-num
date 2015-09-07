;; -*- Mode: Lisp; -*-
(in-package mobius.numeric.matrix)
;;; Matrix generics
;; Dimensionality
(defgeneric matrix-dimensions (m))
(defgeneric matrix-row-num (m))
(defgeneric matrix-col-num (m))

(defgeneric make-matrix (type rows cols)
  (:documentation "Make some initial matrix"))

(defgeneric eye-matrix (type dim)
  (:documentation "identity matrix of size dim"))

;; Reference i j element
(defgeneric mref (matrix i j))
(defgeneric (setf mref) (val matrix i j))
;; add, subtruct and scale: properties of the vector space
(defgeneric add (m1 m2 &key dest allocate)
  (:documentation "ADD M1 and M2. By default, put result into M1.
  If DEST is provided, use it instead of M1.
  If ALLOCATE is T allocate a new instance instead"))

(defgeneric sub (m1 m2 &key dest allocate)
  (:documentation "SUBtract M1 and M2. By default, put result into M1.
  If DEST is provided, use it instead of M1.
  If ALLOCATE is T allocate a new instance instead"))

(defgeneric scale (factor m &key dest allocate)
  (:documentation "SCALE M by FACTOR. By default, put result into M.
  If DEST is provided, use it instead of M.
  If ALLOCATE is T allocate a new instance instead"))

(defgeneric negate (m &key dest allocate)
  (:documentation "NEGATE M. By default, put result into M.
  If DEST is provided, use it instead of M.
  If ALLOCATE is T allocate a new instance instead"))

(defgeneric zero (m &key dest allocate)
  (:documentation "Make ZERO vector in the same space as M.
  By default, put result into M.
  If DEST is provided, use it instead of M.
  If ALLOCATE is T allocate a new instance instead"))

;; Matrix multiplication
(defgeneric mmul (m1 m2 &key dest)
  (:documentation "Matrix multiplication.
  Put the result into DEST. If DEST is not provided, allocate"))

;; Transpose
(defgeneric transpose (m &key dest)
  (:documentation "Transpose matrix M
  Put the result into DEST. If DEST is NIL, allocate"))

(defmacro domatrix ((&rest var-matrices) &body body)
  "Iterate over matrices elements. All matrices must be of the same size
Each variable introduced in VAR-MATRICES is SETF-able place"
  (let ((gmats (loop for x in var-matrices collect (gensym "MAT")))
        (mats (mapcar #'second var-matrices))
        (vars (mapcar #'first var-matrices))
        (n (gensym "N"))
        (m (gensym "M"))
        (i (gensym "I"))
        (j (gensym "J")))
    `(let (,@(mapcar #'list gmats mats))
       (destructuring-bind (,n ,m) (matrix-dimensions ,(first gmats))
         (loop for ,i below ,n
           do (loop for ,j below ,m
                do (symbol-macrolet (,@(mapcar (lambda (var mat)
                                                `(,var (mref ,mat ,i ,j)))
                                               vars
                                               gmats))
                                    ,@body)))))))
