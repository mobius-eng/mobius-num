(in-package mobius.numeric.linear)

(setf grid:*default-grid-type* 'grid:foreign-array)

(defun zero-vector (source &optional destination)
  "Create zero vector with the specs of SOURCE. Use DESTINATION if provided.
SOURCE and DESTINATION can be the same."
  (unless destination
    (setf destination (grid:copy source)))
  (let ((element-type (grid:element-type destination)))
    (grid:map-grid :source source
                   :destination destination
                   :element-function (constantly (coerce 0 element-type)))))

(defun copy-vector (source &optional destination)
  "Copy SOURCE: either create new vector or use DESTINATION"
  (grid:copy source :destination destination))

(defun make-vector (element-type dimension &optional (initial-element 0))
  "Create new vector with provided specs"
  (grid:make-foreign-array element-type
                           :dimensions (list dimension)
                           :initial-element (coerce initial-element element-type)))

(defun list->vector (element-type list)
  "Convert LIST to a vector with provided ELEMENT-TYPE"
  (grid:make-foreign-array element-type :initial-contents list))

(defun vector-of (element-type &rest items)
  "Make vector of ELEMENT-TYPE and provided items"
  (list->vector element-type items))

(defun dfvec (&rest items)
  "Make DOUBLE-FLOAT vector of ITEMS"
  (list->vector 'double-float items))

(defun sb64vec (&rest items)
  "Make (SIGNED-BYTE 64) vector of ITEMS"
  (list->vector '(signed-byte 64) items))


(defun negate-vector (vector &optional destination)
  "Negate VECTOR. Use DESTINATION if provided.
VECTOR and DESTINATION can be the same, in which case destructively negates VECTOR"
  (unless destination
    (setf destination (grid:copy vector)))
  (let ((element-type (grid:element-type destination)))
    (grid:map-grid :source vector
                   :destination destination
                   :element-function (lambda (x) (coerce (- x) element-type)))))

(defun vector-dim (vector)
  "Find VECTOR's dimension (count)"
  (first (grid:dimensions vector)))

(defun m* (A b &optional destination)
  "Matrix multiplication: simplified version of MATRIX-PRODUCT.
Returns the product of A and B, use DESTINATION for the result if provided"
  (unless destination
    (setf destination (copy-vector b)))
  (matrix-product A b destination 1.0 0.0))


;; TODO: update in case A is already decomposed, in this case
;; PERMUTATION must be supplied as well
(defun m/ (A b &optional x work-A permutation)
  "Backslash operation: returns A^(-1) * B. Does not alter A or B
Uses X and WORK-A for the result if provided.
Method uses LU decomposition."
  (if work-A
      (grid:copy A :destination work-A)
      (setf work-A (grid:copy A)))
  (unless x
    (setf x (copy-vector b)))
  (unless permutation
    (setf permutation (make-permutation (vector-dim b))))
  (multiple-value-bind (lu perm) (lu-decomposition work-A permutation)
    (lu-solve lu b perm x)))


(defun dot (a b) (grid:inner a b))

(defun squared-L2-norm (v) (dot v v))

(defvar *norm-type* 2
  "Type of the vector norm used by NORM function.
Possible values: +infinity - infinity norm, 1 - L1 norm, 2 - L2 norm, ...")


(defun reduce-vector (vector reducing-function initial-value
                      &key mapping-function other-vectors)
  "Reduce VECTOR with REDUCTION-FUNCTION starting from INITIAL-VALUE.
If MAPPING-FUNCTION is provided: apply it to the VECTOR item and all corresponding
items of OTHER-VECTORS before applying REDUCTION-FUNCTION.
REDUCTION-FUNCTION accepts (INTERMEDIATE-RESULT VECTOR-ITEM) arguments"
  (let ((n (vector-dim vector))
        (result initial-value))
    (if mapping-function
        (dotimes (i n result)
          (setf result (funcall reducing-function
                                result
                                (apply mapping-function
                                       (grid:aref vector i)
                                       (mapcar (lambda (v) (grid:aref v i))
                                               other-vectors)))))
        
        (dotimes (i n result)
          (setf result (funcall reducing-function
                                result
                                (grid:aref vector i)))))))

(defun norm (v &optional (*norm-type* *norm-type*))
  "Evaluates vector norm according to *NORM-TYPE*"
  (cond ((= *norm-type* 2) (grid:norm v))
        ((= +infinity *norm-type*) (mmax v))
        ((= *norm-type* 1) (reduce-vector v #'+ 0.0 :mapping-function #'abs))
        (t (expt (reduce-vector v #'+ 0.0
                                :mapping-function (lambda (x) (expt (abs x) *norm-type*)))
                 (/ *norm-type*)))))

(defun .abs (v) (funcall (grid:elementwise #'abs) v))

(defun .sin (v) (funcall (grid:elementwise #'sin) v))

(defun .cos (v) (funcall (grid:elementwise #'cos) v))

(defun .tan (v) (funcall (grid:elementwise #'tan) v))

(defun .exp (v) (funcall (grid:elementwise #'exp) v))

(defun .log (v) (funcall (grid:elementwise #'log) v))

(defun .signum (v) (funcall (grid:elementwise #'signum) v))

(defun elementwise! (function)
  "Makes a function that destructively applies an elementary function
to all the items of the vector (only for vectors!)"
  (lambda (v)
    (let ((n (vector-dim v)))
      (dotimes (i n v)
        (setf (grid:aref v i) (funcall function (grid:aref v i)))))))

(defun .abs! (v) (funcall (elementwise! #'abs) v))

(defun .sin! (v) (funcall (elementwise! #'sin) v))

(defun .cos! (v) (funcall (elementwise! #'cos) v))

(defun .tan! (v) (funcall (elementwise! #'tan) v))

(defun .exp! (v) (funcall (elementwise! #'exp) v))

(defun .log! (v) (funcall (elementwise! #'log) v))

(defun .signum! (v) (funcall (elementwise! #'signum) v))

;; Arithmetic

(defun .=+! (vector &rest others)
  "Destructively add OTHERS to VECTOR (elementwise)"
  (reduce #'elt+ others :initial-value vector))

(defun .=*! (vector &rest others)
  "Destructively multiplies OTHERS by VECTOR (elementwise)"
  (reduce #'elt* others :initial-value vector))

(defun .=-! (vector &rest others)
  "Destructively subtruct OTHERS from VECTOR (elementwise).
If no OTHERS are provided, acts as a negation"
  (if (null others)
      (funcall (elementwise! #'-) vector)
      (reduce #'elt- others :initial-value vector)))

(defun .=/! (vector &rest others)
  "Destructively divide VECTOR by others (elementwise).
If no OTHERS are provided, calculates reciprocals"
  (if (null others)
      (funcall (elementwise! #'/) vector)
      (reduce #'elt/ others :initial-value vector)))

(defun .+ (vector &rest others)
  "Non-destructive elementwise addition"
  (let ((result (copy-vector vector)))
    (apply #'.=+! result others)))

(defun .* (vector &rest others)
  "Non-destructive elementwise multiplication"
  (let ((result (copy-vector vector)))
    (apply #'.=*! result others)))

(defun .- (vector &rest others)
  "Non-destructive elementwise subtruction"
  (let ((result (copy-vector vector)))
   (apply #'.=-! result others)))

(defun ./ (vector &rest others)
  "Non-destructive elementwise division"
  (let ((result (copy-vector vector)))
   (apply #'.=/! result others)))

(defun .+! (destination vector &rest others)
  "Returns the result of addition of OTHERS to VECTOR.
Stores the result in DESTINATION "
  (let ((dest (copy-vector vector destination)))
    (apply #'.=+! dest others)))

(defun .-! (destination vector &rest others)
  "Returns the result of subtruction of OTHERS from VECTOR.
Stores the result in DESTINATION"
  (let ((dest (copy-vector vector destination)))
    (apply #'.=-! dest others)))

(defun .*! (destination vector &rest others)
  "Returns the result of multiplication of VECTOR by OTHERS.
Stores the result in DESTINATION"
  (let ((dest (copy-vector vector destination)))
    (apply #'.=*! dest others)))

(defun ./! (destination vector &rest others)
  "Returns the result of division of VECTOR by OTHERS.
Stores the result in DESTINATION"
  (let ((dest (copy-vector vector destination)))
    (apply #'.=/! dest others)))

(defun list->matrix (element-type list)
  "Make matrix of ELEMENT-TYPE from the list"
  (grid:make-foreign-array element-type :initial-contents list))

(defun make-matrix (element-type dimensions &optional (initial-element 0))
  "Create matrix with provided specs"
  (grid:make-foreign-array element-type
                           :dimensions dimensions
                           :initial-element (coerce initial-element element-type)))

(defun identity-matrix (dimension
                        &optional (diagonal-value 1) (element-type 'double-float))
  "Create identity matrix"
  (grid:identity-matrix dimension
                        diagonal-value
                        'grid:foreign-array
                        element-type))

(defun matrix-of (element-type &rest rows)
  (list->matrix element-type rows))

(defmethod compile-criterium ((type (eql :converged)) &rest args)
  (let ((close-p (first args))
        (info (second args)))
    (in-criterium (x (last-v nil))
      (let ((v (iterator:value x)))
        (if (numberp v)
            (cond ((null last-v) (setf last-v v))
                  ((funcall close-p last-v v)
                   (iterator:add-info (iterator:->finished x) :converged info))
                  (t (setf last-v v)))
            (cond ((null last-v) (setf last-v (copy-vector v)))
                  ((funcall close-p last-v v)
                   (iterator:add-info (iterator:->finished x) :converged info))
                  (t (setf last-v (copy-vector v last-v)))))))))

(defmethod compile-criterium ((type (eql :converged-norm)) &rest args)
  (let* ((tolerance (first args))
         (scale (second args))
         (info (third args))
         (err (cond ((numberp scale) (* tolerance scale))
                    (t (* tolerance (norm scale))))))
    (flet ((close-p (x y buffer)
             (< (norm (.-! buffer x y)) err)))
      (in-criterium (x (last-v nil) (buffer nil))
        (let ((v (iterator:value x)))
          (unless buffer
            (setf buffer (copy-vector v)))
          (cond ((null last-v) (setf last-v (copy-vector v)))
                ((close-p last-v v)
                 (iterator:add-info (iterator:->finished x) :converged-norm info))
                (t (setf last-v (copy-vector v last-v)))))))))


(defmethod compile-criterium ((type (eql :converged-numbers)) &rest args)
  (let ((tolerance (or (car args) 1d-16))
        (scale (or (cadr args) 1.0d0))
        (info (caddr args)))
    (declare (type double-float tolerance scale))
    (let ((err (* tolerance scale)))
      (flet ((close-p (x y) (< (abs (- x y)) err)))
        (compile-criterium :converged #'close-p info)))))

(defmethod num= ((a grid:vector-double-float) (b grid:vector-double-float)
                 &optional tolerance)
  (unless tolerance
    (setf tolerance cl-num-utils:*num=-tolerance*))
  (flet ((mapping-norm-1 (x y) (abs (- x y)))
         (mapping-norm-2 (x y) (let ((z (- x y))) (* z z)))
         (mapping-norm-n (x y) (expt (abs (- x y)) *norm-type*)))
    (< (cond ((= *norm-type* 2)
              (sqrt (reduce-vector a #'+ 0
                                   :mapping-function #'mapping-norm-2
                                   :other-vectors (list b))))
             ((= *norm-type* +infinity)
              (reduce-vector a #'max 0 :mapping-function #'mapping-norm-1
                                                      :other-vectors (list b)))
             ((= *norm-type* 1) (reduce-vector a #'+ 0
                                               :mapping-function #'mapping-norm-1
                                               :other-vectors (list b)))
             (t (expt (reduce-vector a #'+ 0
                                     :mapping-function #'mapping-norm-n
                                     :other-vectors (list b))
                      (/ *norm-type*))))
       tolerance)))

(defmethod num= ((a grid:vector-signed-byte-64) (b grid:vector-signed-byte-64)
                 &optional tolerance)
  (unless tolerance
    (setf tolerance cl-num-utils:*num=-tolerance*))
  (flet ((mapping-norm-1 (x y) (abs (- x y)))
         (mapping-norm-2 (x y) (let ((z (- x y))) (* z z)))
         (mapping-norm-n (x y) (expt (abs (- x y)) *norm-type*)))
    (< (cond ((= *norm-type* 2)
              (sqrt (reduce-vector a #'+ 0
                                   :mapping-function #'mapping-norm-2
                                   :other-vectors (list b))))
             ((= *norm-type* +infinity)
              (reduce-vector a #'max 0 :mapping-function #'mapping-norm-1
                                                      :other-vectors (list b)))
             ((= *norm-type* 1) (reduce-vector a #'+ 0
                                               :mapping-function #'mapping-norm-1
                                               :other-vectors (list b)))
             (t (expt (reduce-vector a #'+ 0
                                     :mapping-function #'mapping-norm-n
                                     :other-vectors (list b))
                      (/ *norm-type*))))
       tolerance)))
