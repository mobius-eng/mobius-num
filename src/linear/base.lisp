(in-package mobius.numeric.linear-base)
;; * General vector operations

;; ** Conditions
(define-condition vector-length-mismatch (error)
  ((vector-length-mismatch-length1 :initarg :length1
                                   :reader vector-length-mismatch-length1)
   (vector-length-mismatch-length2 :initarg :length2
                                   :reader vector-length-mismatch-length2))
  (:documentation
   "Condition raised when two vectors have mismatching lengths"))

(defun check-vector-lengths (vec &rest more-vectors)
  "Tests if all vectors have the same length"
  (declare (optimize (speed 3) (debug 1) (safety 1))
           (type (simple-array * *) vec))
  (let ((len (length vec))
        (other-length 0))
    (declare (type fixnum len other-length))
    (or (every (lambda (w)
                 (declare (type (simple-array * *) w))
                 (setf other-length (length w))
                 (eql other-length len))
               more-vectors)
        (error 'vector-length-mismatch
               :length1 len
               :length2 other-length))))

;; ** Vector constructors
(defun make-double-float-vector (length)
  "Make (SIMPLE-ARRAY DOOUBLE-FLOAT LENGTH)"
  (declare (type fixnum length))
  (make-array length :element-type 'double-float))

(defun make-vector (length &optional (type t))
  "Make (SIMPLE-ARRAY TYPE LENGTH)"
  (declare (type fixnum length))
  (make-array length :element-type type))


(defun vec (type &rest items)
  "Construct a vector with ITEMS of a given TYPE"
  (make-array (length items)
              :element-type type
              :initial-contents items))

;; ** Misc operations
(defun copy-vector-to! (source destination)
  "Copy data from vector SOURCE (SIMPLE-ARRAY * *) into
DESTINATION (SIMPLE-ARRAY DOUBLE-FLOAT *) coercing numbers
in the process"
  (declare (type (simple-array * *) source)
           (type (simple-array double-float *) destination)
           (optimize (speed 3) (safety 1) (debug 1)))
  (check-vector-lengths source destination)
  (let ((length (length source)))
    (dotimes (i length)
      (setf (aref destination i)
            (coerce (aref source i) 'double-float)))))

(defun reduce-vector (op init vector)
  "Reduce VECTOR (from beginning) using OP and startibg with INIT
VECTOR must satisfy (SIMPLE-ARRAY * *)"
  (declare (type (simple-array * *) vector)
           (type (function (T T) T) op)
           (optimize (speed 3) (debug 1) (safety 1)))
  (loop for x across vector
     do (setf init (funcall op init x)))
  init)

(defun dot (v w)
  "Dot product of two vectors:
    __      
   \    v w 
   /__   i i
    i

Lengths of V W must be the same.
Both vectors must be (SIMPLE-ARRAY DOUBLE-FLOAT *)"
  (declare (type (simple-array double-float *) v w)
           (optimize (speed 3) (debug 1) (safety 1)))
  (check-vector-lengths v w)
  (let ((result 0d0))
    (declare (type double-float result))
    (dotimes (i (length v) result)
      (incf result (* (aref v i) (aref w i))))))


(defun l2-norm (v)
  "L2 norm of a (SIMPLE-ARRAY DOUBLE-FLOAT *) v
        __________
        / __   2
    |  / \    v 
    | /  /__   i
    |/    i

"
  (declare (type (simple-array double-float *) v)
           (optimize (speed 3) (debug 1) (safety 1)))
  (let ((result 0d0))
    (declare (type double-float result))
    (dotimes (i (length v) (sqrt result))
      (incf result (* (aref v i) (aref v i))))))

(defun square-vector (v)
  "Dot product of (SIMPLE-ARRAY DOUBLE-FLOAT *) V on itself:
    __
   \    v v 
   /__   i i
    i

"
  (declare (type (simple-array * *) v)
           (optimize (speed 3) (debug 1) (safety 1)))
  (loop for x of-type double-float across v
     summing (* x x) of-type double-float))


(defun l2-norm-diff (vector &rest other-vectors)
  "l2-norm of difference between VECTOR and OTHER-VECTORS
Does not allocate any extra space"
  (declare (type (simple-array * *) vector)
           (optimize (speed 3) (debug 1) (safety 1)))
  (apply #'check-vector-lengths vector other-vectors)
  (match other-vectors
    (nil (l2-norm vector))
    ((list vector2)
     (declare (type (simple-array * *) vector2))
     (sqrt (loop for i below (length vector)
              sum (expt (the double-float
                             (- (aref vector i) (aref vector2 i)))
                        2))))
    (otherwise
     (sqrt
      (loop for i below (length vector)
         sum (expt (reduce #'- other-vectors
                           :initial-value (aref vector i)
                           :key (lambda (v) (aref v i)))
                   2))))))




(defun vector-almost-zero-p (x tolerance)
  "Returns T if vector X is almost zero in terms of L2-norm:
        __________
        / __   2
    |  / \    v    ~= 0
    | /  /__   i
    |/    i

X must be (VECTOR DOUBLE-FLOAT *)
TOLERANCE must be DOUBLE-FLOAT
"
  (declare (type double-float tolerance)
           (type (simple-array double-float *) x)
           (optimize (speed 3) (debug 1) (safety 1)))
  (< (l2-norm x) tolerance))

(defun set-vector-to-zero! (v)
  "Set all components of double-float vector V to zero"
  (declare (type (simple-array double-float *) v)
           (optimize (speed 3) (debug 1) (safety 1)))
  (dotimes (i (length v))
    (setf (aref v i) 0.0d0)))

(defun add-with-multipliers! (v &rest mult-arg)
  "Add vectors with multipliers to V:

    v  =  v  +  a p   +  ...  + a p 
                 1 1             n n

Where a  is a DOUBLE-FLOAT number and
       i
p  is a  (VECTOR DOUBLE-FLOAT *)
 i
MULT-ARG is a squence of CONS-cells (CONS a  p )
                                           i  i
"
  (declare (type (simple-array double-float *) v)
           (optimize (speed 3) (debug 1) (safety 1)))
  (match mult-arg
    (nil nil)
    ((list (cons m arg))
     (declare (type double-float m)
              (type (simple-array double-float *) arg))
     (check-vector-lengths v arg)
     (dotimes (i (length v))
       (incf (aref v i) (* m (aref arg i)))))
    ((list (cons m1 arg1) (cons m2 arg2))
     (declare (type double-float m1 m2)
              (type (simple-array double-float *) arg1 arg2))
     (check-vector-lengths v arg1 arg2)
     (dotimes (i (length v))
       (incf (aref v i)
             (+ (* m1 (aref arg1 i))
                (* m2 (aref arg2 i))))))
    (otherwise
     (apply #'check-vector-lengths v (mapcar #'cdr mult-arg))
     (dotimes (i (length v))
       (incf (aref v i)
             (loop for (m . arg) in mult-arg
                summing (* (the double-float m)
                           (the double-float
                                (aref (the (simple-array double-float *) arg) i)))
                  of-type double-float))))))

(defun linear-combination! (k v &rest coeff-vectors)
  "Computes linear combination

    v  <- k v  + c   u  + ... + c  u
     i       i    0   i          N  i
"
  (declare (optimize (speed 3) (debug 1) (safety 1))
           (type double-float k)
           (type (simple-array double-float *) v))
  (match coeff-vectors
    (nil (dotimes (i (length v))
           (setf (aref v i) (* k (aref v i)))))
    ((list (cons c w))
     (declare (type double-float c)
              (type (simple-array double-float *) w))
     (check-vector-lengths v w)
     (dotimes (i (length v))
       (setf (aref v i)
             (+ (* k (aref v i)) (* c (aref w i))))))
    (otherwise
     (apply #'check-vector-lengths v (mapcar #'cdr coeff-vectors))
     (dotimes (i (length v))
       (setf (aref v i)
             (+ (* k (aref v i))
                (let ((result 0d0))
                  (declare (type double-float result))
                  (dolist (entry coeff-vectors result)
                    (destructuring-bind (c . w) entry
                      (declare (type double-float c)
                               (type (simple-array double-float *) w))
                      (incf result (* c (aref w i))))))))))))


(defun scale-vector! (factor vector &optional (result vector))
  "Multiply VECTOR by FACTOR. RESULT must be the vector of the same length"
  (declare (optimize (speed 3) (debug 1) (safety 1))
           (type double-float factor)
           (type (simple-array double-float *) vector result))
  (check-vector-lengths vector result)
  (dotimes (i (length vector))
    (setf (aref result i) (* factor (aref vector i)))))

(defun negate-vector! (vector &optional (result vector))
  "Negate vector VECTOR (VECTOR DOUBLE-FLOAT *)"
  (declare (type (simple-array double-float *) vector result)
           (optimize (speed 3) (debug 1) (safety 1)))
  (check-vector-lengths vector result)
  (dotimes (i (length vector))
    (setf (aref result i) (- (aref vector i)))))


(defun matrix-mul (matrix vector dest)
  "Multiply MATRIX by VECTOR putting the result into DEST.
Optimized for DOUBLE-FLOAT vectors and matrices"
  (declare (type (simple-array double-float (* *)) matrix)
           (type (simple-array double-float *) vector dest)
           (optimize (speed 3) (debug 1) (safety 0)))
  (destructuring-bind (rows cols) (array-dimensions matrix)
    (declare (type fixnum rows cols))
    (let ((sum-tmp 0d0))
      (declare (type double-float sum-tmp))
      (dotimes (i rows)
        (setf sum-tmp 0d0)
        (dotimes (j cols)
          (incf sum-tmp (* (aref matrix i j) (aref vector j))))
        (setf (aref dest i) sum-tmp)))))

;; (setf (aref dest i) 0.0d0)
;;      (dotimes (j cols)
;;        (setf (aref dest i) (+ (aref dest i)
;;                               (* (aref matrix i j) (aref vector j))))
;;        ;; (incf (aref dest i) (* (aref matrix i j) (aref vector j)))
;;        )

(defun matrix-mul-gen (matrix vector dest)
  "Generically multiply MATRIX by VECTOR putting the result into DEST.
Works on all numerical types"
  (declare (type (simple-array * (* *)) matrix)
           (type (simple-array * *) vector)
           (type (simple-array double-float *) dest)
           (optimize (speed 3) (debug 1) (safety 1)))
  (destructuring-bind (rows cols) (array-dimensions matrix)
    (declare (type fixnum rows cols))
    (dotimes (i rows)
      (setf (aref dest i) 0.0d0)
      (dotimes (j cols)
        (incf (aref dest i) (* (aref matrix i j) (aref vector j)))))))

(defun matrix-mul->function (matrix)
  "Produce a function of a vector, application of which is equivalent
to MATRIX multiplication by a vector"
  (lambda (x dest)
    (declare (optimize (speed 3)))
    (matrix-mul matrix x dest)))


;; ** Generic Linear Solver
(defgeneric solve-linear (method a b x)
  (:documentation
   "Solve linear set of equations A * X = B using METHOD
A must be a function of the form (A P Q) equivalent to A * P = Q
Solution will be put into X. X can act as initial approximation for
  an iterative METHOD
Returns (VALUES X SUCCESSFUL-P FINAL-RESIDUAL-L2-NORM MORE-INFO*)
"))
