(in-package mobius.numeric.linear-base)
;; * General vector operations

;; ** Conditions
(define-condition vector-length-mismatch (error)
  ((vector-length-mismatch-length1 :initarg :length1
                                   :reader vector-length-mismatch-length1)
   (vector-length-mismatch-length2 :initarg :length2
                                   :reader vector-length-mismatch-length2)))

(defun check-vector-lengths (vec &rest more-vectors)
  (let ((len (length vec))
        other-length)
    (or (every (lambda (w)
                 (setf other-length (length w))
                 (eql other-length len))
               more-vectors)
        (error 'vector-length-mismatch
               :length1 len
               :length2 other-length))))

;; ** Vector constructors
(defun make-double-float-vector (length)
  "Make (VECTOR DOOUBLE-FLOAT LENGTH)"
  (declare (type fixnum length))
  (make-array length :element-type 'double-float))

(defun make-vector (length &optional (type t))
  "Make (VECTOR TYPE LENGTH)"
  (declare (type fixnum length))
  (make-array length :element-type type))


(defun vec (type &rest items)
  "Construct a vector with ITEMS of a given TYPE"
  (make-array (length items)
              :element-type type
              :initial-contents items))

;; ** Mics operations
(defun copy-vector-to! (source destination)
  "Copy data from vector SOURCE (VECTOR * *) into
DESTINATION (VECTOR DOUBLE-FLOAT *)"
  (declare (type (vector * *) source)
           (type (vector double-float *) destination))
  (check-vector-lengths source destination)
  (let ((length (length source)))
    (dotimes (i length)
      (setf (aref destination i)
            (coerce (aref source i) 'double-float)))))

(defun reduce-vector (op init vector)
  "Reduce VECTOR (from beginning) using OP and startibg with INIT"
  (declare (type (vector * *) vector)
           (type (function (T T) T) op))
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
Both vectors must be (VECTOR DOUBLE-FLOAT *)"
  (declare (type (vector double-float *) v w))
  (check-vector-lengths v w)
  (loop for x across v
     for y across w
     sum (* x y)))


(defun l2-norm (v)
  "L2 norm of a (VECTOR DOUBLE-FLOAT *) v
        __________
        / __   2
    |  / \    v 
    | /  /__   i
    |/    i

"
  (declare (type (vector double-float *) v))
  (sqrt (loop for x across v summing (* x x))))

(defun square-vector (v)
  "Dot product of (VECTOR DOUBLE-FLOAT *) V on itself:
    __
   \    v v 
   /__   i i
    i

"
  (declare (type (vector double-float *) v))
  (loop for x across v summing (* x x)))


(defun l2-norm-diff (vector &rest other-vectors)
  "l2-norm of difference between VECTOR and OTHER-VECTORS
Does not allocate any extra space"
  (declare (type (vector * *) vector))
  (match other-vectors
    (nil (l2-norm vector))
    ((list vector2)
     (declare (type (vector * *) vector2))
     (apply #'check-vector-lengths vector other-vectors)
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
           (type (vector double-float *) x))
  (< (l2-norm x) tolerance))

(defun set-vector-to-zero! (v)
  "Set all components of double-float vector V to zero"
  (declare (type (vector double-float) v))
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
MULT-ARG is a squence of CONS-cells
 (CONS a  p )
        i  i
"
  (declare (type (vector double-float *) v))
  (match mult-arg
    (nil nil)
    ((list (cons m arg))
     (declare (type double-float m)
              (type (vector double-float *) arg))
     (check-vector-lengths v arg)
     (dotimes (i (length v))
       (incf (aref v i) (* m (aref arg i)))))
    ((list (cons m1 arg1) (cons m2 arg2))
     (declare (type double-float m1 m2)
              (type (vector double-float *) arg1 arg2))
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
                  summing (* m (aref arg i))))))))

(defun linear-combination! (k v &rest coeff-vectors)
  "Computes linear combination

    v  <- k v  + c   u  + ... + c  u
     i       i    0   i          N  i
"
  (match coeff-vectors
    (nil (dotimes (i (length v))
           (setf (aref v i) (* k (aref v i)))))
    ((list (cons c w))
     (check-vector-lengths v w)
     (dotimes (i (length v))
       (setf (aref v i)
             (+ (* k (aref v i)) (* c (aref w i))))))
    (otherwise
     (apply #'check-vector-lengths v (mapcar #'cdr coeff-vectors))
     (dotimes (i (length v))
       (setf (aref v i)
             (+ (* k (aref v i))
                (loop for (c . w) in coeff-vectors
                     summing (* c (aref w i)))))))))


(defun matrix-mul (matrix vector dest)
  "Multiply MATRIX by VECTOR putting the result into DEST.
Optimized for DOUBLE-FLOAT vectors and matrices"
  (declare (type (array double-float (* *)) matrix)
           (type (vector double-float *)))
  (destructuring-bind (rows cols) (array-dimensions matrix)
    (declare (type fixnum rows cols))
    (dotimes (i rows)
      (setf (aref dest i) 0.0d0)
      (dotimes (j cols)
        (incf (aref dest i) (* (aref matrix i j) (aref vector j)))))))

(defun matrix-mul-gen (matrix vector dest)
  "Generically multiply MATRIX by VECTOR putting the result into DEST.
Works on all numerical types"
  (declare (type (array * (* *)) matrix)
           (type (vector * *) vector)
           (type (vector double-float *) dest))
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
    (matrix-mul-gen matrix x dest)))


;; ** Generic Linear Solver
(defgeneric solve-linear (method a b x)
  (:documentation
   "Solve linear set of equations A * X = B using METHOD
A must be a function of the form (A P Q) equivalent to A * P = Q
Solution will be put into X. X can act as initial approximation for
  an iterative METHOD
Returns (VALUES X SUCCESSFUL-P FINAL-RESIDUAL-L2-NORM MORE-INFO*)
"))
