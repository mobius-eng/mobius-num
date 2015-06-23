(in-package linfunctions)

;; * More constructors
(defun duplicate-vector (v &optional (dest (zero-vector v)))
  "Duplicate vector V, if DEST is provided, copy V into DEST"
  (map-vector v #'identity :destination dest))

(defun negate-vector (v &optional (dest (zero-vector v)))
  "Negate vector. If V is used as DEST: destructively negate"
  (map-vector v #'(lambda (elt) (- elt)) :destination dest))

(defun one-vector (v &optional dest)
  "Construct vector containing ones"
  (cond ((numberp v) (make-array v
                                 :element-type 'double-float
                                 :initial-element 1.0d0))
        (t (let ((dest (or dest (zero-vector v))))
             (map-vector dest (constantly 1.0d0) :destination dest)))))


;; * DOT and NORM
(defun dot (u v)
  "Dot (inner)product of two vectors.
This function is less strict on vector dimensions compared to INNER-PRODUCT and M*"
  (reduce-vector u #'+ 0.0d0
                 :mapping-function #'*
                 :other-vectors (list v)))

(defun inner-product (u v &optional dest)
  "Inner product of two vector objects"
  (m* u v dest))

(defvar *norm-type* 2
  "The type of norm to be used by NORM. Possible values:
NIL : infinity-norm
1   : L1-norm
2   : L2-norm
...")

(defun squared-L2-norm (v)
  "Squared L2-norm of the vector V"
  (dot v v))

(defun norm (v &optional (*norm-type* *norm-type*))
  "Generalised vector norm. Type of norm is controlled by *NORM-TYPE*"
  (cond ((= *norm-type* +infinity)
         (reduce-vector v #'(lambda (r x) (max r (abs x))) +infinity))
        ((= *norm-type* 1) (reduce-vector v #'(lambda (result x) (+ result (abs x))) 0.0d0))
        ((= *norm-type* 2) (sqrt (reduce-vector v
                                                #'(lambda (result x) (+ result (* x x)))
                                                0.0d0)))
        (t (expt (reduce-vector v
                                #'(lambda (result x) (+ result (expt x *norm-type*)))
                                0.0d0)
                 (/ *norm-type*)))))

;; * Elementwise operations
;; ** Utility: helps to sort out arguments
(declaim (inline separate))
(defun separate (predicate list)
  (declare (optimize (speed 3))
           (type function predicate) (type list list))
  (loop for x in list
     if (funcall predicate x)
     collect x into true-values
     else
     collect x into false-values
     end
     finally (return (values true-values false-values))))

;; ** + - * /
(defmacro define-e-ops (op name)
  "Defines elementwise operations on vectors and numbers of the form
.$, .$! and .=$! where `$' is an elementary operation.
NAME is used to generate a docstring"
  (let ((e-op (intern (format nil ".~A" op)))
        (e-op! (intern (format nil ".~A!" op)))
        (e=op! (intern (format nil ".=~A!" op)))
        (doc-string (format nil "Elementwise ~A of vectors and numbers" name))
        (doc-string!
         (format nil "Elementwise ~A of vectors and numbers. Store result in DEST"
                 name))
        (doc-string=!
         (format nil "Destructive elementwise ~A of vectors and numbers" name)))
    `(progn
       (defun ,e-op (vector &rest others)
         ,doc-string
         (multiple-value-bind (numbers other-vectors) (separate #'numberp others)
           (map-vector vector
                       #'(lambda (&rest args)
                           (apply #',op (nconc args numbers)))
                       :other-vectors other-vectors)))
       (defun ,e-op! (dest vector &rest others)
         ,doc-string!
         (multiple-value-bind (numbers other-vectors) (separate #'numberp others)
           (map-vector vector
                       #'(lambda (&rest args)
                           (apply #',op (nconc args numbers)))
                       :destination dest
                       :other-vectors other-vectors)))
       (defun ,e=op! (vector &rest others)
         ,doc-string=!
         (multiple-value-bind (numbers other-vectors) (separate #'numberp others)
           (map-vector vector
                       #'(lambda (&rest args)
                           (apply #',op (nconc args numbers)))
                       :destination vector
                       :other-vectors other-vectors))))))

(define-e-ops + "addition")
(define-e-ops - "subtruction")
(define-e-ops * "multiplication")
(define-e-ops / "division")

;; ** "Reversed" division
(defun .rev/ (u v)
  "Divide V by U"
  (./ v u))
(defun .rev/! (dest u v)
  "Divide V by U and use DEST to store the result"
  (./! dest v u))

(defun .=rev/! (u v)
  "Divide V by U, store the result in U"
  (cond ((numberp v) (map-vector u #'(lambda (x) (/ v x)) :destination u))
        (t (map-vector u #'(lambda (x y) (/ y x)) :destination u :other-vectors (list v)))))

;; ** Unary elementwise functions
;; *** Helper
(defmacro define-vector-function (function)
  (let ((efunction (intern (format nil ".~A" function)))
        (efunction! (intern (format nil ".~A!" function)))
        (docstring (format nil "Apply ~A elementwise to a vector" function))
        (docstring! (format nil
                            "Destructuvely apply ~A elementwise to a vector"
                            function)))
    `(progn
       (defun ,efunction (vector &optional (dest (zero-vector vector)))
         ,docstring
         (map-vector vector #',function :destination dest))
       (defun ,efunction! (vector)
         ,docstring!
         (map-vector vector #',function :destination vector)))))

;; *** Functions
(define-vector-function abs)
(define-vector-function sin)
(define-vector-function cos)
(define-vector-function tan)
(define-vector-function exp)
(define-vector-function signum)

;; * Some odd ones
(defun identity-matrix (n &optional (element-type 'double-float))
  (if (zerop n)
      (coerce 1 element-type)
      (let ((a (make-array (list n n)
                           :element-type element-type
                           :initial-element 0.0d0)))
        (loop for i below n
           do (setf (aref a i i) 1.0d0))
        a)))

;; More criteria
(defmethod compile-criterium ((type (eql :converged)) &rest args)
  (let ((close-p (first args))
        (info (second args)))
    (in-criterium (x (last-v nil))
      (let ((v (iterator:value x)))
        (cond ((null last-v) (setf last-v (duplicate-vector v)))
              ((funcall close-p last-v v)
               (iterator:add-info (iterator:->finished x) :converged info))
              (t (setf last-v (duplicate-vector v last-v))))))))

;; TODO: Think how to improve it - don't want allocation in .-
(defmethod compile-criterium ((type (eql :converged-norm)) &rest args)
  (let ((tolerance (first args))
        (info (second args)))
    (compile-criterium :converged
                       #'(lambda (x y) (< (norm (.- x y)) (* (norm x) tolerance)))
                       (list :converged-norm info))))

