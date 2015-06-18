(in-package mobius.numeric.linear-operations)

(declaim (optimize (speed 3) (debug 1) (space 1) (safety 1)))

;; * Linear operations on (mostly) vectors
;; ** Generics
;; *** Constructors
;; **** Zero: make vector zero
(defgeneric zero-vector (u)
  (:documentation "Produces new zero-value of the type of U"))
(defgeneric zero-vector! (u) (:documentation "Makes U to become a zero-vector"))
;; **** Make a zero vector of a particular type of length n
(defgeneric make-vector (vector-type n)
  (:documentation "Make vector of type VECTOR-TYPE of length N"))
;; *** Shape
(defgeneric vector-dim (u)
  (:documentation "Returns the dimensiality of a vector"))
;; *** Iterators over vectors
(defgeneric map-vector (u f &key dest other-vectors)
  (:documentation "Apply F elementwise to vector U and OTHER-VECTORS,
use DEST as a buffer for the result (will be created if not provided."))

(defgeneric mapi-vector (u f &key dest other-vectors)
  (:documentation "Apply F elementwise to vector U and OTHER-VECTORS,
use DETS as a buffer for the result if provided. F must accept index
as the first argument and all vector values."))

(defgeneric map-vector! (u f &key other-vectors)
  (:documentation "Destructively apply F to all components
of vector U and OTHER-VECTORS"))

(defgeneric mapi-vector! (u f &key other-vectors))
;; *** Reduction
(defgeneric reduce-vector (u f initial-value)
  (:documentation "Reduce vector U using function F starting from INITIAL-VALUE.
This method makes no guarantee on the order of reduction, thus the operation
implemented by F must be associative"))
;; ** Matrix multiplication and inner product
;; For optimization reasons DOT is not implemented as reduction of
;; vector mapping: this will require memory allocation for a new
;; temporary vector
(defgeneric dot (u v)
  (:documentation "Dot product of two vectors"))

(defgeneric outer-product (u v &optional buffer)
  (:documentation "Outer product of two vectors. If BUFFER is provided, result is placed there"))

(defgeneric m* (A b &optional destination)
  (:documentation "Inner product of A by B. If DESITNATION is provided, store the result there"))

(defgeneric m/ (A b &optional x0 x)
  (:documentation "Solve linear equations Ax=b. X0 is initial approximation, X is the buffer to place the result"))


;; * Functions
;; ** Constructors
(defun duplicate-vector (v &optional (dest (zero-vector v)))
  "Duplicate vector V, if DEST is provided, copy V into DEST"
  (map-vector v #'identity :dest dest))

(defun negate-vector (v &optional (dest (zero-vector v)))
  (map-vector v #'(lambda (elt) (- elt)) :dest dest))

(defun negate-vector! (v)
  (map-vector! v #'(lambda (elt) (- elt))))


;; ** Norm
(defvar *norm-type* 2
  "The type of norm to be used by NORM. Possible values:
NIL : infinity-norm
1   : L1-norm
2   : L2-norm
...")

(defun norm (v)
  "Generalised vector norm. Type of norm is controlled by *NORM-TYPE*"
  (cond ((= *norm-type* ++INF+) (reduce-vector v #'(lambda (r x) (max r (abs x))) +-INF+))
        ((= *norm-type* 1) (reduce-vector v #'(lambda (result x) (+ result (abs x))) 0.0d0))
        ((= *norm-type* 2) (sqrt (reduce-vector v
                                                #'(lambda (result x) (+ result (* x x)))
                                                0.0d0)))
        (t (expt (reduce-vector v
                                #'(lambda (result x) (+ result (expt x *norm-type*)))
                                0.0d0)
                 (/ *norm-type*)))))

;; ** Elementwise arithmetic operations
;; *** Utilities
(declaim (inline separate))
(defun separate (predicate list)
  (declare (optimiz (speed 3))
           (type function predicate) (type list list))
  (loop for x in list
     if (funcall predicate x)
     collect x into true-values
     else
     collect x into false-values
     end
     finally (return (values true-values false-values))))
;; *** + - * /
(defmacro define-e-ops (op name)
  "Defines elementwise operations on vectors and numbers of the form
E$, E$! and E=$! where `$' is an elementary operation.
NAME is used to generate a docstring"
  (let ((e-op (intern (format nil "E~A" op)))
        (e-op! (intern (format nil "E~A!" op)))
        (e=op! (intern (format nil "E=~A!" op)))
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
                       :dest dest
                       :other-vectors other-vectors)))
       (defun ,e=op! (vector &rest others)
         (multiple-value-bind (numbers other-vectors) (separate #'numberp others)
           (map-vector! vector
                        #'(lambda (&rest args)
                            (apply #',op (nconc args numbers)))
                        :other-vectors other-vectors))))))

(define-e-ops + "addition")
(define-e-ops - "subtruction")
(define-e-ops * "multiplication")
(define-e-ops / "division")

;; *** "Reversed" division
(defun e-rev/ (u v)
  "Divide V by U"
  (e/ v u))
(defun e-rev/! (dest u v)
  "Divide V by U and use DEST to store the result"
  (e/! dest v u))
(defun e=-rev/! (u v)
  "Divide V by U, store the result in U"
  (cond ((numberp v)
         (map-vector! u #'(lambda (x) (/ v x))))
        (t (map-vector! u #'(lambda (x y) (/ y x)) :other-vectors (list v)))))

;; *** More functions

(defmacro define-vector-function (function)
  (let ((efunction (intern (format nil "E~A" function)))
        (efunction! (intern (format nil "E~A!" function)))
        (docstring (format nil "Apply ~A elementwise to a vector" function))
        (docstring! (format nil
                            "Destructuvely apply ~A elementwise to a vector"
                            function)))
    `(progn
       (defun ,efunction (vector &optional (dest (zero-vector vector)))
         ,docstring
         (map-vector vector #',function :dest dest))
       (defun ,efunction! (vector)
         ,docstring!
         (map-vector! vector #',function)))))

(define-vector-function abs)
(define-vector-function sin)
(define-vector-function cos)
(define-vector-function tan)
(define-vector-function exp)
(define-vector-function signum)

;; *** Alias for M*
(defun inner-product (u v) (m* u v))

;; *** Some odd-once

(defun identity-matrix (n)
  (if (zerop n)
      1.0d0
      (let ((a (make-array (list n n)
                           :element-type 'double-float
                           :initial-element 0.0d0)))
        (loop for i below n
           do (setf (aref a i i) 1.0d0))
        a)))

;; (defun linspace (start end &key length step)
;;   (cond (length (let ((step (/ (- end start) (1- length))))
;;                   (compute-mvector length
;;                                    (lambda (i) (+ start (* step i))))))
;;         (step (let ((length (1+ (truncate (/ (- end start) step)))))
;;                 (compute-mvector length
;;                                  (lambda (i) (+ start (* step i))))))
;;         (t (error "LINSPACE: You must provide either length or step"))))
