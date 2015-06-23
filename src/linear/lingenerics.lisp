(in-package mobius.numeric.linear-generics)

;; * Constructors
(defgeneric zero-vector (original-vector &optional destination)
  (:documentation
   "Produces zero-vector of type and size of ORIGINAL-VECTOR.
If DESTINATION is provided, use it for the result.
DESTINATION can be EQ to ORIGINAL-VECTOR"))

(defgeneric make-vector (original-vector new-dim)
  (:documentation
   "Makes a vector of the same type as ORIGINAL-VECTOR but of
different dimension"))

(defun list->vector (vector-type list)
  "Convert list into a vector of the specified type.
Most ofthen will return a vector with DOUBLE-FLOAT elements or elements
of the type of the first item in the list"
  (cond ((symbolp vector-type) (list->vector-method vector-type list))
        ((integerp vector-type) (list->vector-method 'integer list))
        ((floatp vector-type) (list->vector-method 'float list))
        ((vectorp vector-type) (list->vector-method 'vector list))
        ((arrayp vector-type) (list->vector-method 'array list))
        (t (list->vector-method (type-of vector-type) list))))

(defgeneric list->vector-method (vector-type list)
  (:documentation "Convenient way of producing vectors from list"))


;; * Shape
(defgeneric vector-dim (vector)
  (:documentation
   "Returns the length (dimensionality of the VECTOR"))

;; * Iterations over vectors
(defgeneric map-vector (vector function &key destination other-vectors with-indices)
  (:documentation
   "Map over items of the VECTOR and OTHER-VECTORS with FUNCTION.
Use DESTINATION (if provided) to keep the result.
VECTOR might be EQ to DESTINATION"))

;; * Reduction
(defgeneric reduce-vector (vector reducing-function initial-value
                           &key mapping-function other-vectors)
  (:documentation
   "Reduce VECTOR with REDUCING-FUNCTION starting from INITIAL-VALUE.
If MAPPING-FUNCTION is provided, transform each VECTOR item using it
before applying REDUCING-FUNCTION"))

;; * Operations
(defgeneric outer-product (u v &optional buffer)
  (:documentation
   "Outer product of two vectors. If BUFFER is provided, result is placed there"))

(defgeneric m* (A b &optional destination)
  (:documentation
   "Matrix product of A by B. If DESITNATION is provided, store the result there"))

(defgeneric m/ (A b &optional x0)
  (:documentation
   "Solve linear equations Ax=b. X0 is initial approximation"))

