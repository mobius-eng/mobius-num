(in-package mobius.numeric.impl-numbers)

;; * Implementation of linear operations for numbers
;; ** Constructors
(defmethod zero-vector ((u number) &optional dest)
  (declare (ignore dest))
  (coerce 0 (type-of u)))

(defmethod make-vector ((u number) n)
  (declare (ignore u n))
  (coerce 0 (type-of u)))

(defmethod list->vector-method ((u (eql 'integer)) list)
  (coerce (first list) 'fixnum))

(defmethod list->vector-method ((u (eql 'float)) list)
  (coerce (first list) 'double-float))

;; ** Shape
(defmethod vector-dim ((u number))
  (declare (ignore u))
  0)
;; ** Mapping
(defmethod map-vector ((vector number) function
                       &key destination other-vectors with-indices)
  (declare (ignore destination))
  (if with-indices
      (apply function 0 vector other-vectors)
      (apply function vector other-vectors)))

(defmethod reduce-vector ((vector number) reducing-function initial-value
                          &key mapping-function other-vectors)
  (if mapping-function
      (funcall reducing-function
               initial-value
               (apply mapping-function vector other-vectors))
      (funcall reducing-function initial-value vector)))

;; ** Structured multiplications
;; For numbers dot and m* return the same result

(defmethod m* ((u number) (v number) &optional destination)
  (declare (ignore destination))
  (* u v))

(defmethod outer-product ((u number) (v number) &optional buffer)
  (declare (ignore buffer))
  (* u v))

;; ** Structured division
(defmethod m/ ((a number) (b number) &optional x0)
  (declare (ignore x0 dest))
  (/ b a))

;; From CL-NUM-UTILS
(defmethod transpose ((x number)) x)
