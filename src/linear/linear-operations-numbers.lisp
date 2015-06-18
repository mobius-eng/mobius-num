(in-package mobius.numeric.linear-operations-numbers)

;; * Implementation of linear operations for numbers
;; ** Constructors
(defmethod zero-vector ((u number)) (declare (ignore u)) 0)
(defmethod zero-vector! ((u number)) (declare (ignore u)) 0)
(defmethod make-vector ((u number) n) (declare (ignore u n)) 0)
;; ** Shape
(defmethod vector-dim ((u number))
  (declare (ignore u))
  0)
;; ** Mapping
(defmethod map-vector ((u number) f &key dest other-vectors)
  (declare (ignore dest))
  (apply f u other-vectors))

(defmethod map-vector! ((u number) f &key other-vectors)
  (apply f u other-vectors))

(defmethod mapi-vector ((u number) f &key dest other-vectors)
  (declare (ignore dest))
  (apply f 0 u other-vectors))

(defmethod mapi-vector! ((u number) f &key other-vectors)
  (apply f 0 u other-vectors))

(defmethod reduce-vector ((u number) f initial-value)
  (funcall f initial-value u))

;; ** Structured multiplications
;; For numbers dot and m* return the same result
(defmethod dot ((u number) (v number)) (* u v))

(defmethod m* ((u number) (v number) &optional destination)
  (declare (ignore destination))
  (* u v))

(defmethod outer-product ((u number) (v number) &optional buffer)
  (declare (ignore buffer))
  (* u v))

;; ** Structured division
(defmethod m/ ((a number) (b number) &optional x0 dest)
  (declare (ignore x0 dest))
  (/ b a))

;; From CL-NUM-UTILS
(defmethod transpose ((x number)) x)
