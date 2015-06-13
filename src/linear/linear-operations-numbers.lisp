(in-package mobius.numeric.linear-operations-numbers)

;; * Implementation of linear operations for numbers
;; ** Shapes

(defmethod vector-dim ((u number))
  (declare (ignore u))
  0)

(defmethod transpose ((x number))
  x)

;; ** Norm
(defmethod norm ((v number)) (abs v))

;; ** Negate
(defmethod elt-negate ((u number)) (- u))
(defmethod elt-negate! ((u number)) (- u))

;; ** Zero
(defmethod elt-zero ((u number)) (declare (ignore u)) 0)
(defmethod elt-zero! ((u number)) (declare (ignore u)) 0)

;; ** Arithmetic
(defmacro define-number-methods-elt (primitive-op)
  (let ((pure-op (intern (format nil "ELT~A" primitive-op)))
        (assign-op (intern (format nil "ELT=~A!" primitive-op)))
        (dest-op (intern (format nil "ELT~A!" primitive-op))))
    (with-gensyms (u v dest)
      `(progn
         (defmethod ,pure-op ((,u number) (,v number)) (,primitive-op ,u ,v))
         (defmethod ,assign-op ((,u number) (,v number))
           (,primitive-op ,u ,v))
         (defmethod ,dest-op ((,dest number) (,u number) (,v number))
           (declare (ignore ,dest))
           (,primitive-op ,u ,v))))))

(define-number-methods-elt +)
(define-number-methods-elt -)
(define-number-methods-elt *)
(define-number-methods-elt /)

(defmethod elt=-rev/! ((u number) (v number)) (/ v u))

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

