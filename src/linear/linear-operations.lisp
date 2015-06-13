(in-package mobius.numeric.linear-operations)

(declaim (optimize (speed 3) (debug 1) (space 1) (safety 1)))

;; * Generics
;; ** Shapes
(defgeneric vector-dim (u)
  (:documentation "Returns the dimensiality of a vector"))


;; ** Norm
(defvar *norm-type* 2
  "The type of norm to be used by NORM. Possible values:
NIL : infinity-norm
1   : L1-norm
2   : L2-norm
...")

(defgeneric norm (v)
  (:documentation "Generalised vector norm. Type of norm is controlled by *NORM-TYPE*"))

;; ** Elementwise arithmetic operations
;; *** Macro to generate all the type of operations
;; For example: ELT+, ELT=+!, ELT+!
(defmacro define-generic-elt (op op-name)
  (let ((pure-op (intern (format nil "ELT~A" op)))
        (assign-op (intern (format nil "ELT=~A!" op)))
        (dest-op (intern (format nil "ELT~A!" op))))
    `(progn
       (defgeneric ,pure-op (u v) (:documentation ,(format nil "Elementwise ~A" op-name)))
       (defgeneric ,assign-op (u v)
         (:documentation ,(format nil "Elementwise ~A, possibly puts the result into U"
                                  op-name)))
       (defgeneric ,dest-op (dest u v)
         (:documentation ,(format nil "Eelementwise ~A, uses DEST to keep the result"
                                  op-name))))))

(define-generic-elt + "addition")
(define-generic-elt - "subtruction")
(define-generic-elt * "multiplication")
(define-generic-elt / "division")

(defgeneric elt=-rev/! (u v)
  (:documentation "Elementwise reverse division, possibly uses U to store the result"))

;; ** Negation
(defgeneric elt-negate (u) (:documentation "Negate the vector"))
(defgeneric elt-negate! (u) (:documentation "Destructive vector negation"))
;; *** Zero: make vector zero
(defgeneric elt-zero (u) (:documentation "Produces new zero-value of the type of U"))
(defgeneric elt-zero! (u) (:documentation "Makes U to become a zero-vector"))

;; ** Matrix multiplication and inner product
(defgeneric dot (u v)
  (:documentation "Dot product of two vectors"))

(defgeneric outer-product (u v &optional buffer)
  (:documentation "Outer product of two vectors. If BUFFER is provided, result is placed there"))

(defgeneric m* (A b &optional destination)
  (:documentation "Inner product of A by B. If DESITNATION is provided, store the result there"))

(defgeneric m/ (A b &optional x0 x)
  (:documentation "Solve linear equations Ax=b. X0 is initial approximation, X is the buffer to place the result"))

;; ** Convinience functions
;; *** Aliases
(defun inner-product (u v) (m* u v))

;; *** Reverse division
(defun e-rev/ (u v) (elt/ v u))
(defun e=-rev/! (u v) (elt=-rev/! u v))
(defun e-rev/! (dest u v) (elt/! dest v u))

;; *** Pure
(defun e+ (u &rest us)
  (cond ((null us) u)
        (t (let ((v (elt+ u (car us))))
             (reduce #'elt=+! (cdr us) :initial-value v)))))

(defun e- (u &rest us)
  (cond ((null us) (elt-negate u))
        (t (let ((v (elt- u (car us))))
             (reduce #'elt=-! (cdr us) :initial-value v)))))

(defun e* (u &rest us)
  (cond ((null us) u)
        (t (let ((v (elt* u (car us))))
             (reduce #'elt=*! (cdr us) :initial-value v)))))

(defun e/ (u &rest us)
  (cond ((null us) (elt/ 1.0d0 u))
        (t (let ((v (elt/ u (car us))))
             (reduce #'elt=/! (cdr us) :initial-value v)))))

;; *** Assigning
(defun e=+! (u &rest us)
  (reduce #'elt=+! us :initial-value u))
(defun e=*! (u &rest us)
  (reduce #'elt=*! us :initial-value u))

(defun e=-! (u &rest us)
  (if (null us)
      (elt-negate! u)
      (reduce #'elt=-! us :initial-value u)))

(defun e=/! (u &rest us)
  (if (null us)
      (elt=-rev/! u 1.0d0)
      (reduce #'elt=/! us :initial-value u)))

(defun e=-rev/ (u v) (elt=-rev/ u v))

;; *** Destructive on destination
(defun e+! (dest u &rest us)
  (cond ((null us)
         (elt-zero! dest)
         (elt=+! dest u))
        (t (reduce (lambda (res v) (elt+! dest res v)) us :initial-value u))))

(defun e-! (dest u &rest us)
  (cond ((null us)
         (elt-zero! dest)
         (elt=-! dest u))
        (t (reduce (lambda (res v) (elt-! dest res v)) us :initial-value u))))

(defun e*! (dest u &rest us)
  (cond ((null us)
         (elt-zero! dest)
         (elt=+! dest u))
        (t (reduce (lambda (res v) (elt*! dest res v)) us :initial-value u))))

(defun e/! (dest u &rest us)
  (cond ((null us)
         (elt-zero! dest)
         (elt=+! dest u)
         (elt=-rev/! dest 1.0d0))
        (t (reduce (lambda (res v) (elt/! dest res v)) us :initial-value u))))

;; (defun linspace (start end &key length step)
;;   (cond (length (let ((step (/ (- end start) (1- length))))
;;                   (compute-mvector length
;;                                    (lambda (i) (+ start (* step i))))))
;;         (step (let ((length (1+ (truncate (/ (- end start) step)))))
;;                 (compute-mvector length
;;                                  (lambda (i) (+ start (* step i))))))
;;         (t (error "LINSPACE: You must provide either length or step"))))
