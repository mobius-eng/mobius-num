(in-package cl-user)

(defpackage #:gsl-linear
  (:use #:cl #:gsl))

(in-package gsl-linear)

(defconstant not-a-number gsl:+nan+)

(defconstant +infinity
  #+sbcl sb-ext:double-float-positive-infinity
  #+clozure 1d++0
  "Representation of floating point +infinity")

(defconstant -infinity
  #+sbcl sb-ext:double-float-negative-infinity
  #+clozure -1d++0
  "Representation of floating point -infinity")


(defun zero-vector (source &optional destination)
  (unless destination
    (setf destination (grid:copy source)))
  (let ((element-type (grid:element-type destination)))
    (grid:map-grid :source source
                   :destination destination
                   :element-function (constantly (coerce 0 element-type)))))

(defun copy-vector (source &optional destination)
  (grid:copy source :destination destination))

(defun make-vector (source new-dimensions)
  (grid:make-foreign-array (grid:element-type source) :dimensions new-dimensions))

(defun list->vector (element-type list)
  (grid:make-foreign-array element-type :initial-contents list))

(defun vector-of (element-type &rest args)
  (list->vector element-type args))

(defun negate-vector (vector &optional destination)
  (unless destination
    (setf destination (grid:copy vector)))
  (let ((element-type (grid:element-type destination)))
    (grid:map-grid :source vector
                   :destination destination
                   :element-function (lambda (x) (coerce (- x) element-type)))))

(defun vector-dim (vector)
  (first (grid:dimensions vector)))

(defun m* (A b &optional destination)
  (unless destination
    (setf destination (copy-vector b)))
  (matrix-product A b destination 1.0 0.0))


;; TODO: update in case A is already decomposed, in this case
;; PERMUTATION must be supplied as well
(defun m/ (A b &optional x work-A permutation)
  (if work-A
      (grid:copy A :destination work-A)
      (setf work-A (grid:copy A)))
  (format t "~&work-A = ~A~%" work-A)
  (unless x
    (setf x (copy-vector b)))
  (unless permutation
    (setf permutation (make-permutation (vector-dim b))))
  (format t "~&x = ~A~%" x)
  (multiple-value-bind (lu perm) (lu-decomposition work-A permutation)
    (lu-solve lu b perm x)))


(defun dot (a b) (grid:inner a b))

(defun squared-L2-norm (v) (dot v v))

(defvar *norm-type* 2)


(defun reduce-vector (vector reducing-function initial-value &key mapping-function other-vectors)
  (let ((n (vector-dim vector))
        (result initial-value))
    (if mapping-function
        (dotimes (i n result)
          (setf result (funcall reducing-function
                                result
                                (apply mapping-function
                                       (grid:aref vector i)
                                       (mapcar (lambda (v) (grid:aref v i)) other-vectors)))))
        
        (dotimes (i n result)
          (setf result (funcall reducing-function
                                result
                                (grid:aref vector i)))))))

(defun norm (v &optional (*norm-type* *norm-type*))
  (cond ((= *norm-type* 2) (grid:norm v))
        ((= +infinity *norm-type*) (mmax v))
        ((= *norm-type* 1) (reduce-vector v #'+ 0.0 :mapping-function #'abs))
        (t (expt (reduce-vector v #'+ 0.0 :mapping-function (lambda (x) (expt x *norm-type*)))
                 (/ *norm-type*)))))

(defun .abs (v) (funcall #.(grid:elementwise #'abs) v))

(defun .sin (v) (funcall #. (grid:elementwise #'sin) v))

(defun .cos (v) (funcall #.(grid:elementwise #'cos) v))

(defun .tan (v) (funcall #.(grid:elementwise #'tan) v))

(defun .exp (v) (funcall #.(grid:elementwise #'exp) v))

(defun .log (v) (funcall #. (grid:elementwise #'log) v))

(defun .signum (v) (funcall #.(grid:elementwise #'signum) v))


(defun .abs! (v)
  (grid:map-grid))
