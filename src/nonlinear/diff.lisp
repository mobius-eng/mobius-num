(in-package mobius-num.diff)

(defvar *diff-step* 1.0d-6)

(defgeneric generic-d (x f df)
  (:documentation "Generic differentiation function"))

(defmethod generic-d ((x number) (f function) df)
  (declare (ignore df))
  (/ (- (funcall f (+ x *diff-step*) nil)
        (funcall f (- x *diff-step*) nil))
     (+ *diff-step* *diff-step*)))

(defmethod generic-d ((x mvector) (f function) df)
  (let* ((n (mvlength x))
         (dx *diff-step*)
         (dx2 (+ *diff-step* *diff-step*))
         (df (or df (make-array (list n n ) :element-type 'double-float)))
         (x+/- (copy-mvector x))
         (f+ (zero-up n))
         (f- (zero-up n)))
    (loop for i from 0 below n
       do (progn
            (setf (mvref x+/- i) (+ (mvref x+/- i) dx))
            (setf f+ (funcall f x+/- f+))
            (setf (mvref x+/- i) (- (mvref x+/- i) dx2))
            (setf f- (funcall f x+/- f-))
            (setf (mvref x+/- i) (mvref x i))
            (elt/! (elt-! f+ f-) dx2)
            (setf (slice df t i) (mvector-datum f+))))
    df))

(defmethod generic-d ((x array) (f function) df)
  (let* ((n (array-dimension x 0))
         (dx *diff-step*)
         (dx2 (+ dx dx))
         (df (or df (make-array (list n n) :element-type 'double-float)))
         (x+/- (make-array n :element-type 'double-float :initial-contents x))
         (f+ (make-array n :element-type 'double-float))
         (f- (make-array n :element-type 'double-float)))
    (loop for i from 0 below n
       do (progn
            (setf (aref x+/- i) (+ (aref x+/- i) dx))
            (setf f+ (funcall f x+/- f+))
            (setf (aref x+/- i) (- (aref x i) dx))
            (setf f- (funcall f x+/- f-))
            (setf (aref x+/- i) (aref x i))
            (loop for j from 0 below n
               do (setf (aref f+ j) (/ (- (aref f+ j)
                                          (aref f- j))
                                       dx2)))
            (setf (slice df t i) f+)))
    df))

(defun deriv (f)
  "Differentiation operator on the function F. Returns a function of X"
  (lambda (x &optional df)
    (generic-d x f df)))
