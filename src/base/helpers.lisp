(in-package numeric-helpers)

(defun almost-zero-p (number &optional (tolerance default-precision))
  "Returns T if a number x is almost zero: |x| ~= 0"
  (declare (type double-float number tolerance)
           (optimize (speed 3) (debug 1) (safety 1)))
  (< (abs number) tolerance))

(defun quad-extremum (a b)
  "Return extremum of quadratic function
            2
    y(x) = a x  + b x + c

Result does not depend on c:
                 b
    lambda = - -----
                2 a
"
  (declare (type double-float a b)
           (optimize (speed 3) (debug 1) (safety 1)))
  (- (/ b (* 2d0 a))))


(defun solve-quadratic (a b c)
  "Solve quadratic equation

       2
    a x  + b x + c

in real numbers as:

         2
    D = b  - 4 a c

                 ____
          -b - |/ D                     2 c
    x  = ------------- (if b > 0) = ----------- (if b < 0)
     1       2 a                       ___
                                     |/ D  - b


                 ___
          -b + |/ D                  - 2 c
    x  = ------------ (if b < 0)= ------------ (if b > 0)
     2       2 a                     ___
                                   |/ D  + b

Returns the list of (LAMBDA ()) functions calculating the solution
"
  (declare (type double-float a b c))
  (let ((d (- (expt b 2) (* 4d0 a c))))
    (cond ((minusp d) nil)
          ((plusp b)
           (let ((sqrt-d (sqrt d)))
             (list (lambda () (/ (- (+ b sqrt-d)) (* 2d0 a)))
                   (lambda () (/ (- (* 2d0 c)) (+ sqrt-d b))))))
          (t
           (let ((sqrt-d (sqrt d)))
             (list (lambda () (/ (* 2d0 c) (- sqrt-d b)))
                   (lambda () (/ (- sqrt-d b) (* 2d0 a)))))))))

(defun cubic-extremum-1 (a b c)
  "Return the smallest extremum (if a > 0) of a cubic function
              3      2
    y(x) = a x  + b x  + c x + d

calculated as the smallest root of
                 2
    y'(x) = 3 a x  + b x + c = 0

If no root is found, returns -INFINITY"
  (match (solve-quadratic (* 3d0 a) (* 2d0 b) c)
    (nil -infinity)
    ((list  x1 _) (funcall x1))))

(defun cubic-extremum-2 (a b c)
  "Return the largest extremum (if a > 0) of a cubic function
              3      2
    y(x) = a x  + b x  + c x + d

calculated as the largest root of
                 2
    y'(x) = 3 a x  + b x + c = 0

If no root is found, returns +INFINITY"
  (match (solve-quadratic (* 3d0 a) (* 2d0 b) c)
    (nil +infinity)
    ((list _ x2) (funcall x2))))

(defun linear-approximation (x0 f0 x1 f1)
  "Calculate coefficients of the line passing through
(x0,f0) and (x1,f1), providing linear approximation to f(x)"
  (values (/ (- f1 f0) (- x1 x0)) f0))

(defun quad-approximation (x0 f0 df0 x1 f1)
  "Calculate quadratic approximation of f(x) satisfying
  - f(x0) = f0
  - [D f](x0) = df0
  - f(x1) = f1
Returns coefficients of the quadratic function approximating f(x)"
  (let ((delta-f (- f1 f0))
        (delta-x (- x1 x0)))
    (let* ((a (- (/ delta-f (expt delta-x 2))
                 (/ df0 delta-x)))
           (b (- df0 (* 2 a x0)))
           (c (- f0 (* a x0 x0) (* b x0))))
      (values a b c))))

(defun cubic-approximation (f0 Df0 x1 f1 x2 f2)
  "Return coefficient of cubic funcation approximating f(x):
  - f(0) = f0
  - [D f] (0) = df0
  - f(x1) = f1
  - f(x2) = f2"
  (let ((delta-x (/ (- x1 x2)))
        (p (- f1 (* df0 x1) f0))
        (q (- f2 (* df0 x2) f0)))
    (let ((a (* delta-x (- (* (expt x1 -2) p)
                           (* (expt x2 -2) q))))
          (b (* delta-x (- (* (/ x1 (expt x2 2)) q)
                           (* (/ x2 (expt x1 2)) p)))))
      (values a b df0 f0))))

(defun comp (f &rest more)
  "Returns composition of functions of one argument"
  (if (null more)
      f
      (lambda (x)
        (reduce (lambda (g v) (funcall g v))
                (cons f more)
                :initial-value x
                :from-end t))))
