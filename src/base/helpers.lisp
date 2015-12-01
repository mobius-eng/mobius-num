(in-package numeric-helpers)

(defun almost-zero-p (x tolerance)
  "Returns T if a number x is almost zero: |x| ~= 0"
  (declare (type double-float x tolerance))
  (< (abs x) tolerance))

(defun quad-extremum (a b)
  "Return extremum of quadratic function
            2
    y(x) = a x  + b x + c

Result does not depend on c:
                 b
    lambda = - -----
                2 a
"
  (- (/ b (* 2 a))))


(defun cubic-extremum-1 (a b c)
  "Return the smallest extremum (if a > 0) of a cubic function
              3      2
    y(x) = a x  + b x  + c x + d

as given by
                        __________
                       / 2
               - b - |/ b  - 3 a c              c
    lambda  = --------------------- = --------------------
                       3 a                 __________
                                          / 2
                                        \/ b  - 3 a c - b

If b > 0 the former is used, if b < 0: the latter"
  (let ((d (sqrt (- (* b b) (* 3 a c)))))
   (if (minusp b)
       (/ c (- d b))
       (/ (- (+ b d)) (* 3 a)))))

(defun cubic-extremum-2 (a b c)
  "Return the largest extremum (if a > 0) of a cubic function
              3      2
    y(x) = a x  + b x  + c x + d

as given by
                        __________
                       / 2
               - b + |/ b  - 3 a c           - c
    lambda  = --------------------- = --------------------
                       3 a                 __________
                                          / 2
                                        \/ b  - 3 a c + b

If b < 0 the former is used, if b > 0: the latter"
  (let ((d (sqrt (- (* b b) (* 3 a c)))))
   (if (minusp b)
       (/ (- d b) (* 3 a))
       (- (/ c (+ d b))))))


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
