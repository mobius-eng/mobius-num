(in-package num-tests-ad)

(def-suite ad-test-suite
    :description "Test suite for AD module")

(in-suite ad-test-suite)

(test test-symbolic-functions
  "Tests the application of common functions to symbolic arguments"
  (is (equalp (sin 'x) '(sin x)))
  (is (equalp (cos 'x) '(cos x)))
  (is (equalp (tan 'x) '(tan x)))
  (is (equalp (+ (sin 'x) (cos 'y))
              '(+ (sin x) (cos y))))
  (is (equalp (funcall (comp #'sin #'exp) 'x)
              '(sin (exp x)))))

(test test-numeric-functions
  "Functions must behave numerically as expected"
  (let ((x (random 1.0d0)))
    (is (eq (type-of (sin x)) 'double-float))
    (is (cl:= (sin x) (cl:sin x)))
    (is (cl:= (+ (sin x) (cos x))
              (cl:+ (cl:sin x) (cl:cos x))))
    (is (cl:= (exp x) (cl:exp x)))))

(test test-literal-vector
  "Literal vector must produce a vector"
  (let ((v (literal-vector 'v 5)))
    (is (vectorp v))
    (is (simple-vector-p v))
    (is (= (length v) 5))
    (is (eq (svref v 2) 'v^2))))

(test test-literal-function
  (let ((f (literal-function 'f)))
    (is (functionp f))
    (is (equalp (funcall f 'x) '(f x)))
    (is (equalp (funcall (comp f #'tan) 'x)
                '(f (tan x))))))


(test test-comparisons
  "Test if comparison operations give expected result"
  (is-false (zerop 'x))
  (is-false (plusp 'p))
  (is-false (minusp 'q))
  (is (= 'x 'x))
  (is (<= 'x 'x))
  (is-false (< 'x 'x))
  (is-false (= 'x 2))
  (is-false (< 'x 'y))
  (is-false (>= 'x 3))
  (is-false (> 'x 'y))
  (is (= 'x 'x 'x))
  (is (< 1 2 3))
  (is (= 3.0d0 3.0d0 3))
  (is (< 1.0d-5 1.0d7)))


(test test-diff-scalar-function
  "Tests the derivative of the scalar function"
  (flet ((scalar-f (x)
           (+ (* x (sin (expt x 2))) (* 3 (exp x)) (tan x)))
         (d-scalar-f (x)
           (+ (sin (expt x 2))
              (* 2 (expt x 2) (cos (expt x 2)))
              (* 3 (exp x))
              1
              (expt (tan x) 2)))
         (d^2-scalar-f (x)
           (+ (* 2 x (cos (expt x 2)))
              (* 4 x (cos (expt x 2)))
              (* -4 (expt x 3) (sin (expt x 2)))
              (* 3 (exp x))
              (* 2 (tan x) (+ 1 (expt (tan x) 2)))))
         (num= (x y)
           (cl:< (cl:abs (cl:- x y)) 10d-7)))
    (let ((x (random 1.0d0)))
      (is (num= (diff #'scalar-f x) (d-scalar-f x)))
      (is (num= (diff (d #'scalar-f) x) (d^2-scalar-f x))))))


(test test-gradient-f
  "Test of the gradient of a function (forward method)"
  (flet ((f (v)
           (+ (* (svref v 0)
                 (sin (svref v 1)))
              (expt (svref v 1) 2)
              (cos (svref v 2))))
         (grad-f (v)
           (vector (sin (svref v 1))
                   (+ (* (svref v 0) (cos (svref v 1)))
                      (* 2 (svref v 1)))
                   (- (sin (svref v 2)))))
         (num= (x y)
           (cl:< (cl:abs (cl:- x y)) 10d-7)))
    (let ((v (vector (random 1.0d0) (random 1.0d0) (random 1.0d0))))
      (is (every #'num=
                 (funcall (gradient-f #'f) v)
                 (grad-f v))))))


(test test-jacobian*vector
  "Test of the directional derivative of function
         n     m
     F: R  -> R
"
  (flet ((f (v)
           (vector
            (* (svref v 0)
               (funcall (comp #'sin #'cos) (svref v 2)))
            (expt (svref v 1) 2)))
         (dir-diff-f (v x)
           (vector (- (* (sin (cos (svref v 2))) (svref x 0))
                      (* (svref v 0) (cos (cos (svref v 2))) (sin (svref v 2)) (svref x 2)))
                   (* 2 (svref v 1) (svref x 1))))
         (num= (x y)
           (cl:< (cl:abs (cl:- x y)) 10d-7)))
    (let ((v (vector (random 1.0d0) (random 1.0d0) (random 1.0d0)))
          (x (vector (random 1.0d0) (random 1.0d0) (random 1.0d0))))
      (let ((j*x (jacobian*vector #'f v x))
            (j*x-true (dir-diff-f v x)))
        (is (simple-vector-p j*x))
        (is (= (length j*x) (length j*x-true) 2))
        (is (every #'num= j*x j*x-true))))))


(test test-jacobian*vector-save
  (flet ((f (v)
           (vector
            (* (svref v 0)
               (funcall (comp #'sin #'cos) (svref v 2)))
            (expt (svref v 1) 2)))
         (dir-diff-f (v x)
           (vector (- (* (sin (cos (svref v 2))) (svref x 0))
                      (* (svref v 0) (cos (cos (svref v 2))) (sin (svref v 2)) (svref x 2)))
                   (* 2 (svref v 1) (svref x 1))))
         (num= (x y)
           (cl:< (cl:abs (cl:- x y)) 10d-7)))
    (let ((x (vector (random 1.0d0) (random 1.0d0) (random 1.0d0)))
          (v (vector (random 1.0d0) (random 1.0d0) (random 1.0d0)))
          (x-dest (vector 0 0 0))
          (df-dest (vector 0 0)))
      (let ((j*v (jacobian*vector-save #'f x v x-dest df-dest))
            (j*v-true (dir-diff-f x v)))
        (is (eq j*v df-dest))
        (is (every #'num= j*v j*v-true))))))


(run! 'ad-test-suite)
