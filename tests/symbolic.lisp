(in-package num-tests-symbolic)

(def-suite symbolic-tests)

(in-suite symbolic-tests)

(test test-symbolic-functions
  "Tests the application of common functions to symbolic arguments"
  (is (equalp (sin 'x) '(:sin x)))
  (is (equalp (cos 'x) '(:cos x)))
  (is (equalp (tan 'x) '(:tan x)))
  (is (equalp (log 'x 10) '(:log x 10)))
  (is (equalp (+ (sin 'x) (* (tan 'x) (expt 'y 'z)))
              '(:+ (:sin x) (:* (:tan x) (:expt y z)))))
  (is (equalp (+ (sin 'x) (cos 'y))
              '(:+ (:sin x) (:cos y))))
  (is (equalp (funcall (comp #'sin #'exp) 'x)
              '(:sin (:exp x))))
  (let ((result (funcall (+ #'sin #'cos 'z) 'x)))
    (is (eq (first result) :+))
    (is-true (find 'z (rest result)))
    (is-true (find '(:sin x) (rest result) :test 'equal))
    (is-true (find '(:cos x) (rest result) :test 'equal))
    (is-false (remove-if (lambda (item)
                           (or (eq item 'z)
                               (equal item '(:sin x))
                               (equal item '(:cos x))))
                         (rest result)))))

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
                '(f (:tan x))))))

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

(run! 'symbolic-tests)
