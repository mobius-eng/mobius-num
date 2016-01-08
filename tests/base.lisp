(in-package mobius.numeric.tests.base)

(def-suite base-suite
    :description "MOBIUS-NUM base functionality")

(in-suite base-suite)

(test utils
  "Tests constants from MOBIUS.NUMERIC.CONSTANTS"
  (is (numberp +infinity))
  (is (> +infinity most-positive-double-float))
  (is (num= 0.0d0 (/ +infinity)))
  (is (numberp -infinity))
  (is (< -infinity most-negative-double-float))
  (is (> default-precision least-positive-double-float)))


(test iterator
  "MOBIUS.NUMERIC.ITERATOR"
  (let* ((x (iterator:continue 1.0d0))
         (x-finished (iterator:->finished x))
         (x-finished-new-value (iterator:replace-value x 10.0d0)))
    (is (iterator:finished-p x))
    (is (= (iterator:value x)
           (iterator:value x-finished)
           (iterator:value x-finished-new-value)
           10.0d0))))

(test fixed-point
  "Test FIXED-POINT on finding SQRT of a number"
  (let ((control (combine-controls
                  (converged-number 1.0d-9)
                  (limit-iterations 7)))
        (init-guess 1.0d0)
        (value (random 10.d0)))
    (flet ((improve (x)
             (average x (/ value x))))
      (let ((result (fixed-point control #'improve init-guess)))
        (is (iterator:finished-p result))
        (is (num= (iterator:value result) 
                  (sqrt value)
                  1.0d-8))))))

(defun run-base-suite ()
  (run! 'base-suite))

(run! 'base-suite)


