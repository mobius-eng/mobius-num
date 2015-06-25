(in-package mobius.numeric.tests.base)

(def-suite base-suite
    :description "MOBIUS-NUM base functionality")

(in-suite base-suite)

(test utils
  "Tests constants from MOBIUS.NUMERIC.UTILS"
  (is (numberp not-a-number))
  (is (and (not (= 1.0d0 not-a-number))
           (not (> 1.0d0 not-a-number))
           (not (< 1.0d0 not-a-number))))
  (is (numberp +infinity))
  (is (> +infinity 1.0d20))
  (is (num= 0.0d0 (/ +infinity)))
  (is (numberp -infinity))
  (is (< -infinity -1.0d20)))


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
  (let ((criteria (make-criteria
                   :converged-numbers '(1.0d-12 1.0d0 "Solved")
                   :limit-iterations 100))
        (init-guess 1.0d0)
        (value 3.0d0))
    (flet ((improve (x buffer)
             (declare (ignore buffer))
             (average x (/ value x))))
      (let ((result (fixed-point criteria #'improve init-guess nil nil)))
        (is (iterator:finished-p result))
        (is (num= (iterator:value result) 
                  (sqrt value)
                  1.0d-10))))))

(defun run-base-suite ()
  (run! 'base-suite))

(run! 'base-suite)


