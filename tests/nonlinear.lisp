(in-package mobius.numeric.tests.nonlinear)

(def-suite nonlinear-suite)

(in-suite nonlinear-suite)

(test nonlinear-scalar
  (let ((f #f(- % (sqrt %)))
        (df #f(- 1.0d0 (/ 0.5d0 (sqrt %))))
        (x0 5.0d0))
    (multiple-value-bind (x fx status) (fsolve f x0 :df df)
      (is (eq status :FINISHED))
      (is (<= (abs fx) 1.0d-9))
      (is (num= x 1.0d0)))))


(test nonlinear-vector
  (let ((f #'(lambda (v buf)
               (let ((x (mvref v 0))
                     (y (mvref v 1))
                     (z (mvref v 2)))
                 (setf (mvref buf 0) (+ (* (cos x) (exp (* -2.0d0 y)))
                                        (* 3.0d0 z)))
                 (setf (mvref buf 1) (- (* y y) x 1.0d0))
                 (setf (mvref buf 2) (+ (* z z z) (sin x) 9.18056d-5))
                 buf)))
        (df #'(lambda (v buf)
                (let ((x (mvref v 0))
                      (y (mvref v 1))
                      (z (mvref v 2)))
                  ;; d0F
                  (setf (aref buf 0 0) (- (* (sin x) (exp (* -2.0d0 y)))))
                  (setf (aref buf 1 0) -1.0d0)
                  (setf (aref buf 2 0) (cos x))
                  ;; d1F
                  (setf (aref buf 0 1) (* -2.0d0 (cos x) (exp (* -2.0d0 y))))
                  (setf (aref buf 1 1) (* 2.0d0 y))
                  (setf (aref buf 2 1) 0.0d0)
                  ;; d2F
                  (setf (aref buf 0 2) 3.0d0)
                  (setf (aref buf 1 2) 0.0d0)
                  (setf (aref buf 2 2) (* 3.0d0 z z))
                  buf)))
        (exact-solution (up 0.0d0 1.0d0 -0.045112d0))
        (x0 (up 0.5d0 0.5d0 -0.1d0))
        (df-buf (make-array '(3 3) :element-type 'double-float)))
    (multiple-value-bind (x fx status) (fsolve f x0
                                               :simple-function nil
                                               :df df
                                               :df-tmp df-buf
                                               :lin-solver #'(lambda (A b &optional x0 x)
                                                               (declare (ignore x0 x))
                                                               (lla:solve A b)))
      (is (eq status :FINISHED))
      (is (< (norm fx) 1.0d-7))
      (is (num= exact-solution x)))))


(run! 'nonlinear-vector)


