(in-package mobius.numeric.tests.linear)

(def-suite linear-suite
    :description "Testing all the functions from LINEAR modules")

(in-suite linear-suite)

(test linear-bicg
  (let ((A (make-array (list 5 5) :element-type 'double-float))
        (b (make-double-float-vector 5))
        (x0 (vector 1 1 1 1 1))
        (x (vector 1 1 1 1 1))
        (num-gen (let ((gen (gen-integer :min -10 :max 10)))
                   (lambda ()  (coerce (funcall gen) 'double-float))))
        (num-gen-pos (let ((gen (gen-integer :min 1 :max 10)))
                       (lambda () (coerce (funcall gen) 'double-float))))
        (log-control (let ((performed -1))
                       (log-computation
                        (lambda (tag x)
                          (declare (ignore tag))
                          (incf performed)
                          (format t "Computation #~D~%x = ~A~%"
                                  performed x))))))
    (dotimes (i 5)
      (setf (aref x i) (* 0.2d0 (funcall num-gen)))
      (setf (aref x0 i) (* (aref x i) (+ 0.5d0 (random 1.0d0))))
      (dotimes (j 5)
        (if (= i j)
            (setf (aref A i j) (* 2.0d0 (funcall num-gen-pos)))
            (setf (aref A i j) (* 0.2d0 (funcall num-gen))))))
    (let ((a-fun (matrix-mul->function A))
          (bicg (make-bicg-stab 5 log-control)))
      (funcall a-fun x b)
      (format t "Problem:~%A = ~A~%x = ~A~%x0 = ~A~%b = ~A~%" A x x0 b)
      (let ((result (bicg-stab-solve bicg a-fun b x0)))
        (format t "Solution:~%~A~%" result)
        (is (iterator:finished-p result))
        (is (almost-zero-p
             (l2-norm-diff x (bicg-stab-solution (iterator:value result)))
             *bicg-stab-tolerance*))))))


;; (run! 'linear-bicg)

(test linear-sparse-bicg
  (let ((A (make-array '(5 5)
                       :element-type 'double-float
                       :initial-contents '(( 5d0  1d0  0d0  0d0 0d0)
                                           (-2d0  5d0  1d0  0d0 0d0)
                                           ( 0d0 -2d0  5d0  1d0 0d0)
                                           ( 0d0  0d0 -2d0  5d0 1d0)
                                           ( 0d0  0d0  0d0 -2d0 5d0))))
        (b  (make-double-float-vector 5))
        (x0 (make-double-float-vector 5))
        (x (vec 'double-float 1d0 1.2d0 1.25d0 1.3d0 1.3d0))
        (log-control (let ((performed -1))
                       (log-computation
                        (lambda (tag x)
                          (declare (ignore tag))
                          (incf performed)
                          (format t "Computation #~D~%x = ~A~%"
                                  performed x))))))
    (dotimes (i 5)
      (setf (aref x0 i) (* (aref x i) (+ 1.0 (- (random 0.8d0) 0.4d0)))))
    (format t "Initial approximation for~%x = ~A~%is~%~A~%" x x0)
    (let ((fun-a (matrix-mul->function A))
          (bicg (make-bicg-stab 5 log-control)))
      (funcall fun-a x b)
      (let ((result (bicg-stab-solve bicg fun-a b x0)))
        (format t "Solution is~%~A~%" result)
        (is (iterator:finished-p result))
        (is (num= (l2-norm-diff x (bicg-stab-solution (iterator:value result)))
                  0.0d0
                  *bicg-stab-tolerance*))
        (is (num= (l2-norm (bicg-stab-residual (iterator:value result)))
                  0.0d0
                  *bicg-stab-tolerance*))))))

;; (run! 'linear-sparse-bicg)


(test linear-sparse-bicg-close-x0
  (let ((A (make-array '(5 5)
                       :element-type 'double-float
                       :initial-contents '(( 5d0  1d0  0d0  0d0 0d0)
                                           (-2d0  5d0  1d0  0d0 0d0)
                                           ( 0d0 -2d0  5d0  1d0 0d0)
                                           ( 0d0  0d0 -2d0  5d0 1d0)
                                           ( 0d0  0d0  0d0 -2d0 5d0))))
        (b  (make-double-float-vector 5))
        (x0 (make-double-float-vector 5))
        (x (vec 'double-float 1d0 1.2d0 1.25d0 1.3d0 1.3d0))
        (log-control (let ((performed -1))
                         (log-computation
                          (lambda (tag x)
                            (declare (ignore tag))
                            (incf performed)
                            (format t "Computation #~D~%x = ~A~%"
                                    performed x))))))
    (dotimes (i 5)
      (setf (aref x0 i) (* (aref x i) (+ 1.0 (- (random 1.0d-10) 0.5d-10)))))
    (format t "Initial approximation for~%x = ~A~%is~%~A~%" x x0)
    (let ((fun-a (matrix-mul->function A))
          (data (make-bicg-stab 5 log-control)))
      (funcall fun-a x b)
      (let ((result (bicg-stab-solve data fun-a b x0)))
        (format t "Solution is~%~A~%" result)
        (is (iterator:finished-p result))
        (is (num= (l2-norm-diff x (bicg-stab-solution (iterator:value result)))
                  0.0d0
                  *bicg-stab-tolerance*))
        (is (num= (l2-norm (bicg-stab-residual (iterator:value result)))
                  0.0d0
                  *bicg-stab-tolerance*))))))

;; (run! 'linear-sparse-bicg-close-x0)

(test linear-sparse-bicg-through-solver
  (let ((A (make-array '(5 5)
                       :element-type 'double-float
                       :initial-contents '(( 5d0  1d0  0d0  0d0 0d0)
                                           (-2d0  5d0  1d0  0d0 0d0)
                                           ( 0d0 -2d0  5d0  1d0 0d0)
                                           ( 0d0  0d0 -2d0  5d0 1d0)
                                           ( 0d0  0d0  0d0 -2d0 5d0))))
        (b  (make-double-float-vector 5))
        (x0 (make-double-float-vector 5))
        (x (vec 'double-float 1d0 1.2d0 1.25d0 1.3d0 1.3d0))
        (data (make-bicg-stab 5)))
    (dotimes (i 5)
      (setf (aref x0 i) (* (aref x i) (+ 1.0 (- (random 1.0d0) 0.5d0)))))
    (format t "Initial approximation for~%x = ~A~%is~%~A~%" x x0)
    (let ((fun-a (matrix-mul->function A)))
      (funcall fun-a x b)
      (multiple-value-bind (solution successful-p final-residual)
          (solve-linear data fun-a b x0)
        (format t "Solution: ~A~%~A~%~A~%" solution successful-p final-residual)
        (is (eq successful-p t))
        (is (eq solution x0))
        (is (almost-zero-p final-residual *bicg-stab-tolerance*))
        (is (almost-zero-p (l2-norm-diff x solution) *bicg-stab-tolerance*))))))

;; (run! 'linear-sparse-bicg-through-solver)


(test bicg-solve-dense-3x3-from-newton
  (let ((a (make-array '(3 3)
                       :element-type 'double-float
                       :initial-contents
                       '((-0.1763708d0 -0.645689165d0 3d0)
                         (-1d0         1d0            0d0)
                         (0.999739d0   0d0            0.03d0))))
        (b (vec 'double-float 0.0228446d0 -1.25d0 0.4785175386d0))
        (x0 (vec 'double-float 0.5d0 0.5d0 -0.1d0))
        (actual-b (make-vector 3 'double-float))
        (log-out (log-computation
                  (let ((n -1))
                    (lambda (tag data)
                      (declare (ignore tag))
                      (incf n)
                      (format t "~&Iteration #~D~%x = ~A~%" n (bicg-stab-solution data)))))))
    (let ((fun-a (matrix-mul->function a))
          (bicg (make-bicg-stab 3 log-out)))
      (let ((result (bicg-stab-solve bicg fun-a b x0)))
        (format t "~&Solution is~%~A~%" result)
        (is (iterator:finished-p result))
        (let ((value (iterator:value result)))
          (funcall fun-a (bicg-stab-solution value) actual-b)
          (is (almost-zero-p
               (l2-norm-diff b actual-b)
               *bicg-stab-tolerance*)))))))

;; (run! 'bicg-solve-dense-3x3-from-newton)

(run! 'linear-suite)
