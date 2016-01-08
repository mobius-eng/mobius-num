(in-package mobius.numeric.tests.nonlinear)

(def-suite nonlinear-suite)

(in-suite nonlinear-suite)

(test nonlinear-linsearch
  "Line search for equation
 
     1/3
    x    = 0

starting from x = 1"
  (let ((g (lambda (x)
             (let ((y (* 0.5d0 (expt (expt (- 1.0d0 (* 3.0d0 x)) 2)
                                     1/3))))
               (format t "~&g(~A) = ~A~%" x y)
               y)))
        (g0 0.5d0)
        (Dg0 -1d0))
    (let ((g1 (funcall g 1.0d0))
          (linsearch (make-linsearch)))
      (multiple-value-bind (final-lambda successful-p final-g)
          (linsearch linsearch g #'newton-step-gamma g0 Dg0 g1)
        (is (eq successful-p t))
        (is (> final-lambda *linsearch-abs-lambda-min*))
        (is (<= final-lambda 1.0d0 ))
        (is (< final-g g0))))))


;; (run! 'nonlinear-linsearch)


(test nonlinear-linsearch-no-action
  "Line search for equation
 
     ___
   \/ x    = x

starting from x = 2: should take the full step"
  (let ((g (lambda (lmbda)
             (let* ((x-old 2.0d0)
                    (p -0.9061637d0)
                    (x-new (+ x-old (* lmbda p)))
                    (y (- (sqrt x-new) x-new)))
               (* 0.5 y y))))
        (g0 0.1715729d0)
        (Dg0 -0.3431458d0))
    (let ((g1 (funcall g 1.0d0))
          (linsearch (make-linsearch)))
      (multiple-value-bind (final-lambda successful-p final-g)
          (linsearch linsearch g #'newton-step-gamma g0 Dg0 g1)
        (is (eq successful-p t))
        (is (> final-lambda *linsearch-abs-lambda-min*))
        (is (<= final-lambda 1.0d0 ))
        (is (< final-g g0))
        (is (= final-lambda 1.0d0))))))


;; (run! 'nonlinear-linsearch-no-action)


(test nonlinear-vector
  (flet ((equation-function (v f-out)
           (with-vector-items ((x 0) (y 1) (z 2)) v
             (with-vector-items ((p 0) (q 1) (r 2)) f-out
               (setf p (+ (* (cos x) (exp (* -2.0d0 y)))
                          (* 3.0d0 z)))
               (setf q (- (expt y 2) x 1.0d0))
               (setf r (+ (expt z 3) (sin x) 9.18056d-5))))))
    (let ((exact-solution (vec 'double-float 0.0d0 1.0d0 -0.045112d0))
          (x0 (vec 'double-float 0.5d0 0.5d0 -0.1d0))
          (newton-method (make-newton 3 :other-controls
                                      (list (log-computation
                                             (lambda (tag val)
                                               (format t "~&~A~%~T~A~%" tag val))))))
          (y (make-vector 3 'double-float)))
      (multiple-value-bind (solution successful-p final-residual)
          (fsolve newton-method #'equation-function x0
                  (jacobian*vector #'equation-function (make-vector 3) (make-vector 3)))
        (format t "~&Solution (~A):~%~A~%" (type-of solution) solution)
        (is-true successful-p)
        (is (almost-zero-p (l2-norm-diff exact-solution solution)
                           (* 1000d0 *newton-tolerance*)))
        (is (almost-zero-p final-residual *newton-tolerance*))
        (equation-function solution y)
        (is (vector-almost-zero-p y *newton-tolerance*))))))

;; (run! 'nonlinear-vector)


;; (flet ((equation-function (v f-out)
;;            (with-vector-items ((x 0) (y 1) (z 2)) v
;;              (with-vector-items ((p 0) (q 1) (r 2)) f-out
;;                (setf p (+ (* (cos x) (exp (* -2.0d0 y)))
;;                           (* 3.0d0 z)))
;;                (setf q (- (expt y 2) x 1.0d0))
;;                (setf r (+ (expt z 3) (sin x) 9.18056d-5))))))
;;     (let ((exact-solution (vec 'double-float 0.0d0 1.0d0 -0.045112d0))
;;           (x0 (vec 'double-float 0.5d0 0.5d0 -0.1d0))
;;           (newton-method (make-newton 3 :other-controls (list
;;                                                          (log-computation
;;                                                           (lambda (tag val)
;;                                                             (format t "~&~A~%~T~A~%"
;;                                                                     tag val))))))
;;           (y (make-vector 3)))
;;       (declare (ignore exact-solution))
;;       (multiple-value-bind (solution successful-p final-residual)
;;           (fsolve newton-method #'equation-function x0
;;                   (jacobian*vector #'equation-function (make-vector 3) (make-vector 3)))
;;         (declare (ignore successful-p final-residual))
;;         (format t "~&Solution:~%~A~%" solution)
;;         (equation-function solution y))))


(test nonlinear-cubic-root
  "Find the solution of 
     1/3
    x    = 0"
  (let ((f (lambda (v y)
             ;; (format t "~&x^1/3 evaluation with x = ~G~%" (aref v 0))
             (setf (aref y 0)
                   (if (plusp (aref v 0))
                       (expt (aref v 0) 1/3)
                       (- (expt (- (aref v 0)) 1/3))))))
        (x0 (vec 'double-float 1d0))
        (method (make-newton 1 :tolerance 1d-7 :max-iterations 100)))
    (multiple-value-bind (x successful-p final-residual)
        (fsolve method f x0 (jacobian*vector f (make-vector 1) (make-vector 1)))
      (format t "~&Solution: x = ~G final-residual = ~G~%" (aref x 0) final-residual)
      (is (eq successful-p t))
      (is (almost-zero-p final-residual 1d-7))
      (is (almost-zero-p (aref x 0) 1d-7)))))


;; (run! 'nonlinear-cubic-root)

(run! 'nonlinear-suite)


;; (let* ((f (let ((f-out (make-vector 3)))
;;             (lambda (v)
;;               (with-vector-items ((x 0) (y 1) (z 2)) v
;;                 (with-vector-items ((p 0) (q 1) (r 2)) f-out
;;                   (setf p (+ (* (cos x) (exp (* -2.0d0 y)))
;;                              (* 3.0d0 z)))
;;                   (setf q (- (expt y 2) x 1.0d0))
;;                   (setf r (+ (expt z 3) (sin x) 9.18056d-5))
;;                   f-out)))))
;;        (df-v (let ((buf1 (make-vector 3))
;;                    (buf2 (make-vector 3)))
;;               (lambda (x v)
;;                 (jacobian*vector-save f x v buf1 buf2))))
;;        (x0 (vec t 0.5d0 0.5d0 -0.1d0)))
;;   (funcall df-v x0 #(1 0 0)))


;; (let ((f (let ((f-out (make-vector 3)))
;;            (lambda (v)
;;              (with-vector-items ((x 0) (y 1) (z 2)) v
;;                (with-vector-items ((p 0) (q 1) (r 2)) f-out
;;                  (setf p (+ (* (cos x) (exp (* -2.0d0 y)))
;;                             (* 3.0d0 z)))
;;                  (setf q (- (expt y 2) x 1.0d0))
;;                  (setf r (+ (expt z 3) (sin x) 9.18056d-5))
;;                  f-out)))))
;;       (exact-solution (vec 'double-float 0.0d0 1.0d0 -0.045112d0))
;;       (x0 (vec t 0.5d0 0.5d0 -0.1d0))
;;       (newton-method (make-newton-method 3)))
;;     (let ((solution (newton-method-solve newton-method f x0)))
;;       (format t "~A~%" solution)))



