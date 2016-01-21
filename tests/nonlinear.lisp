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

(test nonlinear-chemeng-equilibrium
  "Solve the following equations

    K  P F  F  - F  F  = 0
     1    A  B    C  t

    K  P F  F  - F  F  = 0
     2    A  C    D  t

    K  P F  F  - F  F  = 0
     3    C  B    E  t

    F   = F  + F  + 2 F  + F
     A0    A    C      D    E

    F   = F  + F  + F  + 2 F
     B0    B    C    D      E
"
  (let ((k (vec 'double-float 1d0 2d0 1d0))
        (total-pressure 10d0)
        (fa0 1d0)
        (fb0 1d0)
        (init-guess (vec 'double-float 0.5d0 0.5d0 0.1d0 0.1d0 0.1d0))
        (exact-solution (vec 'double-float
                             0.0859992135185769d0 0.13899235860746198d0
                             0.13853376960391228d0 0.2761533873221411d0
                             0.22316024223323777d0))
        (method (make-newton 5
                             :other-controls (list (log-computation
                                                    (lambda (tag val)
                                                      (declare (ignore tag))
                                                      (format t "~&LOG:~Tr = ~A~Tsqrt(2g0) = ~A~Tdir = ~A~%"
                                                              (nonlinear-value-residual
                                                               (newton-value-approximation val))
                                                              (sqrt (* 2d0 (numeric-newton::newton-value-g0 val)))
                                                              (l2-norm (numeric-newton::newton-value-dir val)))))))))
    (flet ((equilibrium (F y)
             (with-vector-items ((fa 0) (fb 1) (fc 2) (fd 3) (fe 4)) F
               (let ((f-total (+ fa fb fc fd fe)))
                 (setf (aref y 0)
                       (- (* (aref k 0) total-pressure fa fb) (* fc f-total)))
                 (setf (aref y 1)
                       (- (* (aref k 1) total-pressure fa fc) (* fd f-total)))
                 (setf (aref y 2)
                       (- (* (aref k 2) total-pressure fc fb) (* fe f-total)))
                 (setf (aref y 3)
                       (- fa0 fa fc (* 2d0 fd) fe))
                 (setf (aref y 4)
                       (- fb0 fb fc fd (* 2d0 fe)))))))
      (multiple-value-bind (solution successful-p final-residual)
          (fsolve method #'equilibrium init-guess (jacobian*vector #'equilibrium (make-vector 5) (make-vector 5)))
        (is-true successful-p)
        (is (almost-zero-p final-residual *newton-tolerance*))
        (add-with-multipliers! exact-solution (cons -1d0 solution))
        (is (vector-almost-zero-p exact-solution *newton-tolerance*))))))

;; (run! 'nonlinear-chemeng-equilibrium)

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



