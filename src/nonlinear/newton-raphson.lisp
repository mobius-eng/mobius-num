(in-package mobius.numeric.newton-raphson)

;; * Newton-Raphson method
;; ** NEXT-LAMBDA: Creating bounds for X
;; *** Utility: inline square function
(declaim (inline sq))
(defun sq (x)
  (declare (type double-float x))
  (* x x))
;; *** NEXT-LAMBDA
;; Choose next lambda to minimize cubic aprroximation of g(lambda)
(defun next-lambda (g0 Dg0 l1 l2 g1 g2)
  "Computes next LAMBDA that minimises function g(lambda) using cubic approximation of g.
  G0 and DG0 are values of g(0) and D[g](0) respectively.
  l1 and gl1 are the most recent lambda and g(lambda)
  l2 and gl2 are the second last lambda and g(lambda)"
  (declare (type double-float g0 Dg0 l1 l2 g1 g2))
  (let ((coeff (/ (- l1 l2)))
        (m (make-array (list 2 2)
                       :element-type 'double-float
                       :initial-contents
                       (list (list (/ (sq l1)) (- (/ (sq l2))))
                             (list (- (/ l2 (sq l1))) (/ l1 (sq l2))))))
        (c (list->vector 'vector (list (- g1 (* dg0 l1) g0) (- g2 (* Dg0 l2) g0)))))
    (let ((a-b (e=*! (m* m c) coeff)))
      (min (max
            (/ (- (sqrt (- (sq (aref a-b 1)) (* 3.0d0 (aref a-b 0) Dg0))) (aref a-b 1))
               (* 3.0d0 (aref a-b 0)))
            (* 0.1d0 l1))
           (* 0.5d0 l1)))))

;; ** Utitlities
;; ** Make buffers
(defun make-buffers (x0)
  (let ((main-buffers (loop repeat 4 collect (zero-vector x0)))
        (other-buffers (loop repeat 7 collect (zero-vector x0))))
    (list* (list (first main-buffers) (second main-buffers))
           (list (third main-buffers) (fourth main-buffers))
           other-buffers)))

(defun full-newton-step (f-value df-value lin-solver buffer)
  "Compute -f(x)/df(x) using BUFFER to keep all intermediate results"
  (negate-vector! (funcall lin-solver df-value f-value f-value buffer)))

(defun abs-f (function x f-buffer)
  (let ((f-value (funcall function x f-buffer)))
    (values (* 0.5d0 (dot f-value f-value)) f-value)))

(defun l-finish-criteria (g0 arg)
  "Finish criteria on backtracking sub-method"
  (let ((l1    (first arg))
        (gl1   (third arg))
        (alpha 1.0d-4))
    (declare (type double-float l1 gl1 alpha))
    (<= gl1 (* (- 1.0d0 (* 2.0d0 alpha l1)) (the double-float g0)))))


;; ** Globally convergent Newton method
(defun newton-method (criteria lin-solver f df x0 df-tmp)
  "Globally convergent Newton-Raphson method to solve f(x)=0.
Arguments:
 criteria   : criteria on the list of (f(x) x)
 lin-solver : linear solver (lin-solver A b x0 dest)
              solving A*x=b with initial guess x0 and possible use of buffer
 f          : (f x buffer) returns f(x), possibly uses buffer to store the result
 df         : Jacobian of f(x): (df x buffer) returns some form of the Jacobian that later
              can be used by LIN-SOLVER as a matrix
 x0         : initial guess
 df-tmp     : buffer for Jacobian value, used when calling (DF X Df-TMP)
Result: ITERATOR with value (list (F x) x) for final approximation x"
  (destructuring-bind (main-buffer1 main-buffer2
                       p-buffer f0-buffer f-full-buffer misc-buffer
                       fl1-buffer l-iter-buffer1 l-iter-buffer2)
      (make-buffers x0)
    (flet ((improve (arg tmp)
             (destructuring-bind ((x0 dx0) (x dx)) (list arg tmp)
               (declare (ignore dx0))
               (let* ((df-value (funcall df x0 df-tmp))
                      (f0 (funcall f f0-buffer))
                      (p (full-newton-step f0 df-value lin-solver p-buffer))
                      (f-full (funcall f (e+! misc-buffer x0 p) f-full-buffer))
                      (f0_2 (dot f0 f0))
                      (g0 (* 0.5d0 f0_2))
                      (Dg0 (- f0_2))
                      (g1 (* 0.5d0 (dot f-full f-full)))
                      (crt-inner (make-criteria
                                  :finished-value (partial #'l-finish-criteria g0)
                                  :limit-iterations 10)))
                 (flet ((g (l tmp)
                          (format t "Calculating g(~A)~%" l)
                          (abs-f f (e=+! (e*! misc-buffer p l) x0) tmp)))
                   (flet ((improve-l (arg tmp)
                            (destructuring-bind (l1 l2 gl1 gl2 fl1) arg
                              (declare (ignore fl1))
                              (let ((new-l (next-lambda g0 Dg0 l1 l2 gl1 gl2)))
                                (multiple-value-bind (new-g new-f) (g new-l tmp)
                                  (list new-l l1 new-g gl1 new-f))))))
                     (if (l-finish-criteria g0 (list 1.0d0 nil g1 nil f-full))
                         (list (e+! (car tmp) f-full) (e+! (cadr tmp) x0 p))
                         (let* ((l0 1.0d0)
                                (gl0 g1)
                                (l1 (* -0.5d0 (/ Dg0 (- g1 g0 Dg0)))))
                           (multiple-value-bind (gl1 fl1) (g l1 fl1-buffer)
                             (let ((l-final (fixed-point crt-inner
                                                         #'improve-l
                                                         (list l1 l0 gl1 gl0 fl1)
                                                         l-iter-buffer1
                                                         l-iter-buffer2)))
                               (cond ((iterator:finished? l-final)
                                      (format t "Acceptable lambda = ~A~%"
                                              (car (iterator:value l-final)))
                                      (let* ((l (car (iterator:value l-final)))
                                             (new-x (e=+! (e*! (cadr tmp) p l) x0)))
                                        (list (funcall f new-x (car tmp)) new-x)))
                                     (t (error "NEWTON-METHOD: cannot keep X bound")))))))))))))
      (fixed-point criteria #'improve
                   (list (funcall f x0 f0-buffer) x0)
                   main-buffer1
                   main-buffer2))))



