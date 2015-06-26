(in-package mobius.numeric.newton-raphson)

;; * Newton-Raphson method
(defstruct (buffers (:constructor mk-buffers))
  p
  f0
  f-full
  misc
  fl1
  l-iter-1
  l-iter-2
  main-1
  main-2)

;; ** Newton data
(defstruct newton-data x0 f0 p x f)

(defun destructure-newton-data (data)
  (with-slots (x0 f0 p x f) data
    (list x0 f0 p x f)))

;; ** Backtrack data
;; Here everithing is a number
(defstruct backtrack-data
  g0
  Dg0
  (new-s 1.0)
  (old-s 0.0)
  (new-g 0.0)
  (old-g 0.0))

(defun move-to-new-step (dest source next-s next-g)
  "Update backtrack data DEST from SOURCE moving new -> old and using
NEXT-S and NEXT-G as NEW-S and NEW-G. DEST and SOURCE can be the same"
  (declare (type backtrack-data dest source))
  (with-slots ((sg0 g0) (sDg0 Dg0) (snew-s new-s) (snew-g new-g)) source
    (with-slots (g0 Dg0 new-s old-s new-g old-g) dest
      (setf g0 sg0)
      (setf Dg0 sDg0)
      (setf new-s next-s)
      (setf new-g next-g)
      (setf old-s snew-s)
      (setf old-g snew-g))))

(defun destructure-backtrack-data (data)
  "Eeasy access to backtracking data"
  (with-slots (g0 Dg0 new-x old-x new-g old-g new-f) data
    (list g0 Dg0 new-x old-x new-g old-g)))

(defun init-backtrack-data (f0 f)
  "Construct new backtrack data initialising G0 and DG0,
also assumes NEW-S=1 and fills in NEW-G"
  (let* ((f0^2 (squared-L2-norm f0))
         (g0 (* 0.5 f0^2))
         (Dg0 (- f0^2))
         (g1 (* 0.5 (squared-L2-norm f))))
    (make-backtrack-data :g0 g0 :Dg0 Dg0 :g1 g1)))

;; ** Backtrack itself: keeps extra buffers
(defstruct backtrack
  newton-data
  data-1
  data-2
  p-buffer)

(defun finished-backtracking-p (data)
  (destructuring-bind (g0 Dg0 l1 l2 gl1 gl2) (destructure-backtrack-data data))
  (declare (ignore l2 gl2) (type double-float l1 gl1))
  (let ((alpha 1.0d-4))
    (declare (type double-float l1 gl1 alpha))
    (<= gl1 (* (- 1.0d0 (* 2.0d0 alpha l1)) (the double-float g0)))))

(defun optimize-step (backtrack function)
  (destructuring-bind (x0 f0 p x f)
      (destructure-newton-data (backtrack-newton-data backtrack))
    (declare (ignore x0 p x))
    (let ((data (init-backtrack-data f0 f)))
      (if (finished-backtracking-p data)
          (backtrack-newton-data backtrack)
          (find-optimal-step backtrack data function)))))

(defun find-optimal-step (backtrack data function)
  (destructuring-bind (x0 f0 p x f)
      (destructure-newton-data (backtrack-newton-data backtrack))
    (let ((p0 (copy-vector p (slot-value backtrack 'p-buffer))))
      (flet ((g (s) (abs-f function (.+! x (.*! p p0 s) x0) f)))
        (let ((new-s (* 0.5 / (Dg0 (- g1 g0 Dg0))))
              (new-g (g new-s)))
          (move-to-new-step data data new-s new-g)
          (let* ((solution (fixed-point *newton-raphson-internal-criteria*
                                        (improve-backtrack function #'g)
                                        data
                                        (backtrack-data-1 backtrack)
                                        (backtrack-data-2 backtrack))))
            (cond ((iterator:finished-p solution) (backtrack-newton-data backtrack))
                  (t (error "NEWTON-RAPHSON:OPTIMIZE-STEP - cannot keep X bound")))))))))

(defun improve-backtrack (function g)
  #'(lambda (arg tmp)
      (let* ((next-x (next-x arg)))
        (move-to-new-step tmp arg next-x (funcall g next-x))
        tmp)))

(defun next-x (data)
  "Computes next LAMBDA that minimises function g(lambda) using cubic approximation of g."
  (destructuring-bind (g0 Dg0 l1 l2 g1 g2) (destructure-backtrack-data data)
    (declare (type double-float g0 Dg0 l1 l2 g1 g2))
    (let ((coeff (/ (- l1 l2)))
          (m (matrix-of real
                        (list (/ (sq l1)) (- (/ (sq l2))))
                        (list (- (/ l2 (sq l1))) (/ l1 (sq l2)))))
          (c (dfvec (- g1 (* dg0 l1) g0) (- g2 (* Dg0 l2) g0))))
      (let ((a-b (.=*! (m* m c) coeff)))
        (min (max
              (/ (- (sqrt (- (sq (grid:aref a-b 1))
                             (* 3.0d0 (grid:aref a-b 0) Dg0)))
                    (grid:aref a-b 1))
                 (* 3.0d0 (grid:aref a-b 0)))
              (* 0.1d0 l1))
             (* 0.5d0 l1))))))

(defclass newton-solver ()
  ((data :initarg data :accessor newton-data)
   (buffers :initarg :buffers :accessor newton-buffers)
   (jacobian :initarg :jacobian :accessor newton-jacobian)  ;; change
   ;; in such that it keeps the buffer in JACOBIAN-BUFFER
   (jacobian-buffer :initarg :jacobian-buffer :accessor newton-jacobian-buffer)))


(defvar *newton-raphson-internal-criteria*
  (make-criteria
   :finished-value #'l-finished-criteria
   :limit-iterations 10)
  "Criteria for backtracking the step along Newton's direction")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun newton-method (method function initial-guess linear-solver)
  #'(lambda (data tmp)
      (destructuring-bind (x0 f0 last-p) (destructure-data data)
        (declare (ignore last-p))
        (let* ((buffers (newton-buffer method))
               (jac (funcall (newton-jacobian method) x0))
               (p   (full-newton-step f0 jac linear-solver (buffers-p buffers)))
               (f-full (funcall function (.+! (buffers-misc buffers) x0 p) (buffers-f-full buffers)))
               (final-p (optimize-step method x0 p f0 f-full (data-change-to-current tmp))) ;; TODO indicate if lambda = 1 was accepted
               )
          (setf (data-x tmp) (.+! (data-x tmp) x0 p))
          (setf (data-change-to-current tmp) final-p) ;; TODO add
          ;; update for F on final-p
          tmp))))

(defun full-newton-step (f-value df-value lin-solver buffer)
  "Compute -f(x)/df(x) using BUFFER to keep all intermediate results"
  (.=-! (linear-solve lin-solver df-value f-value buffer f-value)))

;; TODO Fixme
(defun optimize-step (method function x0 p f0 f-full x-buffer final-p-buffer)
  (destructuring-bind (g0 Dg0 g1) (initial-g-values f0 f-full)
    (let ((data0 (mk-opt-data :g0 g0 :Dg0 Dg0 :l1 1.0 :g1 g1 :f1 f-full)))
      (if (l-finished-criteria data0)
          (list (copy-vector p final-p-buffer) (copy-vector f-full final-f-buffer))
          (flet ((g (l f-buffer) (abs-f function (.=+! (.*! x-buffer p l) x0) f-buffer)))
            (let ((l2 1.0)
                  (l1 (* 0.5 / (Dg0 (- g1 g0 Dg0))))
                  (g2 g1))
              (multiple-value-bind (g1 f1) (g l1 f-buffer???)
                (let* ((data1 (mk-opt-data :g0 g0 :Dg0 Dg0 :l1 l1 :l2 l2 :g1 g1 :g2 g2 :f1 f1))
                       (solution (fixed-point *newton-raphson-internal-criteria*
                                              (improve-lambda function #'g)
                                              data1
                                              buffer1
                                              buffer2))
                       (final-data (iterator:value solution)))
                  (cond ((iteratir:finished? solution)
                         (list (copy-vector (opt-data-p final-data) final-p-buffer)
                               (copy-vector (opt-data-f1 final-data) final-f-buffer)))
                        (t (error "NEWTON-METHOD:OPTIMIZE-STEP cannot keep X bound")))))))))))

(defun improve-lambda (function g)
  #'(lambda (arg tmp)
      (destructuring-bind (g0 Dg0 l1 l2 g1 g2 f1) arg
        (declare (ignore f1))
        (let ((new-l (next-lambda g0 Dg0 l1 l2 g1 g2)))
          (multiple-value-bind (new-g new-f) (funcall g new-l (slot-value tmp  'f1))
            (update-opt-data tmp new-l l1 new-g g1 new-f)
            tmp)))))

;; ** NEXT-LAMBDA: Creating bounds for X
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



