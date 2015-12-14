(in-package mobius.numeric.bicg-stab)
;; * BiCGStab method implementation

;; ** Value: state of BiCGStab algorithm
(defclass bicg-stab-value ()
  ((bicg-rho :initform 1.0d0 :accessor bicg-rho)
   (bicg-alpha :initform 1.0d0 :accessor bicg-alpha)
   (bicg-omega :initform 1.0d0 :accessor bicg-omega)
   (bicg-v :initarg :v :reader bicg-v)
   (bicg-p :initarg :p
           :accessor bicg-p
           :documentation "Search direction for next approximation")
   (bicg-r0 :initarg :r0
            :reader bicg-r0
            :documentation "Residual of the initial approximation")
   (bicg-r :initarg :r
           :reader bicg-r
           :documentation "Residual of approximation")
   (bicg-x :initarg :x
           :reader bicg-x
           :documentation "Current approximation")
   (bicg-s :initarg :s :reader bicg-s)
   (bicg-t :initarg :t :reader bicg-t))
  (:documentation "State of BiCGStab computation"))

(defmethod print-object ((obj bicg-stab-value) out)
  (print-unreadable-object (obj out :type nil)
    (format out "BICG-STAB~%x = ~A~%r = ~A~%r0 = ~A~%"
            (bicg-x obj) (bicg-r obj) (bicg-r0 obj))))

(defun bicg-stab-value (size)
  "Make BICG-DATA with all vector sizes SIZE"
  (make-instance 'bicg-stab-value
    :v (make-double-float-vector size)
    :p (make-double-float-vector size)
    :r0 (make-double-float-vector size)
    :r (make-double-float-vector size)
    :x (make-double-float-vector size)
    :s (make-double-float-vector size)
    :t (make-double-float-vector size)))

(defun bicg-stab-vector-length (bicg-stab-value)
  (length (bicg-x bicg-stab-value)))

(defun residual! (A x b r)
  "Calculate the residual r = b - A(x)"
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type (vector double-float *) r)
           (type (vector * *) x b))
  (funcall A x r)
  (dotimes (i (length r))
    (setf (aref r i) (- (aref b i) (aref r i)))))


(defun init-value! (value A x0 b)
  "Initialize value with approximation X0
Residual (BICG-R) is initialized as b - A*x0"
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type bicg-stab-value value))
  (setf (bicg-alpha value) 1.0d0)
  (setf (bicg-rho value) 1.0d0)
  (setf (bicg-omega value) 1.0d0)
  (residual! A x0 b (bicg-r0 value))
  (set-vector-to-zero! (bicg-p value))
  (set-vector-to-zero! (bicg-v value))
  (copy-vector-to! (bicg-r0 value) (bicg-r value))
  (copy-vector-to! x0 (bicg-x value)))

;; ** BiCGStab computation steps
(defun new-rho ()
  " Calculates rho = (dot r r0)."
  (alter-value
   #f(cons (dot (bicg-r %) (bicg-r0 %)) %)))

(defun new-rho-not-zero (tolerance)
  "Checks if new-rho is zero and if so, terminates computation"
  (failed-value
   (lambda (x)
     (let ((new-rho (car x)))
       (almost-zero-p new-rho (expt tolerance 2))))
   (lambda (x)
     (list "new-rho is zero" (cdr x)))))

;; EXPERIMENTAL!!!
(defun restart-if-rho-is-zero (tolerance)
  (alter-value
   (lambda (x)
     (declare (optimize (speed 3) (debug 0) (safety 1)))
     (destructuring-bind (new-rho . value) x
       (if (almost-zero-p new-rho tolerance)
           (progn
             (copy-vector-to! (bicg-r value) (bicg-r0 value))
             (set-vector-to-zero! (bicg-v value))
             (set-vector-to-zero! (bicg-p value))
             (setf (bicg-rho value) 1d0)
             (setf (bicg-alpha value) 1d0)
             (setf (bicg-omega value) 1d0)
             (cons (dot (bicg-r value) (bicg-r0 value)) value))
           x)))))

(defun new-direction (A)
  "Get new direction

    beta = rho    / rho  * alpha / omega
              i-1      i                i-1
 
    p   =  r    +  beta (p   - omega    v   )
     i      i-1           i-1       i-1  i-1 

   v  = A p
    i      i

"
  (alter-value
   (lambda (x)
     (declare (optimize (speed 3) (safety 1) (debug 0)))
     (destructuring-bind (new-rho . v) x
       (declare (type double-float new-rho))
       (let ((beta (* (/ new-rho (the double-float (bicg-rho v)))
                      (/ (the double-float (bicg-alpha v))
                         (the double-float (bicg-omega v))))))
         (linear-combination!
          beta (bicg-p v)
          (cons 1d0 (bicg-r v))
          (cons (- (* beta (bicg-omega v))) (bicg-v v)))
         (funcall A (bicg-p v) (bicg-v v))
         (setf (bicg-rho v) new-rho)
         v)))))

(defun r0*v ()
  "Calculate dot-product (r0, v)"
  (alter-value
   (lambda (value)
     (cons (dot (bicg-r0 value) (bicg-v value)) value))))

(defun r0*v-is-not-zero (tolerance)
  "Fail if r0*v is zero"
  (failed-value
   (lambda (x)
     (let ((r0*v (car x)))
       (almost-zero-p r0*v (expt tolerance 2))))
   (lambda (x)
     (list "r0*v is zero" (cdr x)))))


(defun new-alpha-s ()
  " Calculate new alpha:

               rho
                  i
   alpha = -----------
             r0 * v
                   i

and

  s = r    - alpha * v
       i-1            i
"
  (alter-value
   (lambda (x)
     (destructuring-bind (r0*v . v) x
       (setf (bicg-alpha v) (/ (bicg-rho v) r0*v))
       (linear-combination!
        0d0 (bicg-s v)
        (cons 1d0 (bicg-r v))
        (cons (- (bicg-alpha v)) (bicg-v v)))
       v))))

(defun finish-if-s-is-small (tolerance)
  "If l2norm of s is small - finish"
  (finished-value
   #f(vector-almost-zero-p (bicg-s %) tolerance)
   (lambda (value)
     (add-with-multipliers! (bicg-x value) (cons (bicg-alpha value) (bicg-p value)))
     (copy-vector-to! (bicg-s value) (bicg-r value))
     value)))

(defun new-t-omega-x-r (A)
  "Update:

               (s, t)  
    omega  = -----------
         i     (t, t)


    x  = x    + alpha p  + omega  s
     i    i-1          i        i

    r  = s - omega  t
     i            i

"
  (alter-value
   (lambda (value)
     (funcall A (bicg-s value) (bicg-t value))
     (setf (bicg-omega value)
           (/ (dot (bicg-s value) (bicg-t value))
              (square-vector (bicg-t value))))
     (add-with-multipliers!
      (bicg-x value)
      (cons (bicg-alpha value) (bicg-p value))
      (cons (bicg-omega value) (bicg-s value)))
     (linear-combination!
      0d0 (bicg-r value)
      (cons 1d0 (bicg-s value))
      (cons (- (bicg-omega value)) (bicg-t value)))
     value)))

(defun finish-if-residual-is-small (tolerance)
  (finished-value
   (lambda (value)
     (vector-almost-zero-p (bicg-r value) tolerance))))

;; *** Computation parameters
(defvar *bicg-stab-tolerance* 1.0d-8)
(defvar *bicg-stab-max-iter-coeff* 10)

(defun bicg-stab-solve (value A b x0 &rest other-controls)
  (check-vector-lengths b x0)
  (assert (= (length b) (bicg-stab-vector-length value))
          ()
          'vector-length-mismatch
          :length1 (length b)
          :length2 (bicg-stab-vector-length value))
  (init-value! value A x0 b)
  (let ((n (length b)))
    (let ((computation (combine-controls
                        (new-rho) (restart-if-rho-is-zero *bicg-stab-tolerance*)
                        (new-direction A)
                        (r0*v) (r0*v-is-not-zero *bicg-stab-tolerance*)
                        (new-alpha-s) (finish-if-s-is-small *bicg-stab-tolerance*)
                        (new-t-omega-x-r A)
                        (finish-if-residual-is-small *bicg-stab-tolerance*)
                        (limit-iterations (* n *bicg-stab-max-iter-coeff*)
                                          #f(list "exceeded max iterations" %)))))
      (iterate (apply #'combine-controls computation other-controls)
               (iterator:continue value)
               (combine-controls
                (finish-if-residual-is-small *bicg-stab-tolerance*)
                (finished-value
                 (lambda (value)
                   (declare (ignore value))
                   (vector-almost-zero-p b *bicg-stab-tolerance*))
                 (lambda (value)
                   (set-vector-to-zero! (bicg-x value))
                   (set-vector-to-zero! (bicg-r value))
                   value)))))))

(defclass bicg-stab ()
  ((bicg-value
    :initarg :value
    :reader bicg-value
    :documentation "Value part of BiCGStab method")
   (bicg-other-controls
    :initarg :other-controls
    :reader bicg-other-controls
    :documentation "Other controls for solver"))
  (:documentation
   "BiCGStab method"))

(defun bicg-stab (size &rest other-controls)
  "Make BiCGSTab method for a problem of size SIZE."
  (make-instance 'bicg-stab
    :value (bicg-stab-value size)
    :other-controls other-controls))

(defun bicg-stab-size (bicg-stab)
  "Method's problem size (vectors lengths)"
  (bicg-stab-vector-length (bicg-value bicg-stab)))

(defun bicg-stab-residual (value)
  "Get residual (error) from BICG-STAB-VALUE"
  (declare (type bicg-stab-value value))
  (bicg-r value))

(defun bicg-stab-solution (value)
  "Get solution from BICG-STAB-VALUE"
  (declare (type bicg-stab-value value))
  (bicg-x value))

(defmethod solve-linear ((method bicg-stab) a b x)
  (match (apply #'bicg-stab-solve (bicg-value method) a b x (bicg-other-controls method))
    ((iterator:iterator :status :finished :value value)
     (copy-vector-to! (bicg-x value) x)
     (values x t (l2-norm (bicg-r value))))
    ((iterator:iterator :status :failed :value x)
     (let ((msg (first x))
           (value (second x)))
       (values x nil (bicg-r value) msg)))))


