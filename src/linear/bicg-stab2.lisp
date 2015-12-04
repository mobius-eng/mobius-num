(in-package mobius.numeric.bicg-stab)
;; * BiCGStab method implementation

;; ** BiCGStab error conditions
(define-condition rho-is-zero (error)
  ((rho-is-zero-rho :initarg :rho :reader rho-is-zero-rho)
   (rho-is-zero-residual :initarg :r :reader rho-is-zero-residual)
   (rho-is-zero-initial-residual :initarg :r0 :reader rho-is-zero-initial-residual))
  (:documentation "This condition is thrown if current residual is orthogonal to
initial residual or both are very small: (r0,r) ~= 0"))

(define-condition r0*v-is-zero (error)
  ((r0*v-is-zero-r0 :initarg :r0 :reader r0*v-is-zero-r0)
   (r0*v-is-zero-v :initarg :v :reader r0*v-is-zero-v)
   (r0*v-is-zero-r0*v :initarg :r0*v :reader r0*v-is-zero-r0*v))
  (:documentation "In calculating new alpha:

             new-rho
   alpha = -----------
             r0 * v

denominator became zero"))

;; ** BiCGStab specific vector operations

(defun new-rho (r r0 tolerance)
  " Calculates rho = (dot r r0). The result must not be zero. Throws an error
if the result is zero"
  (let ((candidate (dot r r0)))
    (if (almost-zero-p candidate (* tolerance tolerance))
        (error 'rho-is-zero :rho candidate :r r :r0 r0)
        candidate)))

(defun residual! (A x b r)
  "Calculate the residual r = b - A(x)"
  (declare (type (vector double-float *) r)
           (type (vector * *) x b))
  (funcall A x r)
  (dotimes (i (length r))
    (setf (aref r i) (- (aref b i) (aref r i)))))


(defun bicg-new-direction! (p r v beta omega)
  "Get new direction

    p   =  r    +  beta (p   - omega    v   )
     i      i-1           i-1       i-1  i-1 


"
  (declare (type (vector double-float *) p r v)
           (type double-float beta omega))
  (dotimes (i (length p))
    (setf (aref p i)
          (+ (aref r i) (* beta (- (aref p i) (* omega (aref v i))))))))


(defun new-alpha (r0 v new-rho tolerance)
  " Calculate new alpha:
             new-rho
   alpha = -----------
             r0 * v

Thorws an error if (dot r0 v) becomes zero.
"
  (let ((r0*v (dot r0 v)))
    (if (almost-zero-p r0*v (* tolerance tolerance))
        (error 'r0*v-is-zero :r0 r0 :v v :r0*v r0*v)
        (/ new-rho r0*v))))


(defun update-s! (s r v alpha)
  "Update vector S

    s  = r      - alpha v 
     i    i - 1          i

"
  (declare (type (vector double-float *) s r v)
           (type double-float alpha))
  (dotimes (i (length r))
    (setf (aref s i) (- (aref r i) (* alpha (aref v i))))))

(defun new-omega (s vt)
  "Returns new omega:

    omega  = (dot s vt) / (square-vector vt)
         i
"
  (declare (type (vector double-float *) s vt))
  (/ (dot vt s) (square-vector vt)))

(defun update-residual! (r s vt omega)
  "Update residual:

    r  = s - omega  t
     i            i
"
  (declare (type (vector double-float *) r s vt)
           (type double-float omega))
  (dotimes (i (length r))
    (setf (aref r i)
          (- (aref s i)
             (* omega (aref vt i))))))

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

(defun make-bicg-stab-value (size)
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

(defun reinit! (value)
  "Reinitialize data for the problem of the same size"
  (setf (bicg-alpha value) 1.0d0)
  (setf (bicg-rho value) 1.0d0)
  (setf (bicg-omega value) 1.0d0))

(defun init-value! (value x0)
  "Initialize data with approximation X0"
  (declare (type bicg-stab-value value))
  (copy-vector-to! (bicg-r0 value) (bicg-r value)) ;;??
  (copy-vector-to! x0 (bicg-x value)))

;; ** Convergence control
;; *** Definition
(defvar *bicg-stab-tolerance* 1.0d-9)
(defvar *bicg-stab-max-iterations* 20)

;; *** Specialized control
(defclass bicg-stab-control ()
  ((bicg-control-tolerance :initarg :tolerance :accessor bicg-control-tolerance)))

(defun make-bicg-stab-control (&optional (tolerance *bicg-stab-tolerance*))
  (make-instance 'bicg-stab-control
    :tolerance tolerance))

(defmethod apply-control ((control bicg-stab-control) value)
  (let ((residual (l2-norm (bicg-r value))))
    (if (< residual (bicg-control-tolerance control))
        (iterator:finished value :bicg-control residual)
        (iterator:continue value))))

(defun make-control-bicg-stab (other-controls)
  (apply #'combine-controls
         (make-bicg-stab-control)
         (limit-iterations *bicg-stab-max-iterations*)
         other-controls))

;; ** BiCGStab method
(defclass bicg-stab ()
  ((bicg-stab-value
    :initarg :value
    :reader bicg-stab-value)
   (bicg-stab-control
    :initarg :control
    :reader bicg-stab-control)))

(defun make-bicg-stab (size &rest other-controls)
  (make-instance 'bicg-stab
    :value (make-bicg-stab-value size)
    :control (make-control-bicg-stab other-controls)))

;; ** BiCGStab Step
(defun make-bicg-stab-step (A tolerance)
  "One step of BiCGStab method.
  A is a function: (funcall A <arg> <dest>) where <arg> is a vector.
     <dest> is expected to be (VECTOR DOUBLE-FLOAT *)
  TOLERANCE is a DOUBLE-FLOAT tolerance to compare real numbers
Returns function of BICG-STAB-VALUE to be used as improving function
  for FIXED-POINT algorithm"
  (lambda (value)
    (declare (type bicg-stab-value value))
    (block nil
     (with-accessors ((r bicg-r) (r0 bicg-r0) (p bicg-p) (x bicg-x) (v bicg-v)
                      (s bicg-s) (vt bicg-t)
                      (omega bicg-omega) (alpha bicg-alpha) (rho bicg-rho))
         value
       (declare (type (vector double-float *) r r0 p x v s vt)
                (type double-float omega alpha rho))
       (let* ((new-rho (new-rho r r0 tolerance))          ; rho = (r,r0)
              (beta (* (/ new-rho rho) (/ alpha omega)))) ; beta = (new-rho/rho)*(alpha/omega)
         (setf rho new-rho)                               ; update rho in value
         (bicg-new-direction! p r v beta omega)           ; p = r + beta * (p - omega * v)
         (funcall A p v)                                  ; v = Ap
         (setf alpha (new-alpha r0 v new-rho tolerance))  ; alpha = new-rho/ r0 * v
         (update-s! s r v alpha)                          ; s = r - alpha v
         (when (vector-almost-zero-p s tolerance)         ; if || s || = 0
           (add-with-multipliers! x (cons alpha p))       ; x = x + alpha * p
           (set-vector-to-zero! r)                        ; r = 0
           (return-from nil value))                       ; return
         (funcall A s vt)                                 ; vt = As
         (setf omega (new-omega s vt))                    ; omega = (s,vt) / (vt,vt)
         (add-with-multipliers! x
                                (cons alpha p)
                                (cons omega s))           ; x = x + alpha*p + omega*s
         (update-residual! r s vt omega)                  ; r = s - omega * vt
         value)))))                                       ; return



;; ** BiCGSolver

(defun bicg-stab-solve (method A b x0)
  "Solve the set of linear equations defined by Ax=b
Arguments:
  METHOD is an instance of BICG-STAB with conforming vector sizes
    and extra computation controls if required
  A is a function (linear operator) (A <arg> <dest>) representing the
    matrix of the equations set. <arg> can be a vector, <dest> is expected
    to be (VECTOR DOUBLE-FLOAT *)
  B is a RHS vector
  X0 is initial approximation

Return value: ITERATOR of BICG-STAB-VALUE"
  (declare (type bicg-stab method)
           (type (vector * *) b x0))
  (let ((value (bicg-stab-value method)))
    (assert (= (length x0) (length b))
          ()
          'vector-length-mismatch
          :length1 (length x0)
          :length2 (length b))
    (assert (= (bicg-stab-vector-length value) (length x0))
            ()
            'vector-length-mismatch
            :length1 (bicg-stab-vector-length value)
            :length2 (length x0))
    (reinit! value)
    (residual! A x0 b (bicg-r0 value))
    (init-value! value x0)
    (cond ((vector-almost-zero-p b *bicg-stab-tolerance*)
           (set-vector-to-zero! (bicg-x value))
           (set-vector-to-zero! (bicg-r value))
           (iterator:finished value :converged "RHS vector B is zero"))
          ((vector-almost-zero-p (bicg-r0 value) *bicg-stab-tolerance*)
           (copy-vector-to! x0 (bicg-x value))
           (iterator:finished value :converged "Good first guess: residual = 0"))
          (t (fixed-point (bicg-stab-control method)
                          (make-bicg-stab-step A *bicg-stab-tolerance*)
                          value)))))

(defun bicg-stab-residual (value)
  "Get residual (error) from BICG-STAB-VALUE"
  (declare (type bicg-stab-value value))
  (bicg-r value))

(defun bicg-stab-solution (value)
  "Get solution from BICG-STAB-VALUE"
  (declare (type bicg-stab-value value))
  (bicg-x value))


(defmethod solve-linear ((method bicg-stab) a b x)
  (let (bicg-error-condition)
   (match
       (handler-case (bicg-stab-solve method a b x)
         (rho-is-zero (c)
           (declare (ignore c))
           (setf bicg-error-condition :rho-is-zero)
           nil)
         (r0*v-is-zero (c)
           (declare (ignore c))
           (setf bicg-error-condition :r0*v-is-zero)
           nil))
     ((iterator:iterator :status :finished :value value)
      (copy-vector-to! (bicg-x value) x)
      (values x t (l2-norm (bicg-r value))))
     (otherwise
      (values x nil (or bicg-error-condition
                        (l2-norm (bicg-r method))))))))

