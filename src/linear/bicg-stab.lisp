(in-package mobius.numeric.bicg-stab)

;; * Data: state of BiCGStab algorithm
(defstruct (data (:constructor mk-data))
  (rho 1.0d0)
  (alpha 1.0d0)
  (w 1.0d0)
  v
  p
  r
  x)

(defun make-data (seed)
  "Make DATA from SEED: a kind of vector on which the problem will be solved"
  (mk-data :v (zero-vector seed)
           :p (zero-vector seed)
           :r (zero-vector seed)
           :x (zero-vector seed)))

(defun init-data (data r0 x0)
  "Initialize data with approximation X0 and residual R0"
  (declare (type data data))
  (setf (slot-value data 'r) (duplicate-vector r0 (slot-value data 'r)))
  (setf (slot-value data 'x) (duplicate-vector x0 (slot-value data 'x))))

(defun decompose (data)
  (declare (type data data))
  (with-slots (rho alpha w v p r x) data
    (values rho alpha w v p r x)))

;; * Buffers: helps avoiding memory allocation
(defstruct buffers data-1 data-2 s mt alpha*p)

(defun destructure-buffers (buffers)
  (declare (type buffers buffers))
  (with-slots (data-1 data-2 s mt alpha*p) buffers
    (list data-1 data-2 s mt alpha*p)))

;; * BiCGStab method
(defclass bicgstab ()
  ((data :initarg :data :accessor bicgstab-data)
   (buffers :initarg :buffers :accessor bicgstab-buffers)))

(defun make-bicgstab-method (seed)
  "Make BiCGStab method. SEED must be a vector object for the problem to be solved"
  (let ((data (make-data seed))
        (buffer-data-1 (make-data seed))
        (buffer-data-2 (make-data seed)))
    (make-instance 'bicgstab
                   :data data
                   :buffers (make-buffers :data-1 buffer-data-1
                                          :data-2 buffer-data-2
                                          :s (zero-vector seed)
                                          :mt (zero-vector seed)
                                          :alpha*p (zero-vector seed)))))

;; * Convergence control
;; ** Specialized criteria
(defmethod compile-criterium ((type (eql :bicg-small-residual)) &rest args)
  (let ((tolerance (first args)))
    (compile-criterium :finished-value
                       #'(lambda (data)
                           (< (norm (data-r data)) tolerance))
                       (format nil
                               "BiCGStab: converged with residum less then ~A"
                               tolerance))))
;; ** Definition
(defvar *bicgstab-criteria*
  (make-criteria
   :bicg-small-residual 1.0d-9
   :limit-iterations '(20 "BiCG max iterations reached")))

;; * Utilities
(defun almost-zero? (x tolerance)
  (declare (type double-float x tolerance))
  (< (abs x) tolerance))

(defun new-rho (r r0 tolerance)
  (let ((candidate (dot r r0)))
    (if (almost-zero? candidate (* tolerance tolerance))
        (error "BICGSTAB: |rho| = 0: ~A; r = ~A and r0 = ~A" candidate r r0)
        candidate)))

(defun new-p (w beta v p r buffer)
  (.=+! (.=*! (.=+! (.*! buffer v (- w)) p) beta) r))

(defun new-alpha (r0 v new-rho tolerance)
  (let ((r0*v (dot r0 v)))
    (if (almost-zero? r0*v (* tolerance tolerance))
        0.0d0 ;; TODO: error???
        (/ new-rho r0*v))))

(defun new-values-1 (A r0 rho alpha w p v r data-buffer tolerance)
  "Produces updates of RHO ALPHA P V"
  (let* ((new-rho   (new-rho r r0 tolerance))
         (beta      (* (/ new-rho rho) (/ alpha w)))
         (new-p     (new-p w beta v p r (data-p data-buffer)))
         (new-v     (m* A new-p (data-v data-buffer)))
         (new-alpha (new-alpha r0 new-v new-rho tolerance)))
    (values new-rho new-alpha new-p new-v)))

(defun update-buffer (data-buffer rho alpha w v p r x)
  "Updated DATA-BUFFER with new values. Higly likely that DATA-BUFFER
contains these values already, but just in case."
  (setf (data-rho data-buffer) rho)
  (setf (data-alpha data-buffer) alpha)
  (setf (data-w data-buffer) w)
  ;; A safe guard: components of DATA and V P R X must refer to the
  ;; same memory location
  (assert (eq (data-v data-buffer) v) () "BICGSTAB: DATA-V and V are not EQ")
  (assert (eq (data-p data-buffer) p) () "BICGSTAB: DATA-P and P are not EQ")
  (assert (eq (data-r data-buffer) r) () "BICGSTAB: DATA-R and R are not EQ")
  (assert (eq (data-x data-buffer) x) () "BICGSTAB: DATA-X and X are not EQ")
  data-buffer)

;; ** Method itself
(defun bicgstab (method A b x0 final-x final-r)
  "Solve Ax=b where A is a generalised linear operator with initial guess x0"
  (format t "~&Matrix A = ~A" A)
  (let* ((r0 (.=+! (.=-! (m* A b)) b))
         (tolerance (car (criterium-arguments *bicgstab-criteria*
                                              :bicg-small-residual))))
    (init-data (bicgstab-data method) r0 x0)
    (destructuring-bind (data-1 data-2 s mt alpha*p)
        (destructure-buffers (bicgstab-buffers method))
      (flet ((improve (arg tmp)
               (multiple-value-bind (rho alpha w v p r x) (decompose arg)
                 (multiple-value-bind (new-rho new-alpha new-p new-v)
                     (new-values-1 A r0 rho alpha w p v r tmp tolerance)
                   ;; s = r - new-alpha * new-v
                   (setf s (.=+! (.*! s new-v (- new-alpha)) r))
                   (setf mt (m* A s mt))
                   (let ((t^2 (dot mt mt)))
                     (declare (type double-float t^2))
                     (if (almost-zero? t^2 tolerance)
                         (let ((new-x (.=+! (.*! (data-x tmp) new-p new-alpha) x))
                               (new-r (.=+! (negate-vector (m* A
                                                               (data-x tmp)
                                                               (data-r tmp))
                                                           (data-r tmp))
                                            b)))
                           (update-buffer tmp new-rho new-alpha w
                                          new-v new-p new-r new-x))
                         (let* ((new-w (/ (dot s mt) t^2))
                                (new-x (.=+! (.=+! (.*! (data-x tmp) s new-w)
                                                   (.*! alpha*p new-p new-alpha))
                                             x))
                                (new-r (.=+! (.*! (data-r tmp) mt (- new-w)) s)))
                           (update-buffer tmp new-rho new-alpha new-w
                                          new-v new-p new-r new-x))))))))
        (cond ((almost-zero? (norm b) tolerance)
               (iterator:finished (list (zero-vector r0 final-x)
                                        (zero-vector r0 final-r))
                                  '((:converged . "RHS is zero"))))
              ((almost-zero? (norm r0) tolerance)
               (iterator:finished (list (duplicate-vector x0 final-x)
                                        (duplicate-vector r0 final-r))
                                  '((:converged . "Good first guess: residual = 0"))))
              (t (let ((y (fixed-point *bicgstab-criteria*
                                       #'improve
                                       (bicgstab-data method)
                                       data-1
                                       data-2)))
                   (cond ((iterator:finished-p y)
                          (let ((x (data-x (iterator:value y)))
                                (r (data-r (iterator:value y))))
                            (iterator:replace-value y
                                                    (list
                                                     (duplicate-vector x final-x)
                                                     (duplicate-vector r final-r)))))
                         (t y)))))))))
