(in-package mobius.numeric.bicg-stab)

;; * BICGSTAB method
;; ** Method state
;; *** Constructor
(defstruct bicgstab
  (rho 1.0d0)
  (alpha 1.0d0)
  (w 1.0d0)
  v
  p
  r
  x)

(defun make-init-data (r0 x0)
  (make-bicgstab :v (zero-vector r0)
                 :p (zero-vector r0)
                 :r (duplicate-vector r0)
                 :x (duplicate-vector x0)))

(defun decompose (data)
  (values (bicgstab-rho data)
          (bicgstab-alpha data)
          (bicgstab-w data)
          (bicgstab-v data)
          (bicgstab-p data)
          (bicgstab-r data)
          (bicgstab-x data)))

;; ** Convergence control
;; *** Specialized method
(defmethod compile-criterium ((type (eql :bicg-small-residual)) &rest args)
  (let ((tolerance (first args)))
    (format t "BICGSTAB:compile-criterium :bicg-small-residual tolerance = ~A"
            tolerance)
    (compile-criterium :finished-value
                       #'(lambda (data)
                           (< (norm (bicgstab-r data)) tolerance))
                       (format nil
                               "BiCGStab: converged with residum less then ~A"
                               tolerance))))
;; *** Definition
(defvar *bicgstab-criteria*
  (make-criteria
   :bicg-small-residual 1.0d-9
   :limit-iterations '(20 "BiCG max iterations reached")))

;; ** Make buffers
(defun make-buffers (b x0 r0)
  (values (make-init-data r0 x0)
          (make-init-data r0 x0)
          (zero-vector b)
          (zero-vector b)
          (zero-vector b)))

;; ** Utilities
(defun almost-zero? (x tolerance)
  (declare (type double-float x tolerance))
  (< (abs x) tolerance))

(defun new-rho (r r0 tolerance)
  (let ((candidate (dot r r0)))
    (if (almost-zero? candidate (* tolerance tolerance))
        (error "BICGSTAB: |rho| = 0: ~A; r = ~A and r0 = ~A" candidate r r0)
        candidate)))

(defun new-p (w beta v p r buffer)
  (e=+! (e=*! (e=+! (e*! buffer v (- w)) p) beta) r))

(defun new-alpha (r0 v new-rho tolerance)
  (let ((r0*v (dot r0 v)))
    (if (almost-zero? r0*v (* tolerance tolerance))
        0.0d0
        (/ new-rho r0*v))))

(defun new-values-1 (A r0 rho alpha w p v r buffer tolerance)
  "Produces updates of RHO ALPHA P V"
  (let* ((new-rho   (new-rho r r0 tolerance))
         (beta      (* (/ new-rho rho) (/ alpha w)))
         (new-p     (new-p w beta v p r (bicgstab-p buffer)))
         (new-v     (m* A new-p (bicgstab-v buffer)))
         (new-alpha (new-alpha r0 new-v new-rho tolerance)))
    (values new-rho new-alpha new-p new-v)))

(defun update-buffer (buffer rho alpha w v p r x)
  (setf (bicgstab-rho buffer) rho)
  (setf (bicgstab-alpha buffer) alpha)
  (setf (bicgstab-w buffer) w)
  (setf (bicgstab-v buffer) v)
  (setf (bicgstab-p buffer) p)
  (setf (bicgstab-r buffer) r)
  (setf (bicgstab-x buffer) x)
  buffer)

;; ** Method itself
(defun bicgstab (A b x0)
  "Solve Ax=b where A is a generalised linear operator with initial guess x0"
  (format t "~&Matrix A = ~A" A)
  (let* ((r0 (e=+! (e=-! (m* A b)) b))
         (tolerance (car (criteria-arguments *bicgstab-criteria*
                                             :bicg-small-residual))))
    (multiple-value-bind (buffer1 buffer2 s mt alpha-p) (make-buffers b x0 r0)
      (flet ((improve (arg tmp)
               (multiple-value-bind (rho alpha w v p r x) (decompose arg)
                 (multiple-value-bind (new-rho new-alpha new-p new-v)
                     (new-values-1 A r0 rho alpha w p v r tmp tolerance)
                   ;; s = r - new-alpha * new-v
                   (setf s (e=+! (e*! s new-v (- new-alpha)) r))
                   (setf mt (m* A s mt))
                   (let ((t^2 (dot mt mt)))
                     (declare (type double-float t^2))
                     (if (almost-zero? t^2 tolerance)
                         (let ((new-x (e=+! (e*! (bicgstab-x tmp) new-p new-alpha) x))
                               (new-r (e=+! (negate-vector! (m* A (bicgstab-x tmp)
                                                                (bicgstab-r tmp)))
                                            b)))
                           (update-buffer tmp new-rho new-alpha w
                                          new-v new-p new-r new-x))
                         (let* ((new-w (/ (dot s mt) t^2))
                                (new-x (e=+! (e=+! (e*! (bicgstab-x tmp) s new-w)
                                                   (e*! alpha-p new-p new-alpha))
                                             x))
                                (new-r (e=+! (e*! (bicgstab-r tmp) mt (- new-w)) s)))
                           (update-buffer tmp new-rho new-alpha new-w
                                          new-v new-p new-r new-x))))))))
        (cond ((almost-zero? (norm b) tolerance)
               (iterator:finished (list (zero-vector r0) (zero-vector r0))
                                  '((:converged . "RHS is zero"))))
              ((almost-zero? (norm r0) tolerance)
               (iterator:finished (list x0 r0)
                                  '((:converged . "Good first guess: residual = 0"))))
              (t (let ((y (fixed-point *bicgstab-criteria*
                                       #'improve
                                       (make-init-data r0 x0)
                                       buffer1
                                       buffer2)))
                   (cond ((iterator:finished? y)
                          (let ((x (bicgstab-x (iterator:value y)))
                                (r (bicgstab-r (iterator:value y))))
                            (iterator:replace-value! y (list x r))))
                         (t y)))))))))
