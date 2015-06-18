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
;; *** Parameters
(defvar *bicgstab-max-iterations* 20)
(defvar *bicgstab-tolerance* 1.0d-9)

;; *** Criteria
(defun bicg-criteria ()
  (let ((tolerance *bicgstab-tolerance*)
        (max-iterations *bicgstab-max-iterations*))
   (criteria:make
    (criteria:finished-value #'(lambda (data) (< (norm (bicgstab-r data)) tolerance)))
    (criteria:limit-iterations max-iterations))))

;; ** Method itself
(defun bicgstab (A b x0)
  (let* ((r0 (e=+! (e=-! (m* A b)) b))
         (buffer1 (make-init-data r0 x0))
         (buffer2 (make-init-data r0 x0))
         (s (zero-vector b))
         (mt (zero-vector b))
         (alpha-p (zero-vector b))
         (crt (bicg-criteria))
         (tolerance *bicgstab-tolerance*))
    (flet ((almost-zero? (x)
             (declare (type double-float x))
             (< (abs x) tolerance)))
     (flet ((improve (arg tmp)
              (multiple-value-bind (rho alpha w v p r x) (decompose arg)
                (declare (type double-float rho alpha w))
                (let* ((new-rho (let ((new-rho (dot r0 r)))
                                  (if (almost-zero? new-rho)
                                      (error "BICGSTAB: |rho| = 0")
                                      (setf (bicgstab-rho tmp) new-rho))))
                       (beta (* (/ new-rho rho) (/ alpha w)))
                       (new-p (e=+! (e=*! (e=+! (e*! (bicgstab-p tmp) v (- w)) p) beta)
                                    r))
                       (new-v (m* A new-p (bicgstab-v tmp)))
                       (r0*v (dot r0 new-v))
                       (new-alpha (setf (bicgstab-alpha tmp) (if (almost-zero? r0*v)
                                                                 0.0d0
                                                                 (/ new-rho r0*v)))))
                  ;; s = r - new-alpha * new-v
                  (setf s (e=+! (e*! s new-v (- new-alpha)) r))
                  (setf mt (m* A s mt))
                  (let ((t^2 (dot mt mt)))
                    (declare (type double-float t^2))
                    (if (almost-zero? t^2)
                        (progn
                          ;; new-x = x + new-alpha * new-p
                          (setf (bicgstab-x tmp)
                                (e=+! (e*! (bicgstab-x tmp) new-p new-alpha) x))
                          ;; r = b - A * new-x
                          (setf (bicgstab-r tmp) (m* A (bicgstab-x tmp)
                                                     (bicgstab-r tmp)))
                          (setf (bicgstab-r tmp) (negate-vector! (bicgstab-r tmp)))
                          (setf (bicgstab-r tmp) (e=+! (bicgstab-r tmp) b))
                          ;; update w : use the old one
                          (setf (bicgstab-w tmp) w)
                          tmp)
                        (let ((new-w (setf (bicgstab-w tmp) (/ (dot s mt) t^2))))
                          (declare (type double-float new-w))
                          (setf (bicgstab-x tmp)
                                (e=+! (e=+! (e*! (bicgstab-x tmp) s new-w)
                                            (e*! alpha-p new-p new-alpha))
                                      x))
                          (setf (bicgstab-r tmp)
                                (e=+! (e*! (bicgstab-r tmp) mt (- new-w)) s))
                          tmp)))))))
       (cond ((almost-zero? (norm b))
              (iterator:finished (list (zero-vector r0) (zero-vector r0))
                                 '((converged . "RHS is zero"))))
             ((almost-zero? (norm r0))
              (iterator:finished (list x0 r0)
                                 '((converged . "Good first guess: residual = 0"))))
             (t (let ((y (fixed-point crt
                                      #'improve
                                      (make-init-data r0 x0)
                                      buffer1
                                      buffer2)))
              (cond ((iterator:finished? y)
                     (let ((x (bicgstab-x (iterator:value y)))
                           (r (bicgstab-r (iterator:value y))))
                       (iterator:replace-value! y (list x r))))
                    (t y)))))))))
