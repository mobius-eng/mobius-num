(in-package mobius-num.bicg-stab)

;; * BICGSTAB method

;; ** Method state
;; *** Constructor
(defun make-init-data (r0 x0)
  (make-array 7
              :initial-contents
              (list 1.0d0 1.0d0 1.0d0
                    (elt-zero b) (elt-zero b)
                    (e+! (elt-zero b) r0)
                    (e+! (elt-zero b) x0))))

;; *** Selectors
(declaim (inline get-rho get-alpha get-w get-v get-p get-r get-x))
(defmacro define-getter (variable index)
  (let ((getter (intern (format nil "GET-~A" variable))))
   (with-gensyms (data new-value)
     `(progn
        (defun ,getter (,data)
          (declare (type simple-vector ,data))
          (svref ,data ,index))
        (defun (setf ,getter) (,new-value ,data)
          (declare (type simple-vector ,data))
          (setf (svref ,data ,index) ,new-value))))))

(define-getter rho 0)
(define-getter alpha 1)
(define-getter w 2)
(define-getter v 3)
(define-getter p 4)
(define-getter r 5)
(define-getter x 6)

(defun decompose (data)
  (values (get-rho data)
          (get-alpha data)
          (get-w data)
          (get-v data)
          (get-p data)
          (get-r data)
          (get-x data)))

;; ** Convergence control
;; *** Parameters
(defvar *bicgstab-max-iterations* 20)
(defvar *bicgstab-tolerance* 1.0d-9)

;; *** Criteria
(defun bicg-criteria ()
  (let ((tolerance *bicgstab-tolerance*)
        (max-iterations *bicgstab-max-iterations*))
   (criteria:build
    (criteria:finished-value (lambda (data)
                               (< (norm (get-rho data)) tolerance)))
    (criteria:limit-iterations max-iterations))))

(defun bicgstab (A b x0)
  (let* ((n (vector-dim b))
         (r0 (e=+! (e=-! (m* A b)) b))
         (buffer1 (make-init-data r0 x0))
         (buffer2 (make-init-data r0 x0))
         (s (elt-zero b))
         (t (elt-zero b))
         (alpha-p (elt-zero b))
         (criteria (bicg-criteria))
         (tolerance *bicgstab-tolerance*))
    (flet ((almost-zero? (x) (< (abs x) tolerance))
           (improve (arg tmp)
             (multiple-value-bind (rho alpha w v p r x) (decompose arg)
               (let* ((new-rho (let ((new-rho (dot r0 r)))
                                 (if (almost-zero? new-rho)
                                     (error "BICGSTAB: |rho| = 0")
                                     (setf (get-rho tmp) new-rho))))
                      (beta (* (/ new-rho rho) (/ alpha w)))
                      (new-p (e=+! (e=*! (e=+! (e*! (get-p tmp) v (- w)) p) beta) r))
                      (new-v (m* A new-p (get-v tmp)))
                      (r0*v (dot r0 new-v))
                      (new-alpha (setf (get-alpha tmp) (if (almost-zero? r0*v)
                                                           0.0d0
                                                           (/ new-rho r0*v)))))
                 (e=+! (e*! s new-v (- new-alpha)) r)                          ; s = r - new-alpha * new-v
                 (setf t (m* A s t))
                 (let ((t^2 (dot t t)))
                   (if (almost-zero? t^2)
                       (progn
                         (e=+! (e*! (get-x tmp) new-p new-alpha) x)            ; new-x = x + new-alpha * new-p
                         (setf (get-r tmp) (m* A (get-x tmp) (get-r tmp)))
                         (negate! (get-r tmp))
                         (e=+! (get-r tmp) b)                                  ; r = b - A * new-x
                         (setf (get-w tmp) w)
                         tmp)
                       (let ((new-w (setf (get-w tmp) (/ (dot s t) t^2))))
                         (setf (get-x tmp)
                               (e=+! (e=+! (e*! (get-x tmp) s new-w) (e*! alpha-p new-p new-alpha)) x))
                         (setf (get-r tmp)
                               (e=+! (e*! (get-r tmp) t (- new-w)) s))
                         tmp)))))))
      (if (almost-zero? (norm r0))
          (iterator:finished (list x0 r0) (list (cons 'converged "Good first guess: residual = 0")))
          (let ((y (fixed-point criteria #'improve (make-init-data r0 x0) buffer1 buffer2)))
            (cond ((iterator:finished? y)
                   (let ((x (get-x (iterator:value y)))
                         (r (get-r (iterator:value y))))
                     (iterator:replace-value! y (list x r))))
                  (t y)))))))
