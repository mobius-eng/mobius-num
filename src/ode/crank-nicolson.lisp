(in-package mobius-num.crank-nicolson)

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;  (series::install))

;;; Ordinary differential equation
(defclass ode ()
  ((mass :initarg :mass :reader ode-mass)
   (stiffness :initarg :stiffness :reader ode-stiffness)
   (fun :initarg :fun :reader ode-fun))
  (:documentation "ODE: m * du/dt = s * u + f(t,u)"))

(defmethod print-object ((obj ode) out)
  (print-unreadable-object (obj out :type t :identity nil)
    (with-slots (mass stiffness) obj
      (format out "M = ~a; S = ~a" mass stiffness))))

;; mass: operator, stiffness: operator
;; fun: double-float value -> value
(defun make-ode (mass stiffness fun)
  (make-instance 'ode
                 :mass mass
                 :stiffness stiffness
                 :fun fun))

;;; Crank-Nicolson method
(defclass crank-nicolson ()
  ((u-n :initarg :u-n :reader cn-u-n)
   (f-n :initarg :f-n :reader cn-f-n)
   (t-n :initarg :t-n :reader cn-t-n)
   (dt :initarg :dt :reader cn-dt))
  (:documentation "Crank-Nicolson: variables specifying n-th step
for equation m * du/dt = s +f(t,u).
u-n: value of u at n-th step
f-n: value of f(u-n)
t-n: corresponding to u-n value
dt: time step"))

(defmethod print-object ((obj crank-nicolson) out)
  (print-unreadable-object (obj out :type t :identity nil)
    (with-slots (u-n f-n t-n dt) obj
      (format out "u-n = ~a; f-n = ~a; t-n = ~a; dt = ~a~%"
              u-n f-n t-n dt))))

(defun make-crank-nicolson (u-n f-n t-n dt)
  (make-instance 'crank-nicolson
                 :u-n u-n
                 :f-n f-n
                 :t-n t-n
                 :dt dt))

(defun cn-copy (cn &key u-n f-n t-n dt)
  (make-instance 'crank-nicolson
                 :u-n (or u-n (cn-u-n cn))
                 :f-n (or f-n (cn-f-n cn))
                 :t-n (or t-n (cn-t-n cn))
                 :dt  (or dt (cn-dt cn))))

(defun cn-update (cn &key u-n f-n t-n dt)
  (make-instance 'crank-nicolson
                 :u-n (or (and u-n (funcall u-n (cn-u-n cn)))
                          (cn-u-n cn))
                 :f-n (or (and f-n (funcall f-n (cn-f-n cn)))
                          (cn-f-n cn))
                 :t-n (or (and t-n (funcall t-n (cn-t-n cn)))
                          (cn-t-n cn))
                 :dt (or (and dt (funcall dt (cn-dt cn)))
                         (cn-dt cn))))

;; -> operator
(defun cn-system-matrix (cn ode)
  "computes operator = (1/dt m - 1/2 s)"
  (operator- (operator-scale (ode-mass ode) (/ (cn-dt cn)))
             (operator-scale (ode-stiffness ode) 0.5d0)))

;; -> value
(defun cn-const-rhs (cn ode)
  "computes value (1/dt m + s/2) u^n + 1/2f^n"
  (let ((u-n (cn-u-n cn))
        (f-n (cn-f-n cn))
        (dt (cn-dt cn)))
    (let ((f-n/2 (value-scale f-n 0.5d0))
          (b (operator+ (operator-scale (ode-mass ode) (/ dt))
                        (operator-scale (ode-stiffness ode) 0.5d0))))
      (value+ (operator-apply b u-n) f-n/2))))

;; -> (value -> value)
(defun cn-nonlinear-fun (cn ode)
  "returns function (value -> value) f(t,u)/2"
  #'(lambda (u)
      (value-scale (funcall (ode-fun ode) (+ (cn-t-n cn) (cn-dt cn)) u)
                   0.5d0)))

;; time: number, u-n: value, dt:number
;; solver: operator value (value -> value) value -> value
;; -> value
(defun cn-attempt (cn ode solver)
  "attempts to advance time step dt, computes u-n+1"
  (let ((u-n+1 (funcall solver
                        (cn-system-matrix cn ode)
                        (cn-const-rhs cn ode)
                        (cn-nonlinear-fun cn ode)
                        (cn-u-n cn)))
        (t-n (cn-t-n cn))
        (dt (cn-dt cn)))
    (make-crank-nicolson u-n+1
                         (funcall (ode-fun ode) (+ t-n dt) u-n+1)
                         (+ t-n dt)
                         dt)))

;; -> (number -> value)
(defun cn-poly-approx (cn1 cn2 ode solver)
  "polynomial approximation of u on (time, time+dt).
Returns a function of tau = current-time - time-n"
  ;;(format t "CN-POLY: cn = ~a; u-n+1 = ~a~%" cn u-n+1)
  (let* ((u-n+1 (cn-u-n cn2))
         (operator (ode-mass ode))
         (rhs (value+ (operator-apply (ode-stiffness ode) (cn-u-n cn1)) (cn-f-n cn1)))
         (dt (- (cn-t-n cn2) (cn-t-n cn1)))
         (c (cn-u-n cn1))
         (b  (funcall solver operator rhs #'(lambda (u) (value-zero u)) (cn-f-n cn1)))
         (a (value-scale (value- u-n+1 (value+ (value-scale b dt) c))
                         (/ (* dt dt)))))
         (lambda (tau)
           (value+ (value-scale a (* tau tau))
                   (value+ (value-scale b tau) c)))))

;; -> (cons crank-nicolson crank-nicolson)
(defun cn-2-half-steps (cn1 cn2 ode solver)
  "Calculate CN approximation from cn1 to cn2 using two half steps"
  (let* ((dt/2 (* (cn-dt cn1) 0.5d0))
         (cn/2 (cn-copy cn1 :dt dt/2))
         (u-n+1/2-est (funcall (cn-poly-approx cn1 cn2 ode solver) dt/2))
         (cn/2-1 (cn-attempt cn/2 ode solver)))
    (cons cn/2-1
          (cn-attempt cn/2-1 ode solver))))

;; -> (values boolean crank-nicolson)
(defun cn-accept-error-p (cn0 cn cn-imp-pair err-fun tolerance)
  (let* ((safety-coeff 0.9d0)
         (eps (/ (* 4/3 (funcall err-fun (cn-u-n cn) (cn-u-n (cdr cn-imp-pair))))
                 tolerance))
         (min-error 7.2d-3)
         (new-dt (* safety-coeff (cn-dt cn) (expt eps -1/3))))
    (cond ((> eps 1.0d0)
           (if (> new-dt (cn-dt (car cn-imp-pair)))
               (values t (cdr cn-imp-pair))
               (values nil (cn-copy cn0 :dt new-dt))))
          ((< eps min-error) (values t (cn-update cn :dt (lambda (dt) (* dt 5.0d0)))))
          (t (values t (cn-copy cn :dt new-dt))))))

;; -> (crank-nicolson -> crank-nicolson)
(defun cn-step (ode solver err-fun tolerance)
  "Advances one time step using Crank-Nicolson algorithm for the equation
   M * du/dt = S * u + f(t,u)"
  (labels ((inner-step (cn)
             (let* ((cn-1 (cn-attempt cn ode solver))
                    (cn-pair (cn-2-half-steps cn cn-1 ode solver)))
               (multiple-value-bind (acceptp new-cn)
                   (cn-accept-error-p cn cn-1 cn-pair err-fun tolerance)
                 (if acceptp
                     new-cn
                     (if (num= (cn-t-n cn) (+ (cn-t-n cn) (cn-dt new-cn)))
                         (error "CN-STEP: unable to control the error, dt=0")
                         (inner-step new-cn)))))))
    #'inner-step))

(defun cn-series (ode time-start init-value init-time-step solver err-fun tolerance)
  "Produces the series of crank-nicolson objects corresponding to the problem"
  (let ((f-0 (funcall (ode-fun ode) time-start init-value)))
    (scan-fn 'crank-nicolson
             (lambda () (make-crank-nicolson init-value f-0 time-start init-time-step))
             (cn-step ode solver err-fun tolerance))))

(defun time-in-between-p (time cn1 cn2)
  "Tests if time lies between CN objects."
  (let ((t1 (cn-t-n cn1))
        (t2 (cn-t-n cn2)))
    (and (>= time t1) (<= time t2))))

(defun cn-combine-with-time (cn-series time-series)
  "Takes the series of CN objects and the series of times and produces
the series of (time cn1 . cn2) such that time lies between cn1 and cn2"
  (declare (optimizable-series-function))
  (let ((g (generator (mapping (((cn1 cn2) (chunk 2 1 cn-series)))
                               (cons cn1 cn2)))))
    (let ((cn-pair (next-in g)))
     (labels ((inner (time)
                (declare (optimize (speed 3) (debug 0) (safety 1)))
                (when (< time (cn-t-n (car cn-pair)))
                  (error
                   (format nil
                           "CN-COMBINE-WITH-TIME:
CN-SERIES does not contain TIME.time=~a, cn-pair=~a~%"
                           time cn-pair)))
                (if (time-in-between-p time (car cn-pair) (cdr cn-pair))
                    (cons time cn-pair)
                    (progn
                      (setf cn-pair (next-in g))
                      (inner time)))))
       (mapping ((time time-series))
                (inner time))))))

(defun cn-produce-pairs (time-cn-pairs ode solver)
  "Takes the list of (time cn1 . cn2) and produces the pairs (time . u-time)"
  (mapping ((entry time-cn-pairs))
           (destructuring-bind (time cn1 . cn2) entry
            (let ((cn1-cor (cn-copy cn1 :dt (- (cn-t-n cn2) (cn-t-n cn1)))))
              (cons time
                    (funcall (cn-poly-approx cn1-cor cn2 ode solver)
                             (- time (cn-t-n cn1))))))))

(defun crank-nicolson (ode time-start init-value solver err-fun tolerance times-out)
  (declare (optimizable-series-function))
  (let ((dt0 (- (if (num= (car times-out) time-start)
                    (cadr times-out)
                    (car times-out))
                time-start))
        (time-series (scan times-out)))
    (collect
        (cn-produce-pairs
         (cn-combine-with-time
          (cn-series ode time-start init-value dt0 solver err-fun tolerance)
          time-series)
         ode
         solver))))
