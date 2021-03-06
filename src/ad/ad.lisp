(in-package mobius.numeric.ad)
;; * Automatic Differentiation

;; ** Some book-keeping
(defvar *e* 0
  "Keeps track of what was differentiated")

;; ** Dual numbers
(defclass dual-number ()
  ((epsilon
    :initarg :epsilon
    :reader epsilon
    :documentation "Book-keeping parameter")
   (primal
    :initarg :primal
    :reader primal
    :documentation "Main part of a number")
   (perturbation
    :initarg :perturbation
    :reader perturbation
    :documentation "Derivative part of a number")))

;; Can print it just as a number, but it is useful to
;; keep track of all perturbations
(defmethod print-object ((obj dual-number) out)
    (cl:print-unreadable-object (obj out :type t)
      (cl:with-slots (epsilon primal perturbation) obj
        (format out "~D ~A ~A" epsilon primal perturbation))))
;; ** Tape
;; Not used so far
(defclass tape ()
  ((epsilon :initarg :epsilon :reader epsilon)
   (primal :initarg :primal :reader primal)
   (factors :initarg :factors :reader factors)
   (tapes :initarg :tapes :reader tapes)
   (fanout :initarg :fanout :accessor fanout)
   (sensitivity :initarg :sensitivity :accessor sensitivity)))

(defmethod print-object ((obj tape) out)
  (print-object (primal obj) out))

;; Surprisingly normal number does not have nice print-object
;; implementation
(defmethod print-object ((obj number) out)
  (prin1 obj out))

(defun dual-number-p (obj)
  (eq (type-of obj) 'dual-number))

(defun tape-p (obj)
  (eq (type-of obj) 'tape))

(defun ad-type (x)
  "Dispatch helper: either :DUAL or :TAPE"
  (cond ((dual-number-p x) :dual)
        ((tape-p x) :tape)))

(defun compare-differentiable (x1 x2)
  "Compare epsilons of two generalized numbers"
  (cond ((cl:< (epsilon x1) (epsilon x2)) :<)
        ((cl:< (epsilon x2) (epsilon x1)) :>)
        (t nil)))

(defun new-tape (epsilon primal factors tapes)
  "Construct new tape"
  (make-instance 'tape
                 :epsilon epsilon
                 :primal primal
                 :factors factors
                 :tapes tapes
                 :fanout 0
                 :sensitivity 0))

(defun new-dual-number (epsilon primal perturbation)
  "Construct new dual number"
  (make-instance 'dual-number
                 :epsilon epsilon
                 :primal primal
                 :perturbation perturbation))

(defun tapify (x)
  (new-tape *e* x nil nil))

(defgeneric gen-lift-real->real (x f df-dx))

(defmethod gen-lift-real->real ((x t) f df-dx)
  (funcall f x))

(defmethod gen-lift-real->real ((x dual-number) f df-dx)
  (new-dual-number (epsilon x)
                   (gen-lift-real->real (primal x) f df-dx)
                   (* (funcall df-dx (primal x)) (perturbation x))))

(defmethod gen-lift-real->real ((x tape) f df-dx)
  (new-tape (epsilon x)
            (gen-lift-real->real (primal x) f df-dx)
            (list (funcall df-dx (primal x)))
            (list x)))

(defun lift-real->real (f df-dx)
  (lambda (x)
    (gen-lift-real->real x f df-dx)))

(defgeneric gen-lift-real*real->real (x1 x2 f df-dx1 df-dx2))

(defmethod gen-lift-real*real->real ((x1 t) (x2 t) f df-dx1 df-dx2)
  (funcall f x1 x2))

(defmethod gen-lift-real*real->real ((x1 function) (x2 function) f df-dx1 df-dx2)
  (lambda (z)
    (gen-lift-real*real->real (funcall x1 z) (funcall x2 z) f df-dx1 df-dx2)))

(defmethod gen-lift-real*real->real ((x1 function) x2 f df-dx1 df-dx2)
  (lambda (z)
    (gen-lift-real*real->real (funcall x1 z) x2 f df-dx1 df-dx2)))

(defmethod gen-lift-real*real->real (x1 (x2 function) f df-dx1 df-dx2)
  (lambda (z)
    (gen-lift-real*real->real x1 (funcall x2 z) f df-dx1 df-dx2)))

(defmethod gen-lift-real*real->real ((x1 dual-number) (x2 t) f df-dx1 df-dx2)
  (new-dual-number (epsilon x1)
                   (gen-lift-real*real->real (primal x1) x2 f df-dx1 df-dx2)
                   (* (funcall df-dx1 (primal x1) x2)
                      (perturbation x1))))

(defmethod gen-lift-real*real->real ((x1 tape) x2 f df-dx1 df-dx2)
  (new-tape (epsilon x1)
            (gen-lift-real*real->real (primal x1) x2 f df-dx1 df-dx2)
            (list (funcall df-dx1 (primal x1) x2))
            (list x1)))

(defmethod gen-lift-real*real->real (x1 (x2 dual-number) f df-dx1 df-dx2)
  (new-dual-number (epsilon x2)
                   (gen-lift-real*real->real x1 (primal x2) f df-dx1 df-dx2)
                   (* (funcall df-dx2 x1 (primal x2))
                      (perturbation x2))))

(defmethod gen-lift-real*real->real (x1 (x2 tape) f df-dx1 df-dx2)
  (new-tape (epsilon x2)
            (gen-lift-real*real->real x1 (primal x2) f df-dx1 df-dx2)
            (list (funcall df-dx2 x1 (primal x2)))
            (list x2)))

(defmethod gen-lift-real*real->real ((x1 dual-number) (x2 dual-number) f df-dx1 df-dx2)
  (case (compare-differentiable x1 x2)
    ((:<) (new-dual-number (epsilon x2)
                           (gen-lift-real*real->real x1 (primal x2) f df-dx1 df-dx2)
                           (* (funcall df-dx2 x1 (primal x2))
                              (perturbation x2))))
    ((:>) (new-dual-number (epsilon x1)
                           (gen-lift-real*real->real (primal x1) x2 f df-dx1 df-dx2)
                           (* (funcall df-dx1 (primal x1) x2)
                              (perturbation x1))))
    (t (new-dual-number (epsilon x1)
                        (gen-lift-real*real->real (primal x1) (primal x2) f df-dx1 df-dx2)
                        (+ (* (funcall df-dx1 (primal x1) (primal x2))
                              (perturbation x1))
                           (* (funcall df-dx2 (primal x1) (primal x2))
                              (perturbation x2)))))))

(defmethod gen-lift-real*real->real ((x1 dual-number) (x2 tape) f df-dx1 df-dx2)
  (case (compare-differentiable x1 x2)
    ((:<) (new-tape (epsilon x2)
                    (gen-lift-real*real->real x1 (primal x2) f df-dx1 df-dx2)
                    (list (funcall df-dx2 x1 (primal x2)))
                    (list x2)))
    ((:>) (new-dual-number (epsilon x1)
                           (gen-lift-real*real->real (primal x1) x2 f df-dx1 df-dx2)
                           (* (funcall df-dx1 (primal x1) x2)
                              (perturbation x1))))
    (t (new-dual-number (epsilon x1)
                        (gen-lift-real*real->real (primal x1) x2 f df-dx1 df-dx2)
                        (* (funcall df-dx1 (primal x1) x2)
                           (perturbation x1))))))

(defmethod gen-lift-real*real->real ((x1 tape) (x2 dual-number) f df-dx1 df-dx2)
  (case (compare-differentiable x1 x2)
    ((:<) (new-dual-number (epsilon x2)
                           (gen-lift-real*real->real x1 (primal x2) f df-dx1 df-dx2)
                           (* (funcall df-dx2 x1 (primal x2))
                              (perturbation x2))))
    ((:>) (new-tape (epsilon x1)
                    (gen-lift-real*real->real (primal x1) x2 f df-dx1 df-dx2)
                    (list (funcall df-dx1 (primal x1) x2))
                    (list x1)))
    (t (new-tape (epsilon x1)
                 (gen-lift-real*real->real (primal x1) x2 f df-dx1 df-dx2)
                 (list (funcall df-dx1 (primal x1) x2))
                 (list x1)))))

(defmethod gen-lift-real*real->real ((x1 tape) (x2 tape) f df-dx1 df-dx2)
  (case (compare-differentiable x1 x2)
    ((:<) (new-tape (epsilon x2)
                    (gen-lift-real*real->real x1 (primal x2) f df-dx1 df-dx2)
                    (list (funcall df-dx2 x1 (primal x2)))
                    (list x2)))
    ((:>) (new-tape (epsilon x1)
                    (gen-lift-real*real->real (primal x1) x2 f df-dx1 df-dx2)
                    (list (funcall df-dx1 (primal x1) x2))
                    (list x1)))
    (t (new-tape (epsilon x1)
                 (gen-lift-real*real->real (primal x1) (primal x2) f df-dx1 df-dx2)
                 (list (funcall df-dx1 (primal x1) (primal x2))
                       (funcall df-dx2 (primal x1) (primal x2)))
                 (list x1 x2)))))

(defun lift-real*real->real (f df-dx1 df-dx2)
  (lambda (x1 x2)
    (gen-lift-real*real->real x1 x2 f df-dx1 df-dx2)))


(defun lift-real-n->real (f df-dx1 df-dx2)
  (lambda (&rest xs)
    (if (null xs)
        (funcall f)
        (reduce (lift-real*real->real f df-dx1 df-dx2) xs)))) ;;what if xs = (x)

(defun lift-real-n+1->real (f df-dx df-dx1 df-dx2)
  (lambda (&rest xs)
    (cond ((null xs) (funcall f))
          ((null (rest xs)) (gen-lift-real->real (first xs) f df-dx))
          (t (reduce (lift-real*real->real f df-dx1 df-dx2) xs)))))

(defmethod primal (x)
  x)

(defun lift-real-n->boolean (f)
  (lambda (&rest xs)
    (apply f (mapcar #'primal xs))))

;; (eval-when (:compile-toplevel :load-toplevel :execute)
(setf (symbol-function '+)
      (lift-real-n->real #'sym:bin+
                         (lambda (x1 x2)
                           (declare (ignore x1 x2))
                           1)
                         (lambda (x1 x2)
                           (declare (ignore x1 x2))
                           1)))

(setf (symbol-function '-)
      (lift-real-n+1->real #'sym:bin-
                           (lambda (x)
                             (declare (ignore x))
                             -1)
                           (lambda (x1 x2)
                             (declare (ignore x1 x2))
                             1)
                           (lambda (x1 x2)
                             (declare (ignore x1 x2))
                             -1)))

(setf (symbol-function '*)
      (lift-real-n->real #'sym:bin*
                         (lambda (x1 x2) (declare (ignore x1)) x2)
                         (lambda (x1 x2) (declare (ignore x2)) x1)))

(setf (symbol-function '/)
      (lift-real-n+1->real #'sym:bin/
                           (lambda (x) (- (/ (expt x 2))))
                           (lambda (x1 x2) (declare (ignore x1)) (/ x2))
                           (lambda (x1 x2) (- (/ x1 (expt x2 2))))))

(setf (symbol-function 'sqrt)
      (lift-real->real #'sym:sqrt (lambda (x) (/ 1 (* 2 (sqrt x))))))

(setf (symbol-function 'exp) (lift-real->real #'sym:exp (lambda (x) (exp x))))

(defun log (x &optional base)
  (funcall (lift-real->real (lambda (x) (sym:log x base))
                            (lambda (x)
                              (if (null base)
                                  (/ x)
                                  (/ (log (exp 1.0d0) base) x))))
           x))

(setf (symbol-function 'expt)
      (lift-real*real->real #'sym:expt
                            (lambda (x1 x2)
                              (let ((new-exp (- x2 1)))
                                (if (cl:= new-exp 1)
                                    (* x2 x1)
                                    (* x2 (expt x1 new-exp)))))
                            (lambda (x1 x2) (* (log x1) (expt x1 x2)))))
;;  )

;; (eval-when (:compile-toplevel :load-toplevel :execute)
(setf (symbol-function 'sin)
      (lift-real->real #'sym:sin (lambda (x) (cos x))))

(setf (symbol-function 'cos)
      (lift-real->real #'sym:cos (lambda (x) (- (sin x)))))

(setf (symbol-function 'tan)
      (lift-real->real #'sym:tan (lambda (x) (+ 1 (expt (tan x) 2)))))

(setf (symbol-function 'asin)
      (lift-real->real #'sym:asin (lambda (x) (/ 1 (sqrt (- 1 (expt x 2)))))))

(setf (symbol-function 'acos)
      (lift-real->real #'sym:acos (lambda (x) (- (/ 1 (sqrt (- 1 (expt x 2))))))))

(setf (symbol-function 'atan)
      (lift-real->real #'sym:atan (lambda (x) (/ 1 (+ 1 (expt x 2))))))

(setf (symbol-function 'sinh)
      (lift-real->real #'sym:sinh (lambda (x) (cosh x))))

(setf (symbol-function 'cosh)
      (lift-real->real #'sym:cosh (lambda (x) (sinh x))))

(setf (symbol-function 'tanh)
      (lift-real->real #'sym:tanh (lambda (x) (- 1 (expt (tanh x) 2)))))

(setf (symbol-function '=)
      (lift-real-n->boolean #'sym:=))

(setf (symbol-function '<)
      (lift-real-n->boolean #'sym:<))

(setf (symbol-function '>)
      (lift-real-n->boolean #'sym:>))

(setf (symbol-function '<=)
      (lift-real-n->boolean #'sym:<=))

(setf (symbol-function '>=)
      (lift-real-n->boolean #'sym:>=))

(setf (symbol-function 'zerop)
      (lift-real-n->boolean #'sym:zerop))
;; )

;; (eval-when (:compile-toplevel :load-toplevel :execute)
(setf (symbol-function 'plusp)
      (lift-real-n->boolean #'sym:plusp))

(setf (symbol-function 'minusp)
      (lift-real-n->boolean #'sym:minusp))

(setf (symbol-function 'numberp)
      (lift-real-n->boolean #'cl:numberp))

(setf (symbol-function 'varibale-p)
      (lift-real-n->boolean #'cl:symbolp))
;; )


(defun literal-function (f)
  (lift-real->real (sym:literal-function f)
                   (lambda (x) (list (list 'diff f) x))))

(defun forward-mode (map-independent map-dependent f x x-perturbation)
  "Functional way of forward mode of differentiation"
  (cl:incf *e*)
  (let ((y-forward
         (funcall f (funcall map-independent
                             (lambda (x x-perturbation)
                               (new-dual-number *e* x x-perturbation))
                             x
                             x-perturbation))))
    (cl:decf *e*)
    (funcall map-dependent
             (lambda (y-forward)
               (if (or (not (dual-number-p y-forward))
                       (cl:< (epsilon y-forward) *e*))
                   0
                   (perturbation y-forward)))
             y-forward)))

(defun forward-mode* (map-independent map-dependent f x x-perturbation dual-x y-forward y dy)
  "Imperative implementation of forward mode of differentiation"
  (flet ((make-dual-number (x x-perturbation)
           (new-dual-number *e* x x-perturbation))
         (extract-perturbation (y-forward)
           (if (or (not (dual-number-p y-forward))
                   (cl:< (epsilon y-forward) *e*))
               0
               (perturbation y-forward))))
    (cl:incf *e*)
    (funcall map-independent dual-x #'make-dual-number x x-perturbation)
    (funcall f dual-x y-forward)
    (cl:decf *e*)
    (list (lambda ()
            (funcall map-dependent y #'primal y-forward))
          (lambda ()
            (funcall map-dependent dy #'extract-perturbation y-forward)))))

(defun D (f)
  (lambda (x)
    (forward-mode (lambda (g x perturbation)
                    (funcall g x perturbation))
                  (lambda (g y-forward)
                    (funcall g y-forward))
                  f
                  x
                  1)))

(defun diff (f x)
  (forward-mode (lambda (f x perturbation) (funcall f x perturbation))
                (lambda (f y-forward) (funcall f y-forward))
                f
                x
                1))

(defun directional-derivative-f (f)
  (lambda (x x-perturbation)
    (forward-mode (lambda (g x x-perturbation)
                    (map 'simple-vector g x x-perturbation))
                  (lambda (g y-forward)
                    (map 'simple-vector g y-forward))
                  f
                  x
                  x-perturbation)))

(defun directional-derivative-forward* (f dual-x y-forward)
  (let ((size (length y-forward)))
    (flet ((map-independent (x-dest g x x-perturbation)
             (dotimes (i size)
               (setf (svref x-dest i)
                     (funcall g (aref x i) (aref x-perturbation i)))))
           (map-dependent (y-dest g y-dual)
             (dotimes (i size)
               (setf (aref y-dest i)
                     (funcall g (svref y-dual i))))))
      (lambda (x x-perturbation dy)
        (-> (forward-mode* #'map-independent #'map-dependent
                           f
                           x x-perturbation dual-x
                           y-forward nil dy)
            second
            funcall)))))

(defun jacobian*vector (f dual-x dual-y)
  (directional-derivative-forward* f dual-x dual-y))


(defun directional-derivative-f-buffer (f x x-perturbation x-dest df-dest)
  "Compute directional derivative of F (-> VECTOR VECTOR) at point X (VECTOR)
in direction X-PERTURBATION (VECTOR).
X-DEST (SIMPLE-VECTOR) is used to store intermmediate values of X with dual
numbers
DF-DEST (VECTOR DOUBLE-FLOAT) is a vector of the result.
This function cannot be applied to symbolic data!"
  (forward-mode (lambda (g x x-perturbation)
                  (dotimes (i (length x) x-dest)
                    (setf (svref x-dest i)
                          (funcall g (aref x i) (aref x-perturbation i)))))
                (lambda (f y-forward)
                  (dotimes (i (length y-forward) df-dest)
                    (setf (aref df-dest i)
                          (coerce (funcall f (svref y-forward i)) 'double-float))))
                f
                x
                x-perturbation))

(defun jacobian*vector-save (f x v x-dest df-dest)
  (directional-derivative-f-buffer f x v x-dest df-dest)
  (dotimes (i (length df-dest))
    (setf (aref df-dest i) (primal (aref df-dest i))))
  df-dest)

(defun update-vector (v i x)
  (let ((v1 (make-array (length v) :initial-contents v)))
    (setf (cl:svref v1 i) x)
    v1))

(defun gradient-f (f)
  (lambda (x)
    (map 'simple-vector
         (lambda (i)
           (diff (lambda (xi) (funcall f (update-vector x i xi)))
                 (svref x i)))
         (iota (length x)))))

(defun partial (n f)
  (lambda (v)
    (diff (lambda (x) (funcall f (update-vector v n x))) (svref v n))))


;; TODO: reverse derivatives!
