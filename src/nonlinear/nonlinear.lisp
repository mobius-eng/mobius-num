(in-package numeric-nonlinear)

(defclass nonlinear-value ()
  ((nonlinear-value-x
    :initarg :x
    :accessor nonlinear-value-x
    :documentation
    "Value x for nonlinear problem")
   (nonlinear-value-f
    :initarg :f
    :accessor nonlinear-value-f
    :documentation
    "Value f(x) for nonlinear problem")))

(defun nonlinear-value (x f)
  "Constructs nonlinear value representation for x and f (=f(x))"
  (make-instance 'nonlinear-value :x x :f f))

(defun init-nonlinear-value (value function x0)
  "Initialize VALUE with X0 and (FUNCTION X0)"
  (copy-vector-to! x0 (nonlinear-value-x value))
  (funcall function (nonlinear-value-f value)))

(defun nonlinear-value-square-residual (value)
  (square-vector (nonlinear-value-f value)))

(defun nonlinear-value-residual (value)
  (sqrt (nonlinear-value-square-residual value)))

(defmethod print-object ((obj nonlinear-value) out)
  (print-unreadable-object (obj out :type t)
    (with-slots (nonlinear-value-x nonlinear-value-f) obj
      (format out "~A ~A" nonlinear-value-x nonlinear-value-f))))


(defgeneric solve-nonlinear (method f x0 jacobian*vector)
  (:documentation
   "Generic solver that needs to be implemented by a particular METHOD
Solves f(x)=0 starting from initial X0. Uses JACOBIAN if necessary (and if
it is provided).
F : function with sigunature (F X RESULT)
JACOBIAN*VECTOR : function with signature (JACOBIAN*VECTOR X VECTOR RESULT)
Returns ITERATOR:ITERATOR of the final NONLINEAR-VALUE"))

(defun fsolve (method function initial-approximation &optional jacobian*vector)
  "Solve nonlinear equation FUNCTION(X)=0 using METHOD, INITIAL-APPROXIMATION
  and (optional) JACOBIAN*VECTOR function.
  (NOTE: some methods might fail if Jacobian is not provided)
FUNCTION: with signature (FUNCTION X RESULT).
INITIAL-APPROXIMATION: initial (starting) argument value
JACOBIAN*VECTOR:  function with signature (JACOBIAN*VECTOR X VECTOR RESULT)
The result is put into X0.
Returns (VALUES X SUCCESSFUL-P FINAL-RESIDUAL)"
  (match (solve-nonlinear method function initial-approximation jacobian*vector)
    ((iterator:iterator :status :finished :value value)
     (copy-vector-to! (nonlinear-value-x value) initial-approximation)
     (values initial-approximation T (nonlinear-value-residual value)))
    ((iterator:iterator :value value)
     (copy-vector-to! (nonlinear-value-x value) initial-approximation)
     (values initial-approximation nil (nonlinear-value-residual value)))))

