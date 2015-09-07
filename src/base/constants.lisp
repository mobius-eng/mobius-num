(in-package mobius.numeric.constants)

(defconstant not-a-number
  #+sbcl #.(/ 0.0d0 0.0d0)
  #+clozure 1d+-0
  "Double floating point NaN")

(defconstant +infinity
  #+sbcl sb-ext:double-float-positive-infinity
  #+clozure 1d++0
  "Double floating point +inf")

(defconstant -infinity
  #+sbcl sb-ext:double-float-negative-infinity
  #+clozure -1d++0
  "Double floating point -inf")


(defun infinity-p (num)
  #+sbcl (sb-ext:float-infinity-p num)
  #+clozure (cc::infinity-p num))

(defun not-a-number-p (num)
  #+sbcl (sb-ext:float-nan-p num)
  #+clozure (eq num not-a-number))


;; (defconstant real 'double-float)
;; (defconstant sb32 '(signed-byte 32))
;; (defconstant sb64 '(signed-byte 64))
;; (defconstant ub32 '(unsigned-byte 32))
;; (defconstant ub64 '(unsigned-byte 64))
