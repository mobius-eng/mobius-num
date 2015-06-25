(in-package mobius.numeric.constants)

(defconstant not-a-number gsl:+nan+
  "Floating point NaN")

(defconstant +infinity
  #+sbcl sb-ext:double-float-positive-infinity
  #+clozure 1d++0
  "Floating point +inf")

(defconstant -infinity
  #+sbcl sb-ext:double-float-negative-infinity
  #+clozure -1d++0
  "Floating point -inf")

(defconstant real 'double-float)
(defconstant sb32 '(signed-byte 32))
(defconstant sb64 '(signed-byte 64))
(defconstant ub32 '(unsigned-byte 32))
(defconstant ub64 '(unsigned-byte 64))
