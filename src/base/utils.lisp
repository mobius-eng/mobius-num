(in-package mobius.numeric.utils)

(defconstant +infinity
  #+sbcl sb-ext:double-float-positive-infinity
  #+clozure 1d++0
  "Representation of floating point +infinity")

(defconstant -infinity
  #+sbcl sb-ext:double-float-negative-infinity
  #+clozure -1d++0
  "Representation of floating point -infinity")

(defconstant not-a-number
  #+sbcl #.(/ 0.0d0 0.0d0)
  #+clozure 1d+-0
  "Representation of floating point NaN")
