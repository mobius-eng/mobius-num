(in-package mobius.numeric.utils)

;; #+clozure
;; (progn
;;   (defconstant ++INF+ #.(unwind-protect
;;                              (progn
;;                                (ccl:set-fpu-mode :division-by-zero nil)
;;                                (/ 0d0))
;;                           (ccl:set-fpu-mode :division-by-zero t))))



;; #+sbcl
;; (progn
;;  (defconstant +NAN+ #.(/ 0.0d0 0.0d0))
 
;;  (defconstant ++INF+ #.(/ 1.0d0 0.0d0))
 
;;  (defconstant +-INF+ #.(/ -1.0d0 0.0d0)))

(defconstant ++INF+
  #+sbcl sb-ext:double-float-positive-infinity
  #+clozure 1d++0)

(defconstant +-INF+
  #+sbcl sb-ext:double-float-negative-infinity
  #+clozure -1d++0)

(defconstant +NAN+
  #+sbcl #.(/ 0.0d0 0.0d0)
  #+clozure #.(error "CLOZURE-CL: Not a number constant is not available"))
