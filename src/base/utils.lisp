(in-package mobius.numeric.utils)

(declaim (optimize (speed 3)))
(declaim (inline sq))
(defun sq (x)
  (declare (type double-float x))
  (* x x))

