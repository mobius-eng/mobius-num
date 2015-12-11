(in-package mobius.numeric.fixed-point)


(defun iterate (control y0 &optional pre-treat)
  "Iteratively apply CONTROL to iterator Y0 until it finishes"
  (init-control pre-treat y0)
  (init-control control y0)
  (loop for y = (apply-control pre-treat y0) then (apply-control control y)
     unless (iterator:continue-p y)
     return y
     end))

(defun fixed-point (control f x0)
  "Fixed point method solving x=f(x) starting from x0.
CONTROL is an object implementing control protocol:
INIT-CONTROL and APPLY-CONTROL.
Function F might change X destuctively"
  (let ((c (alter-value f)))
    (iterate (combine-controls c control) (iterator:continue x0))))
