(in-package mobius.numeric.fixed-point)

(defun fixed-point (criteria function x0 tmp)
  "Fixed point method solving x=f(x) starting from x0.
TMP serves as temporary buffers for values of x. This values mutates
between iterations!
Function F must accept two arguments: x and the buffer for the result.
It returns f(x)"
  (loop
     for x = x0 then (funcall function (iterator:value y) tmp)
     for y = (funcall criteria x0) then (funcall criteria x)
     unless (iterator:continue-p y)
     return y
     end))
