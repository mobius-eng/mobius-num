(in-package mobius.numeric.fixed-point)

(defun fixed-point (criteria f x0 tmp1 tmp2)
  "Fixed point method solving x=f(x) starting from x0.
TMP1 and TMP2 serve as temporary buffers for values of x.
Method guarantees that two most recent values are available,
previous values might be destroyed.
Function F must accept two arguments: x and the buffer for the result;
it returns f(x)"
  (let* ((tmps (let ((lst (list tmp1 tmp2)))
                 (setf (cddr lst) lst)
                 lst)))
    (loop for tmp in tmps
       for x = x0 then (funcall f (iterator:value y) tmp)
       for y = (funcall criteria x0) then (funcall criteria x)
       unless (iterator:continue? y)
       return y
       end)))
