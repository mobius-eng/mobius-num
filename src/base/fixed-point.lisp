(in-package mobius.numeric.fixed-point)

(defun fixed-point (criteria f x0 tmp1 tmp2)
  "Fixed point method solving x=f(x) starting from x0.
TMP1 and TMP2 serve as temporary buffers for values of x.
Method guarantees that two most recent values are available,
previous values might be destroyed.
Function F must accept two arguments: x and the buffer for the result;
it returns f(x)"
  (let ((tmps (circular-list tmp1 tmp2))
        (criteria-function (criteria-function criteria)))
    (loop for tmp in tmps
          for x = x0 then (funcall f (iterator:value y) tmp)
          for y = (funcall criteria-function x0) then (funcall criteria-function x)
          unless (iterator:continue-p y)
            return (values y criteria)
          end)))
