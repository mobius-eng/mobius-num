(in-package mobius.numeric.fixed-point)

(defun fixed-point (control f x0)
  "Fixed point method solving x=f(x) starting from x0.
TMP1 and TMP2 serve as temporary buffers for values of x.
Method guarantees that two most recent values are available,
previous values might be destroyed.
Function F must accept two arguments: x and the buffer for the result;
it returns f(x)"
  (init-control control x0)
  (flet ((update-iterator (iterator-x)
           (iterator:bind
            (iterator:update-value iterator-x f)
            (lambda (x) (apply-control control x)))))
    (let ((iterator-x0 (iterator:continue x0 :fixed-point :init)))
      (loop
         for iterator-x = (update-iterator iterator-x0) then (update-iterator iterator-x)
         unless (iterator:continue-p iterator-x)
         return iterator-x
         end))))
