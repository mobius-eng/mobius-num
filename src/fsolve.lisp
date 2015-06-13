(in-package mobius-num.fsolve)

(defun fsolve (f x0 &key
                      (simple-function t)
                      (criteria (criteria:build (criteria:converged #'num=)
                                                (criteria:limit-iterations 20)))
                      df
                      (df-tmp (outer-product x0 x0))
                      (lin-solver #'m/))
  "Solves linear equation f(x)=0 numerically using Newton-Raphson with backtracking.
Highly inefficient if Jacobian (DF) is not provided.
Arguments list:
  F      : representation of f(x): either (F X) or (F X BUFFER) where BUFFER is a
           location that is used to store the result to reduce memory footprint
  X0     : initial guess
  SIMPLE-FUNCTION : indicates if F has (F X)-form. Will also mean that DF has
                    (DF X)-form
  CRITERIA        : convergence control
  DF     : Jacobian of f(x) of the form (DF X) or (DF X BUFFER)
  DF-TMP : buffer to keep the results of DF
  LIN-SOLVER      : linear solver for Ax=b of the form
                    (LIN-SOLVER A b Y0 Y) where Y0 is initial guess
                    and Y is the buffer for the solution (memory optimisation)
Returns multiple values : solution xsol, value of f(xsol) and convergence status"
  (let* ((f (if simple-function
                #'(lambda (x tmp)
                    (declare (ignore tmp))
                    (funcall f x))
                f))
         (df (cond ((and df simple-function) (lambda (x buffer)
                                               (declare (ignore buffer))
                                               (funcall df x)))
                   (df df)
                   (t (deriv f)))))
    (let ((y (newton-method criteria lin-solver f df x0 df-tmp)))
      (let ((x (cadr (iterator:value y)))
            (fx (car (iterator:value y)))
            (status (iterator:status y)))
        (values x fx status)))))
