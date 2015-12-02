(in-package numeric-nonlinear)


(defgeneric fsolve (method f x0)
  (:documentation
   "Solve nonlinear equation F(X)=0 using METHOD and initial approximation X0
The result is put into X0.
Returns (VALUES X SUCCESSFUL-P FINAL-RESIDUAL)"))
