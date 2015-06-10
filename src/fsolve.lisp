(in-package mobius-num.fsolve)

(defun fsolve (f x0 &key
                      (criteria (criteria:build (criteria:converged #'num=)
                                                (criteria:limit-iterations 20)))
                      (df (deriv f))
                      (df-tmp (outer-product x0 x0))
                      (lin-solver )))
