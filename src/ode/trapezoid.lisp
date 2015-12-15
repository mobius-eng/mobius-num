(in-package numeric-trapezoid)

(defclass ode-trap ()
  ((ode-trap-constant
    :initarg :constant
    :reader ode-trap-constant
    :documentation
    "Constant part of discretization:

           n   1    n   n
    C = - x  - - f(t , x )
               2
")
   (ode-trap-next-value
    :initarg :next-value
    :reader ode-trap-next-value
    :documentation
    "Space for the next value

     n+1       n+1
    x    ~= x(t   )
")
   (ode-trap-nonlinear-solver
    :initarg :nonlinear-solver
    :documentation
    "Nonlinear solver to solve for next value

     n+1
    x

in the discretized equation

 n+1   dt     n+1   n+1     n   1    n   n
x   - ---- f(t   , x   ) - x  - - f(t , x ) = 0
       2                        2
")
   (ode-trap-tmp-value
    :initarg :tmp-value
    :documentation
    "Temporary buffer to store intermediate values"))
  (:documentation
   "Representation of the quality controlled trapezoid method
of solving initial value problem ODE:

                              0
    D[x](t) = f(t,x); x(0) = x

with step discretization

 n + 1    n                                     
x      - x    1    n  n         n + 1   n + 1   
----------- = -(f(t ,x )  +  f(t     , x     )) 
    dt        2                                 

"))

