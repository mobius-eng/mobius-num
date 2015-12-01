(in-package cl-user)

(defpackage #:mobius.numeric.symbolic
  (:nicknames #:sym)
  (:shadow #:+ #:- #:* #:/
           #:sin #:cos #:tan #:asin #:acos #:atan
           #:exp #:expt #:log #:sqrt
           #:sinh #:cosh #:tanh
           #:= #:< #:> #:<= #:>=
           #:zerop #:plusp #:minusp)
  (:use #:cl #:optima)
  (:export #:+ #:- #:* #:/
           #:bin+ #:bin- #:bin* #:bin/
           #:sin #:cos #:tan #:asin #:acos #:atan
           #:exp #:log #:expt #:sqrt
           #:sinh #:cosh #:tanh
           #:= #:< #:> #:<= #:>=
           #:zerop #:plusp #:minusp))

(defpackage #:mobius.numeric.ad
  (:nicknames #:ad)
  (:import-from #:cl #:in-package #:defvar #:defun #:setf #:lambda #:let
                #:&rest #:&optional #:eval-when
                #:svref #:map #:simple-vector #:length #:vector #:aref #:make-array
                #:quote #:function
                #:cond #:if #:case #:loop #:or #:and #:dotimes
                #:eq #:equal #:equalp #:not
                #:t #:nil
                #:defclass #:defgeneric #:defmethod #:type-of #:make-instance
                #:apply #:funcall
                #:first #:second #:rest #:car #:cdr
                #:list #:list* #:cons #:null #:reduce #:mapcar
                #:symbol-function #:intern
                #:print-object #:format
                #:coerce #:double-float)
  (:import-from :alexandria #:iota)
  (:use #:optima #:sym)
  (:export #:+ #:- #:* #:/
           #:sin #:cos #:tan #:asin #:acos #:atan
           #:exp #:log #:expt #:sqrt
           #:sinh #:cosh #:tanh
           #:= #:< #:> #:<= #:>=
           #:zerop #:plusp #:minusp
           #:literal-function
           #:literal-vector
           #:D #:diff #:gradient-f #:jacobian*vector #:jacobian*vector-save
           #:directional-derivative-f
           #:partial
           #:comp))
