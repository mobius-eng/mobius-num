(in-package cl-user)

(defpackage #:mobius.numeric.constants
  (:nicknames #:numeric-constants)
  (:use #:cl)
  (:export #:+infinity
           #:-infinity)
  (:documentation
   "Basic mathematical and physical constants"))

(defpackage #:mobius.numeric.helpers
  (:nicknames #:numeric-helpers)
  (:use #:cl)
  (:export #:almost-zero-p
           #:quad-extremum
           #:cubic-extremum-1 #:cubic-extremum-2
           #:linear-approximation
           #:quad-approximation
           #:cubic-approximation)
  (:documentation
   "Simple helper functions on numbers"))

;; have to by-pass the lock on CONTINUE
;; do not import: use it with ITERATOR: prefix
(defpackage #:mobius.numeric.iterator
  (:use #:cl #:optima)
  (:nicknames #:iterator #:numeric-iterator)
  (:shadow #:continue)
  (:export #:iterator
           #:continue
           #:failed
           #:finished
           #:status
           #:value
           #:info
           #:continue-p
           #:finished-p
           #:failed-p
           #:add-info
           #:update-info
           #:->continue
           #:->failed
           #:->finished
           #:replace-value
           #:update-value
           #:bind)
  (:documentation
   "Computation flow"))

(defpackage #:mobius.numeric.control
  (:use #:cl #:optima)
  (:nicknames #:numeric-control)
  (:import-from #:alexandria #:make-keyword)
  (:export #:apply-control #:init-control
           #:define-simple-constructor
           #:finished-value
           #:failed-value
           #:limit-iterations
           #:log-computation
           #:log-to-info
           #:converged-value
           #:converged-number
           #:alter-value
           #:control #:control-init-function #:control-apply-function
           #:combine-controls)
  (:documentation
   "Computation flow control"))

(defpackage #:mobius.numeric.fixed-point
  (:nicknames #:numeric-fixed-point)
  (:use #:cl #:numeric-control)
  (:export #:fixed-point)
  (:documentation
   "Fixed point algorithm using ITERATOR and NUMERIC-CONTROL"))

