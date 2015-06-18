(in-package cl-user)

;; have to by-pass the lock on CONTINUE
;; do not import: use it with ITERATOR: prefix
(defpackage #:mobius.numeric.utils
  (:use #:cl)
  (:export #:+NAN+
           #:++INF+
           #:+-INF+))


(defpackage #:mobius.numeric.iterator
  (:use #:cl)
  (:nicknames #:iterator)
  (:shadow #:continue)
  (:export #:continue
           #:failed
           #:finished
           #:status
           #:value
           #:info
           #:continue?
           #:finished?
           #:failed?
           #:update-info
           #:add-info!
           #:to-continue!
           #:to-failed!
           #:to-finished!
           #:replace-value!))

(defpackage #:mobius.numeric.criteria
  (:use #:cl)
  (:nicknames #:criteria)
  (:export #:make
           #:define-criterium
           #:finished-value
           #:failed-value
           #:log-value
           #:converged
           #:limit-iterations
           #:modify-value
           #:build))

(defpackage #:mobius.numeric.fixed-point
  (:use #:cl)
  (:export #:fixed-point))

