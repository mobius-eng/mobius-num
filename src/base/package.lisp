(in-package cl-user)

(defpackage #:mobius.numeric.constants
  (:nicknames #:numeric-constants)
  (:use #:cl)
  (:export #:+infinity
           #:-infinity))

(defpackage #:mobius.numeric.helpers
  (:nicknames #:numeric-helpers)
  (:use #:cl)
  (:export #:almost-zero-p
           #:quad-extremum
           #:cubic-extremum-1 #:cubic-extremum-2
           #:linear-approximation
           #:quad-approximation
           #:cubic-approximation))

;; have to by-pass the lock on CONTINUE
;; do not import: use it with ITERATOR: prefix
(defpackage #:mobius.numeric.iterator
  (:use #:cl #:optima)
  (:nicknames #:iterator)
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
           #:bind))

(defpackage #:mobius.numeric.control
  (:use #:cl #:optima)
  (:nicknames #:control)
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
           #:combine-controls))

;; (defpackage #:mobius.numeric.criteria
;;   (:use #:cl)
;;   (:nicknames #:criteria)
;;   (:import-from #:alexandria #:plist-alist)
;;   (:export #:make-criteria
;;            #:get-criteria-function
;;            #:add-to-criteria
;;            #:delete-from-criteria
;;            #:criterium-arguments
;;            #:compile-criterium
;;            #:in-criterium))



(defpackage #:mobius.numeric.fixed-point
  (:nicknames #:fixed-point)
  (:use #:cl #:control)
  (:export #:fixed-point))

