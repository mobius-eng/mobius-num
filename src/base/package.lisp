(in-package cl-user)

(defpackage #:mobius.numeric.constants
  (:nicknames #:numeric-constants)
  (:use #:cl)
  (:export #:+infinity
           #:-infinity
           #:not-a-number
           #:infinity-p
           #:not-a-number-p))

;; have to by-pass the lock on CONTINUE
;; do not import: use it with ITERATOR: prefix
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
           #:continue-p
           #:finished-p
           #:failed-p
           #:add-info
           #:->continue
           #:->failed
           #:->finished
           #:replace-value))

(defpackage #:mobius.numeric.criteria
  (:use #:cl)
  (:nicknames #:criteria)
  (:import-from #:alexandria #:plist-alist)
  (:export #:base-criterium
           #:criterium-info
           #:criterium-function
           #:make-criterium
           #:finished-value-criterium
           #:failed-value-criterium
           #:log-criterium
           #:limit-iter-criterium
           #:modify-value-criterium
           #:make-criteria
           #:criteria-function
           #:criteria-seq
           #:croteria-list
           #:criteria))

(defpackage #:mobius.numeric.fixed-point
  (:nicknames #:fixed-point)
  (:use #:cl #:criteria)
  (:import-from #:alexandria #:circular-list)
  (:export #:fixed-point))

