(in-package cl-user)

(defpackage #:mobius.numeric.utils
  (:nicknames #:numeric-utils)
  (:use #:cl)
  (:export #:+infinity
           #:-infinity
           #:not-a-number))


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
  (:export #:make-criteria
           #:add-to-criteria
           #:delete-from-criteria
           #:compile-criterium
           #:in-criterium))

(defpackage #:mobius.numeric.fixed-point
  (:use #:cl #:criteria)
  (:export #:fixed-point))

