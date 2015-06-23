(in-package mobius.numeric.linear-operations)



;; *** Some odd-once

(defun identity-matrix (n)
  (if (zerop n)
      1.0d0
      (let ((a (make-array (list n n)
                           :element-type 'double-float
                           :initial-element 0.0d0)))
        (loop for i below n
           do (setf (aref a i i) 1.0d0))
        a)))


(defmethod compile-criterium ((type (eql :converged-norm)) &rest args)
  (let ((tolerance (first args))
        (info (second args)))
    (compile-criterium :converged
                       #'(lambda (x y) (< (norm (e- x y)) (* (norm x) tolerance)))
                       (list :converged-norm info))))

;; (defun linspace (start end &key length step)
;;   (cond (length (let ((step (/ (- end start) (1- length))))
;;                   (compute-mvector length
;;                                    (lambda (i) (+ start (* step i))))))
;;         (step (let ((length (1+ (truncate (/ (- end start) step)))))
;;                 (compute-mvector length
;;                                  (lambda (i) (+ start (* step i))))))
;;         (t (error "LINSPACE: You must provide either length or step"))))


(defmethod compile-criterium ((type (eql :converged)) &rest args)
  (let ((close-p (first args))
        (info (second args)))
    (in-criterium (x (last-v nil))
      (let ((v (iterator:value x)))
        (cond ((null last-v) (setf last-v v))
              ((funcall close-p last-v v)
               (iterator:add-info! (iterator:to-finished! x) :converged info))
              (t (setf last-v v)))))))
