(in-package mobius.numeric.criteria)

(defun finished-value (predicate &optional info)
  (lambda (x)
    (if (funcall predicate (iterator:value x))
        (iterator:add-info! (iterator:to-finished! x)
                            'finidhed-value
                            info)
        x)))

(defun failed-value (predicate &optional info)
  (lambda (x)
    (if (funcall predicate (iterator:value x))
        (iterator:add-info! (iterator:to-failed! x)
                            'failed-value
                            info)
        x)))

(defun log-value (msg-fun)
  (lambda (x)
    (funcall msg-fun (iterator:value x))
    x))

(defun converged (close? &optional info)
  (let ((last-x nil))
    (lambda (x)
      (let ((v (iterator:value x)))
       (cond ((null last-x) (setf last-x v) x)
             ((funcall close? last-x v)
              (iterator:add-info! (iterator:to-finished! x)
                                  'converged
                                  info))
             (t (setf last-x v)
                x))))))

(defun limit-iterations (max-count)
  (let ((left max-count))
    (lambda (x)
      (decf left)
      (if (<= left 0)
          (iterator:add-info! (iterator:to-failed! x)
                              'limit-iterations
                              (format nil "Max count reached ~D" max-count))
          x))))

(defun build (&rest criteria)
  (lambda (x)
    (let ((y (iterator:continue x)))
      (loop for c in criteria
         if (iterator:continue? y)
           do (setf y (funcall c y))
         else
           return y
         end
         finally (return y)))))

(defun modify-argument (criteria pre-modifier post-modifier)
  (lambda (x)
    (let* ((modified-x (funcall modification x))
           (y (funcall criteria modified-x)))
      (setf (iterator:value y) (funcall post-modifier
                                        x
                                        (iterator:value y)) ))))
